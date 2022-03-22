module Main where

import qualified Brick                         as B
import qualified Brick.BChan                   as BC
import qualified Control.Concurrent.STM.TVar   as TV
import qualified Graphics.Vty                  as V

import           Control.Concurrent
import           GHC.Conc.Sync
import           Control.Monad
import           System.Directory

import qualified QuickRead.Draw                as QD
import qualified QuickRead.Event               as QE
import qualified QuickRead.Init                as QI
import qualified QuickRead.Theme               as QTH
import qualified QuickRead.Types               as QT
import qualified QuickRead.Util                as QU


app :: B.App QT.Reader QT.Tick QT.Name
app = B.App { B.appDraw         = QD.drawUI
            , B.appChooseCursor = B.neverShowCursor
            , B.appHandleEvent  = QE.eventHandler
            , B.appStartEvent   = return
            , B.appAttrMap      = QTH.styler
            }

-- TODO: Create a saving method upon exit
saveState :: QT.Reader -> IO ()
saveState _ = print "program end"


-- | When at low wpm there is a delay in speed changes,
-- this stops any ongoing delay on speed changes.
programDelay :: IO Int -> TV.TVar Bool -> IO ()
programDelay delay speedChange = delay >>= go . (`div` 100000) -- Divide into 100ms delays
  where
    go 0 = return ()
    go n = do
          unfrozen <- TV.readTVarIO speedChange
          if unfrozen && n > 20 -- Cancel any delay >= 2 seconds
            then atomically $ TV.writeTVar speedChange False 
            else threadDelay 100000 >> go (n-1)

main :: IO ()
main = do

  chan      <- BC.newBChan 10
  delay <- TV.newTVarIO $ QU.wpmToDur QI.defaultWpm
  change <- TV.newTVarIO False -- For instantly changing value on high delays

-- TODO: temporary for faster debugging
  filepath  <- (++ "/.local/src/quickRead/LoremIpsum.txt") <$> getHomeDirectory
  filepath2 <- (++ "/.local/src/quickRead/APickle.txt") <$> getHomeDirectory
  let filenames = [filepath, filepath2]

  void $ forkIO $ forever $ do
    BC.writeBChan chan QT.Tick
    programDelay (TV.readTVarIO delay) change

  let mVty = V.mkVty V.defaultConfig
  vty <- mVty

  B.customMain vty mVty (Just chan) app (
    QI.initialize 
      delay change filenames Nothing)
    >>= saveState
