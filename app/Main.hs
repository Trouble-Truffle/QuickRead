module Main where

import qualified Brick                         as B
import qualified Brick.BChan                   as BC
import qualified Control.Concurrent.STM.TVar   as TV
import qualified Graphics.Vty                  as V

import           Control.Concurrent
import           Control.Monad
import           System.Directory

import qualified QuickRead.Draw                as QD
import qualified QuickRead.Event               as QE
import qualified QuickRead.Init                as QI
import qualified QuickRead.Theme               as QTH
import qualified QuickRead.Types               as QT


app :: B.App QT.Reader QT.Tick QT.Name
app = B.App { B.appDraw         = QD.drawUI
            , B.appChooseCursor = B.neverShowCursor
            , B.appHandleEvent  = QE.eventHandler
            , B.appStartEvent   = return
            , B.appAttrMap      = QTH.styler
            }

saveState :: QT.Reader -> IO ()
saveState _ = print "program end"

main :: IO ()
main = do

  chan      <- BC.newBChan 10
  tvar      <- TV.newTVarIO 100000
  --tvar <- TV.newTVarIO 10

-- TODO: temporary for faster debugging
  filepath  <- (++ "/.local/src/quickRead/LoremIpsum.txt") <$> getHomeDirectory
  filepath2 <- (++ "/.local/src/quickRead/APickle.txt") <$> getHomeDirectory
  let filenames = [filepath, filepath2]

  void $ forkIO $ forever $ do
    BC.writeBChan chan QT.Tick
              -- Prevents large values from suddenly stopping the program by placing a cap
    TV.readTVarIO tvar
      >>= \x -> if x > 3000000 then threadDelay 3000000 else threadDelay x

  let mVty = V.mkVty V.defaultConfig
  vty <- mVty

  B.customMain vty mVty (Just chan) app (QI.initialize tvar filenames Nothing)
    >>= saveState
