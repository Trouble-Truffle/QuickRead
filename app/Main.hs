module Main where

import qualified Brick                         as B
import qualified Brick.BChan                   as BC
import qualified Control.Concurrent.STM.TVar   as TV
import qualified Graphics.Vty                  as V

import           Control.Lens                   ( (^.) )

import           Control.Concurrent
import           Control.Monad
import           GHC.Conc.Sync
import           System.Directory
import           System.Environment

import qualified Data.Tape                     as T
import qualified QuickRead.Arg                 as QA
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
            , B.appStartEvent   = QI.startEvent
            , B.appAttrMap      = QTH.styler
            }

-- TODO: Create a saving method upon exit
saveState :: QT.Reader -> IO ()
saveState _ = print "program end"


-- | Process program speed and speed changes
-- When at low wpm there is a delay in speed changes,
-- this stops any ongoing delay on speed changes.
programDelay :: IO Int -> TV.TVar Bool -> IO ()
programDelay delay speedChange = delay >>= go . (`div` 100000) -- Divide into 100ms delays
 where
  go 0 = return ()
  go n = do
    unfrozen <- TV.readTVarIO speedChange
    if unfrozen && n > 20 -- Cancel any delay >= 2 seconds
      then atomically $ TV.writeTVar speedChange False
      else threadDelay 100000 >> go (n - 1)


main :: IO ()
main = do

  chan   <- BC.newBChan 10
  delay  <- TV.newTVarIO $ QU.wpmToDur 120
  change <- TV.newTVarIO False -- Mainly for instantly changing value on high delays

  void $ forkIO $ forever $ do
    BC.writeBChan chan QT.Tick
    programDelay (TV.readTVarIO delay) change

  confLocation  <- getXdgDirectory XdgConfig "quickRead/config.json"
  cacheLocation <- getXdgDirectory XdgCache "quickRead"
  (opts, files) <- getArgs >>= QA.compileOpts
    (QA.Args { QA._conf  = confLocation
             , QA._cache = cacheLocation
             , QA._wpm   = 120
             }
    )

  let initialState = QT.Reader { QT._textTape     = Nothing
                               , QT._fileQueue    = T.fromList files
                               , QT._manualAction = False
                               , QT._delay        = delay
                               , QT._delayStop    = change
                               , QT._wpm          = opts ^. QA.wpm
                               , QT._paused       = True
                               , QT._finished     = False
                               , QT._lastErr      = QT.None
                               , QT._themeIndex   = 0
                               }

  let mVty = V.mkVty V.defaultConfig
  vty <- mVty

  B.customMain vty mVty (Just chan) app initialState >>= saveState
