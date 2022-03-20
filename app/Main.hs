module Main where

import qualified Brick.BChan as BC
import qualified Brick as B
import qualified Control.Concurrent.STM.TVar as TV
import qualified Graphics.Vty as V

import System.Directory
import Control.Monad
import Control.Concurrent

import qualified QuickRead.Arg as QA
import qualified QuickRead.Event as QE
import qualified QuickRead.Types as QT
import qualified QuickRead.Theme as QTH
import qualified QuickRead.Init as QI
import qualified QuickRead.Draw as QD


app :: B.App QT.Reader QT.Tick QT.Name
app = B.App {
    B.appDraw = QD.drawUI
  , B.appChooseCursor = B.neverShowCursor 
  , B.appHandleEvent = QE.eventHandler
  , B.appStartEvent = return
  , B.appAttrMap = QTH.styler
  }

saveState :: QT.Reader -> IO ()
saveState _ = print "program end"

main :: IO ()
main = do

  chan <- BC.newBChan 10
  tvar <- TV.newTVarIO 100000

-- TODO: temporary for faster debugging
  filepath <- (++"/.local/src/quickRead/LoremIpsum.txt") <$> getHomeDirectory
  filepath2 <- (++"/.local/src/quickRead/APickle.txt") <$> getHomeDirectory
  args <- QA.compileOpts [filepath,filepath2] 
  
  void $ forkIO $ forever $ do
    BC.writeBChan chan QT.Tick
    TV.readTVarIO tvar  >>= threadDelay

  let mVty = V.mkVty V.defaultConfig
  vty <- mVty

  B.customMain vty mVty (Just chan) app (QI.initialize tvar args Nothing ) >>= saveState 




