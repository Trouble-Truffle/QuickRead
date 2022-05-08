module QuickRead.StartMenu.Start where

import Brick

type Tick = ()
type Name = ()

data StartInfo = StartInfo {
    testVal1 :: Int
  , testVal2 :: Int
  }

startApp :: App StartInfo Tick Name
startApp = App {
    appDraw = undefined
  , appChooseCursor = neverShowCursor
  , appHandleEvent = undefined
  , appStartEvent = return
  , appAttrMap = undefined
  }

startMenu :: IO StartInfo
startMenu = defaultMain startApp $
  StartInfo {
      testVal1 = 1
    , testVal2 = 2
    }
