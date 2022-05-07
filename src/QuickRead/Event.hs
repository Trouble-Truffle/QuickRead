module QuickRead.Event where

import           Brick
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Except
import           Control.Concurrent.STM
import           Graphics.Vty hiding ((<|>))
import           QuickRead.Types               as QT
import           QuickRead.Util

import           QuickRead.Logic

-- TODO: Add dynamic keymappings
--
eventHandler :: Reader -> BrickEvent Name Tick -> EventM Name (Next Reader)
eventHandler reader (AppEvent Tick)  =  liftIO (runExceptT $ step reader R 1) >>= either exceptHandle continue

eventHandler reader (VtyEvent (EvKey (KChar 'q') [])) = exit reader

eventHandler reader (VtyEvent (EvKey KUp [])) = speedHandler reader (+) 1
eventHandler reader (VtyEvent (EvKey KUp [MShift])) = speedHandler reader (+) 10
eventHandler reader (VtyEvent (EvKey KUp [MCtrl])) = speedHandler reader (+) 0.1

eventHandler reader (VtyEvent (EvKey KDown [])) = speedHandler reader (-) 1
eventHandler reader (VtyEvent (EvKey KDown [MShift])) = speedHandler reader (-) 10
eventHandler reader (VtyEvent (EvKey KDown [MCtrl])) = speedHandler reader (-) 0.1

eventHandler reader (VtyEvent (EvKey KLeft [])) = spin reader L 
eventHandler reader (VtyEvent (EvKey KLeft [MCtrl])) = liftIO (runExceptT $ advanceQueue reader L ) >>= either exceptHandle  continue

eventHandler reader (VtyEvent (EvKey KRight[])) = spin reader R 
eventHandler reader (VtyEvent (EvKey KRight[MCtrl])) = liftIO (runExceptT $ advanceQueue reader R ) >>= either exceptHandle continue

eventHandler reader (VtyEvent (EvKey (KChar ' ') [])) = continue $ reader & paused %~ not

eventHandler reader _ = continue reader



speedHandler :: Reader -> (Double -> Double -> Double) -> Double ->  EventM Name (Next Reader)
speedHandler reader (+/-) incrValue = do
  let newWpm = roundTo 1 $ clamp 0.001 9999999 $ (reader^.wpm) +/- incrValue

  liftIO $ atomically $ writeTVar (reader^.delayStop) True
  liftIO $ atomically $ writeTVar (reader^.delay) $ wpmToDur newWpm
  continue $ reader & wpm .~ newWpm



