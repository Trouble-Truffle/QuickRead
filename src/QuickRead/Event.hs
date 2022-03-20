{-# LANGUAGE LambdaCase #-}
module QuickRead.Event where

import           Brick
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Concurrent.STM
import qualified Data.Sequence                 as S
import           Graphics.Vty
import           QuickRead.Types               as QT
import           QuickRead.Util
import           Data.Tape                     as T

eventHandler :: Reader -> BrickEvent Name Tick -> EventM Name (Next Reader)

eventHandler reader (AppEvent Tick)  =  liftIO (runMaybeT $ step reader) >>= 
  (\case; Nothing -> exit reader; Just x -> continue x)

eventHandler reader (VtyEvent (EvKey (KChar 'q') [])) = halt reader

eventHandler reader (VtyEvent (EvKey KUp [])) = speedHandler reader (+) 1
eventHandler reader (VtyEvent (EvKey KDown [])) = speedHandler reader (-) 1
eventHandler reader (VtyEvent (EvKey KUp [MShift])) = speedHandler reader (+) 10
eventHandler reader (VtyEvent (EvKey KDown [MShift])) = speedHandler reader (-) 10
eventHandler reader (VtyEvent (EvKey KUp [MCtrl])) = speedHandler reader (+) 0.1
eventHandler reader (VtyEvent (EvKey KDown [MCtrl])) = speedHandler reader (-) 0.1

eventHandler reader _ = continue reader


speedHandler :: Reader -> (Double -> Double -> Double) -> Double ->  EventM Name (Next Reader)
speedHandler reader (+/-) incrValue = do
  let newWpm = roundTo 1 $ clamp 0.001 9999999 $ (reader^.wpm) +/- incrValue

  liftIO $ atomically $ writeTVar (reader^.delay) $ wpmToDur newWpm
  continue $ reader & wpm .~ newWpm


step :: Reader -> MaybeT IO Reader
step reader = case reader ^. textTape of
  Just x  -> return $ reader & textTape .~ moveR x

  Nothing -> do
    (name, names) <- MaybeT $ return $ uncons (reader ^. fileQueue)
    file <- MaybeT $ toTape . words =<< readFile name

    return $ reader & textTape ?~ file 
                    & fileQueue .~ names

   where
    toTape [] = return Nothing
    toTape [_] = return Nothing
    toTape (y:ys) = return $ Just $ Tape S.empty y (S.fromList ys)

-- TODO: create exit messages
exit :: Reader -> EventM Name (Next Reader)
exit = error "TODO: create exit message at Event.hs"
