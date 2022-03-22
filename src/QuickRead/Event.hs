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


data Dir = L | R
data ExitType = LBoundary | EOF


eventHandler :: Reader -> BrickEvent Name Tick -> EventM Name (Next Reader)
eventHandler reader (AppEvent Tick)  =  liftIO (runMaybeT $ step reader R 1) >>= 
  (\case; Nothing -> exit reader; Just x -> continue x)

eventHandler reader (VtyEvent (EvKey (KChar 'q') [])) = halt reader

eventHandler reader (VtyEvent (EvKey KUp [])) = speedHandler reader (+) 1
eventHandler reader (VtyEvent (EvKey KUp [MShift])) = speedHandler reader (+) 10
eventHandler reader (VtyEvent (EvKey KUp [MCtrl])) = speedHandler reader (+) 0.1

eventHandler reader (VtyEvent (EvKey KDown [])) = speedHandler reader (-) 1
eventHandler reader (VtyEvent (EvKey KDown [MShift])) = speedHandler reader (-) 10
eventHandler reader (VtyEvent (EvKey KDown [MCtrl])) = speedHandler reader (-) 0.1

eventHandler reader (VtyEvent (EvKey KLeft [])) = spin reader L
-- TODO: add moving left in queue
--eventHandler reader (VtyEvent (EvKey KLeft [MCtrl])) = liftIO (runMaybeT $ advanceQueue reader L) >>= 
  --(\case; Nothing -> exit reader; Just x -> continue x)
eventHandler reader (VtyEvent (EvKey KRight[])) = spin reader R
eventHandler reader (VtyEvent (EvKey KRight[MCtrl])) = liftIO (runMaybeT $ advanceQueue reader R) >>= 
  (\case; Nothing -> exit reader; Just x -> continue x)

eventHandler reader (VtyEvent (EvKey (KChar ' ') [])) = continue $ reader & paused %~ not
eventHandler reader _ = continue reader


speedHandler :: Reader -> (Double -> Double -> Double) -> Double ->  EventM Name (Next Reader)
speedHandler reader (+/-) incrValue = do
  let newWpm = roundTo 1 $ clamp 0.001 9999999 $ (reader^.wpm) +/- incrValue

  liftIO $ atomically $ writeTVar (reader^.delayStop) True
  liftIO $ atomically $ writeTVar (reader^.delay) $ wpmToDur newWpm
  continue $ reader & wpm .~ newWpm

step :: Reader -> Dir -> Int -> MaybeT IO Reader
step reader dir steps = case reader ^. textTape of
  Just x  -> return $ 
    if reader ^. paused 
      then reader 
      else reader & textTape .~ 
        (case dir of 
            L -> \a b -> (\case -- So moves beyond beginning of file only moves it to first instead of throwing exceptions
                          Nothing -> maxMoveR <$> reader^.textTape; 
                          c -> c ) $ nMoveL a b
            R -> nMoveR) x steps
    
  Nothing -> do
    advanceQueue reader dir

spin :: Reader -> Dir -> EventM Name (Next Reader)
spin reader lr = liftIO (runMaybeT $ step (reader & paused .~ False) lr skipVal) >>= 
                  (\case
                  Nothing -> continue reader
                  Just x -> continue $ if wasPaused 
                                        then x & paused .~ True -- Make it so if reader is paused, moving doesnt unpause it
                                        else x)

  where skipVal = if reader^.paused  then 1  else 5 -- Move 1 space when paused, 5 when unpaused
        wasPaused = reader^.paused

-- Focus on a different file
advanceQueue :: Reader -> Dir -> MaybeT IO Reader
advanceQueue reader _todo = do -- TODO: allow moving backwards in queue
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
