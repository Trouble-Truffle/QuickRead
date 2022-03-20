{-# LANGUAGE LambdaCase #-}
module QuickRead.Event where

import           Brick
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Concurrent.STM
import qualified Data.Sequence                 as S
import           Graphics.Vty
import           QuickRead.Types               as QT
import           Data.Tape                     as T

eventHandler :: Reader -> BrickEvent Name Tick -> EventM Name (Next Reader)

eventHandler reader (AppEvent Tick)  =  liftIO (runMaybeT $ step reader) >>= 
  (\case; Nothing -> exit reader; Just x -> continue x)

eventHandler reader (VtyEvent (EvKey (KChar 'q') [])) = halt reader

eventHandler reader (VtyEvent (EvKey KUp [])) = speedHandler reader (-)
eventHandler reader (VtyEvent (EvKey KDown [])) = speedHandler reader (+)

eventHandler reader _ = continue reader


speedHandler :: Reader -> (Int -> Int -> Int) -> EventM Name (Next Reader)
speedHandler reader (+/-) = do
  newSpeed <- liftIO $ (+/-1000) <$> readTVarIO ( reader ^. delay)
  when (newSpeed > 1000)
    $ liftIO $ atomically $ writeTVar (reader^.delay) newSpeed
  continue reader


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
