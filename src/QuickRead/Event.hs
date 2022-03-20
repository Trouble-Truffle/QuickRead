{-# LANGUAGE LambdaCase #-}
module QuickRead.Event where

import           Brick
import           Control.Lens
import           Control.Monad.IO.Class
import          Control.Monad.Trans.Maybe
import qualified Data.Sequence                 as S
import           Graphics.Vty
import           QuickRead.Types               as QT
import Data.Tape as T

eventHandler :: Reader -> BrickEvent Name Tick -> EventM Name (Next Reader)

eventHandler game (AppEvent Tick)  =  liftIO (runMaybeT $ step game) >>= 
  (\case; Nothing -> exit game; Just x -> continue x)

eventHandler game (VtyEvent (EvKey (KChar 'q') [])) = halt game
eventHandler game _ = continue game

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
