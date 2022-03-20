module QuickRead.Event where

import           Brick
import           Control.Lens
import        Control.Applicative as A
import Control.Monad.IO.Class
import qualified Data.Sequence                 as S
import           Graphics.Vty
import           QuickRead.Types               as QT
import Data.Tape as T

eventHandler :: Reader -> BrickEvent Name Tick -> EventM Name (Next Reader)
eventHandler game (VtyEvent (EvKey (KChar 'q') [])) = halt game
eventHandler game _  =  liftIO (step game) >>= continue 

step :: Reader -> IO Reader
step reader = case reader ^. textTape of
  Just x  -> return $ reader & textTape .~ moveR x

  Nothing -> do
    (name, names) <- maybe mempty return x
    file <- toTape . words =<< readFile name

    return $ reader & textTape ?~ file 
                    & files .~ names

   where
    toTape [] = A.empty
    toTape [_] = A.empty
    toTape (y:ys) = return $ Tape S.empty y (S.fromList ys)

    x = uncons (reader ^. files)
