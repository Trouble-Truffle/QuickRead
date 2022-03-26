module QuickRead.Logic where

import           QuickRead.Types
import           QuickRead.Util

import           Data.Tape

import           Brick
import           Control.Lens

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe

data Dir = L | R

data ExitType = LBoundary | EOF

step :: Reader -> Dir -> Int -> MaybeT IO Reader
step reader dir steps = case reader ^. textTape of
  Just x -> return $ applyWhen
    (not $ reader ^. paused)
    (  textTape
    .~ (case dir of
         L -> nMoveL x steps <|> (maxMoveR <$> reader ^. textTape)
         -- So moves beyond beginning of file only moves it to first instead of throwing exceptions
         R -> nMoveR x steps
       )
    )
    reader

  Nothing -> do
    advanceQueue reader dir

spin :: Reader -> Dir -> EventM Name (Next Reader)
spin reader lr =
  liftIO (runMaybeT $ step (reader & paused .~ False) lr skipVal)
    >>= continue
    .   maybe reader (applyWhen (reader ^. paused) (& paused .~ True))
            -- Make it so if reader is paused, moving doesnt unpause it
  where skipVal = if reader ^. paused then 1 else 5 -- Move 1 space when paused, 5 when unpaused

-- Focus on a different file
--
advanceQueue :: Reader -> Dir -> MaybeT IO Reader
advanceQueue reader lr = do
  newQueue <- MaybeT $ return $ (shift' =<< (reader ^. fileQueue)) <|> (reader ^. fileQueue)
    -- To stop movement in queue when going past beginning or end
    -- TODO: handling of movement past the last item of queue
  
  file     <- MaybeT . return . fromList . words =<< safeReadFile 
    (newQueue ^. focus)

  return $ reader & textTape ?~ file & fileQueue ?~ newQueue
 where
  shift' = case lr of
    L -> moveL
    R -> moveR
