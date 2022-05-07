-- | Controls state changes (scrolling, file switching) and handles any errors caused by such
module QuickRead.Logic where

import           QuickRead.Types
import           QuickRead.Util

import           Data.Tape

import           Brick
import           Control.Lens

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

data Dir = L | R

step :: Reader -> Dir -> Int -> ExceptT Reader IO Reader
step reader dir steps = case reader ^. textTape of
  Just x -> return $ applyWhen
    (not $ reader ^. paused) -- Only step when reader is unpaused
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
  liftIO (runExceptT $ step (reader & paused .~ False) lr skipVal)
    >>= either exceptHandle
               (continue . applyWhen (reader ^. paused) (& paused .~ True))
            -- Make it so if reader is paused, moving doesnt unpause it
  where skipVal = if reader ^. paused then 1 else 5 -- Move 1 space when paused, 5 when unpaused


-- Focus on a different file
--
advanceQueue :: Reader -> Dir -> ExceptT ErrorState IO Reader
advanceQueue reader lr = do
  newQueue <- ExceptT . return $ maybe moveErrorState
                                       Right
                                       (shift' =<< (reader ^. fileQueue))

  file <-
    ExceptT
    $ (fmap . fmap)
        (either (\x -> Left $ reader & lastErr .~ IOErr x)
                (maybe readErrorState Right . fromList . words)
        )
        runExceptT
    $ safeReadFile (newQueue ^. focus)

  return $ reader & textTape ?~ file & fileQueue ?~ newQueue
  --
 where
  readErrorState           = Left $ reader & lastErr .~ EmptyFile

  (shift', moveErrorState) = case lr of
    L -> (moveL, Left $ reader & lastErr .~ LBorder)
    R -> (moveR, Left $ reader & lastErr .~ RBorder)


exceptHandle :: Reader -> EventM Name (Next Reader)
exceptHandle = error "TODO: handle error states"


-- TODO: create exit messages
exit :: Reader -> EventM Name (Next Reader)
exit = error "TODO: create exit message at Event.hs"
