-- | Control movement in the fileQueue
module QuickRead.Scroll.File where

import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (^.)
                                                , (?~)
                                                )

import           Data.Bifunctor
import           Data.Tape
import           QuickRead.Types
import           QuickRead.Util

-- EOQ = End of Queue
data QueueState = SOQ | EOQ | Normal | Empty | ReadError IOError

advanceQueue :: Reader -> Direction -> IO (Reader, QueueState)
advanceQueue reader direction = case reader ^. fileQueue of
  Nothing    -> return (reader, Empty)
  Just queue -> case move queue of
    Nothing -> return (reader, queueEndError)
    Just queue' ->
      first (\x -> reader & textTape .~ x & fileQueue ?~ queue' ) . fileToTape <$> safeReadFile
        (queue' ^. focus)

   where
    (queueEndError, move) = case direction of
      L -> (SOQ, moveL)
      R -> (EOQ, moveR)

fileToTape :: Either IOError String -> (Maybe (Tape String), QueueState)
fileToTape (Left  err) = (Nothing, ReadError err)
fileToTape (Right a  ) = (fromList $ words a, Normal)
