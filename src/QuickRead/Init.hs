module QuickRead.Init where

import Brick
import Control.Lens

import QuickRead.Types
import QuickRead.Util
import Data.Tape
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

startEvent :: Reader -> EventM Name Reader
startEvent reader = do
  case reader^.fileQueue of
    Nothing -> return emptyQueue
    Just x -> liftIO $ either errorReader readFunc =<< runExceptT (safeReadFile $ x^.focus)
  where

    readFunc str' = return $ reader & textTape .~ fromList (words str')
    errorReader x = return $ reader & lastErr .~ Debug (show x)
    emptyQueue = reader & lastErr .~ EmptyFile
    -- ^ Changes textTape with a string applied with `words`


