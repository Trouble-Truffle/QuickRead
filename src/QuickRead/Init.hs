module QuickRead.Init where

import Brick
import Control.Lens

import QuickRead.Types
import QuickRead.Util
import Data.Tape
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

startEvent :: Reader -> EventM Name Reader
startEvent reader = do
  case reader^.fileQueue of
    Nothing -> startEvent reader
    Just x -> liftIO $ maybe (error "TODO: Error on empty file") (readFunc reader) =<< runMaybeT (safeReadFile $ x^.focus)
  where
    readFunc reader' str' = return $ reader' & textTape .~ fromList (words str')


