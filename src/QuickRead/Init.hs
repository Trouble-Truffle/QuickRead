module QuickRead.Init where

import Brick
import Control.Lens

import QuickRead.Types
import QuickRead.Util
import Data.Tape
import Control.Monad.IO.Class

startEvent :: Reader -> EventM Name Reader
startEvent reader = do
  case reader^.fileQueue of
    Nothing -> error "TODO: Handle empty file queue"
    Just x -> liftIO $ either (error "TODO: Handle readfile error") readFunc =<< safeReadFile (x^.focus)
  where

    readFunc str' = return $ reader & textTape .~ fromList (words str')


