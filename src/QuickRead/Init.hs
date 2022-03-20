module QuickRead.Init where

import Control.Concurrent.STM
import QuickRead.Types
import Data.Tape

initialize :: TVar Int -> [String] -> Maybe (Tape String) -> Reader
initialize tvar fileNames texts = 
  Reader {
    _fileQueue = fileNames
  , _textTape = texts

  , _delay = tvar
  , _wpm = 100
  , _paused = False
  , _progress = 0

  , _themeIndex = 0

  }
