module QuickRead.Init where

import Control.Concurrent.STM
import QuickRead.Types
import Data.Tape

initialize :: TVar Int -> (Options, [String]) -> Maybe (Tape String) -> Reader
initialize tvar (opts, fileNames) texts = 
  Reader {
    _fileQueue = fileNames
  , _textTape = texts

  , _delay = tvar
  , _paused = False
  , _progress = 0

  , _themeIndex = 0

  , _appOpts = opts
  }
