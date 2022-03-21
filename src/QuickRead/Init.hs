module QuickRead.Init where

import Control.Concurrent.STM
import QuickRead.Types
import Data.Tape

defaultWpm :: Double
defaultWpm = 100


initialize :: TVar Int -> TVar Bool -> [String] -> Maybe (Tape String) -> Reader
initialize tvar unfreeze fileNames texts = 
  Reader {
    _fileQueue = fileNames
  , _textTape = texts

  , _delay = tvar
  , _delayStop = unfreeze
  , _wpm = defaultWpm
  , _paused = False
  , _progress = 0

  , _themeIndex = 0

  }
