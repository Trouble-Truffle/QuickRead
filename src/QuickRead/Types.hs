{-# LANGUAGE TemplateHaskell #-}

module QuickRead.Types where

import Control.Concurrent.STM.TVar
import Control.Lens
import Data.Tape


data Tick = Tick
type Name = ()

data Reader = Reader {
    _fileQueue :: [FilePath] 
  , _textTape :: Maybe (Tape String)
  , _delay :: TVar Int
  , _wpm :: Double

  , _paused :: Bool

  , _progress :: Int -- In number of words
  -- TODO define save file
  , _themeIndex :: Int
  }
makeLenses ''Reader


