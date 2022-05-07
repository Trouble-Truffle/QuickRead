{-# LANGUAGE TemplateHaskell #-}

module QuickRead.Types where

import Control.Concurrent.STM.TVar
import Control.Lens
import Data.Tape


data Tick = Tick
type Name = ()
data Direction = L | R

data ReaderError = EmptyFile 
                 | EmptyQueue 
                 | LBorder
                 | RBorder
                 | IOErr IOError
                 | Debug String
                 | None
                 deriving (Show)

data Reader = Reader {
    _fileQueue :: Maybe (Tape FilePath)
  , _textTape :: Maybe (Tape String)
  , _manualAction :: Bool

  , _delay :: TVar Int
  , _delayStop :: TVar Bool

  , _wpm :: Double

  , _paused :: Bool

  , _finished :: Bool -- In number of words
  , _lastErr :: ReaderError
  -- TODO define save file
  , _themeIndex :: Int
  }
makeLenses ''Reader

type ErrorState = Reader
-- ^ A type alias for Left Eithers
