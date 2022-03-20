{-# LANGUAGE TemplateHaskell #-}

module QuickRead.Types where

import Control.Concurrent.STM.TVar
import Control.Lens
import Data.Tape


data Tick = Tick
type Name = ()

data Options = Options {
    _speed :: Float
  , _conf :: FilePath
  }
makeLenses ''Options

data Reader = Reader {
    _files :: [FilePath] 
  , _textTape :: Maybe (Tape String)
  , _delay :: TVar Int

  , _paused :: Bool

  , _progress :: Int -- In number of words
  -- TODO define save file
  --, _theme :: AttrMap

  , _appOpts :: Options

  }
makeLenses ''Reader


