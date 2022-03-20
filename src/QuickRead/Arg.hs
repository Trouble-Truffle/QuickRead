{-# LANGUAGE OverloadedStrings #-}
module QuickRead.Arg where

import System.Console.GetOpt

import QuickRead.Types
import Data.Maybe
import Control.Lens

defaultOpt :: Options
defaultOpt = Options {
    _speed = 1
  , _conf = "config.json"
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option 
    ['s'] ["speed"]
    (OptArg
      ((\speedVal opts -> opts & speed .~ read speedVal) 
        . fromMaybe "1")
      "NUMBER"
    )
    "Control the speed of the reader"

  , Option
    ['c'] ["config"]
    (OptArg 
      ((\confLoc opts -> opts & conf .~ confLoc) 
        . fromMaybe (defaultOpt^.conf))
      "FILEPATH"
    )
    "The configuration file of the reader, default is \"~/.config/quickRead/config.json\"" 
  ]

compileOpts :: [String] -> IO (Options, [String])
compileOpts argv = case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOpt o
                         ,n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: [OPTIONS...] filename..."
