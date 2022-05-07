{-# LANGUAGE TemplateHaskell #-}
module QuickRead.Arg where

import           Control.Lens
import           Data.Maybe
import           QuickRead.Util
import           System.Console.GetOpt


data Args = Args
  { _conf  :: FilePath
  , _cache :: FilePath
  , _wpm   :: Double
  }
makeLenses ''Args

options :: Args -> [OptDescr (Args -> Args)]
options defaultOpts =
  [ Option
    ['c']
    ["config"]
    (ReqArg
      (\input opts -> opts & conf .~  input)
      "FILEPATH"
    )

    (unlines
      [ "The configuration file of the reader. Most config options are available as program arguments."
      , "Program arguments will take precedence. Default is: '"
      ++ (defaultOpts ^. conf)
      ++ "'."
      ]
    )
  , Option
    []
    ["cache"]
    (ReqArg
      (\input opts -> opts & conf .~ input)
      "FILEPATH"
    )

    (unlines
      [ "Cache location of the reader, runtime info (recently opened, progress) will be stored in that location."
      , "Default is: '" ++ (defaultOpts ^. cache) ++ "'."
      ]
    )
  , Option
    ['w']
    ["wpm"]
    (ReqArg (\input opts -> opts & wpm .~ fromMaybe (error "UserError: Invalid float in WPM argument") (safeRead input))
            "FLOAT"
    )
    "Speed in WPM"
  ]

compileOpts :: Args -> [String] -> IO (Args, [String])
compileOpts defaultOpt argv = case getOpt Permute options' argv of
  (o, n, []) -> return (foldl (flip id) defaultOpt o, n)
  (_, _, errs) ->
    ioError (userError (concat errs ++ usageInfo header options'))
 where
  options' = options defaultOpt
  header   = "Usage: [OPTIONS...] filename..."
