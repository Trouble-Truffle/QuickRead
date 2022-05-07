{-# LANGUAGE TupleSections #-}
module QuickRead.Scroll.Text where

import           Control.Lens                   ( (&)
                                                , (?~)
                                                , (^.)
                                                )

import           Data.Tape
import           QuickRead.Types

data TextState = Normal | EOF | SOF | Empty

spin :: Reader -> Direction -> (Reader, TextState)
spin reader direction = case reader ^. textTape of
  Nothing   -> (reader, Empty)
  Just tape -> case move tape of
    Nothing    -> (reader, textEndError)
    Just tape' -> (reader & textTape ?~ tape', Normal)
 where
  (textEndError, move) = case direction of
    L -> (SOF, ) $ if not (reader ^. paused) && reader ^. manualAction
      then (`nMoveL` 5)
      else moveL
    R -> (EOF, ) $ if not (reader ^. paused) && reader ^. manualAction
      then (`nMoveR` 5)
      else moveR
    -- Manual actions moves by 5 when unpaused
