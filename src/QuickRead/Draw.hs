module QuickRead.Draw where

import           QuickRead.Types

import           Brick                         as B

import           Control.Lens
import qualified Data.Foldable                 as F
import qualified Data.Tape                     as T

drawUI :: Reader -> [B.Widget Name]
drawUI reader = [maybe B.emptyWidget drawTape (reader ^. textTape)]

drawTape :: T.Tape String -> B.Widget Name
drawTape = go . T.take 10
 where
  go (T.Tape ls c rs) =
    F.foldr (<+>) B.emptyWidget (fmap (B.padLeft (Pad 1) . B.str) ls)
      <=> B.str c
      <=> F.foldr (<+>) B.emptyWidget (fmap (B.padLeft (Pad 1) . B.str) rs)
