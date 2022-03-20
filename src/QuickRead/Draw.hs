module QuickRead.Draw where

import           QuickRead.Types

import           Brick                         as B
import Brick.Widgets.Center as B

import           Control.Lens
import qualified Data.Foldable                 as F
import qualified Data.Tape                     as T

drawUI :: Reader -> [B.Widget Name]
drawUI reader = 
  [
  B.hCenter ( str $ show $ reader^.fileQueue) 
  <=> viewport
  ]
  where
    viewport = drawTape (reader ^. textTape)

drawTape :: Maybe (T.Tape String) -> B.Widget Name
drawTape Nothing = B.emptyWidget
drawTape (Just tape) = go $ T.take 10 tape
 where
  go (T.Tape ls c rs) =
    F.foldr (<+>) B.emptyWidget (fmap (B.padLeft (Pad 1) . B.str) ls)
      <=> B.str c
      <=> F.foldr (<+>) B.emptyWidget (fmap (B.padLeft (Pad 1) . B.str) rs)
