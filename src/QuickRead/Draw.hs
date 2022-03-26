module QuickRead.Draw where

import qualified Data.Tape                     as T
import           QuickRead.Types

import           Brick                         as B
import           Brick.Widgets.Center          as B

import           Control.Lens
import qualified Data.Foldable                 as F
import           Text.Printf

import           Control.Concurrent.STM
import           System.IO.Unsafe

drawUI :: Reader -> [B.Widget Name]
drawUI reader =
  [ B.hCenter (str $ show $ reader ^. fileQueue)
      <=> textViewport
      <=> curWpm
      <=> speed
      <=> (if reader^.paused then B.str "Paused" else B.emptyWidget)
  ]
 where
  textViewport = drawTape (reader ^. textTape)
  curWpm       = str $ printf "%s WPM" $ show $ reader ^. wpm
  speed        = str $ show $ unsafePerformIO $ readTVarIO $ reader ^. delay

drawTape :: Maybe (T.Tape String) -> B.Widget Name
drawTape Nothing     = B.emptyWidget
drawTape (Just tape) = go $ T.take 10 tape
 where
  go (T.Tape ls c rs) =
    F.foldr (<+>) B.emptyWidget (fmap (B.padLeft (Pad 1) . B.str) ls)
      <=> B.str c
      <=> F.foldr (<+>) B.emptyWidget (fmap (B.padLeft (Pad 1) . B.str) rs)
