{-# LANGUAGE OverloadedStrings#-}
module QuickRead.Draw where

import qualified Data.Tape                     as T
import           QuickRead.Types

import           Brick                         as B
import           Brick.Widgets.Border          as BB
import           Brick.Widgets.Center          as BC
import           Brick.Widgets.ProgressBar     as BP

import           Control.Lens
import qualified Data.Foldable                 as F
import qualified Data.Sequence                 as S
import           Text.Printf

import           Control.Concurrent.STM
import           System.IO.Unsafe

drawUI :: Reader -> [B.Widget Name]
drawUI reader =
  [ BC.hCenter (str $ show $ reader ^. fileQueue)
      <=> textViewport
      <=> curWpm
      <=> speed
      <=> drawProgress reader
      <=> (if reader ^. paused then B.str "Paused" else B.emptyWidget)
  ]
 where
  textViewport = drawConts (reader ^. textTape)
  curWpm       = str $ printf "%s WPM" $ show $ reader ^. wpm
  speed        = str $ show $ unsafePerformIO $ readTVarIO $ reader ^. delay

drawConts :: Maybe (T.Tape String) -> B.Widget Name
drawConts Nothing     = B.emptyWidget
drawConts (Just tape) = BB.border $ go $ T.take 10 tape
 where
  go (T.Tape ls c rs) = B.vBox $ map
    BC.hCenter
    [ F.foldr (<+>) (B.str " ") (fmap (B.padLeft (Pad 1) . B.str) ls)
    , drawHighlight c
    , F.foldr (<+>) (B.str " ") (fmap (B.padLeft (Pad 1) . B.str) rs)
    ]

drawProgress :: Reader -> B.Widget Name
drawProgress reader = BB.border bar
 where
  prog = case reader ^. textTape of
    Just (T.Tape l _ r) ->
      fromIntegral (succ $ S.length l) / succ
        (sum $ map (fromIntegral . S.length) [l, r])
    Nothing -> 1

  bar = B.updateAttrMap
    (B.mapAttrNames [("TEST", BP.progressCompleteAttr),
                     ("Highlight", BP.progressIncompleteAttr)]) $
    BP.progressBar Nothing prog

drawHighlight :: String -> B.Widget Name
drawHighlight []     = B.emptyWidget
drawHighlight [x, y] = B.str [x] <+> B.withAttr "Highlight" (B.str [y])
drawHighlight xs     = case uncons mr of
  Nothing      -> B.emptyWidget
  Just (c, rs) -> B.str ls <+> B.withAttr "Highlight" (B.str [c]) <+> B.str rs
  where (ls, mr) = splitAt (length xs `div` 2) xs
