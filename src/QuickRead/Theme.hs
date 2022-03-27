module QuickRead.Theme where

import QuickRead.Types
import Brick
import Graphics.Vty.Attributes
import Data.Bifunctor

data Styles =
  Highlight 
  | TEST
  deriving Show

-- TODO: Create template haskell for automatically making functions from styles

styler :: Reader -> AttrMap
styler _ = attrMap defAttr $ map (first $ attrName . show) 
  [(Highlight, brightYellow `on` black)
  ,(TEST , brightBlue `on` blue)]
