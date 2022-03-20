{-# LANGUAGE OverloadedStrings #-}
module QuickRead.Theme where

import QuickRead.Types
import Brick
import Graphics.Vty.Attributes

styler :: Reader -> AttrMap
styler _ = attrMap defAttr [("a", fg white)]
