module QuickRead.Draw where

import QuickRead.Types
import Brick
import Control.Lens
import Data.Tape

drawUI :: Reader -> [Widget Name]
drawUI reader = [str $ show $ (^.focus) <$> reader^.textTape]
