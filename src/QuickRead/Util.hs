module QuickRead.Util where

import System.IO.Error
import Control.Monad.Trans.Maybe
-- | Converts a wpm value to the correct amount of delay for `threadDelay`
-- 1 WPM == 60,000,000 Nanoseconds
wpmToDur :: Double -> Int
wpmToDur = floor . (*60000000) . recip

-- | Round to specified number of digits
roundTo :: Int -> Double -> Double
roundTo n f = fromInteger (round $ f * (10^n)) / (10.0^^n)

-- | Apply a function if a condition is met
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
applyWhen False _ = id

-- | Returns a maybe type instead of exception on failure
safeReadFile :: FilePath -> MaybeT IO String
safeReadFile file = MaybeT $ either (const Nothing) Just <$> tryIOError (readFile file)
