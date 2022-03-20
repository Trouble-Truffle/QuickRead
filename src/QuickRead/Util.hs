module QuickRead.Util where

-- | Converts a wpm value to the correct amount of delay for `threadDelay`
-- 1 WPM == 60,000,000 Nanoseconds
wpmToDur :: Double -> Int
wpmToDur = floor . (*60000000) . recip

-- | Round to specified number of digits
roundTo :: Int -> Double -> Double
roundTo n f = fromInteger (round $ f * (10^n)) / (10.0^^n)

