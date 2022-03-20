{-# LANGUAGE TemplateHaskell, OverloadedLists #-}
module Data.Tape where

import Data.Sequence as S
import Control.Lens 

data Tape a = Tape {
    _viewL :: Seq a
  , _focus :: a
  , _viewR :: Seq a
} deriving (Show)

makeLenses ''Tape

instance Functor Tape where
  fmap f (Tape ls c rs) = Tape (fmap f ls) (f c) (fmap f rs)

-- | 1 2 [3] 4 -> 1 [2] 3 4
moveL, moveR :: Tape a -> Maybe (Tape a)
moveL (Tape ls c rs) = do
    (l S.:< ls' ) <- lSplit
    return $ Tape ls' l (c S.<| rs)
  where 
    lSplit = (\x -> case x of; EmptyL -> Nothing; _ -> Just x ) $ viewl ls

moveR (Tape ls c rs) = do
    (rs' S.:> r) <- rSplit
    return $ Tape (ls S.|> c)  r rs'
  where 
    rSplit = (\x -> case x of; EmptyR -> Nothing; _ -> Just x ) $ viewr rs


-- | Index left view of tape, indexing works as though sequence is flipped i.e [3,2,1,0]
(<!) :: Tape a -> Int -> Maybe a
(Tape l _ _) <! i = S.reverse l S.!? i

-- | Index right view of tape
(!>) :: Tape a -> Int -> Maybe a
(Tape _ _ r) !> i = r S.!? i

-- | 0 for focus, (-1..)  for left view, (1..) for right view
-- \(\mathcal(f)(x,i) { 
--  i <! x,   i < 0
--  x^.focus, i = 0
--  i !> x,   i > 0
-- }\)
(!?) :: Tape a -> Int -> Maybe a
tape !? i
  | i < 0 = tape <! pred (abs i)
  | i == 0 = Just $ tape ^. focus
  | otherwise = tape !> pred i

-- | take from both sides
-- >>> take 2 $ Tape [1,2,3,4] 5 [6,7,8,9]
-- Tape [3,4] 5 [6,7]
--
take :: Int -> Tape a -> Tape a
take 0 (Tape _ c _)  = Tape S.empty c S.empty 
take i (Tape ls c rs) = Tape (S.reverse $ S.take i $ S.reverse ls) c (S.take i rs)
