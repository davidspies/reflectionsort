module SortedList.Internal
    ( SortedList
    , empty
    , toList
    , merge
    -- , mergeBy
    , singleton
    ) where

newtype SortedList a = SortedList [a]
  deriving (Eq, Show)

toList :: SortedList a -> [a]
toList (SortedList xs) = xs

empty :: SortedList a
empty = SortedList []

singleton :: a -> SortedList a
singleton x = SortedList [x]

merge :: Ord a => SortedList a -> SortedList a -> SortedList a
merge (SortedList xs) (SortedList ys) = SortedList $ mergeLists compare xs ys

mergeLists :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeLists cmp = go
  where
    go [] ys = ys
    go xs [] = xs
    go xs@(x : xs') ys@(y : ys')
      | y `cmp` x == LT = y : go xs ys'
      | otherwise = x : go xs' ys

-- mergeBy :: (a -> a -> Ordering) -> SortedList a -> SortedList a -> SortedList a
-- mergeBy cmp (SortedList xs) (SortedList ys) = SortedList $ mergeLists cmp xs ys
