{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

module SortedList
    ( binarySearch
    , fromList
    , module SortedListI
    ) where

import qualified Data.Vector         as Vector
import           SortedList.Internal as SortedListI

fromList :: Ord a => [a] -> SortedList a
fromList xs0 = fst $ sortPrefix (Prelude.length xs0) xs0

sortPrefix :: Ord a => Int -> [a] -> (SortedList a, [a])
sortPrefix _ [] = (empty, [])
sortPrefix 1 (x : xs) = (singleton x, xs)
sortPrefix n xs =
  let
    halfn = n `quot` 2
    (lsorted, middleOn) = sortPrefix halfn xs
    (rsorted, rest) = sortPrefix (n - halfn) middleOn
  in
  (merge lsorted rsorted, rest)

binarySearch :: Ord a => SortedList a -> a -> Int
binarySearch xs0 = case toList xs0 of
    [] -> const 0
    xs@(x0 : _) -> searchX
      where
        v0 = Vector.fromList xs
        searchX x
          | x <= x0 = 0
          | otherwise = go v0 1
          where
            go v !result =
              let
                mid = Vector.length v `quot` 2
                (l, r) = Vector.splitAt mid v
              in if
                | mid == 0 -> result
                | x <= (r Vector.! 0) -> go l result
                | otherwise -> go r (result + mid)

-- fromListBy :: (a -> a -> Ordering) -> [a] -> SortedList a
-- fromListBy cmp xs = fst $ sortPrefixBy cmp (Prelude.length xs) xs

-- sortPrefixBy :: (a -> a -> Ordering) -> Int -> [a] -> (SortedList a, [a])
-- sortPrefixBy = sortPrefixBy cmp = ... $ mergeBy cmp
