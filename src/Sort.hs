module Sort
    ( sort
    , reverseSort
    , smallestN
    , largestN
    , sortLast3Digits
    , sortModulo17
    ) where

import           Data.Coerce
import           Data.Ord
import qualified SortedList

sort :: Ord a => [a] -> [a]
sort = SortedList.toList . SortedList.fromList

smallestN :: Ord a => Int -> [a] -> [a]
smallestN n = take n . sort


-- reverseSort :: Ord a => [a] -> [a]
-- reverseSort = reverse . sort

-- largestN :: Ord a => Int -> [a] -> [a]
-- largestN n xs = reverse $ drop (length xs - n) $ sort xs

reverseSort :: Ord a => [a] -> [a]
reverseSort = (coerce :: [Down a] -> [a]) . sort . (coerce :: [a] -> [Down a])

largestN :: Ord a => Int -> [a] -> [a]
largestN n = take n . reverseSort


newtype Last3Digits = Last3Digits Integer

{-# ANN module "HLint: ignore Redundant compare" #-}
instance Eq Last3Digits where
  (==) x y = compare x y == EQ
instance Ord Last3Digits where
  compare (Last3Digits x) (Last3Digits y) = compare (x `mod` 1000) (y `mod` 1000)

sortLast3Digits :: [Integer] -> [Integer]
sortLast3Digits =
  (coerce :: [Last3Digits] -> [Integer]) . sort . (coerce :: [Integer] -> [Last3Digits])


newtype Modulo17 = Modulo17 Integer

instance Eq Modulo17 where
  (==) x y = compare x y == EQ
instance Ord Modulo17 where
  compare (Modulo17 x) (Modulo17 y) = compare (x `mod` 17) (y `mod` 17)

sortModulo17 :: [Integer] -> [Integer]
sortModulo17 =
  (coerce :: [Last3Digits] -> [Integer]) . sort . (coerce :: [Integer] -> [Last3Digits])


-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- sortBy cmp = SortedList.toList . SortedList.fromListBy cmp
