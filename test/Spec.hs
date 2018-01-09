{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import           Lib
import           Sort
import           SortedList      (SortedList, binarySearch)
import qualified SortedList
import           Spec.Util
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

isSorted :: Ord a => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)

-- Should always be true for _all_ sorted lists.
-- Impossible to construct a SortedList for which this is false (without unsafety).
sortedListIsSorted :: Ord x => SortedList x -> Bool
sortedListIsSorted = isSorted . SortedList.toList

{-# ANN module "HLint: ignore Avoid reverse" #-}
{-# ANN module "HLint: ignore Use sort" #-}
spec :: Spec
spec = do
  describe "sort" $ do
    it "should return a sorted list" $ property $ \xs -> isSorted $ sort (xs :: [Integer])
    it "should sort an empty list" $ sort ([] :: [Integer]) `shouldBe` []
    it "should retain the same set of elements" $ property $ \xs -> not (null xs) ==> do
      let sorted = sort (xs :: [Integer])
      x <- oneof [elements xs, elements sorted]
      return $ filter (== x) sorted === filter (== x) xs
  describe "binarySearch" $ do
    it "should return the first occurrence when the element is present" $ property $
      \(Positive numOccurrences) x xs ->
        let l = SortedList.fromList (replicate numOccurrences x ++ xs :: [Integer])
            ul = SortedList.toList l
            i = binarySearch l x
        in ul !! i === x .&&. (i === 0 .||. ul !! (i - 1) < x)
    it "should return the insertion point when the element is absent" $ property $
      \x xs ->
        let l = SortedList.fromList (filter (/= x) xs :: [Integer])
            ul = SortedList.toList l
            i = binarySearch l x
        in (i === length ul .||. ul !! i > x) .&&. (i === 0 .||. ul !! (i - 1) < x)
    it "should need to inspect at most ceil(log_2 N) + 1 elements" $ property $
      \x xs ->
        let l = SortedList.fromList (xs :: [Integer])
            evElems = whichEvaluatedSorted l (`binarySearch` x)
        in length (filter id evElems) <= ceillog2 (length xs) + 1
  describe "reverseSort" $
    it "should be equivalent to (reverse . sort)" $ property $ \xs ->
      reverseSort (xs :: [Integer]) === reverse (sort xs)
  describe "largestN" $
    it "should be equivalent to dropping all but the last n" $ property $ \n xs ->
      largestN n (xs :: [Integer]) === reverse (drop (length xs - n) (sort xs))
  describe "sortBy" $ do
    it "should behave the same as sort" $ property $ \xs ->
      sortBy compare (xs :: [Integer]) === sort xs
    it "should be stable" $ property $ \xs -> not (null xs) ==> do
      let sorted = sortBy (\x y -> compare (fst x) (fst y)) (xs :: [(Integer, Integer)])
      x <- fst <$> oneof [elements xs, elements sorted]
      return $ filter ((== x) . fst) sorted === filter ((== x) . fst) xs

ceillog2 :: Int -> Int
ceillog2 = go 0
  where
    go !accum n
      | n <= 1 = accum
      | otherwise = go (accum + 1) ((n + 1) `quot` 2)
