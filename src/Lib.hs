{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    ( sortBy
    ) where

import           Data.Coerce     (coerce)
import           Data.Reflection
import           Sort

type CompareFn a = a -> a -> Ordering

newtype WithComparator a s = WithComparator a

{-# ANN module "HLint: ignore Redundant compare" #-}
instance Reifies s (CompareFn a) => Eq (WithComparator a s) where
  (==) x y = compare x y == EQ

instance Reifies s (CompareFn a) => Ord (WithComparator a s) where
  compare x'@(WithComparator x) (WithComparator y) = let cmp = reflect x' in cmp x y

sortBy :: CompareFn a -> [a] -> [a]
sortBy cmp xs = reify cmp (\proxy -> withoutComparators $ sort (withComparators proxy xs))

-- Equivalent to
-- map WithComparator
withComparators :: proxy s -> [a] -> [WithComparator a s]
withComparators _ = coerce

-- Equivalent to
-- map (\(WithComparator x) -> x)
withoutComparators :: [WithComparator a s] -> [a]
withoutComparators = coerce
