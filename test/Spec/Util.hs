module Spec.Util
    ( whichEvaluated
    , whichEvaluatedSorted
    ) where

import           Control.Exception (evaluate)
import           Control.Monad     (void)
import           Data.IORef
import           SortedList        (SortedList)
import qualified SortedList
import           System.IO.Unsafe  (unsafePerformIO)
import           Unsafe.Coerce     (unsafeCoerce)

whichEvaluated :: [a] -> ([a] -> b) -> [Bool]
whichEvaluated xs f = unsafePerformIO $ do
  bs <- mapM (const $ newIORef False) xs
  let xsu = [unsafePerformIO (writeIORef b True >> return x) | (x, b) <- zip xs bs]
  void $ evaluate $ f xsu
  mapM readIORef bs

whichEvaluatedSorted :: SortedList a -> (SortedList a -> b) -> [Bool]
whichEvaluatedSorted xs f = whichEvaluated (SortedList.toList xs) (f . unsafeSortedList)

unsafeSortedList :: [a] -> SortedList a
unsafeSortedList = unsafeCoerce
