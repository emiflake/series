--------------------------------------------------------------------------------
-- Low level API that may or may not end up being exposed.

-- | Perform binary search on a 'Series' for a time.
-- This is a low-level operation.
module Data.Series.Internal (binarySearch, BinarySearchResult(..), inclusiveSlice, latest, Series(..), DataPoint(..), exact) where

import Data.These (These(..), these)
import Data.Vector qualified as Vector
import Data.Vector (Vector)
import Data.Time (UTCTime)

-- | Represents a data point in a series. It has a time and a value.
-- |
-- | @since 0.1.0.0
data DataPoint a
  = DataPoint
  { time :: !UTCTime
  , value :: a
  }
  deriving stock (Functor, Show, Eq)

-- | A collection of data points. For any given time, we may or may not have a data point.
-- |
-- | @since 0.1.0.0
-- The data points are sorted.
newtype Series a
  = Series
  { getSeries :: Vector (DataPoint a)
  }
  deriving stock (Functor, Show, Eq)

instance Semigroup a => Semigroup (Series a) where
 


binarySearch :: forall a. UTCTime -> Series a -> Maybe (BinarySearchResult a)
binarySearch _t (Series xs) | Vector.null xs = Nothing
binarySearch t (Series xs) =
  Just $ go 0 (len - 1)
  where
    len = Vector.length xs 

    gather :: DataPoint a -> Int -> Maybe (BinarySearchResult a)
    gather dp i | dp.time == t = Just $ ExactMatch i dp
    gather dp 0 | dp.time > t = Just $ Nearest (That (0, dp))
    gather dp i | i == len - 1, dp.time < t = Just $ Nearest (This (i, dp))
    gather dp i | t < dp.time = Just $ Nearest (These (i - 1, xs Vector.! (i - 1)) (i, dp))
    gather dp i | dp.time < t = Just $ Nearest (These (i, dp) (i + 1, xs Vector.! (i + 1)))
    gather _  _ = Nothing
    
    go :: Int -> Int -> BinarySearchResult a
    go a b | a == b, let dp = xs Vector.! a, Just r <- gather dp a = r
    go a b =
      let
        middle = ((a + b) `div` 2)
        dp = xs Vector.! middle
      in
      case compare t dp.time of
        EQ -> ExactMatch middle (xs Vector.! middle)
        GT -> go (min (len - 1) $ middle + 1) b
        LT -> go a (max 0 $ middle - 1)

-- | Represents the result of performing binary search for a particular time in a 'Series'. 
-- This API is quite low-level and exposes index information into the Vector.
data BinarySearchResult a
  = -- | An exact match is found, this is the index and the value.
    ExactMatch !Int !(DataPoint a)
    -- | An exact match isn't found.
    -- | These are the two possible neighbours.
  | Nearest !(These (Int, DataPoint a) (Int, DataPoint a))
  deriving stock (Show, Eq)

-- | Return whichever index and data point is either equal or less than the original search.
exact :: BinarySearchResult a -> Maybe (Int, DataPoint a)
exact (ExactMatch i dp) = Just (i, dp)
exact _ = Nothing

latest :: BinarySearchResult a -> Maybe (Int, DataPoint a)
latest (ExactMatch i v) = Just (i, v)
latest (Nearest t) = these Just (const Nothing) (const Just) t

-- Fetch an index that is >= than the search.
inclusiveIndexUB :: BinarySearchResult a -> Maybe Int
inclusiveIndexUB (ExactMatch i _) = Just i
inclusiveIndexUB (Nearest t) =
  case t of
    This (i, _) -> Just i
    These (i, _) _ -> Just i
    That _ -> Nothing

-- Fetch an index that is <= than the search.
inclusiveIndexLB :: BinarySearchResult a -> Maybe Int
inclusiveIndexLB (ExactMatch i _) = Just i
inclusiveIndexLB (Nearest t) =
  case t of
    This _ -> Nothing
    These _ (i, _) -> Just i
    That (i, _) -> Just i

-- | Create the bounds based on two search results, giving a new slice.
-- | If the slice contains no elements, return Nothing.
inclusiveSlice :: BinarySearchResult a -> BinarySearchResult a -> Maybe (Int, Int)
inclusiveSlice lb ub =
  case (inclusiveIndexLB lb, inclusiveIndexUB ub) of
    (Just i, Just j) -> Just (i, j)
    (_, _) -> Nothing
