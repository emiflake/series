--------------------------------------------------------------------------------
-- Low level API that may or may not end up being exposed.

{- | Perform binary search on a 'Series' for a time.
     This is a low-level operation.
-}
module Data.Series.Internal (binarySearch, linearSearch, SearchResult (..), inclusiveSlice, latest, Series (..), DataPoint (..), exact, resampleSAH, merge, isEmpty, emptySeries, pairWiseZipWith) where

import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.These (These (..), these)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector
import Prelude hiding (lookup)

-- | Represents a data point in a series. It has a time and a value.
data DataPoint a = DataPoint
  { time :: !UTCTime
  , value :: a
  }
  deriving stock (Functor, Show, Eq)

{- | A collection of data points. For any given time, we may or may not have a data point.
     The data points are sorted.
-}
newtype Series a = Series
  { getSeries :: Vector (DataPoint a)
  }
  deriving stock (Functor, Show, Eq)

binarySearch :: forall a. UTCTime -> Series a -> Maybe (SearchResult a)
binarySearch _t (Series xs) | Vector.null xs = Nothing
binarySearch t (Series xs) =
  Just $ go 0 (len - 1)
  where
    len = Vector.length xs

    gather :: DataPoint a -> Int -> Maybe (SearchResult a)
    gather dp i | dp.time == t = Just $ ExactMatch i dp
    gather dp 0 | dp.time > t = Just $ Nearest (That (0, dp))
    gather dp i | i == len - 1, dp.time < t = Just $ Nearest (This (i, dp))
    gather dp i | t < dp.time = Just $ Nearest (These (i - 1, xs Vector.! (i - 1)) (i, dp))
    gather dp i | dp.time < t = Just $ Nearest (These (i, dp) (i + 1, xs Vector.! (i + 1)))
    gather _ _ = Nothing

    go :: Int -> Int -> SearchResult a
    go a b | a == b, let dp = xs Vector.! a, Just r <- gather dp a = r
    go a b | a > b, let dp = xs Vector.! b, Just r <- gather dp b = r
    go a b =
      let
        middle = ((a + b) `div` 2)
        dp = xs Vector.! middle
       in
        case compare t dp.time of
          EQ -> ExactMatch middle (xs Vector.! middle)
          GT -> go (min (len - 1) $ middle + 1) b
          LT -> go a (max 0 $ middle - 1)

linearSearch :: forall a. UTCTime -> Series a -> Maybe (SearchResult a)
linearSearch _t (Series xs) | Vector.null xs = Nothing
linearSearch t (Series xs) =
  Just $ go (Vector.head xs) 0
  where
    len = Vector.length xs

    go :: DataPoint a -> Int -> SearchResult a
    go dp i | dp.time == t = ExactMatch i dp
    go dp 0 | dp.time > t = Nearest (That (0, dp))
    go dp i | dp.time > t = Nearest (These (i - 1, xs Vector.! (i - 1)) (i, dp))
    go dp i | i == len - 1 = Nearest (This (i, dp))
    go _ i = go (xs Vector.! (i + 1)) (i + 1)

-- | True if a 'Series' has any elements.
isEmpty :: Series a -> Bool
isEmpty (Series dps) = Vector.null dps

-- | Series with no data points.
emptySeries :: Series a
emptySeries = Series Vector.empty

findLargestSmallerThan ::
  forall a.
  UTCTime ->
  Series a ->
  Maybe (DataPoint a)
findLargestSmallerThan t (Series xs) =
  lastMaybe $
    Vector.filter
      (\(DataPoint t0 _) -> t0 <= t)
      xs
  where
    lastMaybe :: forall b. Vector b -> Maybe b
    lastMaybe xs' | Vector.null xs' = Nothing
    lastMaybe xs' = Just $ Vector.last xs'

{- | Merge two series, preserving temporal order.

     /O(n+m)/.
-}
merge :: Series a -> Series a -> Series a
merge (Series dpsA) (Series dpsB) =
  Series $ Vector.create $ do
    let newLength = Vector.length dpsA + Vector.length dpsB
    nv <- MVector.new newLength
    a <- newSTRef @Int 0
    b <- newSTRef @Int 0
    let writeAtFrom i ref v = do
          MVector.write nv i $ v
          modifySTRef ref succ

    for_ [0 .. newLength - 1] $ \i -> do
      ai <- readSTRef a
      bi <- readSTRef b
      case (dpsA Vector.!? ai, dpsB Vector.!? bi) of
        (Just av, Nothing) -> writeAtFrom i a av
        (Nothing, Just bv) -> writeAtFrom i b bv
        (Just av, Just bv) ->
          case compare av.time bv.time of
            EQ -> writeAtFrom i a av
            LT -> writeAtFrom i a av
            GT -> writeAtFrom i b bv
        (Nothing, Nothing) ->
          -- FIXME(Emily, 26 June 2023): This should never happen, but let's be lenient for now.
          pure ()
    pure nv

{- | Create a new series with the times in the given vector, at each of the
     times in the vector place the last registered value in the given series
     before that time.

     If a given time from the vector is before the first time in the series,
     drop it.

     Passing an empty vector or series will always result in an empty series.
-}
resampleSAH :: forall a. Vector UTCTime -> Series a -> Series a
resampleSAH _ xs | isEmpty xs = emptySeries
resampleSAH ts xs =
  Series $ Vector.create $ do
    let tLength = Vector.length ts
    nv <- MVector.new tLength
    a <- newSTRef @Int 0
    let writeAtFrom i ref v = do
          MVector.write nv i $ v
          modifySTRef ref succ
    for_ [0 .. tLength - 1] $ \i -> do
      ai <- readSTRef a
      let t = ts Vector.! i
      case findLargestSmallerThan t xs of
        Nothing -> pure ()
        Just (DataPoint _ x) -> writeAtFrom ai a $ DataPoint t x
    ai <- readSTRef a
    pure $ MVector.slice 0 ai nv

{- | Represents the result of performing binary search for a particular time in a 'Series'.
     This API is quite low-level and exposes index information into the Vector.
-}
data SearchResult a
  = -- | An exact match is found, this is the index and the value.
    ExactMatch !Int !(DataPoint a)
  | -- | An exact match isn't found.
    -- | These are the two possible neighbours.
    Nearest !(These (Int, DataPoint a) (Int, DataPoint a))
  deriving stock (Show, Eq)

-- | Return whichever index and data point is either equal or less than the original search.
exact :: SearchResult a -> Maybe (Int, DataPoint a)
exact (ExactMatch i dp) = Just (i, dp)
exact _ = Nothing

latest :: SearchResult a -> Maybe (Int, DataPoint a)
latest (ExactMatch i v) = Just (i, v)
latest (Nearest t) = these Just (const Nothing) (const Just) t

-- Fetch an index that is >= than the search.
inclusiveIndexUB :: SearchResult a -> Maybe Int
inclusiveIndexUB (ExactMatch i _) = Just i
inclusiveIndexUB (Nearest t) =
  case t of
    This (i, _) -> Just i
    These (i, _) _ -> Just i
    That _ -> Nothing

-- Fetch an index that is <= than the search.
inclusiveIndexLB :: SearchResult a -> Maybe Int
inclusiveIndexLB (ExactMatch i _) = Just i
inclusiveIndexLB (Nearest t) =
  case t of
    This _ -> Nothing
    These _ (i, _) -> Just i
    That (i, _) -> Just i

{- | Create the bounds based on two search results, giving a new slice.
     If the slice contains no elements, return Nothing.
-}
inclusiveSlice :: SearchResult a -> SearchResult a -> Maybe (Int, Int)
inclusiveSlice lb ub =
  case (inclusiveIndexLB lb, inclusiveIndexUB ub) of
    (Just i, Just j) -> Just (i, j)
    (_, _) -> Nothing

extractTimes :: forall a. Series a -> Vector UTCTime
extractTimes (Series s) = (.time) <$> s

applySAH :: forall a. Vector UTCTime -> Series a -> Series a
applySAH ts s = merge s $ resampleSAH ts s

lookup :: UTCTime -> Series a -> Maybe a
lookup t s = fmap ((.value) . snd) $ exact =<< binarySearch t s

unsafeLookup :: UTCTime -> Series a -> a
unsafeLookup t s = fromJust $ lookup t s

sortedVectorNub :: Eq a => Vector a -> Vector a
sortedVectorNub v =
  Vector.create $ do
    let len = length v
    nv <- MVector.new len
    l <- newSTRef @Int 0
    let writeAtFrom i ref v' = do
          MVector.write nv i v'
          modifySTRef ref succ
    for_ [0 .. len - 1] $ \i -> do
      let x = v Vector.! i
      l' <- readSTRef l
      if i < len - 1 && x == v Vector.! (i + 1) then
        pure ()
      else
        writeAtFrom l' l x
    l' <- readSTRef l
    pure $ MVector.slice 0 l' nv

pairWiseZipWith :: forall a b c. Eq c => (a -> b -> c) -> Series a -> Series b -> Series c
pairWiseZipWith f sa sb = Series $ sortedVectorNub $ fmap (\dp -> dp {value = f (unsafeLookup dp.time resampledSeriesA) dp.value}) $ deconstr resampledSeriesB
  where
    resampledSeriesA :: Series a
    resampledSeriesA = applySAH (extractTimes sb) sa

    resampledSeriesB :: Series b
    resampledSeriesB = applySAH (extractTimes sa) sb

    deconstr :: forall d. Series d -> Vector (DataPoint d)
    deconstr (Series s) = s
