{- |
Module     : Data.Series
Maintainer : emi@haskell.fyi
Description: Discrete time series.

Discrete time series.
-}
module Data.Series (
  DataPoint (..),
  Series (getSeries),
  emptySeries,
  series,
  Data.Series.lookup,
  (!?),
  findLastTimeBefore,
  slice,
  size,
  singleton,
  merge,
  resampleSAH,
  bounds,
  isEmpty,
  pointwiseZipWith,
  nub,
  nubWith,
  values,
  times,
) where

import Control.Monad ((<=<))
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.STRef (newSTRef, readSTRef)
import Data.STRef.Strict (modifySTRef)
import Data.Series.Internal (
  DataPoint (..),
  Series (..),
  binarySearch,
  emptySeries,
  exact,
  inclusiveSlice,
  isEmpty,
  latest,
 )
import Data.Series.TimeRange (TimeRange (TimeRange))
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector

{- | Create a 'Series' with only a single 'DataPoint'.

     @since 0.1.0.0
-}
singleton :: forall (a :: Type). UTCTime -> a -> Series a
singleton t = Series . Vector.singleton . DataPoint t

{- | Get the number of 'DataPoint's in a 'Series'.

     @since 0.1.0.0
-}
size :: forall (a :: Type). Series a -> Int
size (Series dps) = Vector.length dps

{- | Create a 'Series' from an association list. This will sort the list.

     @since 0.1.0.0
-}
series :: forall (a :: Type). [(UTCTime, a)] -> Series a
series = Series . Vector.fromList . sortOn (.time) . fmap (uncurry DataPoint)

{- | /O(log n)/. Slice a series from one time to another. Inclusive.

     @since 0.1.0.0
-}
slice ::
  forall (a :: Type).
  Show a =>
  UTCTime ->
  UTCTime ->
  Series a ->
  Series a
slice _ _ s | isEmpty s = emptySeries
slice start end s@(Series dps) =
  case sl of
    Nothing -> emptySeries
    Just (a, b) -> Series $ Vector.slice a ((b + 1) - a) dps
  where
    sl = do
      a <- binarySearch start s; b <- binarySearch end s; inclusiveSlice a b

{- | /O(log n)/. Find the value at a specific time.

     @since 0.1.0.0
-}
lookup :: forall (a :: Type). UTCTime -> Series a -> Maybe a
lookup t s = fmap ((.value) . snd) $ exact =<< binarySearch t s

{- | Infix version of 'Data.Series.lookup'.

     /O(log n)/.

     @since 0.1.0.0
-}
(!?) :: forall (a :: Type). Series a -> UTCTime -> Maybe a
(!?) = flip Data.Series.lookup

{- | /O(1)/. Compute the bounds of a 'Series'.

     @since 0.1.0.0
-}
bounds :: forall (a :: Type). Series a -> Maybe TimeRange
bounds (Series s) | Vector.null s = Nothing
bounds (Series s) = Just $ TimeRange (Vector.head s).time (Vector.last s).time

{- | /O(log n)/. Find the last registered time in the given 'Series' before the
     given time.
-}
findLastTimeBefore ::
  forall (a :: Type).
  UTCTime ->
  Series a ->
  Maybe (DataPoint a)
findLastTimeBefore t =
  maybe Nothing (Just . snd) . latest <=< binarySearch t

{- | /O(n+m)/. Merge two 'Series', preserving temporal order.

     @since 0.1.0.0
-}
merge :: forall (a :: Type). Series a -> Series a -> Series a
merge (Series dpsA) (Series dpsB) =
  Series $ Vector.create $ do
    let newLength = Vector.length dpsA + Vector.length dpsB
    nv <- MVector.new newLength
    a <- newSTRef @Int 0
    b <- newSTRef @Int 0
    let writeAtFrom i ref v = do
          MVector.write nv i v
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

{- | /O(n*m)/. Create a new 'Series' with the times in the given 'Vector', at
     each of the times in the 'Vector' place the last registered value in the
     given 'Series' before that time.

     If a given time from the 'Vector' is before the first time in the 'Series',
     it's dropped.

     Passing an empty 'Vector' or 'Series' will always result in an empty 'Series'.

     @since 0.1.0.0
-}
resampleSAH :: forall (a :: Type). Vector UTCTime -> Series a -> Series a
resampleSAH _ xs | isEmpty xs = emptySeries
resampleSAH ts xs =
  Series $ Vector.create $ do
    let tLength = Vector.length ts
    nv <- MVector.new tLength
    a <- newSTRef @Int 0
    let writeAtFrom i ref v = do
          MVector.write nv i v
          modifySTRef ref succ
    for_ [0 .. tLength - 1] $ \i -> do
      ai <- readSTRef a
      let t = ts Vector.! i
      case findLastTimeBefore t xs of
        Nothing -> pure ()
        Just (DataPoint _ x) -> writeAtFrom ai a $ DataPoint t x
    ai <- readSTRef a
    pure $ MVector.slice 0 ai nv

{- | /O(n*m)/. Merge two 'Series', applying the latest value from each with the
     other.

     @since 0.1.0.0
-}
pointwiseZipWith ::
  forall (a :: Type) (b :: Type) (c :: Type).
  Ord c =>
  (a -> b -> c) ->
  Series a ->
  Series b ->
  Series c
pointwiseZipWith f sa sb =
  Series $
    fmap
      (\dp -> dp {value = f (fromJust $ resampledSeriesA !? dp.time) dp.value})
      (deconstr resampledSeriesB)
  where
    resampledSeriesA :: Series a
    resampledSeriesA = applySAH (times sb) sa

    resampledSeriesB :: Series b
    resampledSeriesB = applySAH (times sa) sb

    deconstr :: forall d. Series d -> Vector (DataPoint d)
    deconstr (Series s) = s

{- | /O(n)/. Extract the times 'Vector' from the 'Series'.

     @since 0.1.0.0
-}
times :: forall (a :: Type). Series a -> Vector UTCTime
times (Series s) = Vector.map (.time) s

{- | /O(n)/. Extract the values 'Vector' from the 'Series'.

     @since 0.1.0.0
-}
values :: forall (a :: Type). Series a -> Vector a
values (Series s) = Vector.map (.value) s

-- | /O(n*m)/. Add additional 'DataPoint's based on existing data in the 'Series'.
applySAH :: forall (a :: Type). Vector UTCTime -> Series a -> Series a
applySAH ts s = merge s $ resampleSAH ts s

{- | /O(n)/. Remove duplicates from a 'Series' without care for preserving some
     order of elements.

     @since 0.1.0.0
-}
nub :: forall (a :: Type). Series a -> Series a
nub (Series dps) =
  Series $
    Vector.ifilter
      (\i dp -> maybe True ((dp.time /=) . (.time)) $ dps Vector.!? (i - 1))
      dps

{- | /O(n)*O(f)/ where f is the time complexity of the given criteria. Remove
     duplicates from a 'Series', exposing the choice for preserving element
     to the caller.

     @since 0.1.0.0
-}
nubWith :: forall (a :: Type). (Vector a -> a) -> Series a -> Series a
nubWith f (Series dps) =
  Series $ Vector.create $ do
    let len = Vector.length dps
    count <- newSTRef @Int 0
    nv <- MVector.new len
    let loop s | Vector.null s = MVector.slice 0 <$> readSTRef count <*> pure nv
        loop s = do
          let candidate = Vector.head s
              (cs, rest) = Vector.span (\e -> e.time == candidate.time) s
          count' <- readSTRef count
          MVector.write
            nv
            count'
            (DataPoint candidate.time . f . Vector.map (.value) $ cs)
          modifySTRef count succ
          loop rest
    loop dps
