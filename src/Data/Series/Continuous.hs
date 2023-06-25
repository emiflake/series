module Data.Series.Continuous (Continuous(..), sample, (@), sampleAndHold) where

import Data.Series (Series, DataPoint(..))
import Data.Series.Internal (binarySearch, latest)
import Data.Time (UTCTime(..))

-- | A continuous mapping from time to data.
-- |
-- | @since 0.1.0.0
--   For _every_ moment, we must have a data point.
newtype Continuous a
  = Continuous
  { runContinuous :: UTCTime -> a
  }
  deriving stock (Functor)

-- | Sample and hold values into a continuous function, providing a base value.
-- |
-- | @since 0.1.0.0
sampleAndHold :: a -> Series a -> Continuous a
sampleAndHold base s =
  Continuous (\t -> maybe base (\(_, dp) -> dp.value) $ latest =<< binarySearch t s)

-- | Take a sample of a continuous time-value mapping.
-- |
-- | @since 0.1.0.0
sample :: UTCTime -> Continuous a -> a
sample t (Continuous f) = f t

-- | Infix version of 'sample'.
-- |
-- | @since 0.1.0.0
(@) :: Continuous a -> UTCTime -> a
(@) = flip sample

-- | @since 0.1.0.0
instance Num a => Num (Continuous a) where
  a + b = Continuous $ \t -> a @ t + b @ t
  a * b = Continuous $ \t -> a @ t * b @ t
  a - b = Continuous $ \t -> a @ t - b @ t
  abs a = Continuous $ \t -> abs (a @ t)
  signum a = Continuous $ \t -> signum (a @ t)
  fromInteger = Continuous . const . fromInteger
  negate a = Continuous $ \t -> negate (a @ t)
