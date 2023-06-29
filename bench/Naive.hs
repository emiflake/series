module Naive (Series (..), slice, series) where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Series (DataPoint (DataPoint))
import Data.Time (UTCTime)
import GHC.Generics (Generic)

newtype Series (a :: Type) = Series [DataPoint a]
  deriving stock (Show, Eq, Generic)
  deriving newtype (NFData)

slice ::
  forall (a :: Type).
  UTCTime ->
  UTCTime ->
  Series a ->
  Series a
slice start end (Series xs) = Series [DataPoint x y | DataPoint x y <- xs, x >= start && x <= end]

series :: [(UTCTime, a)] -> Series a
series = Series . fmap (uncurry DataPoint)
