module Test.Gen where

import Unsafe.Coerce
import Data.Proxy
import GHC.TypeLits
import Test.QuickCheck

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound .. maxBound]

instance Show (a -> b) where
  show _ = "<fun>"

instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
  xs == ys = map xs allValues == map ys allValues

data FinSet (n :: Nat) where
  FinOne :: FinSet (n + 1)
  FinSuc :: FinSet n -> FinSet (n + 1)

instance Eq (FinSet n) where
  FinOne == FinOne = True
  FinSuc x' == FinSuc y' = unsafeCoerce @_ @(FinSet n) x' == unsafeCoerce @_ @(FinSet n) y'
  _== _ = False

instance (n ~ n' + 1) => Enum (FinSet n) where
  toEnum n = case n `compare` 1 of
    LT -> error "Cannot convert n < 0 to FinSet"
    EQ -> FinOne
    GT -> FinSuc $ toEnum (n - 1)

instance KnownNat n => Show (FinSet n) where
  show x = "FinSet " <> show (count x) <> " of " <> show (natVal (Proxy @n))
    where
      count :: forall n . FinSet n -> Int
      count = \case FinOne -> 1; FinSuc x' -> 1 + count x'

instance KnownNat n => Arbitrary (FinSet n) where
  arbitrary = undefined
