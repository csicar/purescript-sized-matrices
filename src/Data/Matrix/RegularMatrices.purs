module Data.Matrix.RegularMatrices where

import Prelude
import Data.Matrix (Matrix)
import Data.Matrix.Algorithms
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Typelevel.Num (class Nat, class Pos)

newtype RegularMatrix s a
  = RegularMatrix (Matrix s s a)

derive newtype instance showMatrix :: (Nat s, Show a) => Show (RegularMatrix s a)

derive newtype instance functorMatrix :: (Nat s) => Functor (RegularMatrix s)

derive newtype instance foldableVec :: (Nat s) => Foldable (RegularMatrix s)

derive newtype instance eqMatrix :: (Eq a, Nat s) => Eq (RegularMatrix s a)

derive newtype instance semiringMatrix :: (Nat s, CommutativeRing a) => Semiring (RegularMatrix s a)

derive newtype instance ringMatrix :: (Nat s, CommutativeRing a) => Ring (RegularMatrix s a)

instance divisionRingMatrix :: (Pos s, Field a) => DivisionRing (RegularMatrix s a) where
  recip (RegularMatrix a) = RegularMatrix $ inverse a
