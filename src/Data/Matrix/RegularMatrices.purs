module Data.Matrix.RegularMatrices where

import Prelude

import Data.Matrix (Matrix)
import Data.Matrix.Algorithms
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Typelevel.Num (class Nat, class Pos)

newtype RegularMatrix s a = RegularMatrix (Matrix s s a)

instance showMatrix :: (Nat s, Show a) => Show (RegularMatrix s a) where
  show (RegularMatrix m) = show m

instance functorMatrix :: (Nat s) => Functor (RegularMatrix s) where
  map f (RegularMatrix m) = RegularMatrix $ map f m


instance foldableVec ∷ (Nat s) => Foldable (RegularMatrix s) where
  foldMap f (RegularMatrix xs) = foldMap f xs
  foldr f z (RegularMatrix xs) = foldr f z xs
  foldl f z (RegularMatrix xs) = foldl f z xs

instance eqMatrix ∷ (Eq a, Nat s) => Eq (RegularMatrix s a) where
  eq (RegularMatrix a) (RegularMatrix b) = a == b


instance semiringMatrix :: (Nat s, CommutativeRing a) => Semiring (RegularMatrix s a) where
  add (RegularMatrix a) (RegularMatrix b) = RegularMatrix $ a + b
  zero = RegularMatrix zero
  mul (RegularMatrix a) (RegularMatrix b) = RegularMatrix $ a * b
  one = RegularMatrix one

instance ringMatrix :: (Nat s, CommutativeRing a) => Ring (RegularMatrix s a) where
  sub (RegularMatrix a) (RegularMatrix b) = RegularMatrix $ a - b

instance divisionRingMatrix :: (Pos s, Field a) => DivisionRing (RegularMatrix s a) where
  recip (RegularMatrix a) = RegularMatrix $ inverse a