module Data.Matrix.Algorithms (det, inverse, cofactor, cofactorMatrix, adjunct) where

import Data.Matrix
import Prelude
import Data.Array ((..), zipWith, deleteAt, uncons, length)
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, foldr, sum, maximumBy, product, any, all)
import Data.Int (even)
import Data.Matrix.Operations (findMaxIndex, removeRow)
import Data.Matrix.Transformations (swapRow, transpose)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Typelevel.Num (class Lt, class LtEq, class Mul, class Pos, class Pred, D3)
import Data.Typelevel.Num.Ops (class Add, class Succ)
import Data.Typelevel.Num.Reps (D0, D1, D2)
import Data.Typelevel.Num.Sets (class Nat, toInt)
import Data.Typelevel.Undefined (undefined)
import Data.Vec (Vec)
import Data.Vec as Vec
import Partial.Unsafe (unsafePartial)

cofactorMatrix :: ∀ s a. Pos s => EuclideanRing a => Matrix s s a -> Matrix s s a
cofactorMatrix m = fill $ \j i -> cofactor i j m

cofactor :: ∀ s a. Pos s => EuclideanRing a => CommutativeRing a => Int -> Int -> Matrix s s a -> a
cofactor i j m = (signFor i j) * det' (removeCross i j (toArray m))
  where
  signFor i j
    | even (i + j) = one
    | otherwise = negate one

  removeCross :: ∀ b. Int -> Int -> Array (Array b) -> Array (Array b)
  removeCross i j m = deleteAt' i (map (deleteAt' j) m)

  deleteAt' :: ∀ a. Int -> Array a -> Array a
  deleteAt' i m' = unsafePartial $ fromJust $ deleteAt i m'

adjunct :: ∀ s a. Pos s => Field a => Matrix s s a -> Matrix s s a
adjunct = cofactorMatrix >>> transpose

-- | Computes the multiplicative inverse of a regular matrix.
-- | The function uses [Cramer's Rule](https://en.wikipedia.org/wiki/Cramer%27s_rule) for the computation.
-- Implementation reference:
-- https://de.wikipedia.org/wiki/Inverse_Matrix#Berechnung#Darstellung_über_die_Adjunkte
inverse :: ∀ s a. Pos s => Field a => Matrix s s a -> Matrix s s a
inverse m = map (_ * recip (det m)) $ adjunct m

-- | calculate the determinant for matrix.
det :: ∀ s a. CommutativeRing a => EuclideanRing a => Pos s => Matrix s s a → a
det m = det' (toArray m)

det' :: ∀ a. CommutativeRing a => EuclideanRing a => (Array (Array a)) -> a
det' [] = one

det' [ [ x ] ] = x

det' xss = case (uncons xss) of
  Just { head, tail } -> sum $ zipWith (f tail) (0 .. (length head)) head
  Nothing -> undefined
  where
  f m i colVal = (signFor i) * colVal * det' (fromMaybe [ zero ] <<< deleteAt i <$> m)

  signFor i
    | even i = one
    | otherwise = negate one
