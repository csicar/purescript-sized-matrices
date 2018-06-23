module Data.Matrix.Algorithms (det, inverse, cofactor, cofactorMatrix, luDecomp) where

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

cofactorMatrix :: ∀s a. Pos s => EuclideanRing a => Matrix s s a -> Matrix s s a 
cofactorMatrix m = fill (\x y -> cofactor x y m)

cofactor :: ∀s a. Pos s => EuclideanRing a => CommutativeRing a => Int -> Int -> Matrix s s a -> a 
cofactor i j m = (signFor i j) * det' (removeCross i j (toArray m)) 
  where 
    signFor i j
      | even (i+j) = one
      | otherwise = negate one
    removeCross :: ∀b. Int -> Int -> Array (Array b) -> Array (Array b)
    removeCross i j m = deleteAt' i (map (deleteAt' j) m)

    deleteAt' :: ∀a. Int -> Array a -> Array a 
    deleteAt' i m' = unsafePartial $ fromJust $ deleteAt i m'

type GaussianState s a = {a :: Matrix s s a, i :: Matrix s s a, col :: Int }

inverse :: ∀s a. Pos s => Field a => Matrix s s a -> Matrix s s a
inverse m = map (_ * recip (det m)) $ cofactorMatrix m

-- | calculate determinant for matrix.
det ∷ ∀s a. CommutativeRing a => EuclideanRing a => Pos s => Matrix s s a → a
det m = det' (toArray m)

det' :: ∀a. CommutativeRing a => EuclideanRing a => (Array (Array a)) -> a
det' [] = one
det' [[x]] = x
det' xss = case (uncons xss) of
  Just {head, tail} -> sum $ zipWith (f tail) (0..(length head)) head
  Nothing -> undefined
  where 
    f m i colVal = (signFor i) * colVal * det' (fromMaybe [zero] <<< deleteAt i <$> m)
    signFor i
      | even i = one
      | otherwise = negate one

liftTuple ∷ ∀a b. (a → b) → Tuple a a → Tuple b b
liftTuple f (Tuple a b) = Tuple (f a) (f b)

usePivot ∷ ∀h w a. Pos w => Pos h => EuclideanRing a => Int → Matrix h w a → Matrix h w a
usePivot row m = fill f
    where
      f :: Int → Int → a
      f x y
        | x == row && y > row = zero - chooseFactor x y
        | y > row && x > row = (unsafeIndex m x row) * (chooseFactor x y) + unsafeIndex m x y
        | otherwise = unsafeIndex m x y

      pivot :: a
      pivot = unsafeIndex m row row

      chooseFactor :: Int → Int → a
      chooseFactor x y = zero - (unsafeIndex m row y) / pivot

_lr ∷ ∀h w a. Pos h => Ord a => EuclideanRing a => Pos w => Int → Matrix h w a → Matrix h w a → {p ∷ Matrix h w a, lr ∷ Matrix h w a}
_lr i p m | i == (height m - 1) = {p:p, lr:m}
_lr i p m = _lr (i+1) p' pivM
  where
    maxRowIndex = findMaxIndex $ columnVecUnsafe m i
    swapper = swapRow maxRowIndex i

    m' = swapper m
    p' = swapper p

    pivM = usePivot i m'

type LuDecomp h w a= { l ∷ Matrix h w a, u ∷ Matrix h w a, p ∷ Matrix h w a}

-- | given a invertable matrix `A` with non-zero-able diagonal returns a tuple of L and R with the property:
-- | `A = L*R`
-- | ! not yet correct !
luDecomp ∷ ∀h w a. Ord a => EuclideanRing a => Pos w => Pos h => Matrix h w a → LuDecomp h w a
luDecomp m = {u: fill rConstr, l: fill lConstr, p: p}
    where
        {lr,p} = _lr 0 matrixOne m
        rConstr x y
            | x >= y = unsafeIndex lr x y
            | otherwise = zero
        lConstr x y
            | x < y = unsafeIndex lr x y
            | x == y = one
            | otherwise = zero