module Data.Matrix.Algorithms (det, luDecomp) where

import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximumBy, product)
import Data.Maybe (Maybe, fromJust, fromMaybe)
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
import Data.Matrix
import Data.Matrix.Operations (findMaxIndex)
import Data.Matrix.Transformations (swapRow)
import Prelude



permSign ∷ ∀s a. Eq a => CommutativeRing a => Pos s => Matrix s s a → a
permSign = toVal <<< foldl f true  <<< zipWithE (*) inversionPlaces
  where
    inversionPlaces = fill (\x y → if (y < x) then one else zero)
    f acc a | a == zero = acc
    f acc a = not acc
    toVal true = one
    toVal false = - one

-- | calculate determinant for matrix.
-- | uses luDecomposition. ! not yet correct
det ∷ ∀s a. Ord a => CommutativeRing a => EuclideanRing a => Pos s => Matrix s s a → a
det m = permSign p * product diag
  where
    {u, p} = luDecomp m

    diag ∷ Matrix s s a
    diag = fill (\x y → if x==y then unsafeIndex u x y else one)

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
