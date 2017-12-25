module Data.Matrix where

import Prelude

import Data.Array as Array
import Data.Maybe (fromJust)
import Data.String (joinWith)
import Data.Typelevel.Num (class Lt, class Pos, D3)
import Data.Typelevel.Num.Ops (class Add, class Succ)
import Data.Typelevel.Num.Reps (D0, D1, D2)
import Data.Typelevel.Num.Sets (class Nat, toInt)
import Data.Typelevel.Undefined (undefined)
import Data.Vec (Vec, index')
import Data.Vec as Vec
import Debug.Trace (spy)
import Partial.Unsafe (unsafePartial)


-- stored as Vec of rows
-- | Matrix with height `h`, width `w` and contained value `a`
newtype Matrix h w a = Matrix (Vec.Vec h (Vec.Vec w a))

unsafeVecIndex ∷ ∀s a. Nat s => Vec s a → Int → a
unsafeVecIndex v i = unsafePartial $ fromJust $ index' v i

empty :: ∀a. Matrix D0 D0 a
empty = Matrix Vec.empty

consRowVec :: ∀h h' w w' a. Succ h h' => Nat w =>
  Vec.Vec w a → Matrix h w a → Matrix h' w a
consRowVec vec (Matrix m) = Matrix $ Vec.cons vec m

infixr 4 consRowVec as ⤓

consColVec :: ∀h w w' a. Succ w w' => Nat h => Vec.Vec h a → Matrix h w a → Matrix h w' a
consColVec vec (Matrix m) = Matrix $ Vec.zipWithE (Vec.cons) vec m

infixr 5 consColVec as ⇥

snocRowVec :: ∀h h' w w' a. Succ h h' => Nat w =>
  Vec.Vec w a → Matrix h w a → Matrix h' w a
snocRowVec vec (Matrix m) = Matrix $ Vec.snoc vec m

snocColVec :: ∀h w w' a. Succ w w' => Nat h => Vec.Vec h a → Matrix h w a → Matrix h w' a
snocColVec vec (Matrix m) = Matrix $ Vec.zipWithE (Vec.snoc) vec m

concatV :: forall h1 h2 h w a. Add h1 h2 h => Nat w => Matrix h1 w a → Matrix h2 w a → Matrix h w a
concatV (Matrix a) (Matrix b) = Matrix $ Vec.concat a b

concatH :: forall h w1 w2 w a. Add w1 w2 w => Nat h => Matrix h w1 a → Matrix h w2 a → Matrix h w a
concatH (Matrix a) (Matrix b) = Matrix $ Vec.zipWithE (Vec.concat) a b

singleton :: ∀a. a → Matrix D1 D1 a
singleton x = Vec.singleton x ⤓ Vec.empty ⇥ empty

matrix2d :: ∀a. a → a → a → a → Matrix D2 D2 a
matrix2d x11 x12 x21 x22 =
  Vec.vec2 x11 x12
  ⤓
  Vec.singleton x21 ⇥ (singleton x22)

matrix3d ∷ ∀a. a → a → a → a → a → a → a → a → a → Matrix D3 D3 a
matrix3d x11 x12 x13 x21 x22 x23 x31 x32 x33 =
    Vec.vec3 x11 x12 x13
    ⤓
    Vec.vec3 x21 x22 x23
    ⤓
    Vec.singleton x31 ⇥ Vec.singleton x32 ⇥ (singleton x33)

fill :: ∀h w ih iw a. Nat h => Nat w =>  (Int → Int → a) → Matrix h w a
fill f = Matrix $ Vec.fill (\y → Vec.fill (\x → f x y))

unsafeIndex ∷ ∀h w a. Nat h => Nat w => Matrix h w a → Int → Int → a
unsafeIndex (Matrix m) x y = (m `unsafeVecIndex` y) `unsafeVecIndex` x


replicate' :: ∀w h a. Nat w => Nat h => a → Matrix h w a
replicate' a = Matrix $ Vec.replicate' (Vec.replicate' a)

zipWithE :: ∀w h a b c. Nat w => Nat h =>
  (a → b → c) → Matrix h w a → Matrix h w b → Matrix h w c
zipWithE f (Matrix a) (Matrix b) = Matrix $ Vec.zipWithE (Vec.zipWithE f) a b


instance showMatrix :: (Nat h, Nat w, Show a) => Show (Matrix h w a) where
  show (Matrix m) = "  " <> (joinWith "\n  " $ Vec.toArray $ map show m)

instance functorMatrix :: (Nat h, Nat w) => Functor (Matrix h w) where
  map f (Matrix m) = Matrix $ map (map f) m

instance eqMatrix ∷ (Eq a, Nat h, Nat w) => Eq (Matrix h w a) where
  eq (Matrix a) (Matrix b) = a == b

add :: ∀h w a. Nat h => Nat w =>CommutativeRing a => Matrix h w a → Matrix h w a → Matrix h w a
add a  b = zipWithE (+) a b

negate :: ∀h w a. Nat h => Nat w => CommutativeRing a => Matrix h w a → Matrix h w a
negate = map (\v → zero - v)

columnVec :: ∀h w a x. Nat x => Lt x w => Nat h => Matrix h w a → x → Vec.Vec h a
columnVec (Matrix m) i = map (\row → row `Vec.index` (undefined :: x) ) m

rowVec :: ∀h w a y. Nat y => Lt y h => Nat w => Matrix h w a → y → Vec.Vec w a
rowVec (Matrix m) i = m `Vec.index` (undefined :: y)

rowVecUnsafe :: ∀h w a. Nat h => Nat w => Matrix h w a → Int → Vec.Vec w a
rowVecUnsafe (Matrix m) i =  unsafePartial $ Array.unsafeIndex (Vec.toArray m) i

columnVecUnsafe :: ∀h w a. Nat h => Nat w => Matrix h w a → Int → Vec.Vec h a
columnVecUnsafe (Matrix m) i = map (\row → unsafePartial $ Array.unsafeIndex (Vec.toArray row) i) m

mul :: ∀s a. Nat s => CommutativeRing a => Matrix s s a → Matrix s s a → Matrix s s a
mul a b = fill (\x y → rowVecUnsafe a y `Vec.dotProduct` columnVecUnsafe b x)

transpose ∷ ∀a h w. Nat h => Nat w =>  Matrix h w a → Matrix w h a
transpose m = fill (\x y → unsafeIndex m y x)

deleteRowUnsafe ∷ ∀h h' w a. Nat w => Nat h => Nat h' => Succ h' h => Int → Matrix h w a → Matrix h' w a
deleteRowUnsafe i m = fill (\ x y → unsafeIndex m x (if y < i then y else y+1))

deleteRow ∷ ∀h h' w i a. Nat w => Nat h => Nat h' => Nat i => Succ h' h => Lt i h => i → Matrix h w a → Matrix h' w a
deleteRow it = deleteRowUnsafe $ toInt it

deleteColumn ∷ ∀h w w' i a. Nat w => Nat h => Nat w' => Nat i => Succ w' w => Lt i w => i → Matrix h w a → Matrix h w' a
deleteColumn it m = fill (\ x y → unsafeIndex m (if x < i then x else x+1) y)
  where i = toInt (undefined :: i)

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


lrSingle ∷ ∀h w a. EuclideanRing a => Pos w => Pos h => Matrix h w a → Matrix h w a
lrSingle m = f 0 m
    where
        f row m 
            | row == toInt (undefined :: h) = m
            | otherwise = f (row+1) (usePivot row m)

-- | given a invertable matrix `A` with non-zero-able diagonal returns a tuple of L and R with the property:
-- | `A = L*R`
lrSplit ∷ ∀h w a. EuclideanRing a => Pos w => Pos h => Matrix h w a → { l:: Matrix h w a, r:: Matrix h w a}
lrSplit m = {r : fill rConstr, l: fill lConstr}
    where 
        lr = lrSingle m
        rConstr x y
            | x >= y = unsafeIndex lr x y
            | otherwise = zero
        lConstr x y 
            | x < y = unsafeIndex lr x y
            | x == y = one
            | otherwise = zero

 

instance semiringMatrix :: (Nat s, CommutativeRing a) => Semiring (Matrix s s a) where
  add = add
  zero = replicate' zero
  mul = mul
  one = fill (\x y → if (x==y) then one else zero)

instance ringMatrix :: (Nat s, CommutativeRing a) => Ring (Matrix s s a) where
  sub a b = add a (negate b)

a :: Matrix D3 D3 Number
a = matrix3d 
    1.0 4.0 (0.0 - 1.0)
    3.0 0.0 5.0
    2.0 2.0 1.0

a2 :: Matrix D3 D3 Number
a2 = matrix3d
    1.0 4.0 (0.0-1.0)
    3.0 (0.0-12.0) 8.0
    2.0 (0.0-6.0) 3.0