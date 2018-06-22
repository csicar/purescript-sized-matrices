module Data.Matrix where

import Prelude

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
import Data.Vec (Vec, range')
import Data.Vec as Vec
import Partial.Unsafe (unsafePartial)

-- stored as Vec of rows
-- | Matrix with height `h`, width `w` and contained value `a`
newtype Matrix h w a = Matrix (Vec.Vec h (Vec.Vec w a))

height ∷ ∀h w a. Nat h => Nat w => Matrix h w a → Int 
height _ = toInt (undefined ∷ h)

width ∷ ∀h w a. Nat h => Nat w => Matrix h w a → Int 
width _ = toInt (undefined ∷ w)

index ∷ ∀x y h w a. Nat x => Nat y => Lt x w => Lt y h => Matrix h w a → x → y → a 
index (Matrix m) i j= Vec.index (Vec.index m (undefined ∷ y)) (undefined :: x)

index' ∷ ∀h w a. Nat h => Nat w => Int → Int → Matrix h w a → Maybe a 
index' x y (Matrix m) = do
  row <- Vec.index' m y
  Vec.index' row x

unsafeVecIndex ∷ ∀s a. Nat s => Vec s a → Int → a
unsafeVecIndex v i = unsafePartial $ fromJust $ Vec.index' v i

empty :: ∀a. Matrix D0 D0 a
empty = Matrix Vec.empty

consRowVec :: ∀h h' w a. Succ h h' => Nat w =>
  Vec.Vec w a → Matrix h w a → Matrix h' w a
consRowVec vec (Matrix m) = Matrix $ Vec.cons vec m

infixr 4 consRowVec as ⤓

consColVec :: ∀h w w' a. Succ w w' => Nat h => Vec.Vec h a → Matrix h w a → Matrix h w' a
consColVec vec (Matrix m) = Matrix $ Vec.zipWithE (Vec.cons) vec m

infixr 5 consColVec as ⇥

unconsV ∷ ∀h w h' a. Pred h h' => Pos h => Pos w => Matrix h w a → { head ∷ Vec w a, tail ∷ Matrix h' w a} 
unconsV (Matrix m) = {head: head, tail: Matrix tail}
  where 
    {head, tail} = Vec.uncons m 

-- unconsH ∷ ∀h w w' a. Pred w w' => Pos h => Pos w => Matrix h w a → { head ∷ Vec h a, tail ∷ Matrix h w' a}
-- unconsH (Matrix m) = {head: ?b, tail: ?a}
--   where
--     m' = map Vec.uncons m

snocRowVec :: ∀h h' w a. Succ h h' => Nat w =>
  Vec.Vec w a → Matrix h w a → Matrix h' w a
snocRowVec vec (Matrix m) = Matrix $ Vec.snoc vec m

snocColVec :: ∀h w w' a. Succ w w' => Nat h => Vec.Vec h a → Matrix h w a → Matrix h w' a
snocColVec vec (Matrix m) = Matrix $ Vec.zipWithE (Vec.snoc) vec m

concatV :: forall h1 h2 h w a. Add h1 h2 h => Nat w => Matrix h1 w a → Matrix h2 w a → Matrix h w a
concatV (Matrix a) (Matrix b) = Matrix $ Vec.concat a b

concatH :: forall h w1 w2 w a. Add w1 w2 w => Nat h => Matrix h w1 a → Matrix h w2 a → Matrix h w a
concatH (Matrix a) (Matrix b) = Matrix $ Vec.zipWithE (Vec.concat) a b

fromVec ∷ ∀s h w a. Nat s => Nat h => Nat w => Mul h w s => Vec s a → Matrix h w a 
fromVec vec = fill f
  where
    f x y = unsafeVecIndex vec (x+w*y)
    w = toInt (undefined ∷ w)

singleton :: ∀a. a → Matrix D1 D1 a
singleton x = Vec.singleton x ⤓ Vec.empty ⇥ empty

matrix11 ∷ ∀a. a → Matrix D1 D1 a 
matrix11 = singleton

matrix12 ∷ ∀a. a → a → Matrix D1 D2 a 
matrix12 x11 x12 = Vec.singleton x11 ⇥ (singleton x12)

matrix13 ∷ ∀a. a → a → a → Matrix D1 D3 a 
matrix13 x11 x12 x13 = 
  Vec.singleton x11 ⇥ Vec.singleton x12 ⇥ (singleton x13)

matrix21 ∷ ∀a. a → a → Matrix D2 D1 a 
matrix21 x11 x21 =
  Vec.singleton x11
  ⤓
  singleton x21

matrix22 :: ∀a. a → a → a → a → Matrix D2 D2 a
matrix22 x11 x12 x21 x22 =
  matrix12 x11 x12
  `concatV`
  matrix12 x21 x22

matrix23 ∷ ∀a. a → a → a → a → a → a → Matrix D2 D3 a 
matrix23 x11 x12 x13 x21 x22 x23 =
  matrix13 x11 x12 x13
  `concatV`
  matrix13 x21 x22 x23

matrix31 ∷ ∀a. a → a → a → Matrix D3 D1 a 
matrix31 x11 x21 x31 =
  matrix21
    x11
    x21
  `concatV`
  singleton x31

matrix32 ∷ ∀a. a → a → a → a → a → a → Matrix D3 D2 a 
matrix32 x11 x12 x21 x22 x31 x32 =
  matrix31 x11 x21 x31 `concatH` matrix31 x12 x22 x32

matrix33 ∷ ∀a. a → a → a → a → a → a → a → a → a → Matrix D3 D3 a
matrix33 x11 x12 x13 x21 x22 x23 x31 x32 x33 =
    Vec.vec3 x11 x12 x13
    ⤓
    matrix23 
      x21 x22 x23
      x31 x32 x33

fill :: ∀h w a. Nat h => Nat w =>  (Int → Int → a) → Matrix h w a
fill f = Matrix $ Vec.fill (\y → Vec.fill (\x → f x y))

unsafeIndex ∷ ∀h w a. Nat h => Nat w => Matrix h w a → Int → Int → a
unsafeIndex (Matrix m) x y = (m `unsafeVecIndex` y) `unsafeVecIndex` x

replicate' :: ∀w h a. Nat w => Nat h => a → Matrix h w a
replicate' a = Matrix $ Vec.replicate' (Vec.replicate' a)

zipWithE :: ∀w h a b c. Nat w => Nat h =>
  (a → b → c) → Matrix h w a → Matrix h w b → Matrix h w c
zipWithE f (Matrix a) (Matrix b) = Matrix $ Vec.zipWithE (Vec.zipWithE f) a b

instance showMatrix :: (Nat h, Nat w, Show a) => Show (Matrix h w a) where
  show (Matrix m) = "\n  " <> (joinWith "\n  " $ Vec.toArray $ map show m)

instance functorMatrix :: (Nat h, Nat w) => Functor (Matrix h w) where
  map f (Matrix m) = Matrix $ map (map f) m


instance foldableVec ∷ (Nat h, Nat w) => Foldable (Matrix h w) where
  foldMap f (Matrix xs) = foldMap (foldMap f) xs

  foldr f z (Matrix xs) = foldr (\vec b → foldr f b vec) z xs

  foldl f z (Matrix xs) = foldl (\b vec → foldl f b vec) z xs

instance eqMatrix ∷ (Eq a, Nat h, Nat w) => Eq (Matrix h w a) where
  eq (Matrix a) (Matrix b) = a == b

addMatrix :: ∀h w a. Nat h => Nat w => CommutativeRing a => Matrix h w a → Matrix h w a → Matrix h w a
addMatrix a  b = zipWithE (+) a b

negateMatrix :: ∀h w a. Nat h => Nat w => CommutativeRing a => Matrix h w a → Matrix h w a
negateMatrix = map (\v → zero - v)

columnVec :: ∀h w a x. Nat x => Lt x w => Nat h => Matrix h w a → x → Vec.Vec h a
columnVec (Matrix m) i = map (\row → row `Vec.index` (undefined :: x) ) m

rowVec :: ∀h w a y. Nat y => Lt y h => Nat w => Matrix h w a → y → Vec.Vec w a
rowVec (Matrix m) i = m `Vec.index` (undefined :: y)

rowVecUnsafe :: ∀h w a. Nat h => Nat w => Matrix h w a → Int → Vec.Vec w a
rowVecUnsafe (Matrix m) i =  unsafePartial $ Array.unsafeIndex (Vec.toArray m) i

columnVecUnsafe :: ∀h w a. Nat h => Nat w => Matrix h w a → Int → Vec.Vec h a
columnVecUnsafe (Matrix m) i = map (\row → unsafePartial $ Array.unsafeIndex (Vec.toArray row) i) m

mulMatrix :: ∀h w a. Nat h => Nat w => CommutativeRing a => Matrix h w a → Matrix w h a → Matrix h h a
mulMatrix a b = fill (\x y → rowVecUnsafe a y `Vec.dotProduct` columnVecUnsafe b x)

transpose ∷ ∀a h w. Nat h => Nat w =>  Matrix h w a → Matrix w h a
transpose m = fill (\x y → unsafeIndex m y x)

deleteRowUnsafe ∷ ∀h h' w a. Nat w => Nat h => Nat h' => Succ h' h => Int → Matrix h w a → Matrix h' w a
deleteRowUnsafe i m = fill (\ x y → unsafeIndex m x (if y < i then y else y+1))

deleteRow ∷ ∀h h' w i a. Nat w => Nat h => Nat h' => Nat i => Succ h' h => Lt i h => i → Matrix h w a → Matrix h' w a
deleteRow it = deleteRowUnsafe $ toInt it

deleteColumn ∷ ∀h w w' i a. Nat w => Nat h => Nat w' => Nat i => Succ w' w => Lt i w => i → Matrix h w a → Matrix h w' a
deleteColumn it m = fill (\ x y → unsafeIndex m (if x < i then x else x+1) y)
  where i = toInt (undefined :: i)

resize ∷ ∀h w h' w' a. Nat h => Nat w => Nat h' => Nat w' => LtEq h' h => LtEq w' w => Matrix h w a → Matrix h' w' a
resize m = fill (unsafeIndex m)

findMaxIndex ∷ ∀s a. Ord a => Pos s => Vec s a → Int
findMaxIndex vec = fromMaybe 0 $ map snd $ maximumBy (\(Tuple a _) (Tuple b _) → compare a b) withIndex
  where
    withIndex ∷ Vec s (Tuple a Int)
    withIndex = Vec.zipWithE Tuple vec (range' 0)

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


mkPermutation ∷ ∀h w a. CommutativeRing a => Nat h => Nat w => (Int → Int) → Matrix h w a
mkPermutation pi = fill f
    where
        f x y = if pi x == y then one else zero

swapRow ∷ ∀h w a. Nat h => Nat w => Int → Int → Matrix h w a → Matrix h w a
swapRow i j m = fill f
  where
    f x y
      | y == i = unsafeIndex m x j
      | y == j = unsafeIndex m x i
      | otherwise = unsafeIndex m x y

size ∷ ∀s a. Nat s => Matrix s s a → Int 
size m = toInt (undefined ∷ s)

removeRow ∷ ∀w w' h a. Nat h => Nat w' => Nat w => Pred w w' => Int → Matrix h w a → Matrix h w' a 
removeRow i m = fill f
  where
    f x y | x > i = unsafeIndex m (x+1) y
    f x y = unsafeIndex m x y

removeColumn ∷ ∀w h h' a
  .  Nat w 
  => Nat h
  => Nat h'
  => Pred h h'
  => Int → Matrix h w a → Matrix h' w a
removeColumn i m = fill f
  where
    f x y | y > i = unsafeIndex m x (y+1)
    f x y = unsafeIndex m x y

removeCross ∷ ∀w w' h h' a
  .  Nat w
  => Nat w'
  => Nat h
  => Nat h'
  => Pred w w'
  => Pred h h'
  => Int → Int → Matrix h w a → Matrix h' w' a
removeCross x y = removeRow x <<< removeColumn y

replaceWithIdBlock ∷ ∀h w a
  .  CommutativeRing a
  => Nat w
  => Nat h
  => Int → Int → Matrix h w a → Matrix h w a
replaceWithIdBlock x y m = fill f
  where
    f i j | x == j && y == j = one
    f i j | x == i || y == j = zero
    f i j = unsafeIndex m i j

permSign ∷ ∀s a. Eq a => CommutativeRing a => Pos s => Matrix s s a → a 
permSign = toVal <<< foldl f true  <<< zipWithE (*) inversionPlaces 
  where
    inversionPlaces = fill (\x y → if (y < x) then one else zero)
    f acc a | a == zero = acc
    f acc a = not acc
    toVal true = one
    toVal false = - one

det ∷ ∀s a. Ord a => CommutativeRing a => EuclideanRing a => Pos s => Matrix s s a → a 
det m = permSign p * product diag
  where 
    {u, p} = luDecomp m

    diag ∷ Matrix s s a
    diag = fill (\x y → if x==y then unsafeIndex u x y else one)
    

matrixOne ∷ ∀h w a. Semiring a => Nat h => Nat w => Matrix h w a 
matrixOne = fill (\x y → if (x==y) then one else zero)

instance semiringMatrix :: (Nat s, CommutativeRing a) => Semiring (Matrix s s a) where
  add = addMatrix
  zero = replicate' zero
  mul = mulMatrix
  one = matrixOne

instance ringMatrix :: (Nat s, CommutativeRing a) => Ring (Matrix s s a) where
  sub a b = add a (negateMatrix b)