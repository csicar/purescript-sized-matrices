module Data.Matrix
  ( Matrix(..)
  , fill
  , fromVec
  , toArray
  , replicate'
  , height
  , width
  , size
  , index
  , index'
  , unsafeIndex
  , column
  , columnUnsafe
  , row
  , rowUnsafe
  , concatH
  , concatV
  , zipWithE
  , zipE
  , negateMatrix
  , mulMatrix
  , matrixOne
  , matrixZero
  , scalarMul
  , (\\)
  ) where

import Prelude
import Data.Array as Array
import Data.VectorField (class VectorField)
import Data.Group (class Group)
import Data.Semigroup.Commutative (class Commutative)
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
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (vectorOf)

-- stored as Vec of rows
-- | Matrix with height `h`, width `w` and contained value `a`
newtype Matrix h w a
  = Matrix (Vec.Vec h (Vec.Vec w a))

-- | ## Creation
-- | create Matrix of size hxw with a generator-function
-- |
-- | ```purescript
-- | > fill (\x y -> x) :: Matrix D2 D3 Int
-- |   [0,1,2]
-- |   [0,1,2]
-- | ```
-- |
fill :: ∀ h w a. Nat h => Nat w => (Int -> Int -> a) -> Matrix h w a
fill f = Matrix $ Vec.fill (\y -> Vec.fill (\x -> f x y))

-- |
-- | ```purescript
-- | > fromVec (Vec.vec3 1 2 3) :: Matrix D1 D3 _
-- |   [1,2,3]
-- | 
-- | > fromVec (Vec.vec3 1 2 3) :: Matrix D3 D1 _
-- |   [1]
-- |   [2]
-- |   [3]
-- | ```
-- |
fromVec :: ∀ s h w a. Nat s => Nat h => Nat w => Mul h w s => Vec s a -> Matrix h w a
fromVec vec = fill f
  where
  f x y = unsafePartial $ unsafeVecIndex vec (x + w * y)

  w = toInt (undefined :: w)

-- | Convert Matrix to Array
-- |
-- | ```purescript
-- | > toArray (matrix22 1 2 3 4)
-- | [[1,2],[3,4]]
-- | ```
-- |
toArray :: ∀ h w a. Nat h => Nat w => Matrix h w a -> Array (Array a)
toArray (Matrix m) = Vec.toArray (map Vec.toArray m)

-- | create Matrix with one value
-- |
-- | ```purescript
-- | > replicate' "-" :: Matrix D3 D5 _
-- |  ["-","-","-","-","-"]
-- |  ["-","-","-","-","-"]
-- |  ["-","-","-","-","-"]
-- | ```
-- |
replicate' :: ∀ w h a. Nat w => Nat h => a -> Matrix h w a
replicate' a = Matrix $ Vec.replicate' (Vec.replicate' a)

-- | ## Basic Accesors
-- | height of the matrix (aka number of rows)
height :: ∀ h w a. Nat h => Nat w => Matrix h w a -> Int
height _ = toInt (undefined :: h)

-- | width of the matrix (aka number of columns)
width :: ∀ h w a. Nat h => Nat w => Matrix h w a -> Int
width _ = toInt (undefined :: w)

-- | size of square matrix
size :: ∀ s a. Nat s => Matrix s s a -> Int
size m = toInt (undefined :: s)

-- | value a position in matrix:
-- |
-- | ```purescript
-- | > m
-- |  [1,2,-3]
-- |  [4,9,0]
-- |  [11,2,8]
-- | > index d0 d2 m -- d0, d2 are value-representations of D0 D2
-- | 11
-- | ```
-- |
index :: ∀ x y h w a. Nat x => Nat y => Lt x w => Lt y h => x -> y -> Matrix h w a -> a
index _ _ (Matrix m) = Vec.index (Vec.index m (undefined :: y)) (undefined :: x)

-- | value a position in matrix:
-- |
-- | ```purescript
-- | > m
-- |  [1,2,-3]
-- |  [4,9,0]
-- |  [11,2,8]
-- | > index 0 2 m
-- | Just 11
-- | > index 10 2 m
-- | Nothing
-- | ```
-- |
index' :: ∀ h w a. Nat h => Nat w => Int -> Int -> Matrix h w a -> Maybe a
index' x y (Matrix m) = do
  r <- Vec.index' m y
  Vec.index' r x

unsafeIndex :: ∀ h w a. Partial => Nat h => Nat w => Matrix h w a -> Int -> Int -> a
unsafeIndex (Matrix m) x y = (m `unsafeVecIndex` y) `unsafeVecIndex` x

unsafeVecIndex :: ∀ s a. Partial => Nat s => Vec s a -> Int -> a
unsafeVecIndex v i = unsafePartial $ fromJust $ Vec.index' v i

-- | get vector for column
-- |
-- | ```purescript
-- | > m
-- |   [1,2]
-- |   [0,5]
-- | > column m d1
-- | [2,5]
-- | ```
column :: ∀ h w a x. Nat x => Lt x w => Nat h => Matrix h w a -> x -> Vec.Vec h a
column (Matrix m) i = map (\r -> r `Vec.index` (undefined :: x)) m

columnUnsafe :: ∀ h w a. Partial => Nat h => Nat w => Matrix h w a -> Int -> Vec.Vec h a
columnUnsafe (Matrix m) i = map (\r -> unsafePartial $ Array.unsafeIndex (Vec.toArray r) i) m

-- | get vector for row
-- |
-- | ```purescript
-- | > m
-- |   [1,2]
-- |   [0,5]
-- | > row m d1
-- | [0,5]
-- | ```
row :: ∀ h w a y. Nat y => Lt y h => Nat w => Matrix h w a -> y -> Vec.Vec w a
row (Matrix m) i = m `Vec.index` (undefined :: y)

rowUnsafe :: ∀ h w a. Partial => Nat h => Nat w => Matrix h w a -> Int -> Vec.Vec w a
rowUnsafe (Matrix m) i = unsafePartial $ Array.unsafeIndex (Vec.toArray m) i

-- | ## Basic Operations
-- |
-- | ```purescript
-- | > matrix22 1 2 3 4 `concatV` matrix22 0 1 2 0
-- |  [1,2]
-- |  [3,4]
-- |  [0,1]
-- |  [2,0]
-- | ```
-- |
concatV :: ∀ h1 h2 h w a. Add h1 h2 h => Nat w => Matrix h1 w a -> Matrix h2 w a -> Matrix h w a
concatV (Matrix a) (Matrix b) = Matrix $ Vec.concat a b

infixr 3 concatV as \\

-- |
-- | ```purescript
-- | > matrix22 1 2 3 4 `concatH` matrix22 0 1 2 0
-- |  [1,2,0,1]
-- |  [3,4,2,0]
-- | ```
-- |
concatH :: ∀ h w1 w2 w a. Add w1 w2 w => Nat h => Matrix h w1 a -> Matrix h w2 a -> Matrix h w a
concatH (Matrix a) (Matrix b) = Matrix $ Vec.zipWithE (Vec.concat) a b

-- | Zip Matrices with function with **E**xactly the same size
-- |
-- | ```purescript
-- | > zipWithE Tuple (matrix22 1 2 0 0) (matrix22 1 3 4 5)
-- |  [(Tuple 1 1),(Tuple 2 3)]
-- |  [(Tuple 0 4),(Tuple 0 5)]
-- | ```
-- |
zipWithE ::
  ∀ w h a b c.
  Nat w =>
  Nat h =>
  (a -> b -> c) -> Matrix h w a -> Matrix h w b -> Matrix h w c
zipWithE f (Matrix a) (Matrix b) = Matrix $ Vec.zipWithE (Vec.zipWithE f) a b

-- | Zip Matrices with **E**xactly the same size
-- |
-- | ```purescript
-- | > zipE (matrix22 1 2 0 0) (matrix22 1 3 4 5)
-- |  [(Tuple 1 1),(Tuple 2 3)]
-- |  [(Tuple 0 4),(Tuple 0 5)]
-- | ```
-- |
zipE :: ∀ w h a b. Nat w => Nat h => Matrix h w a -> Matrix h w b -> Matrix h w (Tuple a b)
zipE = zipWithE Tuple

addMatrix :: ∀ h w a. Nat h => Nat w => Semiring a => Matrix h w a -> Matrix h w a -> Matrix h w a
addMatrix a b = zipWithE (+) a b

negateMatrix :: ∀ h w a. Nat h => Nat w => Ring a => Matrix h w a -> Matrix h w a
negateMatrix = map (\v -> zero - v)

mulMatrix :: ∀ h w a. Nat h => Nat w => CommutativeRing a => Matrix h w a -> Matrix w h a -> Matrix h h a
mulMatrix a b = unsafePartial $ fill (\x y -> rowUnsafe a y `Vec.dotProduct` columnUnsafe b x)

matrixOne :: ∀ h w a. Semiring a => Nat h => Nat w => Matrix h w a
matrixOne = fill (\x y -> if (x == y) then one else zero)

matrixZero :: ∀ h w a. Semiring a => Nat h => Nat w => Matrix h w a
matrixZero = replicate' zero

scalarMul :: ∀ h w a. Nat h => Nat w => Semiring a => a -> Matrix h w a -> Matrix h w a
scalarMul a = map (a * _)

instance semigroupMatrix :: (Nat h, Nat w, Semiring a) => Semigroup (Matrix h w a) where
  append = addMatrix

instance commutativeSemigroupMatrix :: (Nat h, Nat w, Semiring a) => Commutative (Matrix h w a)

instance monoidMatrix :: (Nat h, Nat w, Semiring a) => Monoid (Matrix h w a) where
  mempty = matrixZero

instance groupMatrix :: (Nat h, Nat w, Ring a) => Group (Matrix h w a) where
  ginverse = negateMatrix

instance matrixVectorField :: (Field k, Nat s) => VectorField (Matrix s s) k where
  scalarMul = scalarMul

instance showMatrix :: (Nat h, Nat w, Show a) => Show (Matrix h w a) where
  show (Matrix m) = "\n  " <> (joinWith "\n  " $ Vec.toArray $ map show m)

instance functorMatrix :: (Nat h, Nat w) => Functor (Matrix h w) where
  map f (Matrix m) = Matrix $ map (map f) m

instance foldableVec :: (Nat h, Nat w) => Foldable (Matrix h w) where
  foldMap f (Matrix xs) = foldMap (foldMap f) xs
  foldr f z (Matrix xs) = foldr (\vec b -> foldr f b vec) z xs
  foldl f z (Matrix xs) = foldl (\b vec -> foldl f b vec) z xs

instance eqMatrix :: (Eq a, Nat h, Nat w) => Eq (Matrix h w a) where
  eq (Matrix a) (Matrix b) = a == b

instance semiringMatrix :: (Nat s, CommutativeRing a) => Semiring (Matrix s s a) where
  add = addMatrix
  zero = matrixZero
  mul = mulMatrix
  one = matrixOne

instance ringMatrix :: (Nat s, CommutativeRing a) => Ring (Matrix s s a) where
  sub a b = add a (negateMatrix b)

instance arbitratyMatrix :: (Nat h, Nat w, Arbitrary a) => Arbitrary (Matrix h w a) where
  arbitrary = do
    let
      width = toInt (undefined :: w)
    let
      height = toInt (undefined :: h)
    values :: Array (Array a) <- vectorOf width $ vectorOf height arbitrary
    pure $ fill (\x y -> (values `unsafeArrayIndex` x) `unsafeArrayIndex` y)
    where
    unsafeArrayIndex :: ∀ a. Array a -> Int -> a
    unsafeArrayIndex array i = unsafePartial $ fromJust $ Array.index array i
