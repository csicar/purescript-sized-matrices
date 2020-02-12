module Data.Matrix.Operations where

import Prelude
import Data.Matrix
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximumBy, product)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Vec (Vec)
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Vec as Vec
import Data.Typelevel.Num (class Pos, class Nat, class Succ, class Pred, d0, toInt, D1)
import Partial.Unsafe (unsafePartial)

consRowVec ::
  ∀ h h' w a.
  Succ h h' =>
  Nat w =>
  Vec.Vec w a -> Matrix h w a -> Matrix h' w a
consRowVec vec (Matrix m) = Matrix $ Vec.cons vec m

-- |
-- | ```purescript
-- | > (vec2 1 2) ⤓ (matrix22 0 2 3 5)
-- |  [1,2]
-- |  [0,2]
-- |  [3,5]
-- | ```
-- |
infixr 4 consRowVec as ⤓

consColVec :: ∀ h w w' a. Succ w w' => Nat h => Vec.Vec h a -> Matrix h w a -> Matrix h w' a
consColVec vec (Matrix m) = Matrix $ Vec.zipWithE (Vec.cons) vec m

-- |
-- | ```purescript
-- | > (Vec.vec2 1 2) ⇥ (matrix22 0 2 3 5)
-- |   [2,3,5]
-- |   [1,0,2]
-- | ```
-- |
infixr 5 consColVec as ⇥

consSingle :: ∀ a s s'. Succ s s' => a -> Matrix D1 s a -> Matrix D1 s' a
consSingle a = consColVec (Vec.singleton a)

--| ```purescript
--| > 1 & 2 & 3 & empty
--|    [1, 2, 3]
--| ```
-- |
infixr 4 consSingle as &

-- |
-- | ```purescript
-- | > unconsV $ matrix22 1 2 3 4
-- | { head: [1,2], tail: 
-- |   [3,4] }
-- | ```
-- |
unconsV :: ∀ h w h' a. Pred h h' => Pos h => Pos w => Matrix h w a -> { head :: Vec w a, tail :: Matrix h' w a }
unconsV (Matrix m) = { head: head, tail: Matrix tail }
  where
  { head, tail } = Vec.uncons m

-- |
-- | ```purescript
-- | > unconsH $ matrix22 1 2 3 4
-- | { head: [1,3], tail: 
-- |   [2]
-- |   [4] }
-- | ```
-- |
unconsH :: ∀ h w w' a. Pred w w' => Pos h => Pos w => Matrix h w a -> { head :: Vec h a, tail :: Matrix h w' a }
unconsH (Matrix m) = { head: _.head <$> m', tail: Matrix $ _.tail <$> m' }
  where
  m' = map Vec.uncons m

snocRowVec ::
  ∀ h h' w a.
  Succ h h' =>
  Nat w =>
  Vec.Vec w a -> Matrix h w a -> Matrix h' w a
snocRowVec vec (Matrix m) = Matrix $ Vec.snoc vec m

snocColVec :: ∀ h w w' a. Succ w w' => Nat h => Vec.Vec h a -> Matrix h w a -> Matrix h w' a
snocColVec vec (Matrix m) = Matrix $ Vec.zipWithE (Vec.snoc) vec m

findMaxIndex :: ∀ s a. Ord a => Pos s => Vec s a -> Int
findMaxIndex vec = fromMaybe 0 $ map fst $ maximumBy (compare `on` snd) $ mapWithIndex Tuple vec

-- | remove row at index. If index is out of bounds, there will be no change
-- |
-- | ```purescript
-- | > m
-- |  [1,2]
-- |  [3,4]
-- |
-- | > removeRow 0 m
-- |  [2]
-- |  [4]
-- | ```
removeRow :: ∀ w w' h a. Nat h => Nat w' => Nat w => Pred w w' => Int -> Matrix h w a -> Matrix h w' a
removeRow i m = fill f
  where
  f x y
    | x >= i' = unsafePartial $ unsafeIndex m (x + 1) y

  f x y = unsafePartial $ unsafeIndex m x y

  w = width m

  i'
    | i < 0 = 0

  i' = i

-- | remove column at index. If index is out of bounds, there will be no change
-- |
-- | ```purescript
-- | > m
-- |   [1,2]
-- |   [3,4]
-- | 
-- | > removeColumn 1 m
-- |   [1,2]
-- | ```
-- |
removeColumn ::
  ∀ w h h' a.
  Nat w =>
  Nat h =>
  Nat h' =>
  Pred h h' =>
  Int -> Matrix h w a -> Matrix h' w a
removeColumn i m = fill f
  where
  f x y
    | y >= i' = unsafePartial $ unsafeIndex m x (y + 1)

  f x y = unsafePartial $ unsafeIndex m x y

  h = height m

  i'
    | i < 0 = 0

  i' = i

removeCross ::
  ∀ w w' h h' a.
  Nat w =>
  Nat w' =>
  Nat h =>
  Nat h' =>
  Pred w w' =>
  Pred h h' =>
  Int -> Int -> Matrix h w a -> Matrix h' w' a
removeCross x y = removeRow x <<< removeColumn y

replaceWithIdBlock ::
  ∀ h w a.
  CommutativeRing a =>
  Nat w =>
  Nat h =>
  Int -> Int -> Matrix h w a -> Matrix h w a
replaceWithIdBlock x y m = fill f
  where
  f i j
    | x == j && y == j = one

  f i j
    | x == i || y == j = zero

  f i j = unsafePartial $ unsafeIndex m i j
