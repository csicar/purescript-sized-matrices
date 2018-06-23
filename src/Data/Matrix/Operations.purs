module Data.Matrix.Operations where

import Prelude
import Data.Matrix
import Data.Tuple (Tuple(..), snd)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximumBy, product)
import Data.Vec (Vec, range')
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.Vec as Vec
import Data.Typelevel.Num (class Pos, class Nat, class Succ, class Pred)

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


findMaxIndex ∷ ∀s a. Ord a => Pos s => Vec s a → Int
findMaxIndex vec = fromMaybe 0 $ map snd $ maximumBy (\(Tuple a _) (Tuple b _) → compare a b) withIndex
  where
    withIndex ∷ Vec s (Tuple a Int)
    withIndex = Vec.zipWithE Tuple vec (range' 0)


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
