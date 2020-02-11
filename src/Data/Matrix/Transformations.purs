module Data.Matrix.Transformations where

import Data.Matrix
import Prelude
import Data.Typelevel.Num (class LtEq, class Nat, class Succ, class Lt, toInt)
import Data.Typelevel.Undefined (undefined)
import Partial.Unsafe (unsafePartial)

transpose :: ∀ a h w. Nat h => Nat w => Matrix h w a -> Matrix w h a
transpose m = unsafePartial $ fill (\x y -> unsafeIndex m y x)

deleteRowUnsafe :: ∀ h h' w a. Nat w => Nat h => Nat h' => Succ h' h => Int -> Matrix h w a -> Matrix h' w a
deleteRowUnsafe i m = unsafePartial $ fill (\x y -> unsafeIndex m x (if y < i then y else y + 1))

deleteRow :: ∀ h h' w i a. Nat w => Nat h => Nat h' => Nat i => Succ h' h => Lt i h => i -> Matrix h w a -> Matrix h' w a
deleteRow it = deleteRowUnsafe $ toInt it

deleteColumn :: ∀ h w w' i a. Nat w => Nat h => Nat w' => Nat i => Succ w' w => Lt i w => i -> Matrix h w a -> Matrix h w' a
deleteColumn it m = unsafePartial $ fill (\x y -> unsafeIndex m (if x < i then x else x + 1) y)
  where
  i = toInt (undefined :: i)

resize :: ∀ h w h' w' a. Nat h => Nat w => Nat h' => Nat w' => LtEq h' h => LtEq w' w => Matrix h w a -> Matrix h' w' a
resize m = unsafePartial $ fill (unsafeIndex m)

swapRow :: ∀ h w a. Nat h => Nat w => Int -> Int -> Matrix h w a -> Matrix h w a
swapRow i j m = unsafePartial $ fill f
  where
  f :: Partial => Int -> Int -> a
  f x y
    | y == i = unsafeIndex m x j
    | y == j = unsafeIndex m x i
    | otherwise = unsafeIndex m x y

mkPermutation :: ∀ h w a. CommutativeRing a => Nat h => Nat w => (Int -> Int) -> Matrix h w a
mkPermutation pi = fill f
  where
  f x y = if pi x == y then one else zero
