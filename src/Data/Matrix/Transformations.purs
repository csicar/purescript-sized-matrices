module Data.Matrix.Transformations where

import Prelude
import Data.Matrix
import Data.Typelevel.Num (class LtEq, class Nat, class Succ, class Lt, toInt)
import Data.Typelevel.Undefined (undefined)



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

swapRow ∷ ∀h w a. Nat h => Nat w => Int → Int → Matrix h w a → Matrix h w a
swapRow i j m = fill f
  where
    f x y
      | y == i = unsafeIndex m x j
      | y == j = unsafeIndex m x i
      | otherwise = unsafeIndex m x y
