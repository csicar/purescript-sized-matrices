module Data.Matrix.Reps where

import Prelude
import Data.Matrix
import Data.Matrix.Operations
import Data.Typelevel.Num.Reps (D0, D1, D2, D3)
import Data.Vec as Vec

empty :: ∀ a. Matrix D0 D0 a
empty = Matrix Vec.empty

matrix00 = empty

singleton :: ∀ a. a -> Matrix D1 D1 a
singleton x = Vec.singleton x ⤓ Vec.empty ⇥ empty

matrix11 :: ∀ a. a -> Matrix D1 D1 a
matrix11 = singleton

matrix12 :: ∀ a. a -> a -> Matrix D1 D2 a
matrix12 x11 x12 = Vec.singleton x11 ⇥ (singleton x12)

matrix13 :: ∀ a. a -> a -> a -> Matrix D1 D3 a
matrix13 x11 x12 x13 = Vec.singleton x11 ⇥ Vec.singleton x12 ⇥ (singleton x13)

matrix21 :: ∀ a. a -> a -> Matrix D2 D1 a
matrix21 x11 x21 =
  Vec.singleton x11
    ⤓ singleton x21

matrix22 :: ∀ a. a -> a -> a -> a -> Matrix D2 D2 a
matrix22 x11 x12 x21 x22 =
  matrix12 x11 x12
    `concatV`
      matrix12 x21 x22

matrix23 :: ∀ a. a -> a -> a -> a -> a -> a -> Matrix D2 D3 a
matrix23 x11 x12 x13 x21 x22 x23 =
  matrix13 x11 x12 x13
    `concatV`
      matrix13 x21 x22 x23

matrix31 :: ∀ a. a -> a -> a -> Matrix D3 D1 a
matrix31 x11 x21 x31 =
  matrix21
    x11
    x21
    `concatV`
      singleton x31

matrix32 :: ∀ a. a -> a -> a -> a -> a -> a -> Matrix D3 D2 a
matrix32 x11 x12 x21 x22 x31 x32 = matrix31 x11 x21 x31 `concatH` matrix31 x12 x22 x32

matrix33 :: ∀ a. a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix D3 D3 a
matrix33 x11 x12 x13 x21 x22 x23 x31 x32 x33 =
  Vec.vec3 x11 x12 x13
    ⤓ matrix23
        x21
        x22
        x23
        x31
        x32
        x33
