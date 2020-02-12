module Data.Matrix.Reps where

import Prelude
import Data.Matrix
import Data.Matrix.Operations
import Data.Typelevel.Num.Reps (D0, D1, D2, D3)
import Data.Typelevel.Num (class Nat)
import Data.Vec as Vec

emptyRow :: ∀ a w. Nat w => Matrix D0 w a
emptyRow = Matrix Vec.empty

empty :: ∀ a h. Nat h => Matrix h D0 a
empty = Matrix $ Vec.replicate' Vec.empty

matrix00 :: ∀ a. Matrix D0 D0 a
matrix00 = empty

singleton :: ∀ a. a -> Matrix D1 D1 a
singleton x = Vec.singleton x ⤓ Vec.empty ⇥ empty

row1 :: ∀ a. a -> Matrix D1 D1 a
row1 = singleton

matrix11 :: ∀ a. a -> Matrix D1 D1 a
matrix11 = singleton

row2 :: ∀ a. a -> a -> Matrix D1 D2 a
row2 x11 x12 = x11 & x12 & empty

matrix12 :: ∀ a. a -> a -> Matrix D1 D2 a
matrix12 = row2

row3 :: ∀ a. a -> a -> a -> Matrix D1 D3 a
row3 x11 x12 x13 = x11 & x12 & x13 & empty

matrix13 :: ∀ a. a -> a -> a -> Matrix D1 D3 a
matrix13 = row3

matrix21 :: ∀ a. a -> a -> Matrix D2 D1 a
matrix21 x11 x21 =
  emptyRow
    \\ row1 x11
    \\ row1 x21

matrix22 :: ∀ a. a -> a -> a -> a -> Matrix D2 D2 a
matrix22 x11 x12 x21 x22 =
  emptyRow
    \\ row2 x11 x12
    \\ row2 x21 x22

matrix23 :: ∀ a. a -> a -> a -> a -> a -> a -> Matrix D2 D3 a
matrix23 x11 x12 x13 x21 x22 x23 =
  emptyRow
    \\ row3 x11 x12 x13
    \\ row3 x21 x22 x23

matrix31 ∷ ∀ a. a → a → a → Matrix D3 D1 a
matrix31 x11 x21 x31 = singleton x11 \\ singleton x21 \\ singleton x31

matrix32 :: ∀ a. a -> a -> a -> a -> a -> a -> Matrix D3 D2 a
matrix32 x11 x12 x21 x22 x31 x32 =
  emptyRow
    \\ row2 x11 x12
    \\ row2 x21 x22
    \\ row2 x31 x32

matrix33 :: ∀ a. a -> a -> a -> a -> a -> a -> a -> a -> a -> Matrix D3 D3 a
matrix33 x11 x12 x13 x21 x22 x23 x31 x32 x33 =
  emptyRow
    \\ row3 x11 x12 x13
    \\ row3 x21 x22 x23
    \\ row3 x31 x32 x33
