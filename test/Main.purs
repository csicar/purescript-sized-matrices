module Test.Main where

import Effect
import Prelude
import Data.Foldable (maximumBy, product)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (pow)
import Data.Matrix (Matrix(..), column, fill, height, replicate', row, unsafeIndex, width, zipWithE, (\\))
import Data.Matrix.Algorithms (det, inverse)
import Data.Matrix.Operations ((⇥), (⤓), findMaxIndex, (&))
import Data.Matrix.Reps (matrix11, matrix22, matrix33, empty, row3)
import Data.Matrix.Transformations (resize, transpose, mkPermutation)
import Data.Maybe (fromJust)
import Data.Rational (Rational, fromInt, (%), toNumber)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple as Tuple
import Data.Typelevel.Num (D3, D2, D1, d0, d1, D5, D6)
import Data.Vec (vec2, (+>))
import Data.Vec as Vec
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (Result, (===), (<?>))
import Test.QuickCheck.Laws.Data (checkDivisionRing, checkFoldable, checkFunctor, checkMonoid, checkRing, checkSemigroup, checkSemiring)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Data.VectorField ((.*), (*.))
import Effect.Class.Console (logShow)

a1 :: Matrix D3 D3 Number
a1 =
  row3 1.0 4.0 (-1.0)
    \\ row3 3.0 0.0 5.0
    \\ row3 2.0 2.0 1.0

a2 :: Matrix D3 D3 Rational
a2 =
  map fromInt
    $ row3 1 4 (-1)
    \\ row3 3 (-12) 8
    \\ row3 2 (-6) 3

a3 :: Matrix D3 D3 Number
a3 =
  row3 0.0 4.0 (-1.0)
    \\ row3 1.0 2.0 1.0
    \\ row3 2.0 1.0 5.0

a4 :: Matrix D3 D3 Rational
a4 = map fromInt $ row3 1 4 5 \\ row3 1 6 11 \\ row3 2 6 7

a5 ∷ Matrix D3 D3 Number
a5 = row3 2.0 0.0 0.0 \\ row3 0.0 3.0 0.0 \\ row3 0.0 0.0 4.0

main ::
  Effect
    Unit
main =
  runTest do
    suite "Data.Matrix" do
      let
        a = matrix33 1.0 4.0 (-1.0) 3.0 0.0 5.0 2.0 2.0 1.0

        p = matrix33 0 0 0 1 0 0 0 1 0
      -- test "check with quickcheck laws" $ liftEffect $ do
      --   let matrixProxy = Proxy :: Proxy (Matrix D5 D5 Int)
      --   let matrixProxy2 = Proxy2 :: Proxy2 (Matrix D5 D5)
      --   checkSemigroup matrixProxy
      --   checkMonoid matrixProxy
      --   checkFunctor matrixProxy2
      --   checkFoldable matrixProxy2
      --   checkSemiring matrixProxy
      --   checkRing matrixProxy
      test "mkPermutation" do
        equal p (mkPermutation (_ + 1))
      suite "inverse" do
        test "inverse1" do
          let
            m = map (_ % 1) $ matrix22 1 2 3 4

            mInv = map (_ % 2) $ matrix22 (-4) 2 3 (-1)
          equal mInv (inverse m)
        test "inverse2" do
          let
            m = map (_ % 1) $ matrix33 2 (-1) 0 (-1) 2 (-1) 0 (-1) 2

            mInv = map (_ % 4) $ matrix33 3 2 1 2 4 2 1 2 3
          equal mInv (inverse m)
        test "inverse3" do
          let
            m = map (_ % 1) $ matrix33 3 2 5 1 1 3 2 4 6

            mInv = map (_ % (-8)) $ matrix33 (-6) 8 1 0 8 (-4) 2 (-8) 1
          equal mInv (inverse m)
        test "A^(-1) * A = I examples" do
          equal one (a4 * inverse a4)
          equal one (a2 * inverse a2)
        test "I^(-1) = I" do
          equal (one :: Matrix D5 D5 Rational) (inverse one)
          equal (one :: Matrix D6 D6 Rational) (inverse one)
      suite "determinant" do
        test "det1" do
          let
            m = map fromInt $ matrix33 1 4 (-1) 3 0 5 2 2 1
          equal (fromInt 12) (det m)
        test "det2" do
          let
            m2 = map fromInt $ matrix33 1 8 (-1) 3 21 5 2 (-6) 1
          equal (fromInt 167) (det m2)
        test "det(A) = det(A^T)" do
          quickCheck
            $ \(m :: Matrix D5 D5 Int) ->
                det m === det (transpose m)
        test "determinant of triangle matrix is product of diagonal" do
          quickCheck
            $ \(x11 :: Int) x12 x13 x22 x23 x33 ->
                let
                  m =
                    matrix33
                      x11
                      x12
                      x13
                      0
                      x22
                      x23
                      0
                      0
                      x33
                in
                  det m === product [ x11, x22, x33 ]
        test "determinant of 1x1 Matrix is its value" do
          quickCheck
            $ \(x11 :: Int) ->
                det (matrix11 x11) === x11
      test "consRowVec" do
        let
          b = matrix22 1 2 3 4

          m2 = vec2 7 8 ⤓ b
        equal 3 $ height m2
        equal 2 $ width m2
        equal 7 $ unsafePartial $ unsafeIndex m2 0 0
        equal 8 $ unsafePartial $ unsafeIndex m2 1 0
        equal 1 $ unsafePartial $ unsafeIndex m2 0 1
        equal 2 $ unsafePartial $ unsafeIndex m2 1 1
        equal 3 $ unsafePartial $ unsafeIndex m2 0 2
        equal 4 $ unsafePartial $ unsafeIndex m2 1 2
      test "consColVec" do
        let
          b = matrix22 1 2 3 4

          m2 = vec2 7 8 ⇥ b
        equal 2 $ height m2
        equal 3 $ width m2
        equal 7 $ unsafePartial $ unsafeIndex m2 0 0
        equal 1 $ unsafePartial $ unsafeIndex m2 1 0
        equal 2 $ unsafePartial $ unsafeIndex m2 2 0
        equal 8 $ unsafePartial $ unsafeIndex m2 0 1
        equal 3 $ unsafePartial $ unsafeIndex m2 1 1
        equal 4 $ unsafePartial $ unsafeIndex m2 2 1
      test "fill" do
        let
          m = fill (\x y → (1 + x) * (1 + y))

          m' = matrix33 1 2 3 2 4 6 3 6 9
        equal m m'
      test "replicate'" do
        let
          m = replicate' "hi"

          m' = matrix22 "hi" "hi" "hi" "hi"
        equal m' m
      test "zipWithE" do
        let
          m = matrix22 "hi" "hello" "foo" "bar"

          n = matrix22 "there" "asd" "bsd" "asd"

          u = matrix22 "hithere" "helloasd" "foobsd" "barasd"
        equal u $ zipWithE (<>) m n
      test "map" do
        let
          m = matrix22 1 2 3 4

          n = matrix22 1 4 9 16
        equal n $ map (_ `pow` 2) m
      test "add" do
        let
          m = matrix22 1 2 3 4

          n = matrix22 5 6 7 8

          r = matrix22 6 8 10 12
        equal r $ m + n
      test "negate" do
        let
          m = matrix22 1 2 3 4

          r = matrix22 (-1) (-2) (-3) (-4)
        equal m $ negate r
      test "column" do
        let
          m = matrix22 5 3 0 6

          r = vec2 5 0

          r' = vec2 3 6
        equal r $ column m d0
        equal r' $ column m d1
      test "row" do
        let
          m = matrix22 5 3 0 6

          r = vec2 5 3

          r' = vec2 0 6
        equal r $ row m d0
        equal r' $ row m d1
      test "mul" do
        let
          m = matrix33 1 2 3 4 5 6 7 8 9

          n = matrix33 1 0 0 2 3 0 3 0 0

          r = matrix33 14 6 0 32 15 0 50 24 0

          r' = matrix33 1 2 3 14 19 24 3 6 9
        equal m $ one * m
        equal n $ one * n
        equal zero $ zero * m
        equal zero $ zero * n
        equal r $ m * n
        equal r' $ n * m
      suite "transpose" do
        test "example 1" do
          let
            m = matrix22 1 2 3 4

            m' = matrix22 1 3 2 4
          equal m' $ transpose m
        test "self-inverse" do
          quickCheck \(m :: Matrix D5 D5 Int) ->
            m == transpose (transpose m)
      test "resize" do
        let
          a = matrix33 1.0 4.0 (-1.0) 3.0 0.0 5.0 2.0 2.0 1.0

          sa = resize a

          sa' = matrix22 1.0 4.0 3.0 0.0
        equal sa' sa
      test "findMaxIndex" do
        equal (findMaxIndex (4 +> 2 +> 5 +> 1 +> Vec.empty)) 2
        quickCheck \(x :: Int) ->
          (findMaxIndex (x +> Vec.empty)) === 0
        quickCheck \(x1 :: Int) x2 x3 ->
          let
            actual = findMaxIndex (x1 +> x2 +> x3 +> Vec.empty)

            expected =
              unsafePartial
                ( [ x1, x2, x3 ]
                    # mapWithIndex Tuple
                    # maximumBy (compare `on` Tuple.snd)
                    # fromJust
                    # Tuple.fst
                )
          in
            actual === expected
