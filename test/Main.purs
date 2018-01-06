module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Int (pow)
import Data.Matrix (Matrix(..), a2, a3, columnVec, fill, height, lrSplit, matrix22, matrix33, mkPermutation, replicate', resize, rowVec, transpose, unsafeIndex, width, zipWithE, (⇥), (⤓))
import Data.Rational (Rational)
import Data.Typelevel.Num (D3, d0, d1)
import Data.Vec (vec2)
import Test.QuickCheck (Result, (===))
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

identityProp ∷ Matrix D3 D3 Rational → Result
identityProp m = let 
  {l, r, p} = lrSplit m
  in
    l*r === p*m

main ::                  
  Eff                         
    ( console :: CONSOLE
    , random :: RANDOM
    , testOutput :: TESTOUTPUT
    , avar :: AVAR          
    )                         
    Unit
main = runTest do
      
  suite "Data.Matrix" do
    let
      a = matrix33 1.0 4.0 (0.0 - 1.0) 3.0 0.0 5.0 2.0 2.0 1.0
      p = matrix33 0 0 0 1 0 0 0 1 0
     
    test "mkPermutation" do
      equal p (mkPermutation (_ + 1))
    -- test "lrSplit quickCheck" do
      -- quickCheck identityProp -- needs Arbituary implementation
    test "lrSplit 3" do
      let
        {l, r, p} = lrSplit a3
      equal (p*a3) (l*r)
    test "lrSplit 2" do
      let
        {l, r, p} = lrSplit a2
      equal (p * a2) (l * r)
    test "consRowVec" do
      let
        b = matrix22 1 2 3 4
        m2 = vec2 7 8 ⤓ b
      equal 3 $ height m2
      equal 2 $ width m2

      equal 7 $ unsafeIndex m2 0 0
      equal 8 $ unsafeIndex m2 1 0
      equal 1 $ unsafeIndex m2 0 1
      equal 2 $ unsafeIndex m2 1 1
      equal 3 $ unsafeIndex m2 0 2
      equal 4 $ unsafeIndex m2 1 2

    test "consColVec" do
      let
        b = matrix22 1 2 3 4
        m2 = vec2 7 8 ⇥ b
      equal 2 $ height m2
      equal 3 $ width m2

      equal 7 $ unsafeIndex m2 0 0
      equal 1 $ unsafeIndex m2 1 0
      equal 2 $ unsafeIndex m2 2 0
      equal 8 $ unsafeIndex m2 0 1
      equal 3 $ unsafeIndex m2 1 1
      equal 4 $ unsafeIndex m2 2 1
    
    test "fill" do
      let
        m = fill (\x y → (1+x)*(1+y))
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
        r = matrix22 (0-1) (0-2) (0-3) (0-4)
      equal m $ negate r
    
    test "columnVec" do
      let
        m = matrix22 5 3 0 6
        r = vec2 5 0
        r' = vec2 3 6
      equal r $ columnVec m d0
      equal r' $ columnVec m d1
    
    test "rowVec" do
      let
        m = matrix22 5 3 0 6
        r = vec2 5 3
        r' = vec2 0 6
      equal r $ rowVec m d0
      equal r' $ rowVec m d1
    
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
    
    test "transpose" do
      let
        m = matrix22 1 2 3 4
        m' = matrix22 1 3 2 4
      equal m' $ transpose m
    test "resize" do
      let 
        a = matrix33 1.0 4.0 (0.0 - 1.0) 3.0 0.0 5.0 2.0 2.0 1.0        
        sa = resize a
        sa' = matrix22 1.0 4.0 3.0 0.0
      equal sa' sa
