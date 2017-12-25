module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Matrix (lrSingle, matrix3d)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main ::                  
  Eff                         
    ( console :: CONSOLE      
    , testOutput :: TESTOUTPUT
    , avar :: AVAR          
    )                         
    Unit
main = runTest do
  suite "Data.Matrix" do
    let 
      a = matrix3d 1.0 4.0 (0.0 - 1.0) 3.0 0.0 5.0 2.0 2.0 1.0
      lr_a = matrix3d 1.0 4.0 (0.0-1.0) 3.0 (0.0-12.0) 8.0 2.0 0.5 (0.0-1.0)
    test "lr" do
      equal lr_a $ lrSingle a
