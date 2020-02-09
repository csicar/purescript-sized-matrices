Sized-Matrices
====

```purescript
> import Data.Typelevel.Num.Reps
> import Data.Matrix
> (one :: Matrix D3 D3 Int)
  [1,0,0]
  [0,1,0]
  [0,0,1]
> (zero :: Matrix D7 D7 Int)
  [0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0]
  [0,0,0,0,0,0,0]
> (one :: Matrix D4 D4 Int) * one
  [1,0,0,0]
  [0,1,0,0]
  [0,0,1,0]
  [0,0,0,1]
> import Data.Matrix.Reps
> m1 = matrix22 1 2 3 4 * matrix22 1 2 3 4 
> m1
  [7,10]
  [15,22]
> transpose m1
  [7,15]
  [10,22]
> concatH m1 (transpose m1)
  [7,10,7,15]
  [15,22,10,22]
> fill (\x y -> x + y) :: Matrix D3 D3 Int
  [0,1,2]
  [1,2,3]
  [2,3,4]
> import Data.Matrix.Algorithms
> det $ matrix22 1.0 2.0 3.0 4.0
-2.0
> import Data.Matrix.RegularMatrices
> recip $ RegularMatrix $ matrix22 1.0 2.0 3.0 4.0
  [-2.0,1.0]
  [1.5,-0.5]
> vec = 1 +> 2 +> 3 +> 4 +> 5 +> Vec.singleton 6
> vec
[1,2,3,4,5,6]
> fromVec vec :: Matrix D2 D3 Int
  [1,2,3]
  [4,5,6]
> fromVec vec :: Matrix D3 D2 Int
  [1,2]
  [3,4]
  [5,6]
> fromVec vec :: Matrix D2 D2 Int
... will be an error, since dimensions don't match up
```

Features
-- 

- typesafe size
- Complete `Semiring` and `Ring` implementation
- Matrix multiplication
- Transpose
- Matrix addition
- Determinant
- Inverse Matrix