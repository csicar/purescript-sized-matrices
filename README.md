Sized-Matrices
====

```purescript
> one :: Matrix D3 D3 Int
  [1,0,0]
  [0,1,0]
  [0,0,1]
> zero :: Matrix D7 D7 Int
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
> m1 = matrix2d 1 2 3 4 * matrix2d 1 2 3 4 
  [7,10]
  [15,22]
> transpose m1
  [7,15]
  [10,22]

```

Features
-- 

- typesafe size
- Complete `Semiring` and `Ring` implementation
- Mmtrix multiplication
- transpose
- matrix addition