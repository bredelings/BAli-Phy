{-# LANGUAGE NoImplicitPrelude #-}

import Numeric.LinearAlgebra
import Compiler.Fractional
import Compiler.Num
import System.IO (print)

-- Exercise the hmatrix construction and conversion interface for both native
-- element representations, including singleton and empty row conformance.
main = do
    print (toLists (matrix 3 [1..6]))
    print (toLists (row [1,2,3]), toLists (col [4,5]))
    let intRows = [fromList [1,2], fromList [3]] :: [Vector Int]
    print (toLists (fromRows intRows), toRows (fromRows intRows))
    let doubleColumns = [fromList [1,2], fromList [3]] :: [Vector Double]
    print (toLists (fromColumns doubleColumns), toColumns (fromColumns doubleColumns))
    print (rows (fromRows [fromList [], fromList [7]] :: Matrix Int),
           cols (fromRows [fromList [], fromList [7]] :: Matrix Int))
    print (toLists (build (2,3) (\i j -> 10*i+j) :: Matrix Int))
    print (toLists (build (2,2) (\i j -> i/2+j) :: Matrix Double))
    print (toLists (ident 3 :: Matrix Int))
    print (toLists (diag (fromList [2,3,4] :: Vector Int)))
    print (toLists (diagl [1.5,2.5]))
    print (toLists (diagRect 7 (fromList [10,20,30] :: Vector Int) 4 5))
    print (toList (takeDiag ((2 >< 3) [1..6] :: Matrix Int)))
    print (toList (linspace 0 (1,5) :: Vector Double))
    print (toList (linspace 1 (1,5) :: Vector Double))
    print (toList (linspace 5 (-3,7) :: Vector Double))
