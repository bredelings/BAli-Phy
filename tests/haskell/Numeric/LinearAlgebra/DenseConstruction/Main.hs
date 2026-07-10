{-# LANGUAGE NoImplicitPrelude #-}

import Numeric.LinearAlgebra
import Compiler.Fractional
import Compiler.Integral (fromIntegral)
import Compiler.Num
import Data.List (map)
import Data.Ord ((>))
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
    let sourceVector = fromList [0..7] :: Vector Int
    print (toList (subVector 2 3 sourceVector))
    print (takesV [2,3] sourceVector)
    print (toList (vjoin [subVector 0 2 sourceVector, subVector 5 3 sourceVector]))
    let sourceMatrix = (4 >< 5) [0..19] :: Matrix Int
    print (toLists (subMatrix (1,1) (2,3) sourceMatrix))
    print (toLists (takeRows 2 sourceMatrix), toLists (dropColumns 3 sourceMatrix))
    print (toLists (flipud sourceMatrix), toLists (fliprl sourceMatrix))
    print (toLists (sourceMatrix ?? (Pos (idxs [2,1]), Range 4 (-2) 0)))
    print (toLists (sourceMatrix ?? (PosCyc (idxs [-7,80]), Take 3)))
    let left = (2 >< 1) [1,2] :: Matrix Int
        right = (1 >< 2) [3,4] :: Matrix Int
    print (toLists (left ||| right), toLists (left === right))
    print (toLists (fromBlocks [[left, right], [right, left]]))
    print (toLists (diagBlock [left, right]))
    print (toLists (repmat left 2 3))
    print (map (map toLists) (toBlocks [1,2] [2,1] sourceMatrix))
    print (map (map toLists) (toBlocksEvery 3 2 sourceMatrix))
    let reductionVector = fromList [3,1,4,1,5] :: Vector Int
    print (toList (cmap (\x -> fromIntegral x / 2) reductionVector :: Vector Double))
    print (sumElements reductionVector, prodElements reductionVector,
           minElement reductionVector, maxElement reductionVector)
    print (minIndex reductionVector, maxIndex reductionVector, find (> 2) reductionVector)
    print (toList (sortVector reductionVector), toList (sortIndex reductionVector))
    let reductionMatrix = (2 >< 3) [3,1,4,1,5,2] :: Matrix Int
    print (toLists (cmap (\x -> fromIntegral x / 2) reductionMatrix :: Matrix Double))
    print (sumElements reductionMatrix, prodElements reductionMatrix,
           minElement reductionMatrix, maxElement reductionMatrix)
    print (minIndex reductionMatrix, maxIndex reductionMatrix, find (> 2) reductionMatrix)
    print (toList (conj reductionVector), toList (cmod 3 reductionVector))
    print (toLists (conj reductionMatrix), toLists (cmod 3 reductionMatrix))
