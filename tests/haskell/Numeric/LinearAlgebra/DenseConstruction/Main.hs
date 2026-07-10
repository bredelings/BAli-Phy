{-# LANGUAGE NoImplicitPrelude #-}

import Numeric.LinearAlgebra
import Compiler.Fractional
import Compiler.Floating
import Compiler.Integral (fromIntegral)
import Compiler.Num
import Data.List (map)
import Data.Bool (Bool(False, True))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Ord ((<), (>))
import System.IO (print)

smallResidual left right = maxElement (abs (left - right)) < 1.0e-10

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
    let floating = fromList [0.25,0.5] :: Vector Double
    print (toList (floating / 2), toList (recip floating))
    print (toList (exp floating), toList (sqrt floating), toList (log floating))
    print (toList (floating ** 2), toList (logBase floating 2))
    print (toList (sin floating), toList (tan floating), toList (cos floating))
    print (toList (asin floating), toList (atan floating), toList (acos floating))
    print (toList (sinh floating), toList (tanh floating), toList (cosh floating))
    print (toList (asinh floating), toList (atanh floating),
           toList (acosh (fromList [1.25,2] :: Vector Double)))
    print (toList (log1p floating), toList (expm1 floating),
           toList (log1pexp floating), toList (log1mexp (negate floating)))
    let floatingMatrix = (2 >< 2) [1,4,9,16] :: Matrix Double
    print (toLists (floatingMatrix / 2), toLists (2 / floatingMatrix))
    print (toLists (sqrt floatingMatrix), toLists (floatingMatrix ** 2))
    print (toLists (sin floatingMatrix), toLists (log1p floatingMatrix))
    let coefficients = fromLists [[4,1],[2,3]] :: Matrix Double
        rhs = fromLists [[1],[2]] :: Matrix Double
    print (det coefficients)
    print (smallResidual (coefficients <> inv coefficients) (ident 2))
    case linearSolve coefficients rhs of
        Nothing -> print False
        Just solution -> print (smallResidual (coefficients <> solution) rhs)
    let singular = fromLists [[1,2],[2,4]] :: Matrix Double
    case linearSolve singular rhs of
        Nothing -> print True
        Just _ -> print False
    let rectangular = fromLists [[1,0],[0,1],[1,1]] :: Matrix Double
        expected = fromLists [[2],[3]] :: Matrix Double
        rectangularRhs = rectangular <> expected
        leastSquares = linearSolveLS rectangular rectangularRhs
    print (smallResidual (rectangular <> leastSquares) rectangularRhs)
    let positiveDefinite = fromLists [[4,2],[2,3]] :: Matrix Double
        factor = chol positiveDefinite
    print (smallResidual (tr factor <> factor) positiveDefinite)
    let diagonal = fromLists [[1,0],[0,2]] :: Matrix Double
        expectedExponential = fromLists [[exp 1,0],[0,exp 2]] :: Matrix Double
    print (smallResidual (expm diagonal) expectedExponential)
    let symmetric = fromLists [[2,1],[1,2]] :: Matrix Double
        (eigenvalues, eigenvectors) = eigSH symmetric
    print (smallResidual (symmetric <> eigenvectors)
                         (eigenvectors <> diag eigenvalues))
    print (maxElement (abs (eigenvalues - eigenvaluesSH symmetric)) < 1.0e-10)
    let decompositionInput = fromLists [[1,2],[3,4],[5,6]] :: Matrix Double
        (fullU, fullS, fullV) = svd decompositionInput
        k = size fullS
        fullReconstruction = takeColumns k fullU <> diag fullS <> tr (takeColumns k fullV)
    print ((rows fullU, cols fullU), (rows fullV, cols fullV),
           smallResidual fullReconstruction decompositionInput)
    let (thinU, thinS, thinV) = thinSVD decompositionInput
    print ((rows thinU, cols thinU), (rows thinV, cols thinV),
           smallResidual (thinU <> diag thinS <> tr thinV) decompositionInput)
    print (maxElement (abs (thinS - singularValues decompositionInput)) < 1.0e-10)
    let (fullQ, fullR) = qr decompositionInput
        (thinQ, thinR) = thinQR decompositionInput
    print ((rows fullQ, cols fullQ), (rows fullR, cols fullR),
           smallResidual (fullQ <> fullR) decompositionInput)
    print ((rows thinQ, cols thinQ), (rows thinR, cols thinR),
           smallResidual (thinQ <> thinR) decompositionInput)
