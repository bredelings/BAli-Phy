{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Bio.Alphabet (dna)
import Data.Function (($))
import Numeric.LinearAlgebra
import SModel.Parsimony (unitCostMatrix)
import System.IO (putStrLn)
import Text.Show (show)

-- Exercise construction, conversion, empty shapes, and the full basic
-- operation set for both native element representations.
main = do
    let matrix = fromLists [[1, 2], [3, 4]] :: Matrix Double
    putStrLn $ show $ nrows matrix
    putStrLn $ show $ ncols matrix
    putStrLn $ show $ toList $ flatten $ transpose matrix
    putStrLn $ show $ toList $ flatten $ matrix * identity 2
    putStrLn $ show $ toList $ flatten $ signum (fromLists [[-2, 0, 3]] :: Matrix Double)
    let costs = unitCostMatrix dna :: Matrix Int
    putStrLn $ show (nrows costs, ncols costs)
    putStrLn $ show $ getElem 0 1 costs
    putStrLn $ show $ toList $ flatten costs
    putStrLn $ show costs
    putStrLn $ show $ toList $ flatten $ transpose costs
    putStrLn $ show $ toList $ flatten $ scaleMatrix 2 costs
    putStrLn $ show $ toList $ flatten $ costs + costs
    putStrLn $ show $ toList $ flatten $ costs - costs
    putStrLn $ show $ toList $ flatten $ costs %*% costs
    putStrLn $ show $ toList $ flatten $ costs * costs
    putStrLn $ show $ toList $ flatten $ abs $ negate costs
    putStrLn $ show $ toList $ flatten $ signum $ negate costs
    let intMatrix = (2 >< 3) [1, 2, 3, 4, 5, 6] :: Matrix Int
    putStrLn $ show $ toLists intMatrix
    putStrLn $ show $ toList $ flatten (fromLists [[1, 2], [3, 4]] :: Matrix Int)
    putStrLn $ show $ toList $ flatten (zero 2 3 :: Matrix Int)
    putStrLn $ show $ toList $ flatten (zero 2 2 :: Matrix Double)
    putStrLn $ show $ toList $ flatten (identity 3 :: Matrix Int)
    putStrLn $ show $ toList $ flatten (identity 2 :: Matrix Double)
    let zeroRows = (0 >< 3) [] :: Matrix Int
    putStrLn $ show (nrows zeroRows, ncols zeroRows, toLists zeroRows)
    let zeroCols = fromLists [[], []] :: Matrix Double
    putStrLn $ show (nrows zeroCols, ncols zeroCols, toLists zeroCols)
    putStrLn $ show $ toList $ flatten (7 :: Matrix Int)
