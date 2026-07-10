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
    putStrLn $ show $ rows matrix
    putStrLn $ show $ cols matrix
    putStrLn $ show $ toList $ flatten $ tr matrix
    putStrLn $ show $ toList $ flatten $ matrix * ident 2
    putStrLn $ show $ toList $ flatten $ signum (fromLists [[-2, 0, 3]] :: Matrix Double)
    let costs = unitCostMatrix dna :: Matrix Int
    putStrLn $ show (rows costs, cols costs)
    putStrLn $ show $ atIndex costs (0,1)
    putStrLn $ show $ toList $ flatten costs
    putStrLn $ show costs
    putStrLn $ show $ toList $ flatten $ tr costs
    putStrLn $ show $ toList $ flatten $ scale 2 costs
    putStrLn $ show $ toList $ flatten $ costs + costs
    putStrLn $ show $ toList $ flatten $ costs - costs
    putStrLn $ show $ toList $ flatten $ costs %*% costs
    putStrLn $ show $ toList $ flatten $ costs * costs
    putStrLn $ show $ toList $ flatten $ abs $ negate costs
    putStrLn $ show $ toList $ flatten $ signum $ negate costs
    let intMatrix = (2 >< 3) [1, 2, 3, 4, 5, 6] :: Matrix Int
        intColumn = (3 >< 1) [2, 3, 4] :: Matrix Int
    putStrLn $ show $ toLists intMatrix
    putStrLn $ show $ toList $ flatten $ intMatrix * intColumn
    let doubleMatrix = (2 >< 3) [1, 2, 3, 4, 5, 6] :: Matrix Double
        doubleMatrix2 = (3 >< 2) [7, 8, 9, 10, 11, 12] :: Matrix Double
    putStrLn $ show $ toList $ flatten $ doubleMatrix * doubleMatrix2
    putStrLn $ show $ toList $ flatten (fromLists [[1, 2], [3, 4]] :: Matrix Int)
    putStrLn $ show $ toList $ flatten (konst 0 (2,3) :: Matrix Int)
    putStrLn $ show $ toList $ flatten (konst 0 (2,2) :: Matrix Double)
    putStrLn $ show $ toList $ flatten (ident 3 :: Matrix Int)
    putStrLn $ show $ toList $ flatten (ident 2 :: Matrix Double)
    let zeroRows = (0 >< 3) [] :: Matrix Int
    putStrLn $ show (rows zeroRows, cols zeroRows, toLists zeroRows)
    let zeroCols = fromLists [[], []] :: Matrix Double
    putStrLn $ show (rows zeroCols, cols zeroCols, toLists zeroCols)
    putStrLn $ show $ toList $ flatten (7 :: Matrix Int)
