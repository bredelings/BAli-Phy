{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Bio.Alphabet (dna)
import Data.Function (($))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid (mconcat, mempty)
import Data.Semigroup (sconcat)
import Numeric.LinearAlgebra
import SModel.Parsimony (unitCostMatrix)
import System.IO (putStrLn)
import Text.Show (show)

-- Exercise construction, conversion, empty shapes, and the full basic
-- operation set for both native element representations.
main = do
    let intVector = fromList [1, 2, 3] :: Vector Int
    putStrLn $ show $ toList $ intVector + 10
    putStrLn $ show $ toList $ 2 * intVector
    putStrLn $ show $ toList $ intVector - 1
    putStrLn $ show $ toList $ signum $ negate intVector
    putStrLn $ show $ toList $ (fromList [0.5] :: Vector Double) + vector [1, 2]
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
    putStrLn $ show $ toList $ flatten $ costs * costs
    putStrLn $ show $ toList $ flatten $ abs $ negate costs
    putStrLn $ show $ toList $ flatten $ signum $ negate costs
    let intMatrix = (2 >< 3) [1, 2, 3, 4, 5, 6] :: Matrix Int
        intRow = (1 >< 3) [10, 20, 30] :: Matrix Int
        intColumn = (2 >< 1) [1, 2] :: Matrix Int
    putStrLn $ show $ dot intVector (fromList [4, 5, 6])
    putStrLn $ show $ intVector <.> fromList [4, 5, 6]
    putStrLn $ show $ toList $ intMatrix #> fromList [2, 3, 4]
    putStrLn $ show $ toList $ (fromList [1, 2] :: Vector Int) <# intMatrix
    putStrLn $ show $ toList $ flatten $ outer (fromList [1, 2]) (fromList [3, 4, 5] :: Vector Int)
    putStrLn $ show $ toList $ flatten $ (2 :: Matrix Int) <> intMatrix
    putStrLn $ show $ toList $ flatten $ intMatrix <> (3 :: Matrix Int)
    putStrLn $ show $ toLists intMatrix
    putStrLn $ show $ toList $ flatten $ intMatrix + 10
    putStrLn $ show $ toList $ flatten $ intMatrix + intRow
    putStrLn $ show $ toList $ flatten $ intMatrix + intColumn
    putStrLn $ show $ toList $ flatten $ intRow + intColumn
    let doubleMatrix = (2 >< 3) [1, 2, 3, 4, 5, 6] :: Matrix Double
        doubleRow = (1 >< 3) [0.5, 1, 2] :: Matrix Double
        doubleRight = (3 >< 2) [7, 8, 9, 10, 11, 12] :: Matrix Double
    putStrLn $ show $ toList $ flatten $ doubleMatrix <> doubleRight
    putStrLn $ show $ toList $ flatten $ doubleMatrix * doubleRow
    putStrLn $ show $ toList $ flatten $ doubleMatrix - 1
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
    let chainA = (2 >< 3) [1, 2, 3, 4, 5, 6] :: Matrix Int
        chainB = (3 >< 2) [7, 8, 9, 10, 11, 12] :: Matrix Int
        chainC = (2 >< 1) [1, 2] :: Matrix Int
    putStrLn $ show $ toList $ flatten (optimiseMult [] :: Matrix Int)
    putStrLn $ show $ toList $ flatten (mconcat [] :: Matrix Int)
    putStrLn $ show $ toList $ flatten $ optimiseMult [chainA]
    putStrLn $ show $ toList $ flatten $ optimiseMult [chainA, chainB, chainC]
    putStrLn $ show $ toList $ flatten $ mconcat [chainA, chainB, chainC]
    putStrLn $ show $ toList $ flatten $ sconcat (chainA :| [chainB, chainC])
    putStrLn $ show $ toList $ flatten $ optimiseMult [(2 :: Matrix Int), chainA, (3 :: Matrix Int), chainB]
    putStrLn $ show $ toList $ flatten $ mempty <> chainA
    putStrLn $ show $ toList $ flatten $ mconcat [(1 >< 2) [1, 2] :: Matrix Double, (2 >< 1) [3, 4]]
