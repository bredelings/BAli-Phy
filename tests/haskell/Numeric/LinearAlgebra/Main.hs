{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Bio.Alphabet (dna)
import Data.Function (($))
import Numeric.LinearAlgebra
import SModel.Parsimony (unitCostMatrix)
import System.IO (putStrLn)
import Text.Show (show)

-- Exercise Double construction and the full basic operation set on an
-- existing native Int matrix.
main = do
    let matrix = fromLists [[1, 2], [3, 4]] :: Matrix Double
    putStrLn $ show $ nrows matrix
    putStrLn $ show $ ncols matrix
    putStrLn $ show $ toList $ transpose matrix
    putStrLn $ show $ toList $ matrix * identity 2
    putStrLn $ show $ toList $ signum (fromLists [[-2, 0, 3]] :: Matrix Double)
    let costs = unitCostMatrix dna :: Matrix Int
    putStrLn $ show (nrows costs, ncols costs)
    putStrLn $ show $ getElem 0 1 costs
    putStrLn $ show $ toList costs
    putStrLn $ show costs
    putStrLn $ show $ toList $ transpose costs
    putStrLn $ show $ toList $ scaleMatrix 2 costs
    putStrLn $ show $ toList $ costs + costs
    putStrLn $ show $ toList $ costs - costs
    putStrLn $ show $ toList $ costs %*% costs
    putStrLn $ show $ toList $ costs * costs
    putStrLn $ show $ toList $ abs $ negate costs
    putStrLn $ show $ toList $ signum $ negate costs
