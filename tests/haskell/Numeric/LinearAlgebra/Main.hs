{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import Numeric.LinearAlgebra
import System.IO (putStrLn)
import Text.Show (show)

-- Exercise construction, shape, transpose, and multiplication through the
-- renamed public matrix module.
main = do
    let matrix = fromLists [[1, 2], [3, 4]] :: Matrix Double
    putStrLn $ show $ nrows matrix
    putStrLn $ show $ ncols matrix
    putStrLn $ show $ toList $ transpose matrix
    putStrLn $ show $ toList $ matrix * identity 2
    putStrLn $ show $ toList $ signum (fromLists [[-2, 0, 3]] :: Matrix Double)
