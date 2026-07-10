{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Eq ((==))
import Numeric.LinearAlgebra
import System.IO (print)

-- Exercise native Int and Double vectors, including the matrix conversions
-- that replace the old boxed-vector path.
main = do
    let ints = fromList [1, 2, 3] :: Vector Int
    let doubles = vector [1.5, 2.5]
    print (size ints, toList ints, atIndex ints 1)
    print (size doubles, toList doubles, atIndex doubles 0)
    print (toList (fromList [] :: Vector Int))
    print (toList ((3 |> [4, 5, 6, 7]) :: Vector Int))
    print (ints == idxs [1, 2, 3], doubles == fromList [1.5, 3.5])
    let matrix = (2 >< 3) [1, 2, 3, 4, 5, 6] :: Matrix Int
    print (sumElements ints, sumElements doubles, sumElements matrix)
    print (toList (flatten matrix))
    print (toLists (reshape 2 (range 6)))
    print (toLists (asRow ints), toLists (asColumn ints))
