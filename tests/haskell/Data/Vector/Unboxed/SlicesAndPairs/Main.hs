{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Enum
import Compiler.Fractional
import Compiler.Num
import qualified Data.Vector.Unboxed as U
import System.IO (print)

-- Exercise metadata-only primitive views and normalized structure-of-arrays
-- pair views, including truncating zip and nested pairs.
main = do
    let source = U.fromList [0..7] :: U.Vector Int
    print (U.toList (U.slice 0 8 source))
    print (U.toList (U.slice 2 4 source))
    print (U.toList (U.slice 1 2 (U.slice 2 5 source)))
    print (U.toList (U.unsafeSlice 3 2 source))
    print (U.toList (U.take (-2) source), U.toList (U.take 20 source))
    print (U.toList (U.drop (-2) source), U.toList (U.drop 20 source))
    let pairs = U.zip (U.fromList [1,2,3,4] :: U.Vector Int)
                      (U.fromList [1.5,2.5] :: U.Vector Double)
        (left, right) = U.unzip pairs
    print (U.length pairs, U.toList pairs)
    print (U.length left, U.toList left, U.length right, U.toList right)
    let nested = U.zip pairs (U.fromList [10,20,30] :: U.Vector Int)
    print (U.toList nested)
