{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Fractional
import Compiler.Enum
import Compiler.Num
import Data.Bool (Bool(False, True))
import Data.Eq
import Data.Maybe (Maybe(Nothing, Just))
import Data.Ord
import qualified Data.Vector.Unboxed as U
import System.IO (print)

maybeIndex values index = case values U.!? index of
    Nothing -> -1
    Just value -> value

boolInt :: Bool -> Int
boolInt False = 0
boolInt True = 1

-- Exercise primitive and nested-pair construction, indexing, conversion, and
-- the conventional immutable instances.
main = do
    print (U.toList (U.empty :: U.Vector Int))
    print (U.toList (U.singleton 4 :: U.Vector Int))
    print (U.toList (U.replicate 3 7 :: U.Vector Int))
    let ints = U.fromList [0,1,2,3,4] :: U.Vector Int
        doubles = U.fromList [1.5,2.5] :: U.Vector Double
        nested = U.fromList [((1,1.5),10),((2,2.5),20)] ::
            U.Vector ((Int,Double),Int)
    print (U.toList ints)
    print (U.toList doubles)
    print (U.length ints, boolInt (U.null ints),
           boolInt (U.null (U.empty :: U.Vector Int)))
    print (ints U.! 2, maybeIndex ints 3, maybeIndex ints (-1),
           maybeIndex ints 99)
    print (boolInt (ints == U.fromList [0,1,2,3,4]),
           boolInt (ints < U.fromList [0,1,3]))
    print ints
    print (U.toList nested)
    let moderate = U.fromList [0..999] :: U.Vector Int
    print (U.length moderate, moderate U.! 0, moderate U.! 999)
