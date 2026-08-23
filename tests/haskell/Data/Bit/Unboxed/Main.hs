{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Compiler.Classes
import Data.Bit
import Data.Bits
import Data.Bool (Bool(False, True))
import Data.Eq
import Data.Function ((.))
import Data.List (map)
import qualified Data.Vector.Unboxed as U
import System.IO (print)
import Text.Show (show)

bits :: [Bool] -> U.Vector Bit
bits = U.fromList . map Bit

plain :: U.Vector Bit -> [Bool]
plain = map unBit . U.toList

-- Protect the exact-owner representation and upstream width-changing operations;
-- ordinary likelihood tests do not distinguish offset leaks or shift semantics.
main = do
    let source = bits [True, False, True, True, False]
        sliced = U.slice 1 3 source
        chained = U.slice 1 1 sliced
        short = bits [True, True]

    print (show (Bit False), show (Bit True))
    print (finiteBitSize (Bit True), countLeadingZeros (Bit False), countTrailingZeros (Bit True))
    print (U.length sliced, plain sliced, plain chained, popCount sliced)
    print (plain (source .&. short), plain (source .|. short), plain (source `xor` short),
           plain (short .|. source))
    print (testBit source (-1), testBit source 5)
    print (plain (setBit source 1), plain (clearBit source 2), plain (complementBit source 3))
    print (plain (bit (-1) :: U.Vector Bit), plain (bit 3 :: U.Vector Bit))
    print (plain (shift source 2), plain (shift source (-2)), plain (shift source (-8)))
    print (plain (rotate source 1), plain (rotate source (-1)), plain (rotate source 11),
           plain (rotate (U.empty :: U.Vector Bit) 3))
