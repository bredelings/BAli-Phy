{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Data.Bits
import Compiler.Num
import Compiler.Integral (mod)
import Compiler.Base (error)
import Data.Eq
import Data.Bool
import Data.Maybe
import Data.List (map, and)
import System.IO (putStrLn)

-- Check native scalar semantics and word boundaries, which the bit-vector tests do not cover.
-- Keep these checks while Bits uses separate scalar implementations in C++.
main = do
 let width = finiteBitSize (0::Int)
     top = bit (width-1)::Int
     big = bit 100::Integer
     ints = and [ (6::Int) .&. 3 == 2, (6::Int) .|. 3 == 7, xor (6::Int) 3 == 5,
                  complement (0::Int) == -1, shift (-3::Int) (-1) == -2,
                  shiftL (-1::Int) width == 0, shiftR (-1::Int) width == -1,
                  shift (1::Int) top == 0, rotate (1::Int) (width-1) == top,
                  rotateR top (width-1) == 1, rotate (5::Int) width == 5,
                  rotateR (5::Int) top == rotateR 5 (top `mod` width),
                  popCount (-1::Int) == width, countLeadingZeros (0::Int) == width,
                  countTrailingZeros (0::Int) == width, countLeadingZeros (1::Int) == width-1,
                  countTrailingZeros top == width-1, setBit (0::Int) 3 == 8,
                  clearBit (-1::Int) 0 == -2, complementBit (3::Int) 1 == 1,
                  testBit top (width-1), not (testBit (1::Int) width),
                  bitSizeMaybe (0::Int) == Just width, isSigned (0::Int),
                  unsafeShiftL (1::Int) 2 == 4, unsafeShiftR (-3::Int) 1 == -2 ]
     integers = and [ (big .|. 3) .&. 7 == 3, xor (-1::Integer) big == -big-1,
                      complement big == -big-1, shiftR (-3::Integer) 1 == -2,
                      shiftL (-3::Integer) 2 == -12, shift big (-100) == 1,
                      rotate big (-100) == 1, rotateR big 100 == 1,
                      rotateR big (-1) == 2*big, popCount (big+3) == 3,
                      popCount (-big-3) == -3, testBit (-big) 100, testBit (-big) 1000,
                      not (testBit (-big) 99), bitSizeMaybe big == Nothing,
                      clearBit (big+3) 100 == 3, setBit (0::Integer) 100 == big,
                      complementBit (big+3) 100 == 3, isSigned big ]
 if ints && integers then putStrLn "completed" else error "scalar Bits mismatch"
