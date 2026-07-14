module Main where

import Compiler.FFI.Import
import Compiler.IO
import qualified Data.Vector.Unboxed as U

foreign import ecall "Prelude:div_int"
    primitiveRaw :: RawImport (Int -> Double)

type PairInput = (Int, Double) -> Int

foreign import ecall "Prelude:div_int"
    pairInputRaw :: RawImport PairInput

type NestedPairInput = ((Int, Double), Char) -> Int

foreign import ecall "Prelude:div_int"
    nestedPairInputRaw :: RawImport NestedPairInput

type PairOutput = Int -> (Double, Int)

foreign import ecall "Pair:c_pair"
    pairOutputRaw :: RawImport PairOutput

type VectorImport = U.Vector Int -> U.Vector Double

foreign import bpcall "NativeVector:vectorKonstNative"
    vectorRaw :: RawImport VectorImport

foreign import bpcall "NativeVector:vectorKonstNative"
    ioRaw :: RawImport (Int -> IO Double)

type MixedImport =
    U.Vector Int -> (Double, Int) -> IO (U.Vector Double)

foreign import bpcall "NativeVector:vectorKonstNative"
    mixedRaw :: RawImport MixedImport

foreign import ecall "Prelude:div_int"
    ordinaryRaw :: Int -> Int -> Int

type HiddenRawImport = RawImport (Int -> Int)

foreign import ecall "Prelude:div_int"
    hiddenRaw :: HiddenRawImport
