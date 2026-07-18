module Main where

import Compiler.FFI.Import
import Compiler.IO
import qualified Data.Vector.Unboxed as U

type PairIO = (Int, Double) -> IO (Int, Double)

foreign import ecall "Pair:c_pair"
    pairIORaw :: RawImport PairIO

type VectorIO = U.Vector Int -> IO (U.Vector Double)

foreign import bpcall "NativeVector:vectorKonstNative"
    vectorIORaw :: RawImport VectorIO

foreign import trcall "NativeVector:vectorKonstNative"
    vectorIOSugar :: VectorIO
