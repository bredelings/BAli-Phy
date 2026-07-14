module Main where

import Compiler.FFI.Import
import qualified Data.Vector.Unboxed as U
import Foreign.Pair
import Numeric.LogDouble
import System.Environment (getEnv)
import System.FilePath (takeFileName)

type CombinePair = (String, String) -> String

foreign import bpcall "File:combine"
    combinePairRaw :: RawImport CombinePair

combinePair :: CombinePair
combinePair = fromCImport combinePairRaw

type MakePair = Int -> Double -> (Int, Double)

foreign import ecall "Pair:c_pair"
    makePairRaw :: RawImport MakePair

makePair :: MakePair
makePair = fromCImport makePairRaw

type ReplicateInts = Int -> Int -> U.Vector Int

foreign import bpcall "NativeVector:vectorKonstNative"
    replicateIntsRaw :: RawImport ReplicateInts

replicateInts :: ReplicateInts
replicateInts = fromCImport replicateIntsRaw

type MultinomialDensity =
    Int -> U.Vector Double -> U.Vector Int -> LogDouble

foreign import bpcall "Distribution:multinomial_density"
    multinomialDensityRaw :: RawImport MultinomialDensity

multinomialDensity :: MultinomialDensity
multinomialDensity = fromCImport multinomialDensityRaw

foreign import trcall "Distribution:multinomial_density"
    multinomialDensitySugar :: MultinomialDensity

main = do
    print (takeFileName "/tmp/directional.txt")
    print (takeFileName (combinePair ("left", "right")))
    print (makePair 3 4.5)
    print (U.toList (replicateInts 7 3))

    let probabilities = U.slice 1 2 (U.fromList [99.0, 0.25, 0.75, 99.0])
        counts = U.slice 1 2 (U.fromList [99, 1, 2, 99])
        lower = toLogDouble (0.4218 :: Double)
        upper = toLogDouble (0.4220 :: Double)
        withinExpected value = lower < value && value < upper
    print (withinExpected (multinomialDensity 3 probabilities counts))
    print (withinExpected (multinomialDensitySugar 3 probabilities counts))
    path <- getEnv "PATH"
    print (not (null path))
