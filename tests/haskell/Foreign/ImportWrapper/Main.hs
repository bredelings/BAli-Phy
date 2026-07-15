module Main where

import Compiler.FFI.Import
import Compiler.IO
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal
    (doubleVectorNativeView, intVectorNativeView)
import Foreign.NativeVector
import Foreign.Pair
import Foreign.String

data Ignored = Ignored

instance CInput Ignored where
    type CInputType Ignored result = result
    withCInput _ continuation = continuation

ignoredInput :: Ignored -> Int
ignoredInput = fromCImport (11 :: Int)

rawUnit :: () -> ()
rawUnit value = value

unitIdentity :: () -> ()
unitIdentity = fromCImport rawUnit

rawPairInput :: Int -> Double -> Int
rawPairInput integer floating = integer + floor floating

pairInput :: (Int, Double) -> Int
pairInput = fromCImport rawPairInput

separateInputs :: Int -> Double -> Int
separateInputs = fromCImport rawPairInput

rawNestedPairInput :: Int -> Double -> Char -> Char
rawNestedPairInput _ _ character = character

nestedPairInput :: ((Int, Double), Char) -> Char
nestedPairInput = fromCImport rawNestedPairInput

rawPairOutput :: Int -> EPair Int Double
rawPairOutput integer = c_pair integer (fromIntegral integer + 0.5)

pairOutput :: Int -> (Int, Double)
pairOutput = fromCImport rawPairOutput

rawString :: CPPString -> CPPString
rawString value = value

stringIdentity :: String -> String
stringIdentity = fromCImport rawString

textIdentity :: Text.Text -> Text.Text
textIdentity = fromCImport rawString

rawIO :: Int -> RealWorld -> Int
rawIO integer _ = integer + 1

ioOutput :: Int -> IO Int
ioOutput = fromCImport rawIO

rawVectorInput :: Int -> Int -> NativeVector Int -> Int
rawVectorInput offset count _ = offset * 100 + count

vectorInput :: U.Vector Int -> Int
vectorInput = fromCImport rawVectorInput

rawPairVectorInput :: Int -> Int -> NativeVector Int ->
                      Int -> Int -> NativeVector Double -> Int
rawPairVectorInput leftOffset leftCount _ rightOffset rightCount _ =
    leftOffset * 1000 + leftCount * 100 + rightOffset * 10 + rightCount

pairVectorInput :: U.Vector (Int, Double) -> Int
pairVectorInput = fromCImport rawPairVectorInput

intOwner :: NativeVector Int
intOwner = case intVectorNativeView (U.fromList [4, 5, 6]) of
    (_, _, owner) -> owner

vectorOutput :: U.Vector Int
vectorOutput = fromCImport intOwner

doubleOwner :: NativeVector Double
doubleOwner = case doubleVectorNativeView (U.fromList [1.5, 2.5]) of
    (_, _, owner) -> owner

doubleVectorOutput :: U.Vector Double
doubleVectorOutput = fromCImport doubleOwner

pairVectorOutput :: U.Vector (Int, Double)
pairVectorOutput = fromCImport (c_pair intOwner doubleOwner)

rawCombined :: Int -> Int -> NativeVector Int -> Double -> Int ->
               RealWorld -> NativeVector Double
rawCombined _ _ _ _ _ _ = doubleOwner

combined :: U.Vector Int -> (Double, Int) -> IO (U.Vector Double)
combined = fromCImport rawCombined

main = do
    print (ignoredInput Ignored)
    print (unitIdentity ())
    print (pairInput (3, 4.5))
    print (separateInputs 4 5.5)
    print (nestedPairInput ((1, 2.0), 'x'))
    print (pairOutput 5)
    print (stringIdentity "directional")
    print (Text.unpack (textIdentity (Text.pack "text")))
    result <- ioOutput 8
    print result
    print (vectorInput (U.slice 1 2 (U.fromList [10, 20, 30, 40])))
    print (pairVectorInput
        (U.zip (U.slice 1 2 (U.fromList [10, 20, 30, 40]))
               (U.slice 2 2 (U.fromList [1.0, 2.0, 3.0, 4.0, 5.0]))))
    print (U.toList vectorOutput)
    print (U.toList doubleVectorOutput)
    print (U.toList pairVectorOutput)
    vectorResult <- combined (U.fromList [1, 2]) (3.0, 4)
    print (U.toList vectorResult)
