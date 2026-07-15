module SModel.Empirical where

import Bio.Alphabet
import SModel.ReversibleMarkov
import Numeric.LinearAlgebra.Data

foreign import trcall "SModel:empirical" empiricalNative :: Alphabet -> String -> Matrix Double
foreign import trcall "SModel:pam" pamNative :: Alphabet -> Matrix Double
foreign import trcall "SModel:jtt" jttNative :: Alphabet -> Matrix Double
foreign import trcall "SModel:wag" wagNative :: Alphabet -> Matrix Double
foreign import bpcall "SModel:wag_frequencies" wagFrequenciesNative :: Alphabet -> NativeVector Double
foreign import bpcall "SModel:lg_frequencies" lgFrequenciesNative :: Alphabet -> NativeVector Double
foreign import trcall "SModel:lg" lgNative :: Alphabet -> Matrix Double
foreign import trcall "SModel:symmetricMatrixFromLowerTriangle" symmetricNative :: Int -> Vector Double -> Matrix Double

empiricalMatrix operation alphabet = overrideMatrixDims dimension dimension (operation alphabet)
  where dimension = alphabetSize alphabet

pam alphabet = empiricalMatrix pamNative alphabet
jtt alphabet = empiricalMatrix jttNative alphabet
wag alphabet = empiricalMatrix wagNative alphabet
lg alphabet = empiricalMatrix lgNative alphabet

builtin_wag_frequencies alphabet = vectorFromNative (alphabetSize alphabet)
    (wagFrequenciesNative alphabet)

builtin_lg_frequencies alphabet = vectorFromNative (alphabetSize alphabet)
    (lgFrequenciesNative alphabet)

symmetricMatrixFromLowerTriangle n xs = overrideMatrixDims n n
    (symmetricNative n (fromList xs))

empirical a filename = empiricalMatrix (\alphabet -> empiricalNative alphabet filename) a

wag_frequencies a = zip (getLetters a) (toList $ builtin_wag_frequencies a)
lg_frequencies a = zip (getLetters a) (toList $ builtin_lg_frequencies a)

normalizeFrequencies xs = map (/sum xs) xs
                   
