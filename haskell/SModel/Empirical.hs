module SModel.Empirical where

import Bio.Alphabet
import Foreign.String
import SModel.ReversibleMarkov
import Numeric.LinearAlgebra.Data

foreign import bpcall "SModel:empirical" empiricalNative :: Alphabet -> CPPString -> NativeMatrix Double
foreign import bpcall "SModel:pam" pamNative :: Alphabet -> NativeMatrix Double
foreign import bpcall "SModel:jtt" jttNative :: Alphabet -> NativeMatrix Double
foreign import bpcall "SModel:wag" wagNative :: Alphabet -> NativeMatrix Double
foreign import bpcall "SModel:wag_frequencies" wagFrequenciesNative :: Alphabet -> NativeVector Double
foreign import bpcall "SModel:lg_frequencies" lgFrequenciesNative :: Alphabet -> NativeVector Double
foreign import bpcall "SModel:lg" lgNative :: Alphabet -> NativeMatrix Double
foreign import bpcall "SModel:symmetricMatrixFromLowerTriangle" symmetricNative :: Int -> NativeVector Double -> NativeMatrix Double

empiricalMatrix operation alphabet = matrixFromNative dimension dimension (operation alphabet)
  where dimension = alphabetSize alphabet

pam alphabet = empiricalMatrix pamNative alphabet
jtt alphabet = empiricalMatrix jttNative alphabet
wag alphabet = empiricalMatrix wagNative alphabet
lg alphabet = empiricalMatrix lgNative alphabet

builtin_wag_frequencies alphabet = vectorFromNative (alphabetSize alphabet)
    (wagFrequenciesNative alphabet)

builtin_lg_frequencies alphabet = vectorFromNative (alphabetSize alphabet)
    (lgFrequenciesNative alphabet)

symmetricMatrixFromLowerTriangle n xs = matrixFromNative n n
    (symmetricNative n (nativeVector (fromList xs)))

empirical a filename = empiricalMatrix (\alphabet -> empiricalNative alphabet (list_to_string filename)) a

wag_frequencies a = zip (getLetters a) (toList $ builtin_wag_frequencies a)
lg_frequencies a = zip (getLetters a) (toList $ builtin_lg_frequencies a)

normalizeFrequencies xs = map (/sum xs) xs
                   
