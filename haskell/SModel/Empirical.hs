module SModel.Empirical where

import Bio.Alphabet
import Foreign.Vector
import Foreign.String
import SModel.ReversibleMarkov

foreign import bpcall "SModel:empirical" builtin_empirical :: Alphabet a -> CPPString -> Matrix Double
-- NOTE: Temporary bridge to C++ empirical matrices that still accept any runtime Alphabet.
-- Keep callers on AminoAcid wrappers and remove this once C++ exposes typed entry points.
foreign import bpcall "SModel:pam" pamRaw :: Alphabet a -> Matrix Double
foreign import bpcall "SModel:jtt" jttRaw :: Alphabet a -> Matrix Double
foreign import bpcall "SModel:wag" wagRaw :: Alphabet a -> Matrix Double
foreign import bpcall "SModel:wag_frequencies" builtin_wag_frequencies :: Alphabet a -> EVector Double
foreign import bpcall "SModel:lg_frequencies" builtin_lg_frequencies :: Alphabet a -> EVector Double
foreign import bpcall "SModel:lg" lgRaw :: Alphabet a -> Matrix Double
foreign import bpcall "SModel:symmetricMatrixFromLowerTriangle" builtinSymmetricMatrixFromLowerTriangle :: Int -> EVector Double -> Matrix Double

symmetricMatrixFromLowerTriangle n xs = builtinSymmetricMatrixFromLowerTriangle n (listToVector xs)

empirical a filename = builtin_empirical a (list_to_string filename)
pam :: AminoAcid a => Alphabet a -> Matrix Double
pam = pamRaw
jtt :: AminoAcid a => Alphabet a -> Matrix Double
jtt = jttRaw
wag :: AminoAcid a => Alphabet a -> Matrix Double
wag = wagRaw
lg :: AminoAcid a => Alphabet a -> Matrix Double
lg = lgRaw

wag_frequencies a = zip (getLetters a) (vectorToList $ builtin_wag_frequencies a)
lg_frequencies a = zip (getLetters a) (vectorToList $ builtin_lg_frequencies a)

normalizeFrequencies xs = map (/sum xs) xs
                   
