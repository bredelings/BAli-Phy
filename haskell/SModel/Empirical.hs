module SModel.Empirical where

import Bio.Alphabet
import Foreign.Vector
import Foreign.String
import SModel.ReversibleMarkov

foreign import bpcall "SModel:empirical" builtin_empirical :: Alphabet -> CPPString -> Matrix Double
foreign import bpcall "SModel:" pam :: Alphabet -> Matrix Double
foreign import bpcall "SModel:" jtt :: Alphabet -> Matrix Double
foreign import bpcall "SModel:" wag :: Alphabet -> Matrix Double
foreign import bpcall "SModel:wag_frequencies" builtin_wag_frequencies :: Alphabet -> EVector Double
foreign import bpcall "SModel:lg_frequencies" builtin_lg_frequencies :: Alphabet -> EVector Double
foreign import bpcall "SModel:" lg :: Alphabet -> Matrix Double

empirical a filename = builtin_empirical a (list_to_string filename)

wag_frequencies a = zip (letters a) (list_from_vector $ builtin_wag_frequencies a)
lg_frequencies a = zip (letters a) (list_from_vector $ builtin_lg_frequencies a)

