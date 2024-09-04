module SModel.Empirical where

import Bio.Alphabet
import Foreign.Vector
import Foreign.String
import SModel.ReversibleMarkov

foreign import bpcall "SModel:empirical" builtin_empirical :: Alphabet -> CPPString -> Matrix Double
foreign import bpcall "SModel:pam" pam_raw :: Alphabet -> Matrix Double
foreign import bpcall "SModel:jtt" jtt_raw :: Alphabet -> Matrix Double
foreign import bpcall "SModel:wag" wag_raw :: Alphabet -> Matrix Double
foreign import bpcall "SModel:wag_frequencies" builtin_wag_frequencies :: Alphabet -> EVector Double
foreign import bpcall "SModel:lg_frequencies" builtin_lg_frequencies :: Alphabet -> EVector Double
foreign import bpcall "SModel:lg" lg_raw :: Alphabet -> Matrix Double


pam a = ExchangeModel a (pam_raw a)
jtt a = ExchangeModel a (jtt_raw a)
wag a = ExchangeModel a (wag_raw a)
lg a = ExchangeModel a (lg_raw a)

empirical a filename = ExchangeModel a (builtin_empirical a (list_to_string filename))

wag_frequencies a = zip (letters a) (list_from_vector $ builtin_wag_frequencies a)
lg_frequencies a = zip (letters a) (list_from_vector $ builtin_lg_frequencies a)

