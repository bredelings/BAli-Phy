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


pam = ExchangeModel aa (pam_raw aa)
jtt = ExchangeModel aa (jtt_raw aa)
wag = ExchangeModel aa (wag_raw aa)
lg = ExchangeModel aa (lg_raw aa)

empirical a filename = ExchangeModel a (builtin_empirical a (list_to_string filename))

wag_frequencies = zip (letters aa) (list_from_vector $ builtin_wag_frequencies aa)
lg_frequencies = zip (letters aa) (list_from_vector $ builtin_lg_frequencies aa)

