module SModel.Frequency (module SModel.Frequency, Markov.plus_gwf_matrix, Markov.plus_f_matrix) where
-- Rename to SModel.Frequencies?

import Bio.Alphabet
import Foreign.Vector
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import qualified Markov

foreign import bpcall "SModel:average_frequency" averageFrequencyNative :: NativeMatrix Double -> NativeVector Double

builtin_average_frequency matrix = vectorFromNative (cols matrix)
    (averageFrequencyNative (nativeMatrix matrix))

-- pi is [Double] here
uniform_frequencies a = Markov.uniform_frequencies $ alphabetSize a

uniform_frequencies_dict a = zip (getLetters a) (uniform_frequencies a)

-- pi is [(String,Double)] here
select_element key dict = case lookup key dict of Just value -> value
                                                  Nothing    -> error $ "Can't find element " ++ show key ++ " in dictionary!"

select_elements keys dict = map (flip select_element dict) keys

get_ordered_elements xs xps plural = if length xs == length xps
                                     then select_elements xs xps
                                     else error $ "Expected "++show (length xs)++" "++plural
                                              ++" but got "++ show (length xps)++"!"

frequenciesFromDict a pi = get_ordered_elements (getLetters a) pi "frequencies"
