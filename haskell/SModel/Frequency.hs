module SModel.Frequency (module SModel.Frequency, Markov.plus_gwf_matrix, Markov.plus_f_matrix) where
-- Rename to SModel.Frequencies?

import Bio.Alphabet
import qualified Data.Map as Map
import Foreign.Vector
import Numeric.LinearAlgebra
import qualified Markov

builtin_average_frequency matrix = konst 1 (rows matrix) <# matrix

-- pi is [Double] here
uniform_frequencies a = Markov.uniform_frequencies $ alphabetSize a

uniform_frequencies_dict a = Map.fromList $ zip (getLetters a) (uniform_frequencies a)

-- Look up a required model parameter while retaining the existing diagnostic for a missing key.
select_element key dict = case Map.lookup key dict of Just value -> value
                                                      Nothing    -> error $ "Can't find element " ++ show key ++ " in dictionary!"

select_elements keys dict = map (flip select_element dict) keys

-- The size check plus one lookup per expected key proves exact domain equality without constructing key sets.
get_ordered_elements xs xps plural = if length xs == Map.size xps
                                     then select_elements xs xps
                                     else error $ "Expected "++show (length xs)++" "++plural
                                              ++" but got "++ show (Map.size xps)++"!"

frequenciesFromDict a pi = get_ordered_elements (getLetters a) pi "frequencies"
