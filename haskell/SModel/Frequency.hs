module SModel.Frequency where

import Bio.Alphabet
import Foreign.Vector
foreign import bpcall "SModel:plus_gwf_matrix" plus_gwf_matrix :: EVector Double -> Double -> Matrix Double
foreign import bpcall "SModel:weighted_frequency_matrix" builtin_weighted_frequency_matrix :: EVector Double -> EVector (EVector Double) -> Matrix Double
foreign import bpcall "SModel:frequency_matrix" builtin_frequency_matrix :: EVector (EVector Double) -> Matrix Double

-- pi is a vector double here
plus_f_matrix pi = plus_gwf_matrix pi 1.0

-- pi is [Double] here
uniform_frequencies a = replicate n $ 1.0/(intToDouble n) where n = alphabetSize a

uniform_frequencies_dict a = zip (letters a) (uniform_frequencies a)

-- pi is [(String,Double)] here
select_element key dict = case lookup key dict of Just value -> value
                                                  Nothing    -> error $ "Can't find element " ++ show key ++ " in dictionary!"

select_elements keys dict = map (flip select_element dict) keys

get_ordered_elements xs xps plural = if length xs == length xps
                                     then select_elements xs xps
                                     else error $ "Expected "++show (length xs)++" "++plural
                                              ++" but got "++ show (length xps)++"!"

frequencies_from_dict a pi = get_ordered_elements (letters a) pi "frequencies"
