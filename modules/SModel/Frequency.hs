module SModel.Frequency where

import Alphabet
builtin builtin_plus_gwf 3 "plus_gwF" "SModel"

-- pi is a vector double here
plus_f_matrix a pi = plus_gwf_matrix a pi 1.0

-- pi is a vector double here
plus_gwf_matrix a pi f = builtin_plus_gwf a f pi

-- pi is [Double] here
uniform_frequencies a = replicate n $ 1.0/(intToDouble n) where n = alphabetSize a

uniform_frequencies_dict a = zip (alphabet_letters a) (uniform_frequencies a)

-- pi is [(String,Double)] here
select_element key dict = case lookup key dict of Just value -> value
                                                  Nothing    -> error $ "Can't find element " ++ show key ++ " in dictionary!"

select_elements keys dict = map (flip select_element dict) keys

get_ordered_elements xs xps plural = if length xs == length xps
                                     then select_elements xs xps
                                     else error $ "Expected "++show (length xs)++" "++plural
                                              ++" but got "++ show (length xps)++"!"

frequencies_from_dict a pi = get_ordered_elements (alphabet_letters a) pi "frequencies"
