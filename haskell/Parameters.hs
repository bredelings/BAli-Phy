module Parameters where  

import Range
import Foreign.Pair       -- for pair_from_c

builtin maybe_modifiable_structure 1 "maybe_modifiable_structure" "Modifiables"

builtin builtin_random_variable 5 "random_variable" "Modifiables"
builtin builtin_register_random_variable 1 "register_random_variable" "Modifiables"
builtin modifiable 1 "modifiable" "Modifiables"

builtin register_likelihood 1 "register_likelihood" "Modifiables"
-- builtin builtin_register_likelihood 2 "register_likelihood" "Modifiables"
-- register_likelihood pr = IOAction (pair_from_c . builtin_register_likelihood pr)

random_variable x pdf range rate = let rv = builtin_random_variable x pdf range (c_range range) rate
                                   in rv `seq` builtin_register_random_variable rv `seq` rv
