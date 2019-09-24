module Parameters where  

import Range
import Foreign.Pair       -- for pair_from_c

builtin builtin_register_prior 2 "register_prior" "Modifiables"
builtin builtin_register_likelihood 2 "register_likelihood" "Modifiables"
builtin maybe_modifiable_structure 1 "maybe_modifiable_structure" "Modifiables"

builtin builtin_random_variable 5 "random_variable" "Modifiables"
builtin builtin_register_random_variable 1 "register_random_variable" "Modifiables"
builtin modifiable 1 "modifiable" "Modifiables"

register_prior pr = IOAction (pair_from_c . builtin_register_prior pr)
register_likelihood pr = IOAction (pair_from_c . builtin_register_likelihood pr)

c_range (OpenInterval a b) = getBounds (OpenInterval a b)
c_range (IntegerInterval a b) = getIntegerBounds (IntegerInterval a b)
c_range r = r

-- should this be in IO?  It does have the side-effect of registering the random variable
--register_random_variable x pdf range rate = IOAction1 builtin_register_random_variable (builtin_random_variable x pdf range (c_range range) rate)
random_variable x pdf range rate = builtin_register_random_variable $ builtin_random_variable x pdf range (c_range range) rate
