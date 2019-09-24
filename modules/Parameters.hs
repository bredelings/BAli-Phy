module Parameters where  

import Range
import Foreign.Pair       -- for pair_from_c

builtin builtin_register_prior 2 "register_prior" "Modifiables"
builtin builtin_register_likelihood 1 "register_likelihood" "Modifiables"
builtin maybe_modifiable_structure 1 "maybe_modifiable_structure" "Modifiables"

builtin builtin_random_variable 5 "random_variable" "Modifiables"
builtin builtin_register_random_variable 1 "register_random_variable" "Modifiables"
builtin modifiable 1 "modifiable" "Modifiables"
builtin builtin_add_named_head 2 "add_named_head" "Modifiables"

register_likelihood pr = IOAction1 builtin_register_likelihood pr
register_prior pr = IOAction (pair_from_c . builtin_register_prior pr)

c_range (OpenInterval a b) = getBounds (OpenInterval a b)
c_range (IntegerInterval a b) = getIntegerBounds (IntegerInterval a b)
c_range r = r

-- should this be in IO?  It does have the side-effect of registering the random variable
random_variable x pdf range rate = builtin_register_random_variable $ builtin_random_variable x pdf range (c_range range) rate
register_random_variable x pdf range rate = IOAction1 builtin_register_random_variable (builtin_random_variable x pdf range (c_range range) rate)

add_named_head name r = IOAction2 builtin_add_named_head (listToString name) r
