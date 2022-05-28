module Parameters where

import Range
import Foreign.Pair       -- for pair_from_c
import Foreign.String     -- for list_to_string

foreign import bpcall "Modifiables:maybe_modifiable_structure" maybe_modifiable_structure 1

foreign import bpcall "Modifiables:modifiable" modifiable 1

foreign import bpcall "Modifiables:register_prior" builtin_register_prior 2
register_prior event prob = IOAction (\s -> (s+1, builtin_register_prior event prob))

foreign import bpcall "Modifiables:register_likelihood" builtin_register_likelihood 2
register_likelihood event prob = IOAction (\s -> (s+1, builtin_register_likelihood event prob))


foreign import bpcall "Modifiables:register_in_edge" builtin_register_in_edge 3
register_in_edge var dist role = IOAction (\s -> (s+1,builtin_register_in_edge var dist (list_to_string role)))

foreign import bpcall "Modifiables:register_out_edge" builtin_register_out_edge 2
register_out_edge dist var = IOAction (\s -> (s+1, builtin_register_out_edge dist var `seq` var))

foreign import bpcall "Modifiables:register_dist_property" builtin_register_dist_property 3
register_dist_property dist value property = IOAction (\s -> (s+1, builtin_register_dist_property dist value (list_to_string property)))

foreign import bpcall "Modifiables:register_dist" builtin_register_dist 2
-- The extra parameter to prevent invocations out of functions being floated
-- out of let statements by referencing a local variable.
register_dist_sample  name = IOAction (\s -> (s+1, builtin_register_dist (list_to_string name) 0))
register_dist_observe name = IOAction (\s -> (s+1, builtin_register_dist (list_to_string name) 1))
