module Parameters where

import Range
import Foreign.Pair       -- for pair_from_c
import Foreign.String     -- for list_to_string

builtin "Modifiables:maybe_modifiable_structure" maybe_modifiable_structure 1

builtin "Modifiables:modifiable" modifiable 1

builtin "Modifiables:register_prior" builtin_register_prior 2
register_prior event prob = IOAction (\s -> (s+1, builtin_register_prior event prob))

builtin "Modifiables:register_likelihood" builtin_register_likelihood 2
register_likelihood event prob = IOAction (\s -> (s+1, builtin_register_likelihood event prob))


builtin "Modifiables:register_in_edge" builtin_register_in_edge 3
register_in_edge var dist role = IOAction (\s -> (s+1,builtin_register_in_edge var dist (list_to_string role)))

builtin "Modifiables:register_out_edge" builtin_register_out_edge 2
register_out_edge dist var = IOAction (\s -> (s+1, builtin_register_out_edge dist var `seq` var))

builtin "Modifiables:register_dist_property" builtin_register_dist_property 3
register_dist_property dist value property = IOAction (\s -> (s+1, builtin_register_dist_property dist value (list_to_string property)))

builtin "Modifiables:register_dist" builtin_register_dist 2
-- The extra parameter to prevent invocations out of functions being floated
-- out of let statements by referencing a local variable.
register_dist_sample  name = IOAction (\s -> (s+1, builtin_register_dist (list_to_string name) 0))
register_dist_observe name = IOAction (\s -> (s+1, builtin_register_dist (list_to_string name) 1))
