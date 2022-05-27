module Parameters where

import Range
import Foreign.Pair       -- for pair_from_c
import Foreign.String     -- for list_to_string

builtin maybe_modifiable_structure 1 "Modifiables:maybe_modifiable_structure"

builtin modifiable 1 "Modifiables:modifiable"

builtin builtin_register_prior 2 "Modifiables:register_prior"
register_prior event prob = IOAction (\s -> (s+1, builtin_register_prior event prob))

builtin builtin_register_likelihood 2 "Modifiables:register_likelihood"
register_likelihood event prob = IOAction (\s -> (s+1, builtin_register_likelihood event prob))


builtin builtin_register_in_edge 3 "Modifiables:register_in_edge"
register_in_edge var dist role = IOAction (\s -> (s+1,builtin_register_in_edge var dist (list_to_string role)))

builtin builtin_register_out_edge 2 "Modifiables:register_out_edge"
register_out_edge dist var = IOAction (\s -> (s+1, builtin_register_out_edge dist var `seq` var))

builtin builtin_register_dist_property 3 "Modifiables:register_dist_property"
register_dist_property dist value property = IOAction (\s -> (s+1, builtin_register_dist_property dist value (list_to_string property)))

builtin builtin_register_dist 2 "Modifiables:register_dist"
-- The extra parameter to prevent invocations out of functions being floated
-- out of let statements by referencing a local variable.
register_dist_sample  name = IOAction (\s -> (s+1, builtin_register_dist (list_to_string name) 0))
register_dist_observe name = IOAction (\s -> (s+1, builtin_register_dist (list_to_string name) 1))
