module Parameters where

import Range
import Foreign.Pair       -- for pair_from_c
import Foreign.String     -- for list_to_string

builtin maybe_modifiable_structure 1 "maybe_modifiable_structure" "Modifiables"

builtin modifiable 1 "modifiable" "Modifiables"

builtin register_prior 2 "register_prior" "Modifiables"

builtin register_likelihood 1 "register_likelihood" "Modifiables"



builtin builtin_register_in_edge 3 "register_in_edge" "Modifiables"
register_in_edge var dist role = builtin_register_in_edge var dist (list_to_string role)

builtin builtin_register_out_edge 3 "register_out_edge" "Modifiables"
register_out_edge dist var role = builtin_register_out_edge dist var (list_to_string role)

builtin builtin_register_dist_property 3 "register_dist_property" "Modifiables"
register_dist_property dist value property = builtin_register_dist_property dist value (list_to_string property)

builtin builtin_register_dist 1 "register_dist" "Modifiables"
builtin get_step_for_effect 1 "get_step_for_effect" "Modifiables"
register_dist name = get_step_for_effect $  builtin_register_dist (list_to_string name)
