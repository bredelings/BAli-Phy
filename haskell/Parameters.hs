module Parameters where  

import Range
import Foreign.Pair       -- for pair_from_c

builtin maybe_modifiable_structure 1 "maybe_modifiable_structure" "Modifiables"

builtin modifiable 1 "modifiable" "Modifiables"

builtin register_prior 2 "register_prior" "Modifiables"

builtin register_likelihood 1 "register_likelihood" "Modifiables"
