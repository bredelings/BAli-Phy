module MCMC where

import Foreign.Pair
import Range

builtin register_transition_kernel 2 "register_transition_kernel" "MCMC"

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
builtin builtin_gibbs_sample_categorical 4 "gibbs_sample_categorical" "MCMC"
gibbs_sample_categorical x n c = IOAction (pair_from_c . builtin_gibbs_sample_categorical x n c)

builtin builtin_discrete_uniform_avoid_mh 5 "discrete_uniform_avoid_mh" "MCMC"
discrete_uniform_avoid_mh x low high c = IOAction (pair_from_c . builtin_discrete_uniform_avoid_mh x low high c)

-- It would be nice if we could (i) seq x and bounds HERE, and (ii) convert range to bounds HERE.
-- But the seq needs to be done during changeable execution, and we execute the IO unchangeably.
builtin builtin_inc_dec_mh 4 "inc_dec_mh" "MCMC"
inc_dec_mh x bnds c = IOAction (pair_from_c . builtin_inc_dec_mh x bnds c)

builtin builtin_slice_sample_real_random_variable 4 "slice_sample_real_random_variable" "MCMC"
slice_sample_real_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_real_random_variable x bnds c)

builtin builtin_slice_sample_integer_random_variable 4 "slice_sample_integer_random_variable" "MCMC"
slice_sample_integer_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_integer_random_variable x bnds c)

