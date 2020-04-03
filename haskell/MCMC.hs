module MCMC where

import Foreign.Pair

builtin register_transition_kernel 2 "register_transition_kernel" "MCMC"

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
builtin builtin_gibbs_sample_categorical 4 "gibbs_sample_categorical" "MCMC"
gibbs_sample_categorical x n c = IOAction (pair_from_c . builtin_gibbs_sample_categorical x n c)

builtin builtin_discrete_uniform_avoid_mh 5 "discrete_uniform_avoid_mh" "MCMC"
discrete_uniform_avoid_mh x low high c = IOAction (pair_from_c . builtin_discrete_uniform_avoid_mh x low high c)

builtin builtin_inc_dec_mh 3 "inc_dec_mh" "MCMC"
inc_dec_mh x c = IOAction (pair_from_c . builtin_inc_dec_mh x c)

builtin builtin_slice_sample_real_random_variable 3 "slice_sample_real_random_variable" "MCMC"
slice_sample_real_random_variable x c = IOAction (pair_from_c . builtin_slice_sample_real_random_variable x c)

builtin builtin_slice_sample_integer_random_variable 3 "slice_sample_integer_random_variable" "MCMC"
slice_sample_integer_random_variable x c = IOAction (pair_from_c . builtin_slice_sample_integer_random_variable x c)

