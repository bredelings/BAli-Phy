module MCMC where

import Foreign.Pair

builtin register_transition_kernel 2 "register_transition_kernel" "MCMC"

builtin builtin_gibbs_sample_categorical 4 "gibbs_sample_categorical" "MCMC"
builtin builtin_discrete_uniform_avoid_mh 5 "discrete_uniform_avoid_mh" "MCMC"

builtin builtin_slice_sample_real_random_variable 3 "slice_sample_real_random_variable" "MCMC"
builtin builtin_slice_sample_integer_random_variable 3 "slice_sample_integer_random_variable" "MCMC"

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
gibbs_sample_categorical x n c = IOAction (pair_from_c . builtin_gibbs_sample_categorical x n c)

discrete_uniform_avoid_mh x low high c = IOAction (pair_from_c . builtin_discrete_uniform_avoid_mh x low high c)

slice_sample_real_random_variable x c = IOAction (pair_from_c . builtin_slice_sample_real_random_variable x c)

slice_sample_integer_random_variable x c = IOAction (pair_from_c . builtin_slice_sample_integer_random_variable x c)

