module MCMC where

import Foreign.Pair

builtin builtin_gibbs_sample_categorical 4 "gibbs_sample_categorical" "MCMC"
builtin builtin_slice_sample_real_random_variable 3 "slice_sample_real_random_variable" "MCMC"
builtin builtin_slice_sample_integer_random_variable 3 "slice_sample_integer_random_variable" "MCMC"
builtin register_transition_kernel 2 "register_transition_kernel" "MCMC"

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
gibbs_sample_categorical x n c = IOAction (pair_from_c . builtin_gibbs_sample_categorical x n c)

slice_sample_real_random_variable x c = IOAction (pair_from_c . builtin_slice_sample_real_random_variable x c)

slice_sample_integer_random_variable x c = IOAction (pair_from_c . builtin_slice_sample_integer_random_variable x c)

