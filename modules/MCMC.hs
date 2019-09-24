module MCMC where

import Foreign.Pair

builtin builtin_gibbs_sample_categorical 4 "gibbs_sample_categorical" "MCMC"
builtin builtin_register_transition_kernel 2 "register_transition_kernel" "MCMC"

-- Add the transition kernel r to the machine
register_transition_kernel r = IOAction (pair_from_c . builtin_register_transition_kernel r)

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
gibbs_sample_categorical x n c = IOAction (pair_from_c . builtin_gibbs_sample_categorical x n c)

