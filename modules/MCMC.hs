module MCMC where
{
builtin builtin_gibbs_sample_categorical 3 "gibbs_sample_categorical" "MCMC";
builtin builtin_register_transition_kernel 1 "register_transition_kernel" "MCMC";

register_transition_kernel r = IOAction1 builtin_register_transition_kernel r;
gibbs_sample_categorical x n c = IOAction3 builtin_gibbs_sample_categorical x n c;
}
