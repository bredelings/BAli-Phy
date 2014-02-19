module MCMC where
{
builtin builtin_gibbs_sample_categorical 3 "gibbs_sample_categorical" "MCMC";
gibbs_sample_categorical x n c = IOAction3 builtin_gibbs_sample_categorical x n c;
}
