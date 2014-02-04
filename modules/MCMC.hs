module MCMC where
{
builtin builtin_gibbs_sample_categorical 3 "gibbs_sample_categorical" "sum_out_coals";
gibbs_sample_categorical x n c = IOAction3 builtin_gibbs_sample_categorical x n c;
}
