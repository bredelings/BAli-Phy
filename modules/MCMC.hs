module MCMC where
{
builtin builtin_gibbs_sample_categorical 4 "gibbs_sample_categorical" "sum_out_coals";
gibbs_sample_categorical x n pr t = IOAction4 builtin_gibbs_sample_categorical x n pr t;
}
