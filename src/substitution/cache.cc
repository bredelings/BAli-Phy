#include "cache.H"

object_ptr<Likelihood_Cache_Branch> SparseLikelihoods::DenseLikelihoods() const
{
    if (not DenseLikelihoods_)
    {
	int matrix_size = n_models() * n_states();

	auto LCB = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(n_columns(), n_models(), n_states()));

	LCB->bits = bits;

	int L = column_offsets.size() - 1;

	int i=0;
        for(int c=0;c<L;c++)
	{
	    double* S = (*LCB)[i];

            for(int k=0; k<matrix_size; k++)
                S[k] = 0.0;

	    for(int m=0;m<n_models();m++)
		for(int j=column_offsets[i];j<column_offsets[i+1];j++)
		    S[m*n_states() + states[j]] = values[j];

	    i++;
	}

	DenseLikelihoods_ = LCB;
    }

    return DenseLikelihoods_;
}

