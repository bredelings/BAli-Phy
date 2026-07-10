#include "ops.H"

using std::pair;

void calc_transition_prob_from_parent(DenseMatrix<double>& S, const pair<int,int>& state_model_parent, const R::RVector& Ps)
{
    auto [mp,lp] = state_model_parent;

    int n_states = S.cols();

    // If there IS no parent character, then we can sample from F
    assert(mp != -1);

    auto& Pr = Ps[mp].as_<Box<DenseMatrix<double>>>();
    assert(mp >= 0);
    element_assign(S,0);
    for(int l=0;l<n_states;l++)
	S(mp,l) = Pr(lp,l);
}

void calc_transition_prob_from_parent(DenseMatrix<double>& S, const pair<int,int>& state_model_parent, const R::RVector& Ps, const DenseMatrix<double>& WF)
{
    auto [mp,lp] = state_model_parent;

    // If there IS no parent character, then we can sample from F
    if (mp == -1)
	S = WF;
    else
	calc_transition_prob_from_parent(S, state_model_parent, Ps);
}

DenseMatrix<double> propagate_frequencies(const DenseMatrix<double>& F, const R::RVector& transition_P)
{
    int n_models = F.rows();
    int n_states = F.cols();

    DenseMatrix<double> F2(n_models, n_states);

    for(int m = 0;m<n_models;m++)
    {
	const DenseMatrix<double>& P = transition_P[m].as_<Box<DenseMatrix<double>>>();
	for(int s2=0;s2<n_states;s2++)
	{
	    double p = 0;
	    for(int s1=0;s1<n_states;s1++)
		p += F(m,s1) * P(s1,s2);
	    F2(m,s2) = p;
	}
	// TODO - maybe normalize these to sum to one to reduce roundoff error?
    }

    return F2;
}
