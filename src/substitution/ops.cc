#include "ops.H"

using std::pair;

void calc_transition_prob_from_parent(Matrix& S, const pair<int,int>& state_model_parent, const EVector& Ps)
{
    auto [mp,lp] = state_model_parent;

    int n_states = S.size2();

    // If there IS no parent character, then we can sample from F
    assert(mp != -1);

    auto& Pr = Ps[mp].as_<Box<Matrix>>();
    assert(mp >= 0);
    element_assign(S,0);
    for(int l=0;l<n_states;l++)
	S(mp,l) = Pr(lp,l);
}

void calc_transition_prob_from_parent(Matrix& S, const pair<int,int>& state_model_parent, const EVector& Ps, const Matrix& WF)
{
    auto [mp,lp] = state_model_parent;

    // If there IS no parent character, then we can sample from F
    if (mp == -1)
	S = WF;
    else
	calc_transition_prob_from_parent(S, state_model_parent, Ps);
}

