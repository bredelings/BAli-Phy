#include "alignment-sums.H"
#include "likelihood.H"
#include "substitution.H"

double other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes) {
  double p = 0.0;

  for(int column=0;column < A.length();column++) {
    bool present = false;
    for(int i=0;i<nodes.size();i++) {
      if (not A.gap(column,nodes[i]))
	present = true;
    }
    if (present) continue;

    p += substitution::Pr(A, P.T, P.SModel(), P, column);
  }

  return p;
}

double other_prior(const alignment& A, const Parameters& P, int n0) {
  const tree& T = P.T;

  double p = 0.0;

  // Add in the branch alignments
  for(int b=0;b<T.branches();b++) {
    int parent = T.branch(b).parent();
    int child = T.branch(b).child();
    if (n0 == parent or n0 == child)
      continue;
    p += prior_branch(A,P.IModel(),parent,child);
  }


  for(int n=0;n<T.num_nodes()-1;n++) {
    if (T[n].leaf())
      continue;

    if (n == n0)
      continue;

    p -= 2.0*P.IModel().lengthp(A.seqlength(n));
  }

  return p;
}



