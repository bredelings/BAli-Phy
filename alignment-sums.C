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

  return p/P.Temp;
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

  return p/P.Temp;
}



/// Distributions function for a star tree
vector< vector<valarray<double> > > distributions_star(const alignment& A,const Parameters& P,
						       const vector<int>& seq,int root,const valarray<bool>& group) {
  const alphabet& a = A.get_alphabet();
  const substitution::MultiRateModel& MRModel = P.SModel();
  const SequenceTree& T = P.T;

  vector< vector< valarray<double> > > dist(seq.size(),vector< valarray<double> >(MRModel.nrates()) );

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());

    for(int r=0;r<MRModel.nrates();r++) {
      dist[i][r].resize(a.size(),1.0);

      for(int n=0;n<T.leaves();n++) {
	if (not group[n]) continue;

	int letter = A(seq[i],n);
	if (not a.letter(letter)) continue;

	const Matrix& Q = P.transition_P(r,n);

	// Pr(root=l) includes Pr(l->letter)
	for(int l=0;l<a.size();l++)
	  dist[i][r][l] *= Q(l,letter);

      }
    }
  }

  return dist;
}



/// Distributions function for a full tree
vector< vector<valarray<double> > > distributions_tree(const alignment& A,const Parameters& P,
					const vector<int>& seq,int root,const valarray<bool>& group) {
  const alphabet& a = A.get_alphabet();
  const substitution::MultiRateModel& MRModel = P.SModel();

  vector< vector< valarray<double> > > dist(seq.size(),vector< valarray<double> >(MRModel.nrates()) );

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);
    for(int r=0;r<MRModel.nrates();r++) {
      dist[i][r].resize(a.size());
      dist[i][r] = substitution::peel(residues,
				      P.T,
				      MRModel.BaseModel(),
				      P.transition_P(r),
				      root,group);
    }

    // note: we could normalize frequencies to sum to 1
  }

  return dist;
}

