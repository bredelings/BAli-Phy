#include "alignment-sums.H"
#include "likelihood.H"
#include "substitution.H"
#include "util.H"

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

double other_prior(const alignment& A, const Parameters& P,const vector<int>& nodes) {
  const Tree& T = P.T;

  double p = 0;

  // Add in the branch alignments
  for(int b=0;b<T.n_branches();b++) {
    int target = T.branch(b).target();
    int source = T.branch(b).source();

    if (includes(nodes,target) and includes(nodes,source))
      continue;

    p += prior_branch(A,P.IModel(),target,source);
  }


  // Add in the node length corrections
  for(int n=0;n<T.n_nodes();n++) {
    if (T[n].is_leaf_node())
      continue;

    if (includes(nodes,n)) {
      vector<const_nodeview> neighbors_NV;
      append(T[n].neighbors(),neighbors_NV);
      vector<int> neighbors(neighbors_NV.size());
      for(int i=0;i<neighbors.size();i++)
	neighbors[i] = neighbors_NV[i];
      if (includes(nodes,neighbors))
	continue;
    }

    p -= 2.0*P.IModel().lengthp(A.seqlength(n));
  }

  return p/P.Temp;
}



/// Distributions function for a star tree
vector< Matrix > distributions_star(const alignment& A,const Parameters& P,
				    const vector<int>& seq,int root,const valarray<bool>& group) {
  const alphabet& a = A.get_alphabet();
  const substitution::MultiModel& MModel = P.SModel();
  const SequenceTree& T = P.T;

  vector< Matrix > dist(seq.size(), Matrix(MModel.nmodels(),a.size()) );

  for(int column=0;column<dist.size();column++) {
    vector<int> residues(A.size2());

    for(int m=0;m<MModel.nmodels();m++) {
      for(int l=0;l<a.size();l++)
	dist[column](m,l) = 1.0;

      for(int n=0;n<T.n_leaves();n++) {
	if (not group[n]) continue;

	int letter = A(seq[column],n);
	if (not a.letter(letter)) continue;

	const Matrix& Q = P.transition_P(m,n);

	// Pr(root=l) includes Pr(l->letter)
	for(int l=0;l<a.size();l++)
	  dist[column](m,l) *= Q(l,letter);

      }
    }
  }

  return dist;
}



/// Distributions function for a full tree
vector< Matrix > distributions_tree(const alignment& A,const Parameters& P,
				    const vector<int>& seq,int root,const valarray<bool>& group) {
  const alphabet& a = A.get_alphabet();
  const substitution::MultiModel& MModel = P.SModel();

  vector< Matrix > dist(seq.size(), Matrix(MModel.nmodels(),a.size()) );

  for(int i=0;i<dist.size();i++) {
    vector<int> residues(A.size2());
    for(int j=0;j<residues.size();j++)
      residues[j] = A(seq[i],j);

    for(int m=0;m<MModel.nmodels();m++) {
      valarray<double> temp = substitution::peel(residues,
						 P.T,
						 MModel.get_model(m),
						 P.transition_P(m),
						 root,group);
      for(int l=0;l<a.size();l++)
	dist[i](m,l) = temp[l];
    }

    // note: we could normalize frequencies to sum to 1
  }

  return dist;
}

void check_match_P(const alignment& A,const Parameters& P, double OS, double OP, const vector<int>& path, const DPengine& Matrices) {

  /*------------------- Check offsets from path_Q -> P -----------------*/
  vector<int> path_g = Matrices.generalize(path);

  double qs = Matrices.path_Q_subst(path_g) + OS;
  double ls = P.likelihood(A,P);

  double qpGQ = Matrices.path_GQ_path(path_g) + Matrices.generalize_P(path);
  double qpQ  = Matrices.path_Q_path(path);
  std::cerr<<"GQ(path) = "<<qpGQ<<"   Q(path) = "<<qpQ<<endl<<endl;
  assert(std::abs(qpGQ-qpQ) < 1.0e-9);
  
  double qp = Matrices.path_GQ_path(path_g) + Matrices.generalize_P(path) + OP;
  double lp = prior_HMM(A,P)/P.Temp;

  double qt = qs + qp + prior(P)/P.Temp;
  double lt = P.probability(A,P);

  std::cerr<<"ls = "<<ls<<"    qs = "<<qs<<endl;
  std::cerr<<"lp = "<<lp<<"    qp = "<<qp<<" = "<<Matrices.path_GQ_path(path_g)<<" + "<<Matrices.generalize_P(path)<<" + "<<OP<<endl;
  std::cerr<<"lt = "<<lt<<"    qt = "<<qt<<endl;
  std::cerr<<endl;

  if ( (std::abs(qs - ls) > 1.0e-9) or (std::abs(qp - lp) > 1.0e-9) or (std::abs(qt - lt) > 1.0e-9)) {
    std::cerr<<A<<endl;
    throw myexception()<<__PRETTY_FUNCTION__<<": sampling probabilities were incorrect";
  }

}

vector<double> sample_P(const alignment& A,const Parameters& P,
			double OS, double OP, double P_choice,
			const vector<int>& path, const DPengine& Matrices) 
{
  vector<double> PR(3);

  vector<int> path_g = Matrices.generalize(path);

  // Probability
  PR[0] = P.probability(A,P);

  // Probability of sampling 
  PR[1] = P_choice + Matrices.path_P(path_g) + Matrices.generalize_P(path);

  std::cerr<<"PrS = "<<P_choice<<" + "<<Matrices.path_P(path_g)<<" + "<<Matrices.generalize_P(path)<<endl;

  PR[2] = Matrices.path_Q(path_g) + Matrices.generalize_P(path)+ prior(P)/P.Temp + OS + OP;

  return PR;
}

