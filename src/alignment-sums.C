#include "alignment-sums.H"
#include "likelihood.H"
#include "substitution.H"
#include "util.H"

efloat_t other_subst(const alignment& A, const Parameters& P, const vector<int>& nodes) {
  efloat_t p = substitution::other_subst(A,P,nodes);

  return pow(p,1.0/P.Temp);
}

efloat_t other_prior(const alignment& A, const Parameters& P,const vector<int>& nodes) {
  const Tree& T = P.T;

  efloat_t p = 1;

  // Add in the branch alignments
  for(int b=0;b<T.n_branches();b++) {
    int target = T.branch(b).target();
    int source = T.branch(b).source();

    if (includes(nodes,target) and includes(nodes,source))
      continue;

    p *= prior_branch(A,P.branch_HMMs[b],target,source);
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

    p /= P.IModel().lengthp(A.seqlength(n));
    p /= P.IModel().lengthp(A.seqlength(n));
  }

  return pow(p,1.0/P.Temp);
}



/// Distributions function for a star tree
vector< Matrix > distributions_star(const alignment& A,const Parameters& P,
				    const vector<int>& seq,int,const valarray<bool>& group)
{
  const alphabet& a = A.get_alphabet();
  const substitution::MultiModel& MModel = P.SModel();
  const SequenceTree& T = P.T;

  //FIXME modify this to add a shift of 2

  vector< Matrix > dist(seq.size(), Matrix(MModel.n_base_models(),a.size()) );

  for(int column=0;column<dist.size();column++) {
    vector<int> residues(A.n_sequences());

    for(int m=0;m<MModel.n_base_models();m++) {
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
vector< Matrix > distributions_tree(const alignment& A,const Parameters& P,const vector<int>& seq,int root,const valarray<bool>& group)
{
  const Tree& T = P.T;

  vector<int> branches;
  vector<const_nodeview> neighbors;
  append(T[root].neighbors(),neighbors);
  for(int i=0;i<neighbors.size();i++)
    if (group[neighbors[i]])
      branches.push_back(T.directed_branch(neighbors[i],root));

  vector<int> required;
  if (group[root])
    required.push_back(root);
  else {
    for(int i=0;i<branches.size();i++)
      required.push_back(T.directed_branch(branches[i]).source());
  }

  vector< Matrix > dist = substitution::get_column_likelihoods(A,P,branches,required,seq,2);
  // note: we could normalize frequencies to sum to 1
  assert(dist.size() == seq.size()+2);

  return dist;
}

///Checks integration and sampling from collapsed state
void check_match_P(const alignment& A,const Parameters& P, efloat_t OS, efloat_t OP, const vector<int>& path, const DPengine& Matrices) 
{

  /*------------------- Check offsets from path_Q -> P -----------------*/
  vector<int> path_g = Matrices.generalize(path);

  efloat_t qs = Matrices.path_Q_subst(path_g) * OS;
  efloat_t ls = P.likelihood(A,P);
  
  efloat_t qpGQ = Matrices.path_GQ_path(path_g) *  Matrices.generalize_P(path);
  efloat_t qpQ  = Matrices.path_Q_path(path);

  std::cerr<<"GQ(path) = "<<qpGQ<<"   Q(path) = "<<qpQ<<endl<<endl;
  assert(std::abs(log(qpGQ)-log(qpQ)) < 1.0e-9);
  
  efloat_t qp = Matrices.path_GQ_path(path_g) * Matrices.generalize_P(path) *OP;
  efloat_t lp = pow(prior_HMM(A,P),1.0/P.Temp);

  efloat_t qt = qs * qp * pow(prior(P),1.0/P.Temp);
  efloat_t lt = P.probability(A,P);

  std::cerr<<"ls = "<<ls<<"    qs = "<<qs<<endl;
  std::cerr<<"lp = "<<lp<<"    qp = "<<qp
	   <<" = "<<Matrices.path_GQ_path(path_g)<<" + "<<Matrices.generalize_P(path)<<" + "<<OP<<endl;
  std::cerr<<"lt = "<<lt<<"    qt = "<<qt<<endl;
  std::cerr<<endl;

  if ( (std::abs(log(qs) - log(ls)) > 1.0e-9) or 
       (std::abs(log(qp) - log(lp)) > 1.0e-9) or 
       (std::abs(log(qt) - log(lt)) > 1.0e-9)) {
    std::cerr<<A<<endl;
    std::cerr<<"Can't match up DP probabilities to real probabilities!\n"<<show_stack_trace();
    std::abort();
  }

}

/// Computes true, sampling, and proposal probabilities
vector<efloat_t> sample_P(const alignment& A,const Parameters& P,
			  efloat_t ratio, efloat_t rho,
			  const vector<int>& path, const DPengine& Matrices) 
{
  vector<efloat_t> PR(4);

  vector<int> path_g = Matrices.generalize(path);

  // Probability
  PR[0] = P.probability(A,P);

  // Probability of sampling A | i
  PR[1] = Matrices.path_P(path_g) * Matrices.generalize_P(path);

  // Proposal probability
  PR[2] = rho;

  // Ratio of P_0i/P_i0
  PR[3] = ratio;

  //  std::cerr<<"PrS = "<<P_choice<<" + "<<Matrices.path_P(path_g)<<" + "<<Matrices.generalize_P(path)<<endl;

  return PR;
}

/// Check that [ pi(A,i) * rho[i] / P(A|i) ] * P(i,0)/P(0,i) = [ pi(A`,0) * rho[0] / P(A`|0) ]

void check_sampling_probabilities(const vector< vector<efloat_t> >& PR,const vector<alignment>& a) 
{
  const vector<efloat_t>& P1 = PR.back();
  efloat_t ratio1 = P1[0]*P1[2]/P1[1];


  for(int i=0;i<PR.size();i++) 
  {
    const vector<efloat_t>& P2 = PR[i];
    
    std::cerr<<"\noption = "<<i<<"     rho"<<i<<" = "<<P2[2]<<endl;

    //    std::cerr<<" Pr1 * Rho1  = "<<P1[0]*P1[2]<<"    Pr2 * Rho2  = "<<P2[0]*P2[2]<<
    //      "    Pr2 * Rho2  - Pr1 * Rho1  = "<<P2[0]*P2[2]/(P1[0]*P1[2])<<endl;

    //    std::cerr<<" PrS1 = "<<P1[1]<<"    PrS2 = "<<P2[1]<<"    PrS2 - PrS1 = "<<P2[1] / PR.back()[1]<<endl;
    
    efloat_t ratio2 = (P2[0]*P2[2]/P2[1]) / P2[3];
    double diff = log(ratio2/ratio1);

    std::cerr<<"diff = "<<diff<<endl;
    if (std::abs(diff) > 1.0e-9) {
      std::cerr<<a.back()<<endl;
      std::cerr<<a[i]<<endl;
      
      throw myexception()<<": sampling probabilities were incorrect\n"<<show_stack_trace();
    }
  }
}
