#include "likelihood.H"
#include "logsum.H"

double prior_no_tree(const alignment& A,const Parameters& Theta) {
  int total_gaps = A.length()*A.num_sequences();
  for(int i=0;i<A.num_sequences();i++) 
    total_gaps -= A.seq(i).size();

  int init_gaps=0;
  for(int column=0;column<A.length();column++) {
    for(int i=0;i<A.num_sequences();i++) {
      if (A(column,i) == alphabet::gap and
	  (column == 0 or A(column-1,i) != alphabet::gap))
	init_gaps++;

    }    
  }
  int extend_gaps = total_gaps - init_gaps;
  return Theta.IModel.lambda_O*init_gaps + Theta.IModel.lambda_E*extend_gaps;
}

// Symmetric - doesn't matter which is parent and child
double prior_branch(const alignment& A,const IndelModel& IModel,int n1,int n2) {
  int gap = 0;
  int init_gaps = 0;
  int extend_gaps = 0;
  for(int column=0;column<A.length();column++) {
    if (A(column,n1) == alphabet::gap and A(column,n2) != alphabet::gap) {
      if (gap == 1) extend_gaps++;
      else init_gaps++;
      gap = 1;
    }
    else if (A(column,n1) != alphabet::gap and A(column,n2) == alphabet::gap) {
      if (gap == 2) extend_gaps++;
      else init_gaps++;
      gap = 2;
    }
    else if (A(column,n1) != alphabet::gap and A(column,n2) != alphabet::gap)
      gap=0;
  }
  return IModel.lambda_O*init_gaps + IModel.lambda_E*extend_gaps;
}


double prior_internal(const alignment& A,const Parameters& Theta) {
  const tree& T = Theta.T;

  double P=0;
  for(int b=0;b<T.branches();b++) {
    int parent = T.branch(b).parent();
    int child  = T.branch(b).child();
    P += prior_branch(A,Theta.IModel,parent,child);
  }
  return P;
}

/** FIXME - numerically check that choice of root node doesn't matter **/
double prior_branch_HMM(const alignment& A,const IndelModel& IModel,int parent,int child) {
  vector<int> state = get_path(A,parent,child);

  double P = log_0;
  for(int i=0;i<4;i++)
    P = logsum(P,IModel.pi[i] + IModel.Q(i,state[0]));

  for(int i=1;i<state.size();i++) 
    P += IModel.Q(state[i-1],state[i]);
  
  return P;
}

/** FIXME - numerically check that choice of root node doesn't matter **/
double prior_branch_HMM_Given(const alignment& A,const IndelModel& IModel,int parent,int child) {
  double P = prior_branch_HMM(A,IModel,parent,child);

  P -= IModel.lengthp(A.seqlength(parent));

  return P;
}

double prior_HMM_nogiven(const alignment& A,const Parameters& Theta) {
  const tree& T = Theta.T;

  double P = 0;

  for(int b=0;b<T.branches();b++) {
    int parent = T.branch(b).parent();
    int child  = T.branch(b).child();
    double p = prior_branch_HMM(A,Theta.IModel,parent,child);
    P += p;
    std::cerr<<parent<<" -> "<<child<<":  "<<p<<endl;
  }
  
  return P;
}

double prior_HMM(const alignment& A,const Parameters& Theta) {
  const tree& T = Theta.T;

  int highest_node = T.get_nth(T.num_nodes()-2);
  highest_node = T.branch_up(highest_node).parent();
  double P = Theta.IModel.lengthp(A.seqlength(highest_node));

  for(int b=0;b<T.branches();b++) {
    int parent = T.branch(b).parent();
    int child  = T.branch(b).child();
    P += prior_branch_HMM_Given(A,Theta.IModel,parent,child);
  }
  
  return P;
}

double prior_branch_HMM_star_nogiven(const alignment& A,const IndelModel& IModel,int child) {
  const vector<double>& pi = IModel.pi;
  const Matrix& Q = IModel.Q;

  vector<int> state(A.length()+1);
  for(int column=0;column<A.length();column++) {
    state[column] = 0;
    if (A.gap(column,child))
      state[column] = 2;
  }
  state[A.length()] = 3;

  double Pr = log_0;
  for(int i=0;i<4;i++)
    Pr = logsum(Pr,pi[i] + Q(i,state[0]) );

  for(int i=1;i<state.size();i++) 
    Pr += Q(state[i-1],state[i]);
  
  return Pr;
}

double prior_branch_HMM_star(const alignment& A,const IndelModel& IModel,int child) {
  double Pr = prior_branch_HMM_star_nogiven(A,IModel,child);
  Pr -= IModel.length_plus_p(A.length());
  return Pr;
}

double prior_HMM_star(const alignment& A,const Parameters& P) {
  const tree& T =P.T;

  double Pr = P.IModel.lengthp(A.length());
  for(int b=0;b<T.branches();b++) 
    Pr += prior_branch_HMM_star(A,P.IModel,b);

  return Pr;
}

double Pr_tgaps_tletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM(A,P);
  Pr += substitution(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

double Pr_tgaps_sletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM(A,P);
  Pr += substitution_star(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

double Pr_sgaps_tletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM_star(A,P);
  Pr += substitution(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

double Pr_sgaps_sletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM_star(A,P);
  Pr += substitution_star(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

