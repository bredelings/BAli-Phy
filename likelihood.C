#include "likelihood.H"
#include "logsum.H"

double log_double_factorial(int n) {
  double x = 0;
  for(int i=3;i<=n;i+=2)
    x += log(i);
  return x;
}

// Tree prior: branch lengths & topology
double prior(const SequenceTree& T,double branch_mean) {
  double p = 0;

  /* ----- 1/(number of topologies) -----*/
  if (T.leaves()>3)
    p = -log_double_factorial(2*T.leaves()-5);

  /* ---- PROD_i exp(-lambda * T[i]) ---- */
  for(int i=0;i<T.branches();i++) 
    p -= branch_mean*T.branch(i).length();
  return p;
}

// Tree prior + SModel prior
double prior(const Parameters& Theta) {
  double p = 0;

  p += prior(Theta.T,Theta.branch_mean);
  p += Theta.SModel().prior();

  return p;
}

/** FIXME - numerically check that choice of root node doesn't matter **/
double prior_branch(const alignment& A,const IndelModel& IModel,int parent,int child) {
  vector<int> state = get_path(A,parent,child);

  double P = log_0;
  for(int i=0;i<4;i++)
    P = logsum(P,IModel.pi[i] + IModel.Q(i,state[0]));

  for(int i=1;i<state.size();i++) 
    P += IModel.Q(state[i-1],state[i]);
  
  return P;
}

/** FIXME - numerically check that choice of root node doesn't matter **/
double prior_branch_Given(const alignment& A,const IndelModel& IModel,int parent,int child) {
  double P = prior_branch(A,IModel,parent,child);

  P -= IModel.lengthp(A.seqlength(parent));

  return P;
}

double prior_HMM_nogiven(const alignment& A,const Parameters& Theta) {
  const tree& T = Theta.T;

  double P = 0;

  for(int b=0;b<T.branches();b++) {
    int parent = T.branch(b).parent();
    int child  = T.branch(b).child();
    double p = prior_branch(A,Theta.IModel,parent,child);
    P += p;
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
    P += prior_branch_Given(A,Theta.IModel,parent,child);
  }
  
  return P;
}

double prior_branch_notree_nogiven(const alignment& A,const IndelModel& IModel,int child) {
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

double prior_branch_notree(const alignment& A,const IndelModel& IModel,int child) {
  double Pr = prior_branch_notree_nogiven(A,IModel,child);
  Pr -= IModel.length_plus_p(A.length());
  return Pr;
}

double prior_HMM_notree(const alignment& A,const Parameters& P) {
  const tree& T =P.T;

  double Pr = P.IModel.lengthp(A.length());
  for(int b=0;b<T.branches();b++) 
    Pr += prior_branch_notree(A,P.IModel,b);

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
  Pr += substitution_star_estimate(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

double Pr_sgaps_tletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM_notree(A,P);
  Pr += substitution(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

double Pr_sgaps_sletters(const alignment& A,const Parameters& P) {
  double Pr=0;
  Pr += prior_HMM_notree(A,P);
  Pr += substitution_star_estimate(A,P); // also deals w/ frequencies
  Pr += prior(P);
  return Pr;
}

