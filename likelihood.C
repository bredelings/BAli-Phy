#include "likelihood.H"

double prior_no_tree(const alignment& A,const Parameters& Theta) {
  int total_gaps = A.length()*A.num_sequences();
  for(int i=0;i<A.num_sequences();i++) 
    total_gaps -= A.seq(i).size();

  int init_gaps=0;
  for(int column=0;column<A.length();column++) {
    for(int i=0;i<A.num_sequences();i++) {
      if (A(column,i) == alphabet::gap &&
	  (column == 0 || A(column-1,i) != alphabet::gap))
	init_gaps++;

    }    
  }
  int extend_gaps = total_gaps - init_gaps;
  return Theta.IModel.lambda_O*init_gaps + Theta.IModel.lambda_E*extend_gaps;
}

double prior_branch(const alignment& A,const IndelModel& IModel,int n1,int n2) {
  int gap = 0;
  int init_gaps = 0;
  int extend_gaps = 0;
  for(int column=0;column<A.length();column++) {
    if (A(column,n1) == alphabet::gap && A(column,n2) != alphabet::gap) {
      if (gap == 1) extend_gaps++;
      else init_gaps++;
      gap = 1;
    }
    else if (A(column,n1) != alphabet::gap && A(column,n2) == alphabet::gap) {
      if (gap == 2) extend_gaps++;
      else init_gaps++;
      gap = 2;
    }
    else if (A(column,n1) != alphabet::gap && A(column,n2) != alphabet::gap)
      gap=0;
  }
  return IModel.lambda_O*init_gaps + IModel.lambda_E*extend_gaps;
}


double prior_internal(const alignment& A,const Parameters& Theta) {
  double P=0;
  for(int i=0;i<Theta.T.branches();i++) {
    int parent = Theta.T.parent(i);
    P += prior_branch(A,Theta.IModel,i,parent);
  }
  return P;
}

/** FIXME - numerically check that choice of root node doesn't matter **/
double prior_branch_HMM(const alignment& A,const IndelModel& IModel,int n1,int n2) {
  vector<int> state;

  state.reserve(A.length());
  for(int column=0;column<A.length();column++) {
    if (A(column,n1) == alphabet::gap) {
      if (A(column,n2) == alphabet::gap)
	continue;
      else
	state.push_back(1);
    }
    else {
      if (A(column,n2) == alphabet::gap)
	state.push_back(2);
      else
	state.push_back(0);
    }
  }

  int length1 = A.seqlength(n1);
  double P = IModel.pi[state[0]];
  int l1 = (state[0]==0 or state[0]==2)?1:0;
  for(int i=1;i<state.size();i++) {
    if (l1<length1)
      P += IModel.P[state[i-1]][state[i]];
    else 
      P += IModel.R[state[i-1]][state[i]];

    if (state[i]==0 or state[i]==2) l1++;
  }
  assert(l1 == length1);
  
  return P;
}

double prior_HMM(const alignment& A,const Parameters& Theta) {
  int highest_node = Theta.T.num_nodes()-2;
  double P = Theta.IModel.lengthp(A.seqlength(highest_node));

  for(int i=0;i<Theta.T.branches();i++) {
    int parent = Theta.T.parent(i);
    P += prior_branch_HMM(A,Theta.IModel,parent,i);
  }
  return P;
}
