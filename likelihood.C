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
  return Theta.lambda_O*init_gaps + Theta.lambda_E*extend_gaps;
}

double prior_branch(const alignment& A,const Parameters& Theta,int n1,int n2) {
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
  return Theta.lambda_O*init_gaps + Theta.lambda_E*extend_gaps;
}


double prior_internal(const alignment& A,const Parameters& Theta) {
  double P=0;
  for(int i=0;i<Theta.T.branches();i++) {
    int parent = Theta.T.parent(i);
    P += prior_branch(A,Theta,i,parent);
  }
  return P;
}
