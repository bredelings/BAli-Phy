#include "substitution-cache.H"

using std::vector;

void Conditional_Likelihoods::invalidate_all() {
  for(int b=0;b<up_to_date.size();b++)
    up_to_date[b] = false;
}

void Conditional_Likelihoods::invalidate_branch(const Tree& T,int b) {
  vector<const_branchview> branch_list = branches_after(T,b);
  for(int i=0;i<branch_list.size();i++)
    up_to_date[branch_list[i]] = false;
}

void Conditional_Likelihoods::invalidate_node(const Tree& T,int n) {
  vector<const_branchview> branch_list = branches_from_node(T,n);
  for(int i=0;i<branch_list.size();i++)
    up_to_date[branch_list[i]] = false;
}

void Conditional_Likelihoods::set_length(int l2) {
  int delta = l2 - size();
  if (delta <= 0) return;

  reserve(l2);
  for(int i=0;i<delta;i++)
    push_back(Matrix(2*n_branches+1,asize));
}


Conditional_Likelihoods::Conditional_Likelihoods(const Tree& T, int r, int s) 
  :root(r),
   asize(s),
   n_branches(T.n_branches()),
   up_to_date(2*T.n_branches(),false)
{
  
}
