#include "substitution-cache.H"

using std::vector;

void Likelihood_Cache::invalidate_one_branch(int b) {
  up_to_date[b] = false;
  for(int i=0;i<size();i++)
    (*this)[i][b].uniquify();
}

void Likelihood_Cache::invalidate_all() {
  for(int b=0;b<up_to_date.size();b++)
    invalidate_one_branch(b);
}

void Likelihood_Cache::invalidate_directed_branch(const Tree& T,int b) {
  vector<const_branchview> branch_list = branches_after(T,b);
  for(int i=0;i<branch_list.size();i++)
    invalidate_one_branch(branch_list[i]);
}

void Likelihood_Cache::invalidate_branch(const Tree& T,int b) {
  invalidate_directed_branch(T,b);
  invalidate_directed_branch(T,T.directed_branch(b).reverse());
}

void Likelihood_Cache::invalidate_node(const Tree& T,int n) {
  vector<const_branchview> branch_list = branches_from_node(T,n);
  for(int i=0;i<branch_list.size();i++)
    invalidate_one_branch(branch_list[i]);
}

void Likelihood_Cache::set_length(int l2) {
  int delta = l2 - size();
  if (delta <= 0) return;

  vector< RefMatrix > column = unshareable_column();

  reserve(l2);
  for(int i=0;i<delta;i++)
    push_back(column);
}


vector<RefMatrix> Likelihood_Cache::unshareable_column() {
  RefMatrix M(n_models,asize);
  M.mark_unshareable();
  vector< RefMatrix > column(2*n_branches+1,M);
  for(int i=0;i<column.size();i++)
    column[i].mark_unshareable();
  return column;
}

Likelihood_Cache::Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int l)
  :root(T.n_branches()-1),
   n_branches(T.n_branches()),
   n_models(M.nmodels()),
   asize(M.Alphabet().size()),
   up_to_date(2*T.n_branches(),false)
{
  set_length(l);
}
