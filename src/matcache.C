#include "matcache.H"

using std::vector;

/// Set branch 'b' to have length 'l', and compute the transition matrices
void MatCache::setlength(int b,double l,Tree& T,const substitution::MultiModel& SModel) {
  assert(l >= 0);
  assert(b >= 0 and b < T.n_branches());
  T.branch(b).length() = l;
  for(int r=0;r<SModel.nmodels();r++)
    transition_P_[r][b] = SModel.transition_p(l,r);
}
  
void MatCache::recalc(const Tree& T,const substitution::MultiModel& SModel) {
  for(int b=0;b<T.n_branches();b++)
    for(int r=0;r<SModel.nmodels();r++)
      transition_P_[r][b] = SModel.transition_p(T.branch(b).length(),r);
}

MatCache::MatCache(const Tree& T,const substitution::MultiModel& SM) 
  :transition_P_(vector< vector <Matrix> >(SM.nmodels(),
					   vector<Matrix>(T.n_branches(),
							  Matrix(SM.Alphabet().size(),
								 SM.Alphabet().size()
								 )
							  ) 
					   ) 
		 )
  { 
    recalc(T,SM);
  }

