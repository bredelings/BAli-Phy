#include "parameters.H"
#include "exponential.H"


void Parameters::load_rates(const char* filename) {
  ifstream matrix_file(filename);

  for(int i=0;i<a->size();i++) 
    for(int j=0;j<a->size();j++) 
      matrix_file>>rates(i,j);
      
  matrix_file.close();
  recalc();
}

void Parameters::recalc() {
  substitution_.clear();
  for(int i=0;i<T.branches();i++) {
    assert(rates.size1() > 0);
    substitution_.push_back(exp(rates,T.branch(i).length));
  }
}



Parameters::Parameters(const alphabet& a_,const SequenceTree& t)
  :a(&a_),rates(a->size(),a->size()),T(t),frequency(1.0/a->size(),a->size())
{
  for(int i=0;i<rates.size1();i++) {
    for(int j=0;j<rates.size2();j++) {
      rates(i,j) = 0;
    }
  }
  recalc();
}
