#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "sequence.H"
#include "mytypes.H"
#include "tree.H"

class Parameters {
  const alphabet* a;

  Matrix rates;

  vector<Matrix> substitution_;
  void recalc();

public:
  const alphabet& get_alphabet() const {return *a;}
  void load_rates(const char*);

  const Matrix& substitution(int i) const {if (i==T.num_nodes()-2) i--;return substitution_[i];}
  
  SequenceTree T;

  double lambda_O;
  double lambda_E;

  // parameter for exponential prior on branch length
  double branch_mean; 

  std::valarray<double> frequency;

  Parameters(const alphabet& a_,const SequenceTree& t);
};


#endif
