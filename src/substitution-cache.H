#ifndef SUBSTITUTION_CACHE_H
#define SUBSTITUTION_CACHE_H

#include <vector>
#include "mytypes.H"
#include "tree.H"

class Conditional_Likelihoods: public std::vector<Matrix> {
public:
  int root;

  int asize;

  int n_branches;

  vector<bool> up_to_date;

  void invalidate_all();

  void invalidate_branch(const Tree&,int b);

  void invalidate_node(const Tree&,int n);

  void set_length(int);

  Conditional_Likelihoods(const Tree& T, int r,int s);
};

#endif
