#ifndef SUBSTITUTION_CACHE_H
#define SUBSTITUTION_CACHE_H

#include <vector>
#include "mytypes.H"
#include "tree.H"
#include "refcount.H"
#include "smodel.H"

struct storage: public RefObject, public Matrix {
  storage(int i,int j):Matrix(i,j) {}
};

template<> inline void RefPtr<storage>::uniquify() {
  if (data) {
    storage* old_data = data;
    data = new storage(old_data->size1(),old_data->size2());
    data->inc();
    old_data->dec();
  }
}

class Multi_Likelihood_Cache: public std::vector< std::vector<Matrix> >,
			      public RefObject 
{
  mutable int cursor;

  int B; // # branches (including scratch)
  int M; // # models
  int A; // # letters

  vector<int> mapping;
  vector<int> i_mapping;

  vector<bool> active;

  int index(int token,int branch) const {return branch + token*B;}
  int token_from(int i) const {return i/B;}
  int branch_from(int i) const {return i%B;}

  int unused_location() const;

  void calc_imapping();
public:

  vector<bool> up_to_date_;

  int operator()(int token,int branch) const {return mapping[index(token,branch)];}

  bool up_to_date(int token,int branch) const {return up_to_date_[(*this)(token,branch)];}
  void mark_branch_up_to_date(int token,int branch)  {up_to_date_[(*this)(token,branch)]=true;}
  void mark_location_up_to_date(int loc) {up_to_date_[loc]=true;}

  int n_slots() const {return B;}
  int scratch() const {return B-1;}
  int n_branches() const {return B/2;}
  int n_models() const {return M;}
  int n_letters() const {return A;}

  void invalidate_one_branch(int token,int branch);
  void invalidate_all(int token);

  void set_length(int l);

  int find_free_token() const;
  int add_token();
  int claim_token();
  void copy_token(int token1,int token2);
  void release_token(int token);
  
  Multi_Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int l=0);
};


class Likelihood_Cache {
  RefPtr<Multi_Likelihood_Cache> cache;

  int token;

public:
  double old_value;

  int root;

  int scratch() const {return (*cache)(token,cache->scratch());}
  int n_branches() const {return cache->n_branches();}
  int n_models() const {return cache->n_models();}
  int n_letters() const {return cache->n_letters();}

  void invalidate_all();

  void invalidate_directed_branch(const Tree&,int b);

  void invalidate_branch(const Tree&,int b);

  void invalidate_node(const Tree&,int n);

  void set_length(int);

  int location(int b) const {return (*cache)(token,b);}

  bool up_to_date(int b) const {return cache->up_to_date(token,b);}
  void mark_branch_up_to_date(int b) {cache->mark_branch_up_to_date(token,b);}
  void mark_location_up_to_date(int loc) {cache->mark_location_up_to_date(loc);}

  vector<Matrix>& operator[](int col) {return (*cache)[col];}

  Likelihood_Cache& operator=(const Likelihood_Cache&);

  Likelihood_Cache(const Likelihood_Cache& LC);
  Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int l=0);

  ~Likelihood_Cache();
};

void select_root(const Tree& T,int b,Likelihood_Cache& LC);

#endif
