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
  void mark_branch_up_to_date(int token,int branch)       {up_to_date_[(*this)(token,branch)]=true;}

  int n_slots() const {return B;}

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


class Column_Likelihood_Cache {
  std::vector<Matrix>& data;
  const std::vector<int>& mapping;
public:
  Matrix& operator[](int b) {
    return data[mapping[b]];
  }

  int scratch() const {return mapping.size()-1;}
  int n_branches() const {return mapping.size()/2;}
  int n_models() const {return data[0].size1();}
  int n_letters() const {return data[0].size2();}

  Column_Likelihood_Cache(std::vector<Matrix>& v,const std::vector<int>& m)
    :data(v),
     mapping(m)
  { }
};


class Likelihood_Cache {
  RefPtr<Multi_Likelihood_Cache> cache;

  int token;

  vector<int> mapping;

  void recalc();

public:
  int root;

  int n_branches() {return mapping.size()/2;}

  void invalidate_all();

  void invalidate_directed_branch(const Tree&,int b);

  void invalidate_branch(const Tree&,int b);

  void invalidate_node(const Tree&,int n);

  void set_length(int);

  bool up_to_date(int b) const {return cache->up_to_date(token,b);}
  void mark_branch_up_to_date(int b) {cache->mark_branch_up_to_date(token,b);}

  Column_Likelihood_Cache operator[](int col) {
    return Column_Likelihood_Cache((*cache)[col],mapping);
  }

  Likelihood_Cache& operator=(const Likelihood_Cache&);

  Likelihood_Cache(const Likelihood_Cache& LC);
  Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int l=0);

  ~Likelihood_Cache();
};

#endif
