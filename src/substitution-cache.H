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

class RefMatrix {

  RefPtr<storage> data;

public:
  int size1() const {return data->size1();}
  int size2() const {return data->size2();}

  double   operator()(int i, int j) const { return (*data)(i,j);}
  double&  operator()(int i, int j)       { return (*data)(i,j);}

  bool is_shared() const {return data->is_shared();}
  bool is_shareable() const {return data->is_shareable();}
  void mark_unshareable() {data->mark_unshareable();}
  void uniquify() {data.uniquify();}

  operator const Matrix&() const {return *data;}
  operator       Matrix&()       {return *data;}

  RefMatrix(int s1,int s2) 
    :data(new storage(s1,s2))
  { }
};



class Likelihood_Cache: public std::vector< std::vector< RefMatrix > > {
protected:
  void invalidate_one_branch(int b,bool unique=false);

public:
  int root;

  int n_branches;

  int n_models;

  int asize;

  vector<bool> up_to_date;

  vector<RefMatrix> unshareable_column();

  void invalidate_all(bool unique=false);

  void invalidate_directed_branch(const Tree&,int b,bool unique=false);

  void invalidate_branch(const Tree&,int b,bool unique=false);

  void invalidate_node(const Tree&,int n,bool unique=false);

  void set_length(int);

  Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int l=0);
};

#endif
