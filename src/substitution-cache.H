#ifndef SUBSTITUTION_CACHE_H
#define SUBSTITUTION_CACHE_H

#include <vector>
#include "mytypes.H"
#include "tree.H"
#include "refcount.H"
#include "smodel.H"

class Multi_Likelihood_Cache: public std::vector< Matrix >,
			      public RefObject 
{
  int M; // # models
  int A; // # letters

  // mapping[token](column,branch) -> location
  vector<ublas::matrix<int> > mapping;

  // number of users of each location
  vector<int> uses;

  // a stack of unused_locations
  vector<int> unused_locations;

  // is each token in use or not?
  vector<bool> active;

  // length of each active cache (in columns)
  vector<int> length;

  int get_unused_location();
  void release_location(int loc);

  void allocate(int s);

public:

  ublas::matrix_row<ublas::matrix<int> > column_map(int t,int c) {
    return ublas::matrix_row<ublas::matrix<int> >(mapping[t],c);
  }

  int n_unused_locations() const {return size()-unused_locations.size();}

  vector<bool> up_to_date_;

  bool up_to_date(int token,int branch) const {
    return up_to_date_[mapping[token](0,branch)];
  }
  void validate_branch(int token,int branch);
  void validate_location(int loc) {up_to_date_[loc]=true;}

  int n_models() const {return M;}
  int n_letters() const {return A;}

  void invalidate_one_branch(int token,int branch);
  void invalidate_all(int token);

  void set_length(int token, int l);
  int  get_length(int token) const {return length[token];}

  int find_free_token() const;
  int add_token(int B);
  int claim_token(int C,int B);
  void copy_token(int token1,int token2);
  void init_token(int token);
  void release_token(int token);
  
  Multi_Likelihood_Cache(const substitution::MultiModel& M);
};

class Column_Interface {
  vector<Matrix>& cache;
  ublas::matrix_row<ublas::matrix<int> > mapping;
public:

  Matrix& operator[](int i) {return cache[mapping[i]];}

  Column_Interface(Multi_Likelihood_Cache& MLC,int token,int column)
    :cache(MLC),mapping(MLC.column_map(token,column))
  {}
};


class Likelihood_Cache {
  RefPtr<Multi_Likelihood_Cache> cache;

  int B;

  int token;

public:
  double old_value;

  int root;

  int length() const {return cache->get_length(token);}
  int scratch() const {return B-1;}
  int n_branches() const {return B/2;}
  int n_models() const {return cache->n_models();}
  int n_letters() const {return cache->n_letters();}

  void invalidate_all();

  void invalidate_directed_branch(const Tree&,int b);

  void invalidate_branch(const Tree&,int b);

  void invalidate_node(const Tree&,int n);

  void set_length(int);

  bool up_to_date(int b) const {return cache->up_to_date(token,b);}
  void validate_branch(int b) {cache->validate_branch(token,b);}

  Column_Interface operator[](int col) {return Column_Interface(*cache,token,col);}

  Likelihood_Cache& operator=(const Likelihood_Cache&);

  Likelihood_Cache(const Likelihood_Cache& LC);
  Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int l=0);

  ~Likelihood_Cache();
};

void select_root(const Tree& T,int b,Likelihood_Cache& LC);

#endif
