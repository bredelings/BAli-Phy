#ifndef SUBSTITUTION_CACHE_H
#define SUBSTITUTION_CACHE_H

#include <vector>
#include "mytypes.H"
#include "tree.H"
#include "refcount.H"
#include "smodel.H"

class Multi_Likelihood_Cache: public std::vector< std::vector< Matrix > >,
			      public RefObject 
{

protected:
  int C; // # the (maximum) number of columns available per branch
  int M; // # models
  int A; // # letters

  // mapping[token][branch] -> location
  std::vector<std::vector<int> > mapping;

  // number of users of each location
  std::vector<int> n_uses;

  // a stack of unused_locations
  std::vector<int> unused_locations;

  // is each token in use or not?
  std::vector<bool> active;

  // length of each active cache (in columns)
  std::vector<int> length;

  int get_unused_location();
  void release_location(int loc);

  /// Allocate space for s new 'branches'
  void allocate(int s);

  std::vector<bool> up_to_date_;

public:

  int location(int t,int b) const {return mapping[t][b];}

  int n_unused_locations() const {return size()-unused_locations.size();}

  bool up_to_date(int token,int branch) const {
    return up_to_date_[ mapping[token][branch] ];
  }

  void validate_branch(int token,int branch);

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

class Likelihood_Cache {
  RefPtr<Multi_Likelihood_Cache> cache;

  int B;

  int token;

public:
  double cached_value;

  int root;

  int length() const {return cache->get_length(token);}
  int scratch() const {return B-1;}
  int n_branches() const {return B/2;}
  int n_models() const {return cache->n_models();}
  int n_letters() const {return cache->n_letters();}

  void invalidate_all();

  void invalidate_directed_branch(const Tree&,int b);

  void invalidate_branch(const Tree&,int b);

  void invalidate_branch_alignment(const Tree&,int b);

  void invalidate_node(const Tree&,int n);

  void set_length(int);
  int get_length() const {return cache->get_length(token);}

  bool up_to_date(int b) const {return cache->up_to_date(token,b);}
  void validate_branch(int b) {cache->validate_branch(token,b);}

  Matrix& operator()(int i,int b) {
    int loc = cache->location(token,b);
    assert(0 <= i and i < get_length());
    return (*cache)[loc][i];
  }

  Matrix& scratch(int i) {
    int loc = cache->location(token,B-1);
    assert(0 <= i and i < get_length());
    return (*cache)[loc][i];
  }

  Likelihood_Cache& operator=(const Likelihood_Cache&);

  Likelihood_Cache(const Likelihood_Cache& LC);
  Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int l=0);

  ~Likelihood_Cache();
};

void select_root(const Tree& T,int b,Likelihood_Cache& LC);

#endif
