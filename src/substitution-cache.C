#include "substitution-cache.H"
#include "util.H"

using std::vector;


int Multi_Likelihood_Cache::get_unused_location() {
  assert(unused_locations.size());

  int loc = unused_locations.back();
  unused_locations.pop_back();

  assert(uses[loc] == 0);
  uses[loc] = 1;

  up_to_date_[loc] = false;

  return loc;
}

void Multi_Likelihood_Cache::release_location(int loc) {
  uses[loc]--;
  if (not uses[loc])
    unused_locations.push_back(loc);
}

void Multi_Likelihood_Cache::allocate(int s) {
  int old_size = size();
  int new_size = old_size + s;

  reserve(new_size);
  uses.reserve(new_size);
  up_to_date_.reserve(new_size);
  unused_locations.reserve(new_size);

  for(int i=0;i<s;i++) {
    push_back(Matrix(M,A));
    uses.push_back(0);
    up_to_date_.push_back(false);
    unused_locations.push_back(old_size+i);
  }
}

void Multi_Likelihood_Cache::validate_branch(int token, int b) {
  ublas::matrix<int>& m = mapping[token];

  for(int c=0;c<length[token];c++)
    up_to_date_[m(c,b)] = true;
}

void Multi_Likelihood_Cache::invalidate_one_branch(int token, int b) {
  ublas::matrix<int>& m = mapping[token];

  for(int c=0;c<m.size1();c++) {
    int loc = m(c,b);
    if (uses[loc] > 1) {
      release_location(m(c,b));
      m(c,b) = get_unused_location();
    }
    else
      up_to_date_[loc] = false;
  }
}

void Multi_Likelihood_Cache::invalidate_all(int token) {
  for(int b=0;b<mapping[token].size2();b++)
    invalidate_one_branch(token,b);
}

// If the length is not the same, this may invalidate the mapping
void Multi_Likelihood_Cache::set_length(int t,int C) {
  int reserved=0;
  for(int i=0;i<active.size();i++) {
    if (not active[i] or i==t) continue;

    reserved += length[i]*mapping[i].size2();
  }
  assert(reserved <= size());

  int B = mapping[t].size2();

  if (C > mapping[t].size1())
    mapping[t].resize(C,mapping[t].size2());

  int delta = C*B - (size()-reserved);
  if (delta > 0) 
    allocate(delta);

  length[t] = C;
}

int Multi_Likelihood_Cache::find_free_token() const {
  int token=-1;
  for(int i=0;i<active.size();i++)
    if (not active[i]) {
      token = i;
      break;
    }

  return token;
}

int Multi_Likelihood_Cache::add_token(int B) {
  // add token properties
  int token = active.size();

  // add the token
  active.push_back(false);
  length.push_back(0);
  mapping.push_back(ublas::matrix<int>(0,B));

  return token;
}

int Multi_Likelihood_Cache::claim_token(int C,int B) {
  int token = find_free_token();

  if (token == -1)
    token = add_token(B);

  // set the length correctly
  set_length(token,C);
  
  active[token] = true;

  return token;
}

void Multi_Likelihood_Cache::init_token(int token) {
  ublas::matrix<int>& m = mapping[token];

  for(int c=0;c<mapping[token].size1();c++)
    for(int b=0;b<mapping[token].size2();b++)
      m(c,b) = get_unused_location();
}

// initialize token1 mappings from the mappings of token2
void Multi_Likelihood_Cache::copy_token(int token1, int token2) {
        ublas::matrix<int>& m1 = mapping[token1];
  const ublas::matrix<int>& m2 = mapping[token2];

  assert(m1.size2() == m2.size2());

  set_length(token1,length[token2]);

  m1 = m2;

  for(int c=0;c<length[token1];c++)
    for(int b=0;b<mapping[token1].size2();b++)
      uses[m1(c,b)]++;
}

void Multi_Likelihood_Cache::release_token(int token) {
  ublas::matrix<int>& m = mapping[token];

  for(int c=0;c<mapping[token].size1();c++)
    for(int b=0;b<mapping[token].size2();b++)
      release_location( m(c,b) );

  active[token] = false;
}


Multi_Likelihood_Cache::Multi_Likelihood_Cache(const substitution::MultiModel& MM)
  :M(MM.nmodels()),
   A(MM.Alphabet().size())
{ }

//------------------------------- Likelihood_Cache------------------------------//

void Likelihood_Cache::invalidate_all() {
  cache->invalidate_all(token);
}

void Likelihood_Cache::invalidate_directed_branch(const Tree& T,int b) {
  vector<const_branchview> branch_list = branches_after(T,b);
  for(int i=0;i<branch_list.size();i++)
    cache->invalidate_one_branch(token,branch_list[i]);
}

void Likelihood_Cache::invalidate_node(const Tree& T,int n) {
  vector<const_branchview> branch_list = branches_from_node(T,n);
  for(int i=0;i<branch_list.size();i++)
    cache->invalidate_one_branch(token,branch_list[i]);
}

void Likelihood_Cache::invalidate_branch(const Tree& T,int b) {
  invalidate_directed_branch(T,b);
  invalidate_directed_branch(T,T.directed_branch(b).reverse());
}

void Likelihood_Cache::set_length(int C) {
  invalidate_all();
  cache->set_length(token,C);
  cache->init_token(token);
}


Likelihood_Cache& Likelihood_Cache::operator=(const Likelihood_Cache& LC) {
  B = LC.B;
  old_value = LC.old_value;
  cache->release_token(token);
  cache = LC.cache;
  token = cache->claim_token(LC.length(),B);
  cache->copy_token(token,LC.token);
  root = LC.root;

  return *this;
}

Likelihood_Cache::Likelihood_Cache(const Likelihood_Cache& LC) 
  :cache(LC.cache),
   B(LC.B),
   token(cache->claim_token(LC.length(),B)),
   old_value(LC.old_value),
   root(LC.root)
{
  cache->copy_token(token,LC.token);
}

Likelihood_Cache::Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int C)
  :cache(new Multi_Likelihood_Cache(M)),
   B(T.n_branches()*2+1),
   token(cache->claim_token(C,B)),
   old_value(0),
   root(T.n_nodes()-1)
{
  cache->init_token(token);
}

Likelihood_Cache::~Likelihood_Cache() {
  cache->release_token(token);
}

void select_root(const Tree& T,int b,Likelihood_Cache& LC) {
  int r = T.directed_branch(b).reverse();
  if (T.subtree_contains(r,LC.root))
    b = r;

  LC.root = T.directed_branch(b).target();
}
