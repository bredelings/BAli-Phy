#include "substitution-cache.H"
#include "util.H"

using std::vector;


int Multi_Likelihood_Cache::unused_location() const {
  // find an unused location
  int location = -1;
  for(int i=0;i<i_mapping.size();i++) {

    int l = (i + cursor)%i_mapping.size();

    if (not i_mapping[l]) {
      location = l;
      break;
    }
  }
  assert(location != -1);
  assert(0 <= location  and location < i_mapping.size());

  cursor = location+1;
  cursor %= i_mapping.size();

  return location;
}

void Multi_Likelihood_Cache::calc_imapping() {
  assert(mapping.size() == i_mapping.size());

  for(int i=0;i<i_mapping.size();i++) 
    i_mapping[i] = 0;

  for(int t=0;t<active.size();t++)
    if (active[t])
      for(int b=0;b<B;b++) {
	int I = index(t,b);
	i_mapping[mapping[I]]++;
      }
}

void Multi_Likelihood_Cache::invalidate_one_branch(int token, int branch) {

  int I = index(token,branch);
  int location = mapping[I];

  if (i_mapping[location] > 1) {

    // stop using old,shared location
    i_mapping[location]--;
    assert(i_mapping[location]>=0);

    // pick new,unused location
    location = unused_location();
    mapping[I] = location;
    i_mapping[location]++;
  }

  up_to_date_[location] = false;
}

void Multi_Likelihood_Cache::invalidate_all(int token) {
  for(int b=0;b<B;b++)
    invalidate_one_branch(token,b);
}

void Multi_Likelihood_Cache::set_length(int l2) {
  int delta = l2 - size();
  if (delta <= 0) return;

  vector< Matrix > column = vector<Matrix>(M,Matrix(B*active.size(),A));

  reserve(l2);
  for(int i=0;i<delta;i++)
    push_back(column);
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

int Multi_Likelihood_Cache::add_token() {
  // add token properties
  active.push_back(false);

  // allocate the extra cache space
  int length = size();
  clear();
  set_length(length);
  for(int i=0;i<up_to_date_.size();i++)
    up_to_date_[i] = false;

  // increase the size of the indexes
  for(int i=0;i<B;i++) {
    mapping.push_back(mapping.size());
    i_mapping.push_back(0);
    up_to_date_.push_back(false);
  }
  
  return active.size()-1;
}

int Multi_Likelihood_Cache::claim_token() {
  int token = find_free_token();
  if (token == -1)
    token = add_token();

  for(int b=0;b<B;b++) {
    int location = unused_location();
    int I = index(token,b);
    mapping[I] = location;
    i_mapping[location]++;
  }

  active[token] = true;

  return token;
}

// initialize token1 mappings from the mappings of token2
void Multi_Likelihood_Cache::copy_token(int token1, int token2) {
  for(int b=0;b<B;b++) {
    // release current location
    int I1 = index(token1,b);
    int location = mapping[I1];
    i_mapping[location]--;

    // acquire location of token2
    int I2 = index(token2,b);
    location = mapping[I2];
    i_mapping[location]++;
    mapping[I1] = location;
  }
}

void Multi_Likelihood_Cache::release_token(int token) {
  for(int b=0;b<B;b++) {
    // release current location
    int I1 = index(token,b);
    int location = mapping[I1];
    i_mapping[location]--;
  }
  active[token] = false;
}


Multi_Likelihood_Cache::Multi_Likelihood_Cache(const Tree& T, const substitution::MultiModel& MM,int l)
  :cursor(0),
   B(2*T.n_branches()+1),
   M(MM.nmodels()),
   A(MM.Alphabet().size()),
   up_to_date_()
{
  set_length(l);
}

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

void Likelihood_Cache::set_length(int l2) {
  cache->set_length(l2);
  invalidate_all();
}


Likelihood_Cache& Likelihood_Cache::operator=(const Likelihood_Cache& LC) {
  old_value = LC.old_value;
  cache->release_token(token);
  cache = LC.cache;
  token = cache->claim_token();
  cache->copy_token(token,LC.token);
  root = LC.root;

  return *this;
}

Likelihood_Cache::Likelihood_Cache(const Likelihood_Cache& LC) 
  :cache(LC.cache),
   token(cache->claim_token()),
   old_value(LC.old_value),
   root(LC.root)
{
  cache->copy_token(token,LC.token);
}

Likelihood_Cache::Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int l)
  :cache(new Multi_Likelihood_Cache(T,M,l)),
   token(cache->claim_token()),
   old_value(0),
   root(T.n_nodes()-1)
{
  set_length(l);
}

Likelihood_Cache::~Likelihood_Cache() {
  cache->release_token(token);
}

void select_root(int n1,int n2,Likelihood_Cache& LC) {
  if (n1 > n2)
    std::swap(n1,n2);
  if (LC.root == n1)
    std::swap(n1,n2);

  LC.root = n2;
}
