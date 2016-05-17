/*
   Copyright (C) 2004-2005,2007,2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#include "substitution-cache.H"
#include "util.H"

using std::vector;

#define CONSERVE_MEM 1

int Multi_Likelihood_Cache::get_unused_location() 
{
#ifdef CONSERVE_MEM
  if (not unused_locations.size()) {
    double s = size();
    int ns = int(s*1.1)+4;
    int delta = ns - size();
    assert(delta > 0);
    allocate_branch_slots(delta);
  }
#endif

  assert(unused_locations.size());

  int loc = unused_locations.back();
  unused_locations.pop_back();

  assert(n_uses[loc] == 0);
  n_uses[loc] = 1;

  up_to_date_[loc] = false;

  return loc;
}

void Multi_Likelihood_Cache::release_location(int loc) 
{
  assert(loc != -1);

  n_uses[loc]--;
  if (not n_uses[loc])
    unused_locations.push_back(loc);
}

/// Allocate space for s new 'branches'
void Multi_Likelihood_Cache::allocate_branch_slots(int s) 
{
  int old_size = size();
  int new_size = old_size + s;
  if (log_verbose) {
    std::cerr<<"Allocating "<<old_size<<" -> "<<new_size<<" branches ("<<s<<")\n";
    std::cerr<<"  Each branch has "<<C<<" columns.\n";
  }

  reserve(new_size);
  n_uses.reserve(new_size);
  up_to_date_.reserve(new_size);
  unused_locations.reserve(new_size);

  for(int i=0;i<s;i++) {
    push_back(Likelihood_Cache_Branch(C,M,S));
    n_uses.push_back(0);
    up_to_date_.push_back(false);
    unused_locations.push_back(old_size+i);
  }
}

void Multi_Likelihood_Cache::allocate_location(int t, int b)
{
  if (not location_allocated(t,b))
    mapping[t][b] = get_unused_location();
}


void Multi_Likelihood_Cache::validate_branch(int t, int b) {
  assert(location_allocated(t,b));
  up_to_date_[location(t,b)] = true;
}

void Multi_Likelihood_Cache::invalidate_one_branch(int token, int b) 
{
  int loc = mapping[token][b];
  if (location_allocated(token,b))
    release_location(loc);
  mapping[token][b] = -1;

  cv_up_to_date_[token] = false;
}

void Multi_Likelihood_Cache::invalidate_all(int token) {
  for(int b=0;b<mapping[token].size();b++)
    invalidate_one_branch(token,b);
}

// If the length is not the same, this may invalidate the mapping
void Multi_Likelihood_Cache::request_length(int l)
{
  // FIXME - calling of this function is essentially a timer function for garbage collection.
  if (l < C)
  {
    iterations_too_long++;

    if (l < C/2)
      set_length(C/2);
    else if (l < C*3/4 and iterations_too_long > 100*n_uses.size() )
      set_length(C*3/4);
    else if (l < C*5/6 and iterations_too_long > 200*n_uses.size() )
      set_length(C*5/6);
  }
  else
    iterations_too_long = 0;
  
  // Increase overall length if necessary
  if (l > C) 
  {
    int l2 = 4+(int)(1.1*l);
    set_length(l2);
  }
}

// If the length is not the same, this may invalidate the mapping
void Multi_Likelihood_Cache::set_length(int l)
{
  assert(l >= 0);

  iterations_too_long = 0;

  int C_old = C;

  // Shrink
  if (l < C)
  {
    for(int i=0;i<size();i++)
      (*this)[i].resize(l);
  }
  // Grow
  else if (l > C)
  {
    int delta = l-C;
    
    for(int i=0;i<size();i++)
      for(int j=0;j<delta;j++)
	(*this)[i].push_back(Matrix(M,S));
  }
  C = l;

  // Report if the length changes
  if (log_verbose and C != C_old)
    std::clog<<"  MLC now has "<<C<<" columns and "<<size()<<" branches.\n";

  // Check that we are long enough, and know how long we are.
  for(int i=0;i<size();i++) {
    assert((*this)[i].size() == C);
    assert((*this)[i].size() >= l);
  }
}

// If the length is not the same, this may invalidate the mapping
void Multi_Likelihood_Cache::set_length(int t,int l) 
{
  length[t] = l;

  int new_length = max(length);

  request_length(new_length);
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
  int token = active.size();

  // add the token
  active.push_back(false);
  length.push_back(0);
  mapping.push_back(std::vector<int>(B));
  cv_up_to_date_.push_back(false);

#ifndef CONSERVE_MEM
  // add space used by the token
  allocate_branch_slots(B);
#endif

  if (log_verbose)
    std::clog<<"There are now "<<active.size()<<" tokens.\n";

  return token;
}

int countt(const vector<bool>& v) {
  int c=0;
  for(int i=0;i<v.size();i++)
    if (v[i]) c++;
  return c;
}

int Multi_Likelihood_Cache::claim_token(int l,int B) {
  //  std::clog<<"claim_token: "<<countt(active)<<"/"<<active.size()<<" -> ";
  int token = find_free_token();

  if (token == -1)
    token = add_token(B);

  // set the length correctly
  set_length(token,l);
  
  active[token] = true;

  //  std::cerr<<"-> "<<countt(active)<<"/"<<active.size()<<std::endl;
  return token;
}

void Multi_Likelihood_Cache::init_token(int token) 
{
  /// Out branches initially don't point to any backing store
  for(int b=0;b<mapping[token].size();b++)
    mapping[token][b] = -1;

  cv_up_to_date_[token] = false;
}

// initialize token1 mappings from the mappings of token2
void Multi_Likelihood_Cache::copy_token(int token1, int token2) 
{
  assert(mapping[token1].size() == mapping[token2].size());

  // is the complete likelihood up to date?
  cv_up_to_date_[token1] = cv_up_to_date_[token2];

  // copy the length from token2, and reserve space
  length[token1] = 0;
  set_length(token1,length[token2]);

  // token one now uses the same slots/locations as token2
  mapping[token1] = mapping[token2];

  // mark each slot/location used by token 1 as having another user
  for(int b=0;b<mapping[token1].size();b++)
    if (mapping[token1][b] != -1)
      n_uses[mapping[token1][b]]++;
}

void Multi_Likelihood_Cache::release_token(int token) {
  //  std::cerr<<"release_token: "<<countt(active)<<"/"<<active.size()<<" -> ";
  for(int b=0;b<mapping[token].size();b++)
    if (location_allocated(token,b))
      release_location( location(token,b) );

  active[token] = false;
  //  std::cerr<<"-> "<<countt(active)<<"/"<<active.size()<<std::endl;
}


Multi_Likelihood_Cache::Multi_Likelihood_Cache(const Mat_Cache& MC)
  :C(0),
   M(MC.n_base_models()),
   S(MC.n_states()),
   iterations_too_long(0)
{ }

//------------------------------- Likelihood_Cache------------------------------//

void Likelihood_Cache::invalidate_all() {
  cache->invalidate_all(token);
}

void Likelihood_Cache::invalidate_directed_branch(const TreeInterface& t,int b) {
  
  cv_up_to_date() = false;

  if (up_to_date(b))
  {
    vector<int> branches;
    branches.reserve(t.n_branches());
    branches.push_back(b);
    for(int i=0;i<branches.size();i++)
    {
      int b2 = branches[i];
      if (up_to_date(b2) or branch_available(b2))
      {
	t.append_branches_after(b2, branches);
	invalidate_one_branch(b2);
      }
    }
  }

#ifndef NDEBUG
  for(int b2: t.all_branches_after_inclusive(b))
      assert(not up_to_date(b2));
  for(int b2: t.all_branches_after_inclusive(b))
      assert(not branch_available(b2));
#endif
}

void Likelihood_Cache::invalidate_node(const TreeInterface& t,int n) {
  for(int b: t.branches_out(n))
    invalidate_directed_branch(t,b);
}

void Likelihood_Cache::invalidate_one_branch(int b) {
  cache->invalidate_one_branch(token,b);
}

void Likelihood_Cache::invalidate_branch(const TreeInterface& t,int b) {
  invalidate_directed_branch(t, b);
  invalidate_directed_branch(t, t.reverse(b));
}

void Likelihood_Cache::invalidate_branch_alignment(const TreeInterface& t,int b)
{
  for(int b2: t.branches_after(b))
    invalidate_directed_branch(t,b2);

  for(int b2: t.branches_after(t.reverse(b)))
    invalidate_directed_branch(t,b2);
}

void Likelihood_Cache::set_length(int C,int b) 
{
  int L = cache->get_length(token);

  lengths[b] = C;

  // Get the new maximum length
  L = max(lengths);

  // If the length is -1, then we don't know any of the lengths, so don't do anything
  if (L > -1)
    cache->set_length(token,L);
}


Likelihood_Cache& Likelihood_Cache::operator=(const Likelihood_Cache& LC) 
{
  B = LC.B;

  cached_value = LC.cached_value;

  cache->release_token(token);
  cache = LC.cache;
  lengths = LC.lengths;
  token = cache->claim_token(LC.allocated_length(),B);
  cache->copy_token(token,LC.token);

  scratch_matrices = LC.scratch_matrices;

  root = LC.root;

  return *this;
}

Likelihood_Cache::Likelihood_Cache(const Likelihood_Cache& LC) 
  :cache(LC.cache),
   B(LC.B),
   token(cache->claim_token(LC.allocated_length(),B)),
   scratch_matrices(LC.scratch_matrices),
   lengths(LC.lengths),
   cached_value(LC.cached_value),
   root(LC.root)
{
  cache->copy_token(token,LC.token);
}

Likelihood_Cache::Likelihood_Cache(const TreeInterface& t, const Mat_Cache& MC,int C)
  :cache(new Multi_Likelihood_Cache(MC)),
   B(t.n_branches()*2),
   token(cache->claim_token(C,B)),
   scratch_matrices(10,Matrix(MC.n_base_models(), MC.n_states())),
   lengths(B,-1),
   cached_value(0),
   root(t.n_nodes()-1)
{
  cache->init_token(token);
}

Likelihood_Cache::~Likelihood_Cache() {
  cache->release_token(token);
}

void select_root(const TreeInterface& t,int b,Likelihood_Cache& LC) {
  int r = t.reverse(b);
  if (t.subtree_contains(r,LC.root))
    b = r;

  LC.root = t.target(b);
}
