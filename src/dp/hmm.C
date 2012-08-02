/*
   Copyright (C) 2004-2007 Benjamin Redelings

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

///
/// \file hmm.H
///
/// \brief This file implements the generic HMM class.
///

#include <cmath>
#include "hmm.H"
#include "logsum.H"
#include "choose.H"
#include "2way.H"
#include "imodel.H"

using std::abs;
using std::vector;
using std::valarray;

// Is this state silent and in a loop of silent states?
bool HMM::silent_network(int S) const {
  assert(S <= n_states());
  return (silent_network_[S] != -1);
}

/// Replace a sequence of silent_network states with just
/// the first state in the sequence, producing a path that
/// would be emitted from the generalized HMM.
vector<int> HMM::generalize(const vector<int>& path) const {
  vector<int> g_path = path;
  for(int i=g_path.size()-1;i>0;i--) {
    int S1 = g_path[i-1];
    int S2 = g_path[i];
    if (silent_network(S1) and silent_network(S2))
      g_path.erase(g_path.begin()+i);
  }
  return g_path;
}

/// Expand generalized states (i.e. silent network states in the
/// generalized HMM) into a sequence of silent network states, as
/// we might observe in the normal HMM.  This is a random process,
/// and the distribution is chosen so that sampling from the generalized
/// HMM and then ungeneralizing is like sampling from the ungeneralized
/// HMM. (But we can't sample from the ungeneralized HMM using DP)
vector<int> HMM::ungeneralize(const vector<int>& g_path) const {
  vector<int> path = g_path;

  // Go backwards along the path, expanding silent states
  for(int i=path.size()-1;i>=0;i--) {
    int S1 = path[i];

    if (silent_network(S1)) {
      int S_end = path[i+1];
      vector<int> extra_states;

      // Choose the extra states... S1->S2->.....>S_end
      do {
	vector<double> P(silent_network_states.size()+1);
	for(int s2=0;s2<silent_network_states.size();s2++) {
	  int S2 = silent_network_states[s2];
	  P[s2] = Q(S1,S2) * GQ(S2,S_end);
	}
	P[silent_network_states.size()] = Q(S1,S_end);

	int next = choose(P);

	if (next == silent_network_states.size()) {
	  //	  std::cerr<<S1<<"  -> "<<S_end<<"(last)"<<endl<<endl;
	  break;
	}
	else {
	  assert(connected_Q(S1,silent_network_states[next]));
	  extra_states.push_back(silent_network_states[next]);
	  //	  std::cerr<<S1<<"  -> "<<silent_network_states[next]<<endl;
	}
	S1 = extra_states.back();
      } while (1);

      // Insert the extra states after the current position
      path.insert(path.begin()+i+1,extra_states.begin(),extra_states.end());
    }
  }

  return path;
}

efloat_t HMM::generalize_P_one(vector<int>::const_iterator s1,int n) const 
{
  efloat_t Pr = 1;

  int S_end = *(s1 + n);
  assert(not silent_network(S_end));

  for(int i=0;i<n;i++) {
    int S1 = *(s1 + i);

    vector<double> P(silent_network_states.size()+1);
    for(int s2=0;s2<silent_network_states.size();s2++) {
      int S2 = silent_network_states[s2];
      P[s2] = Q(S1,S2) * GQ(S2,S_end);
    }
    P[silent_network_states.size()] = Q(S1,S_end);
    int choice = silent_network_states.size();
    if (i+1 != n)
      choice = silent_network_[ *(s1+i+1) ];
    Pr *= choose_P(choice,P);
   }
  assert(Pr > 0.0);

  return Pr;
}

/// What is the probability that the generalized path corresponding to this
/// path would emit this ungeneralized path?
efloat_t HMM::generalize_P(const vector<int>& path) const {
  efloat_t Pr = 1;
  for(int i=0; i<path.size()-1; i++) {
    if (silent_network(path[i])) {
      int start = i;
      // look along until we find a state not in the silent network (terminates at E)
      while(silent_network(path[i]))
	i++;
      Pr *= generalize_P_one(path.begin()+start,i-start);
    }
  }
  assert(Pr > 0.0);
  return Pr;
}
//      if (silent_network(S2))
//	Pr += Q(S1,S2);
//      else
//	Pr += log(1.0-exp(Q(S1,S1)));

efloat_t HMM::path_Q_path(const vector<int>& path) const {

  efloat_t Pr = 0.0;
  for(int S=0;S<n_states()-1;S++)
    if (not silent(S))
      Pr += start_P[S] * Q(S,path[0]);

  for(int l=1;l<path.size();l++)
    Pr *= Q(path[l-1],path[l]);

  return Pr;
}

efloat_t HMM::path_GQ_path(const vector<int>& g_path) const {

  efloat_t Pr = 0.0;
  for(int S=0;S<n_states()-1;S++)
    if (not silent(S))
      Pr += start_P[S] * GQ(S,g_path[0]);

  for(int l=1;l<g_path.size();l++)
    Pr *= GQ(g_path[l-1],g_path[l]);

  return Pr;
}

// IF (and only if) T > 1, then GQ(i,j) can be > 0....
void GQ_exit(Matrix& G,const vector<int>& silent,const vector<int>& non_silent) 
{
  vector<int> all = silent;
  all.insert(all.end(), non_silent.begin(), non_silent.end());

  //------------ Compute first hitting probabilities -------------//
  for(int i=0; i < silent.size(); i++) {
    int a = silent[i];

    double factor = 1.0/(1.0-G(a,a));

    // consider b->x [ for x not yet eliminated ]
    for(int j=i+1; j < all.size(); j++) {
      int x = all[j];

      // eliminate a->a
      G(a,x) *= factor;   // calculate G_(k+1)[z,j]

      // eliminate b->a->x
      for(int k=0; k < silent.size(); k++) {
	int b = silent[k];
	if (b != a)
	  G(b,x) += G(b,a) * G(a,x); // calculate G_(k+1)[i,j]
      }
    }
  }

  for(int i=0;i<silent.size();i++)
    for(int j=0;j<silent.size();j++)
      G(silent[i],silent[j]) = 0;
}



// The start state could be in this list, but only if its reachable from itself.
// The end state should never be, because it is not reachable from itself.  Therefore it can't be in a cycle.
void HMM::find_and_index_silent_network_states() 
{
  vector<int>& S = silent_network_states;
  vector<int>& s = silent_network_;

  //--------------------- Find silent states ------------------------//
  S.clear();
  for(int i=0;i<n_states()-1;i++)
    if (silent(i))
      S.push_back(i);

  //------------- Remove silent nodes if there are no cycles --------//
  bool changed;

  do {
    changed=false;
    for(int i = S.size()-1;i>=0;i--)
    {
      int a = S[i];

      bool connected = false;
      for(int j=0;j<S.size() and not connected;j++) {
	int b = S[j];
	if (connected_Q(a,b)) connected = true;
      }

      if (not connected) {
	changed = true;
	S.erase(S.begin()+i);
      }
    }
  } while (changed);

  //------------ Compute index of silent network states -------------//
  for(int i=0;i<s.size();i++)
    s[i] = -1;
  for(int i=0;i<S.size();i++)
  {
    assert(S[i] != endstate());
    s[S[i]] = i;
  }
}


void HMM::update_GQ()
{
  GQ = Q;

  if (not silent_network_states.size()) return;

  GQ_exit(GQ, silent_network_states, non_silent_network);
}

HMM::HMM(const indel::PairHMM& P)
  :HMM({3,2,1,0},P.start_pi(), P, 1.0)
{ }

HMM::bitmask_t HMM::all_bits() const
{
  bitmask_t mask;
  for(bitmask_t m: state_emit)
    mask |= m;
  return mask;
}

int HMM::n_characters() const
{
  return all_bits().count();
}

using std::bitset;

bitset<64> remap_bits(const bitset<64>& bits, const vector<int>& map)
{
  bitset<64> mask;

  int B = map.size();
  for(int j=0;j<B;j++)
    mask.set(map[j], bits.test(j));

  return mask;
}

void HMM::remap_bits(const vector<int>& map)
{
  assert(map.size() == n_characters());
  
  for(auto& mask: state_emit)
    mask = ::remap_bits(mask, map);

  hidden_bits = ::remap_bits(hidden_bits, map);
}

// Don't scale Q and GQ until the end???
HMM::HMM(const vector<bitmask_t>& bitmasks, const vector<double>& start_p,const Matrix& M,double Beta)
  :HMM(bitmasks, -1, bitmasks.size()-1, start_p, M,  Beta)
{ }

// Don't scale Q and GQ until the end???
HMM::HMM(const vector<bitmask_t>& bitmasks, int start_index, int end_index, const vector<double>& start_p,const Matrix& M,double Beta)
  :silent_network_(bitmasks.size()),
   state_emit(bitmasks),
   start(start_index),
   end(end_index),
   start_P(start_p),
   Q(M),GQ(M.size1(),M.size2()),
   B(Beta)
{
  //--------------- Find and index nodes in silent networks ---------------//
  find_and_index_silent_network_states();

  //----------------------  Compute the state order for DP-----------------------//
  // FIXME - I think that perhaps the endstate must be last?  If so, why?
  vector<int> temp;
  temp.reserve(n_states()-1);
  dp_order_.reserve(n_states());
  for(int S1=0;S1<n_states();S1++) 
  {
    // FIXME - Handle silent states that aren't in a network, and aren't the end state.
    //
    //         They could go after all the emitting states, but might have particular ordering
    //         constraints relative to other silent-cycle states and silent-non-cycle states.

    if (silent(S1))
      assert(silent_network(S1) or S1 == endstate());

    if (S1 == endstate())
      ;
    else if (silent(S1))
      temp.push_back(S1);
    else
      dp_order_.push_back(S1);
  }
  dp_order_.insert(dp_order_.end(),temp.begin(),temp.end());

  //------------ Compute the Generalized Transition Matrix, if we've got silent_network states

  //index the non silent network states
  non_silent_network.reserve(n_states());
  for(int S=0;S<n_states();S++)
    if (not silent_network(S))
      non_silent_network.push_back(S);

  //---------------- compute the probability of -------------------//
  update_GQ();

#ifndef NDEBUG
  //---------------------------- Check GQ --------------------------//
  for(int i=0;i<silent_network_states.size();i++) {
    int S1 = silent_network_states[i];
    for(int ns=0;ns<non_silent_network.size();ns++) {
      int NS = non_silent_network[ns];

      assert(GQ(S1,NS) >= Q(S1,NS));

      for(int s2 =0;s2<silent_network_states.size();s2++) {
	int S2 = silent_network_states[s2];
	assert(GQ(S1,NS) +1.0e-10 >= Q(S1,NS) + Q(S1,S2)*GQ(S2,NS));
      }
    }
  }
#endif

#ifndef NDEBUG
  for(int s1=0;s1<n_dp_states();s1++) {
    int S1 = dp_order(s1);
    if (not silent(S1)) continue;

    for(int s2=s1;s2<n_dp_states();s2++) {
      int S2 = dp_order(s2);
      assert(not connected(S1,S2));
    }
  }
#endif

  // At the moment, the DP table state (in dp_order) are not allowed to contain the start or end state.
  for(int i: dp_order_)
  {
    assert(i != startstate());
    assert(i != endstate());
  }
}


int mhmm::n_characters() const
{
  return all_bits.count();
}

void mhmm::remap_bits(const vector<int>& map)
{
  assert(map.size() == n_characters());
  int B = map.size();
  
  vector<bitmask_t> state_emit2(state_emit.size());
  all_bits.reset();
  for(int i=0;i<state_emit.size();i++)
  {
    bitmask_t mask;
    for(int j=0;j<B;j++)
      mask.set(map[j],state_emit[i].test(j));
    state_emit2[i] = mask;
    all_bits |= mask;
  }

  std::swap(state_emit, state_emit2);
}

mhmm::mhmm(const indel::PairHMM& P)
  :state_emit({3,2,1,0,0}),
   start(A2::states::S),
   end(A2::states::E),
   Q(P),
   start_P(P.start_pi()),
   all_bits(3)
{
}


mhmm remap_bits(const mhmm& m1, const vector<int>& map)
{
  mhmm m2 = m1;
  m2.remap_bits(map);
  return m2;
}

// s:s (m/i):(m/d) (m,i,s)_r:I D:(m,d,e)_c e:e
mhmm Glue(const mhmm& top, const mhmm& bottom)
{
  int glue_bit = -1;
  for(int i=0;i<64;i++)
    if (top.all_bits.test(i) and bottom.all_bits.test(i))
    {
      assert(glue_bit == -1);
      glue_bit = i;
    }

  // 1. Classify top states into S, E, M/I, and D
  vector<int> m_or_i1;
  vector<int> d1;

  for(int i=0;i<top.state_emit.size();i++)
  {
    if (i == top.start or i == top.end) continue;

    assert(top.state_emit[i].any());

    if (top.state_emit[i].test(glue_bit))
      m_or_i1.push_back(i);
    else
      d1.push_back(i);
  }

  // 2. Classify bottom states into S, E, M/I, and D
  vector<int> m_or_d2;
  vector<int> i2;

  for(int i=0;i<bottom.state_emit.size();i++)
  {
    if (i == bottom.start or i == bottom.end) continue;

    assert(bottom.state_emit[i].any());

    if (bottom.state_emit[i].test(glue_bit))
      m_or_d2.push_back(i);
    else
      i2.push_back(i);
  }

  // 4. Construct states for the new HMM
  enum status_t {active, remembered, committed};

  struct parts
  {
    int state1;
    status_t status1;
    int state2;
    status_t status2;
  };

  mhmm G;
  G.all_bits = top.all_bits | bottom.all_bits;
  vector<parts> state_parts;

  // M/I:M/D
  for(int s1: m_or_i1)
    for(int s2: m_or_d2)
    {
      G.state_emit.push_back(top.state_emit[s1] | bottom.state_emit[s2]);
      state_parts.push_back({s1,active,s2,active});
    }

  // How do we record the info on the hidden states?
  // Actually, this is the hardest part of the whole thing!

  // (M,I,S)_r:I
  for(int s1: m_or_i1)
    for(int s2: i2)
    {
      G.state_emit.push_back(bottom.state_emit[s2]);
      state_parts.push_back({s1,remembered,s2,active});
    }

  /*
  for(int s2: i2)
  {
    G.state_emit.push_back(bottom.state_emit[s2]);
    state_parts.push_back({top.start, remembered,s2,active});
  }
  */

  for(int s1: d1)
    for(int s2: m_or_d2)
    {
      G.state_emit.push_back(top.state_emit[s1]);
      state_parts.push_back({s1, active,s2,committed});
    }

  for(int s1: d1)
  {
    G.state_emit.push_back(top.state_emit[s1]);
    state_parts.push_back({s1, active, bottom.end, committed});
  }

  // e:e
  G.end = G.state_emit.size();
  G.state_emit.push_back(mhmm::bitmask_t());
  state_parts.push_back({top.end, active, bottom.end, active});

  // s:s
  G.start = G.state_emit.size();
  G.state_emit.push_back(mhmm::bitmask_t());
  state_parts.push_back({top.start, active, bottom.start, active});

  // The D/* -> */I transition is forbidden by the fact that D is never remembered and I is never committed.
  // Being committed to M, D, or E is basically equivalent to entering a wait state. Here, we add a separate
  //  wait states M_c, D_c, and E_c which always go to M, D, and E with probability 1.

  // 5. Compute transition matrix
  G.Q.resize( G.state_emit.size(), G.state_emit.size() );
  for(int i=0;i<G.state_emit.size();i++)
    for(int j=0;j<G.state_emit.size();j++)
    {
      double p = 1;
      //      status_t si1 = state_parts[i].status1;
      status_t si2 = state_parts[i].status2;
      status_t sj1 = state_parts[j].status1;
      //      status_t sj2 = state_parts[j].status2;
      if (sj1 == active)
	p = top.Q(state_parts[i].state1,state_parts[j].state1);
      else if (sj1 == remembered and state_parts[i].state1 != state_parts[j].state1)
	p = 0;

      if (si2 == active)
	p *= bottom.Q(state_parts[i].state2,state_parts[j].state2);
      else if (si2 == committed and state_parts[i].state2 != state_parts[j].state2)
	p = 0;

      G.Q(i,j) = p;
    }

  // 6. Find compute the start_pi
  G.start_P.resize( G.state_emit.size() );
  for(int i=0;i<G.state_emit.size();i++)
  {  
    G.start_P[i] = top.start_P[state_parts[i].state1] * bottom.start_P[state_parts[i].state2];

    // The idea here is to find some collection of states such that starting in distribution start_P on 
    // those states leads to the same result as starting in S.  In practice, this is problematic, because
    // when we sample backwards the only way to know when we've gotten to the "start" is if we hit an emitting
    // state before the first character.
  }

  return G;
}


/*
  OK, to how are the state indices used?
  - we need to store their emission patterns
  - we need to index into the DP table
  - 

  Q: When would the start state be part of the DP table?
  A: It must be part of the DP table if it exists and any other state transitions to it. 
  A: Determining reachable states might depend on whether you start from (a) start or (b) start_p.
     Perhaps it should.
  A: OK, lets go with the theory that the states in order_ should be the DP states.
  A: Thus, End should be in order iff End is in the DP table.
  A: However, End should always be ALLOWED to be in the DP table.  Hence we put End in order_,
     check order_, and then remove end_.
 */
