#include "dp_hmm.H"

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
/// \file dp_hmm.C
///
/// \brief This file implements the generic Dynamic Programin specific HMM class.
///

#include "probability/choose.H"

using std::vector;

// Is this state silent and in a loop of silent states?
bool dp_HMM::silent_network(int S) const {
  assert(S <= n_states());
  return (silent_network_[S] != -1);
}

/// Replace a sequence of silent_network states with just
/// the first state in the sequence, producing a path that
/// would be emitted from the generalized HMM.
vector<int> dp_HMM::generalize(const vector<int>& path) const {
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
vector<int> dp_HMM::ungeneralize(const vector<int>& g_path) const {
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

log_double_t dp_HMM::generalize_P_one(vector<int>::const_iterator s1,int n) const 
{
  log_double_t Pr = 1;

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
log_double_t dp_HMM::generalize_P(const vector<int>& path) const {
  log_double_t Pr = 1;
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

log_double_t dp_HMM::path_GQ_path(const vector<int>& g_path) const {

  log_double_t Pr = 0.0;
  for(int S=0;S<n_states()-1;S++)
    if (not silent(S))
      Pr += start_P[S] * GQ(S,g_path[0]);

  for(int l=1;l<g_path.size();l++)
    Pr *= GQ(g_path[l-1],g_path[l]);

  return Pr;
}


// The start state could be in this list, but only if its reachable from itself.
// The end state should never be, because it is not reachable from itself.  Therefore it can't be in a cycle.
void dp_HMM::find_and_index_silent_network_states() 
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


void dp_HMM::update_GQ()
{
  GQ = Q;

  if (not silent_network_states.size()) return;

  GQ_exit(GQ, silent_network_states, non_silent_network);
}


// Don't scale Q and GQ until the end???
dp_HMM::dp_HMM(const HMM& H)
  :HMM(H),
   silent_network_(n_states()),
   GQ(n_states(),n_states())
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
      assert(silent_network(S1) or S1 == startstate() or S1 == endstate());

    if (S1 == startstate())
      ;
    else if (S1 == endstate())
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

