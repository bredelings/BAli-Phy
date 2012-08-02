#undef NDEBUG
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
#include "2way.H"
#include "imodel.H"

using std::vector;

//      if (silent_network(S2))
//	Pr += Q(S1,S2);
//      else
//	Pr += log(1.0-exp(Q(S1,S1)));

efloat_t HMM::path_Q_path(const vector<int>& path) const 
{
  efloat_t Pr = 0.0;
  for(int S=0;S<n_states()-1;S++)
    if (not silent(S))
      Pr += start_P[S] * Q(S,path[0]);

  for(int l=1;l<path.size();l++)
    Pr *= Q(path[l-1],path[l]);

  return Pr;
}

HMM::HMM(const indel::PairHMM& P)
  :HMM({3,2,1,0,0}, A2::states::S, A2::states::E, P.start_pi(), P, 1.0)
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
  :state_emit(bitmasks),
   start(start_index),
   end(end_index),
   start_P(start_p),
   Q(M),
   B(Beta)
{
  assert(Q.size1() >= n_states());
  assert(Q.size2() >= n_states());
}

// s:s (m/i):(m/d) (m,i,s)_r:I D:(m,d,e)_c e:e
HMM Glue(const HMM& top, const HMM& bottom)
{
  int glue_bit = -1;
  for(int i=0;i<64;i++)
    if (top.all_bits().test(i) and bottom.all_bits().test(i))
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

  HMM G;
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
  G.state_emit.push_back(HMM::bitmask_t());
  state_parts.push_back({top.end, active, bottom.end, active});

  // s:s
  G.start = G.state_emit.size();
  G.state_emit.push_back(HMM::bitmask_t());
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

