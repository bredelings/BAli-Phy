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
#include "imodel/imodel.H"

using std::vector;
using std::pair;

//      if (silent_network(S2))
//	Pr += Q(S1,S2);
//      else
//	Pr += log(1.0-exp(Q(S1,S1)));

log_double_t HMM::path_Q_path(const vector<int>& path) const 
{
    log_double_t Pr = 0.0;
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

HMM::bitmask_t remap_bits(HMM::bitmask_t bits, const vector<int>& mapping)
{
    HMM::bitmask_t mask;

    int B = mapping.size();
    for(int j=0;j<B;j++)
	mask.set(mapping[j], bits.test(j));

    return mask;
}

//FIXME - Should be more general - this couldn't (for example) remap {0,1} to {5,6}
vector<HMM::bitmask_t> remap_bitpath(const vector<HMM::bitmask_t>& path, const vector<int>& mapping)
{
    vector<HMM::bitmask_t> path2 = path;
    for(auto& mask: path2)
	mask = remap_bits(mask,mapping);
    return path2;
}

HMM::bitmask_t remap_bits(HMM::bitmask_t bits, const vector<pair<int,int>>& mapping)
{
    HMM::bitmask_t mask;

    for(const auto& x: mapping)
	mask.set(x.second, bits.test(x.first));

    return mask;
}

vector<HMM::bitmask_t> remap_bitpath(const vector<HMM::bitmask_t>& path, const vector<pair<int,int>>& mapping)
{
    vector<HMM::bitmask_t> path2 = path;
    for(auto& mask: path2)
	mask = remap_bits(mask,mapping);
    return path2;
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

    // The idea with start_P is that all of these states are emitting states.
    // We can therefore discern, when moving back to one of these states in back-sampling
    //  that we have emitted too many letters, and stop the back-sampling.
    // Q: Could we allow the Start state to be one of the DP states?
    // A: Yes, we could.
    //  assert(start_P.size() <= bitmasks.size());

    // Q: Can we, in general, compute a non-silent start_P in the Glue code?
    // A: ... ?
}

int find_first_set_bit(const HMM::bitmask_t& mask)
{
    const int L = mask.size();
    for(int i=0;i<L;i++)
	if (mask.test(i))
	    return i;
    return -1;
}

int find_only_set_bit(const HMM::bitmask_t& mask)
{
    if (mask.count() > 1)
	throw myexception()<<"bitmask has more than 1 bit set!";
    return find_first_set_bit(mask);
}

/*
  The general idea:

  start  M  M  I  I  0  D  end
  start  M  D  M  D  I  0  end

  I think we that every time the bottom HMM gets a tick we do M or D and then all following I's until
  the next time we would require a tick.  We then return to the top level to wait for it.

  The start state should, I think, be as if we have just recieved a tick.  That is, we start by emitting
  any possible I residues before our first read where we wait for an input tick.

  With augmentation, we have:
  s:s (m/i):(m/d) (m,i,s)_r:I D:(m,d,e)_c e:e
*/
HMM Glue(const HMM& top, const HMM& bottom)
{
    int glue_bit = find_only_set_bit( top.all_bits() & bottom.all_bits() );

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
    if (top.start != -1 or bottom.start != -1)
    {
	assert(top.start != -1 and bottom.start != -1);
	G.start = G.state_emit.size();
	G.state_emit.push_back(HMM::bitmask_t());
	state_parts.push_back({top.start, active, bottom.start, active});
    }
    else
	G.start = -1;

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

int bitlength(const vector<HMM::bitmask_t>& bits, int b)
{
    int count = 0;
    for(const auto& mask: bits)
	if (mask.test(b)) count++;
    return count;
}

int bitslength(const vector<HMM::bitmask_t>& bits, HMM::bitmask_t m)
{
    int count = 0;
    for(const auto& mask: bits)
	if ((mask&m).any()) count++;
    return count;
}

vector<HMM::bitmask_t> remove_silent(const vector<HMM::bitmask_t>& bits, HMM::bitmask_t emit)
{
    vector<HMM::bitmask_t> new_bits;
    new_bits.reserve(bits.size());
    for(const auto& b: bits)
    {
	auto b2 = b & emit;
	if (b2.any())
	    new_bits.push_back(b2);
    }
    return new_bits;
}

HMM::bitmask_t get_all_bits(const std::vector<HMM::bitmask_t>& a)
{
    HMM::bitmask_t bits;
    for(const auto& b: a)
	bits |= b;
    return bits;
}

/*
 * Take two emission patterns and glue them together based on their overlapping bit.
 */
std::vector<HMM::bitmask_t> Glue_A(const std::vector<HMM::bitmask_t>& top, const std::vector<HMM::bitmask_t>& bottom)
{

    HMM::bitmask_t top_mask = get_all_bits(top);
    HMM::bitmask_t bottom_mask = get_all_bits(bottom);

    int glue_bit = find_only_set_bit(top_mask & bottom_mask);

    vector<HMM::bitmask_t> a;
    a.reserve(top.size() + bottom.size());

    // It seems possible that the glue sequence could be empty.
    // In this case we just emit the entire bottom sequence first?
    if (glue_bit == -1)
    {
	a.insert(a.end(),bottom.begin(), bottom.end());
	a.insert(a.end(),top.begin(), top.end());
	return a;
    }

    assert(bitlength(top, glue_bit) == bitlength(bottom, glue_bit));

    int i=0;
    int j=0;

    while(true)
    {
	// Add insertions
	while (j < bottom.size() and not bottom[j].test(glue_bit))
	    a.push_back(bottom[j++]);
	// Now bottom[j] is either past the end or at the next input site.
    
	// Add deletions:
	while( i< top.size() and not top[i].test(glue_bit))
	    a.push_back(top[i++]);
	// Now top[i] is either past the end or at the next output site.
    
	// We are done if there is nothing left to read
	if (i == top.size() and j == bottom.size()) break;

	// If we are looking for a top state to emit a tick, but top is too short, then abort.
	if (i == top.size()) std::abort();

	// If we are looking for a bottom state to absorb a tick, but bottom is too short, then abort.
	if (j == bottom.size()) std::abort();
    
	// Emit (M/I):(M/D)
	assert(top[i].test(glue_bit) and bottom[j].test(glue_bit));
	a.push_back(top[i++] | bottom[j++]);
    }

    return a;
}

vector<HMM::bitmask_t> get_bits_from_path(const vector<int>& path, const HMM& H)
{
    vector<HMM::bitmask_t> bit_path;

    for(int S: path)
	bit_path.push_back(H.state_emit[S]);

    return bit_path;
}

/*
 * This raises the question of how to generally handle start states, if the start
 * state isn't a silent state that is unreachable from itself.
 * (1) How shall we decide to end the back-sampling?  Could we probabilistically decide
 *     that the next previous character doesn't exist?
 * (2) What if the start state is an emitting state?
 * (3) What is the start state is a silent state, but is reachable from itself?
 * (3a) What if the start state forms a silent loop?
 * (4) Do we need a level in the DP matrix for the start state?  Just like the end state
 *     state, it would seem not.
 * (5) If the start state is an emitting state, how could we tell if we began a path with
 *     the start state, or not?
 *     
 *
 * We could ask similar questions about the end state.  For example, what if the end state
 * is an emitting state?  When we transition to it, should be emit anything?
 */

/// This path should have an end state, but no start state! (Its going to be evaluated using start_P)
vector<int> get_path_unique(const vector<HMM::bitmask_t>& path1, const HMM& H)
{
    // Issue: we basically need to make the start-state M/M/M.

    // Another issue: we actually DO need to look ahead, because only some states
    //                are remembered - some are committed!

    // 0. Here we treat all bits as non-silent.  Thus we use H.Q instead of H.QG.

    // 1. Find the only allowed start_P state
    int before_state = -1;
    for(int i=0;i<H.start_P.size();i++)
	if (H.start_P[i] > 0)
	{
	    assert(before_state == -1);
	    before_state = i;
	}

    // 2. Forward pass: find possible states at each (non-silent) location for path1
    vector< vector<int> > possible_states(path1.size());
    for(int i=0;i<path1.size();i++)
    {
	for(int j=0;j<H.n_states();j++)
	{
	    if (H.state_emit[j] != path1[i]) continue;

	    if (i==0)
	    {
		if (H.connected_Q(before_state, j))
		    possible_states[i].push_back(j);
	    }
	    else
	    {
		bool ok = false;
		for(int last_state: possible_states[i-1])
		    if (H.connected_Q(last_state,j))
			ok = true;
		if (ok)
		    possible_states[i].push_back(j);
	    }
	}
	assert(not possible_states[i].empty());
    }

    // 3. Backward pass: resolve possible states to a single state at each location of path1
    vector<int> path2;
    path2.reserve(path1.size()+2);
    path2.push_back(H.endstate());
    int last_state = H.endstate();
    for(int i=path1.size()-1; i>=0; i--)
    {
	int next_state = -1;
	for(int S:possible_states[i])
	    if (H.connected_Q(S,last_state))
	    {
		assert(next_state == -1);
		next_state = S;
	    }
	assert(next_state != -1);
	path2.push_back(next_state);
	last_state = path2.back();
    }
    std::reverse(path2.begin(), path2.end());
    return path2;
}

matrix<int> get_indices_from_bitpath_w_wo(const vector<HMM::bitmask_t>& bit_path, const vector<int>& rows, HMM::bitmask_t keep, HMM::bitmask_t exclude)
{
    matrix<int> index(bit_path.size(), rows.size());
    vector<int> I(rows.size(), 0);
  
    int c = 0;
    for(int i=0; i<bit_path.size(); i++)
    {
	auto bits = bit_path[i];

	if ((bits & keep).any() and (bits & exclude).none())
	{
	    // Record and advance indices to next column
	    for(int j=0;j<rows.size();j++)
		if (bits.test(rows[j]))
		    index(c,j) = I[j]++;
		else
		    index(c,j) = -1;

	    // Go to next column in index matrix
	    c++;
	}
	else
	{
	    // Just advance indices to next column
	    for(int j=0;j<rows.size();j++)
		if (bits.test(rows[j]))
		    I[j]++;
	}
    }

    index.resize(c, rows.size());

    return index;
}

matrix<int> get_indices_from_bitpath_wo(const vector<HMM::bitmask_t>& bit_path, const vector<int>& rows, HMM::bitmask_t exclude)
{
    HMM::bitmask_t keep;
    for(int row: rows)
	keep.set(row);

    return get_indices_from_bitpath_w_wo(bit_path, rows, keep, exclude);
}

matrix<int> get_indices_from_bitpath_w(const vector<HMM::bitmask_t>& bit_path, const vector<int>& rows, HMM::bitmask_t keep)
{
    HMM::bitmask_t exclude;

    return get_indices_from_bitpath_w_wo(bit_path, rows, keep, exclude);
}

matrix<int> get_indices_from_bitpath(const vector<HMM::bitmask_t>& bit_path, const vector<int>& rows)
{
    HMM::bitmask_t exclude;

    return get_indices_from_bitpath_wo(bit_path, rows, exclude);
}

matrix<int> get_indices_n(int n)
{
    matrix<int> I(n,1);
    for(int i=0;i<n;i++)
	I(i,0) = i;
    return I;
}
