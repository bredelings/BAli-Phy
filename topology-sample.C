#include <cmath>
#include <valarray>
#include <algorithm>
#include <iostream>
#include "sample.H"
#include "logsum.H"
#include "choose.H"
#include "bits.H"
#include "mcmc.H"
// for prior(P) [versus prior(A,P) ]
#include "likelihood.H"

//CHECK - all those states that can't be reached - can s2_ == s2?
//CHECK (how?) - do the differences for both +/- (5 alignments) and 
//               different substitution scores, match?


using std::valarray;

//need 10 bits (2+2+2+2+2) to remember saved stated for all 3 sub-alignments
const int nstates=1024;
const int present10=783;
const int present01=1008;
const int present11=1023;
const int statesmask = 1023;

namespace states {
  const int M  = 0;
  const int G1 = 1;
  const int G2 = 2;
  const int E  = 3;
};

bool legal(int bits) {
  if (not bitset(bits,4)) {
    int temp = 0;
    if (bitset(bits,0))
      temp++;
    if (bitset(bits,1))
      temp++;
    if (bitset(bits,2) or bitset(bits,3) or bitset(bits,5))
      temp++;
							     
    if (temp>1)   // not legal for n5
      return false;
  }
   
  if (not bitset(bits,5)) {
    int temp = 0;
    if (bitset(bits,2))
      temp++;
    if (bitset(bits,3))
      temp++;
    if (bitset(bits,0) or bitset(bits,1) or bitset(bits,4))
      temp++;
							     
    if (temp > 1) // not legal for n5
      return false;
  }
  return true;
}

static int bits_to_state(int bits,int b1,int b2) {
  if (not bitset(bits,b1)) {
    if (bitset(bits,b2))
      return states::G1;
    else
      return 3;
  }
  else {
    if (bitset(bits,b2))
      return states::M;
    else
      return states::G2;
  }
}

// Takes a bitmask of character-presence in sequences 0,1,2,3,4,5
// Returns the state for each alignment, 
//  with the validity of sub-alignments 0,1,2,3,4 marked in bits 10,11,12,13,14
static int get_column_states(int bits) {
  int S=0;
  S |= bits_to_state(bits,4,0)<<0;
  S |= bits_to_state(bits,4,1)<<2;
  S |= bits_to_state(bits,5,2)<<4;
  S |= bits_to_state(bits,5,3)<<6;
  S |= bits_to_state(bits,4,5)<<8;

  for(int i=0;i<5;i++) {
    if (bits_present(S>>(2*i),3)) { // if alignment i is missing
      S &= ~(3<<(2*i));      // clear its state
      S = setbit(S,10+i);    // and mark it missing
    }
  }
  return S;
}

// Takes the alignments for this column, and fill in the missing
//  states using states calculated from the previous column
int get_all_states(int all_states1,int column_states2) {
  int S = column_states2;
  for(int i=0;i<5;i++) {
    if (bitset(column_states2,10+i)) // alignment i is missing
      S |= (all_states1&(3<<(2*i)));
  }
  return S & statesmask;
}


// can we always recover the info that was lost in masking stuff out?
static int chain_to_alignments(int bits1,int s1) {
  bits1 = setbit(bits1,4, bits_present(s1,present10));
  bits1 = setbit(bits1,5, bits_present(s1,present01));

  int column_states1 = get_column_states(bits1);

  int saved_states1 = s1;
  if (bitset(bits1,4))   //zero out non-saved states
    saved_states1 &= ~present10;
  
  if (bitset(bits1,5))   //zero out non-saved states
    saved_states1 &= ~present01;

  int all_states1 = get_all_states(saved_states1,column_states1);

  return all_states1;
}


static int alignments_to_chain(int bits2,int all_states2) {
  int s2 = all_states2;

  if (bitset(bits2,4))   // save only state for 5-2,5-3
    s2 |= present10;
  if (bitset(bits2,5))   // save only state for 4-0,4-1
    s2 |= present01;

  assert(all_states2 == chain_to_alignments(bits2,s2));

  return s2;
}

static double p_move(int states1, int states2,const IndelModel& IModel) {
  double P=0;
  for(int i=0;i<5;i++) {
    if (not bitset(states2,10+i)) {    // this sub-alignment is present in this column
      int s1 = (states1>>(2*i))&3;
      int s2 = (states2>>(2*i))&3;
      P += IModel.Q[s1][s2];
    }
  }
  return P;
}

double get_DP_array(vector< vector<double> >& P, const vector<int>& bits, const IndelModel& IModel) {
  //bits2   = bitmask of presence of n1,n2,n3,n4
  //states2 = state of the sub-alignment HMMs at this location
  //s2      = state of this presence/absence chain at this location

  /************** Calculate DP array ********************/
  for(int character=1;character<P.size();character++) {
    for(int state=0;state<4;state++) {
      int bits2 = bits[character] | (state<<4);

      if (not legal(bits2))
	continue;

      for(int s1=0;s1<nstates;s1++) {
	if (P[character-1][s1] == log_0)
	  continue;

	// Get previous states (hidden, or calculated)
	int all_states1 = chain_to_alignments(bits[character-1],s1);

	// Get current states (only present ones)
	int column_states2 = get_column_states(bits2);

	double p = p_move(all_states1,column_states2,IModel);

	// Copy previous states into alignments where not present
	int all_states2 = get_all_states(all_states1,column_states2);
	int s2 = alignments_to_chain(bits2,all_states2);

	P[character][s2] = logsum(P[character][s2],p + P[character-1][s1]);
      }
    }
  }
  int endstates = (states::E<<8) | (states::E<<6) | (states::E<<4) | 
    (states::E<<2) | (states::E);
  double Pr=log_0;
  for(int s1=0;s1<nstates;s1++) {
    int all_states1 = chain_to_alignments(bits[bits.size()-1],s1);

    Pr = logsum(Pr,P[P.size()-1][s1] + p_move(all_states1,endstates,IModel));
  }
  return Pr;
}

vector<int> sample_path(const vector< vector<double> >& P,const vector<int>& bits,
			const IndelModel& IModel) {
  vector<int> path;
  path.reserve(P.size());

  vector<double> choices(nstates);
  int endstates = (states::E<<8) | (states::E<<6) | (states::E<<4) | 
    (states::E<<2) | (states::E);
  for(int s1=0;s1<nstates;s1++) {
    int all_states1 = chain_to_alignments(bits[bits.size()-1],s1);
    
    choices[s1] = P[P.size()-1][s1] + p_move(all_states1,endstates,IModel);
  }

  int s2 = choose(choices);
  for(int character=P.size()-1;;) {

    path.push_back(s2);

    character--;
    if (not (character>0)) break;

    int bits2 = bits[character+1];
    bits2 = setbit(bits2,4,bits_present(s2,present10));
    bits2 = setbit(bits2,5,bits_present(s2,present01));

    assert(legal(bits2));

    for(int s1=0;s1<nstates;s1++) {
      if (P[character][s1] == log_0) {
	choices[s1] = log_0;
	continue;
      }

      int all_states1 = chain_to_alignments(bits[character],s1);
      
      int column_states2 = get_column_states(bits2);

      int all_states2 = get_all_states(all_states1,column_states2);
      int s2_ = alignments_to_chain(bits2,all_states2);

      if (s2 == s2_) {
	double p = p_move(all_states1,column_states2,IModel);
	choices[s1] = P[character][s1] + p;
      }
      else
	choices[s1] = log_0;
    }
    
    s2 = choose(choices);
  }
  std::reverse(path.begin(),path.end());
  return path;
}

vector<int> get_path(const alignment& A,int b) {
  vector<int> path;
  path.reserve(A.length());

  vector<int> bits;
  for(int column=0;column<A.length();column++) {
  }
  return bits;
}

/********* Which nodes adjacent to this brranch *********/
vector<int> get_nodes(const alignment& A, const tree& T,int b) {
  vector<int> nodes(6);

  nodes[4] = T.branch(b).child();
  nodes[5] = T.branch(b).parent();
  assert(nodes[5] = T[nodes[4]].parent());
  
  // This must be an internal branch
  nodes[0] = T[nodes[4]].left();
  nodes[1] = T[nodes[4]].right();
  nodes[2] = T[nodes[5]].left();
  nodes[3] = T[nodes[5]].right();
  
  if (nodes[2] == nodes[4])
    nodes[2] = T[nodes[5]].parent();
  else if (nodes[3]==nodes[4])
    nodes[3] = T[nodes[5]].parent();
  
  return nodes;
}

/****** Which nodes are present in which column? *******/
vector<int> get_bits(const alignment& A, const tree& T,int b) {

  vector<int> nodes = get_nodes(A,T,b);

  vector<int> bits;
  bits.push_back(0);

  for(int column=0;column<A.length();column++) {
    int state=0;
    if (not A.gap(column,nodes[0]))
      state |= 1<<0;
    if (not A.gap(column,nodes[1]))
      state |= 1<<1;
    if (not A.gap(column,nodes[2]))
      state |= 1<<2;
    if (not A.gap(column,nodes[3]))
      state |= 1<<3;

    if (state) 
      bits.push_back(state);
  }
  return bits;
}

alignment construct(const alignment& old,const tree& T,const vector<int>& path,int b) {
  alignment A = old;
  vector<int> nodes = get_nodes(A,T,b);

  /**** Get columns where our alignments are present ****/
  vector<int> columns;
  for(int column=0;column<A.length();column++) {
    bool present = false;
    if (not A.gap(column,nodes[0]))
      present = true;
    if (not A.gap(column,nodes[1]))
      present = true;
    if (not A.gap(column,nodes[2]))
      present = true;
    if (not A.gap(column,nodes[3]))
      present = true;

    if (present) {
      columns.push_back(column);
    }
  }

  /***** Write the state info from $path into the matrix *****/
  for(int character=0;character<path.size();character++) {
    int s = path[character];
    if (bits_present(s,present10))
      A(columns[character],nodes[4]) = alphabet::not_gap;
    else
      A(columns[character],nodes[4]) = alphabet::gap;

    if (bits_present(s,present01))
      A(columns[character],nodes[5]) = alphabet::not_gap;
    else
      A(columns[character],nodes[5]) = alphabet::gap;
  }

  /************** Remove columns w/ only gaps *************/
  remove_empty_columns(A);

  assert(valid(A));

  return A;
}


bool sample_topology_sgaps(alignment& A,Parameters& P1,const Parameters& P2, 
				     const Parameters& P3,int b) {
  double Pr1 = P1.probability(A,P1);
  double Pr2 = P1.probability(A,P2);
  double Pr3 = P1.probability(A,P3);

  /*********** Choose A Topology ************/
  int choice = choose(Pr1,Pr2,Pr3);

  bool success = false;
  if (choice == 1) {
    P1 = P2;
    success = true;
  }
  else if (choice == 2) {
    P1 = P3;
    success = true;
  }
  return true;
}


/// Do a Gibbs sample between a set of three topologies/parameters
bool sample_topology(alignment& A,Parameters& P1,const Parameters& P2, 
			       const Parameters& P3,int b) {
  const IndelModel& IModel = P1.IModel();

  SequenceTree& T1 = P1.T;
  const SequenceTree& T2 = P2.T;
  const SequenceTree& T3 = P3.T;

  vector <int> bits1 = get_bits(A,T1,b);
  vector <int> bits2 = get_bits(A,T2,b);
  vector <int> bits3 = get_bits(A,T3,b);

  //FIXME - assert something here are having the same nodes
  //for the different topologies w/ get_nodes
  //Also check the tree topologies
  assert(bits1.size() == bits2.size());
  assert(bits3.size() == bits2.size());

  /*** Set up initial conditions (for character -1) ***/
  vector< vector<double> > DP1;
  DP1.push_back(vector<double>(nstates,log_0));
  
  for(int s1=0;s1<3;s1++)
    for(int s2=0;s2<3;s2++)
      for(int s3=0;s3<3;s3++) 
	for(int s4 = 0;s4<3;s4++)
	  for(int s5 = 0;s5<3;s5++) {
	    int h = (s5<<8)|(s4<<6)|(s3<<4)|(s2<<2)|s1;
	    DP1[0][h] = IModel.pi[s1] + IModel.pi[s2] + IModel.pi[s3]
	      + IModel.pi[s4] + IModel.pi[s5];
      }

  for(int i=0;i<bits1.size()-1;i++) 
    DP1.push_back(vector<double>(nstates,log_0));

  vector< vector<double> > DP2 = DP1;
  vector< vector<double> > DP3 = DP1;

  /********* Calculate Forward Probabilities **********/
  double PA1 = get_DP_array(DP1,bits1,IModel);
  double PA2 = get_DP_array(DP2,bits2,IModel);
  double PA3 = get_DP_array(DP3,bits3,IModel);

  std::cerr<<" PA1 = "<<PA1<<"       PA2 = "<<PA2<<"       PA3 = "<<PA3<<std::endl;

  double PS1 = P1.likelihood(A,P1);
  double PS2 = P1.likelihood(A,P2);
  double PS3 = P1.likelihood(A,P3);

  std::cerr<<" PS1 = "<<PS1<<"       PS2 = "<<PS2<<"       PS3 = "<<PS3<<std::endl;

  double PP1 = prior(P1);
  double PP2 = prior(P2);
  double PP3 = prior(P3);

  /*********** Choose A Topology ************/
  int choice = choose(PA1+PS1+PP1, PA2+PS2+PP2, PA3+PS3+PP3);

  const vector<int>* chosen_bits = &bits1;
  const vector< vector<double> >* chosen_DP = &DP1;
  const Parameters* chosen_P = &P1;
  double PS = PS1;

  if (choice == 1) {
    chosen_bits = &bits2;
    chosen_DP = &DP2;
    chosen_P = &P2;
    PS = PS2;
  }
  else if (choice == 2) {
    chosen_bits = &bits3;
    chosen_DP = &DP3;
    chosen_P = &P3;
    PS = PS3;
  }
  
  // do traceback - how to calculate probability of observing 
  vector<int> path1 = get_path(A,b);
  vector<int> path2 = sample_path(*chosen_DP,*chosen_bits,IModel);

  /****************** Do traceback ********************/
  const alignment old = A;
  A = construct(A,chosen_P->T,path2,b);

  //  std::cerr<<old<<endl<<endl;
  //  std::cerr<<A<<endl<<endl;
  double l1 = PS1 + P1.prior(old,P1);
  double l2 = PS + P1.prior(A,*chosen_P);

  std::cerr<<" L1 = "<<l1<<"    L2 = "<<l2<<std::endl;

  std::cerr<<" choice = "<<choice<<std::endl;
  if (choice != 0) {
    P1 = *chosen_P;
    return true;
  }
  else
    return false;
}

MCMC::result_t sample_topology(alignment& A,Parameters& P1,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  vector<int> nodes = get_nodes(A,P1.T,b);

  /****** Generate the Different Topologies *******/
  Parameters P2 = P1;
  Parameters P3 = P1;

  SequenceTree& T2 = P2.T;
  SequenceTree& T3 = P3.T;

  T2.exchange(nodes[1],nodes[2]);
  T3.exchange(nodes[1],nodes[3]);
  
  bool success;
  if (P1.IModel().full_tree)
    success = sample_topology(A,P1,P2,P3,b);
  else
    success = sample_topology_sgaps(A,P1,P2,P3,b);

  if (success)
    result[1] = 1;

  return result;
}

