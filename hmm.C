#include <cmath>
#include "hmm.H"
#include "logsum.H"
#include "choose.H"
#include "inverse.H"

using std::abs;

// Is this state silent and in a loop of silent states?
bool HMM::silent_network(int S) const {
  assert(S <= nstates()+1);
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
  for(int i=path.size()-1;i>0;i--) {
    int S1 = path[i];

    if (silent_network(S1)) {
      int S_end = path[i+1];
      vector<int> extra_states;

      // Choose the extra states... S1->S2->.....>S_end
      do {
	vector<double> P(silent_network_states.size()+1);
	for(int s2=0;s2<silent_network_states.size();s2++) {
	  int S2 = silent_network_states[s2];
	  P[s2] = Q(S1,S2) + GQ(S2,S_end);
	}
	P[silent_network_states.size()] = Q(S1,S_end);

	int next = choose(P);
	if (next == silent_network_states.size())
	  break;
	else
	  extra_states.push_back(silent_network_states[next]);
      } while (1);

      // Insert the extra states after the current position
      path.insert(path.begin()+i+1,extra_states.begin(),extra_states.end());
    }
  }

  return path;
}

double HMM::generalize_P_one(vector<int>::const_iterator s1,int n) const {
  double Pr = 0;

  int S_end = *(s1 + n);
  assert(not silent_network(S_end));

  for(int i=0;i<n;i++) {
    int S1 = *(s1 + i);

    vector<double> P(silent_network_states.size()+1);
    for(int s2=0;s2<silent_network_states.size();s2++) {
      int S2 = silent_network_states[s2];
      P[s2] = Q(S1,S2) + GQ(S2,S_end);
    }
    P[silent_network_states.size()] = Q(S1,S_end);
    int choice = silent_network_states.size();
    if (i+1 != n)
      choice = silent_network_[ *(s1+i+1) ];
    Pr += choose_P(choice,P);
  }
  return Pr;
}

/// What is the probability that the generalized path corresponding to this
/// path would emit this ungeneralized path?
double HMM::generalize_P(const vector<int>& path) const {
  double Pr = 0;
  for(int i=0; i<path.size()-1; i++) {
    if (silent_network(path[i])) {
      int start = i;
      // look along until we find a state not in the silent network (terminates at E)
      while(silent_network(path[i]))
	i++;
      Pr += generalize_P_one(path.begin()+start,i-start);
    }
  }
  return Pr;
}
//      if (silent_network(S2))
//	Pr += Q(S1,S2);
//      else
//	Pr += log(1.0-exp(Q(S1,S1)));

double HMM::path_Q_path(const vector<int>& g_path) const {

  double Pr = log_0;
  for(int S=0;S<nstates();S++)
    if (not silent(S))
      Pr = logsum(Pr,start_P[S] + GQ(S,g_path[0]));

  for(int l=1;l<g_path.size();l++)
    Pr += GQ(g_path[l-1],g_path[l]);

  return Pr;
}

//FIXME - 

HMM::HMM(const vector<int>& v1,const vector<double>& v2,const Matrix& M)
  :silent_network_(v1.size()),Q(M),GQ(M),
   start_P(v2),state_emit(v1) 
{
  assert(start_P.size() == nstates());

  // Compute which nodes are part of the silent network

  //  o First see if silent nodes are connected to silent nodes
  for(int S1=0;S1<nstates()+1;S1++) {
    if (state_emit[S1])
      silent_network_[S1] = -1;
    else {
      bool connected=false;
      for(int S2=0;S2<nstates()+1;S2++) {
	if (Q(S1,S2) > log_limit and not state_emit[S2]) {
	  connected=true;
	  break;
	}
      }
      silent_network_[S1] = connected?1:-1;
    }
  }
  silent_network_[endstate()] = -1;

  //  o Then see if some of these silent nodes aren't actually part of the network
  bool changed=true;
  while(changed) {
    changed=false;
    for(int S1=0;S1<nstates()+1;S1++) {
      if (silent_network_[S1] == -1) continue;

      bool connected = false;
      for(int S2=0;S2<nstates()+1;S2++) {
	if (Q(S1,S2) > log_limit and silent_network_[S2] == 1)
	  connected = true;
      }
      silent_network_[S1] = connected?1:-1;
      if (not connected) changed = true;
    }
  }

  // Compute the backwards mapping: silent_network_ -> silent_network_states
  for(int S1=0;S1<nstates();S1++) {
    if (silent_network(S1)) {
      silent_network_states.push_back(S1);
      silent_network_[S1] = silent_network_states.size()-1;  
    }
  }

  // FIXME - If there are silent states that aren't part of the silent network,
  // then their order matters - they must be before states they lead to.
  // We don't deal with that in computing the state ordering yet (see below) :

  // Compute the state order
  vector<int> temp;
  for(int S1=0;S1<nstates()+1;S1++) {
    // we haven't implemented this case yet - see FIXME above
    assert(not silent(S1) or silent_network(S1) or S1 == endstate());
    if (silent(S1))
      temp.push_back(S1);
    else
      order_.push_back(S1);
  }
  order_.insert(order_.end(),temp.begin(),temp.end());


  // Compute the Generalized Transition Matrix, if we've got silent_network states
  if (silent_network_states.size()) {

    // construct a mapping from node index to non-silent-node index
    vector<int> non_silent_network;
    for(int S=0;S<nstates()+1;S++)
      if (not silent_network(S))
	non_silent_network.push_back(S);

    typedef ublas::matrix<double,ublas::column_major> MatrixC;
    // Solve equation G = Q2 + Q1*G for G; IMQ1 = I - Q1
    MatrixC IMQ1(silent_network_states.size(),silent_network_states.size());
    MatrixC Q2(silent_network_states.size(),non_silent_network.size());
    
    // fill IMQ1 = I - Q1
    for(int i=0;i<silent_network_states.size();i++)
      for(int j=0;j<silent_network_states.size();j++) {
	IMQ1(i,j) = - exp( Q(silent_network_states[i],silent_network_states[j]) );
	if (i==j) IMQ1(i,j) += 1.0;
      }
    
    // fill Q2
    for(int i=0;i<silent_network_states.size();i++)
      for(int a=0;a<non_silent_network.size();a++)
	Q2(i,a) = exp ( Q(silent_network_states[i],non_silent_network[a]) );
    
    Matrix G = solve(IMQ1,Q2);
    
    // Actually modify the elements of GQ
    for(int i=0;i<silent_network_states.size();i++) {
      int S1 = silent_network_states[i];
      // silent network -> not silent network
      for(int j=0;j<non_silent_network.size();j++) {
	int S2 = non_silent_network[j];
	if (Q(S1,S2) < log_limit)
	  GQ(S1,S2) = log_0;
	else {
	  assert(G(i,j) > 0);
	  GQ(S1,S2) = log(G(i,j));
	}
      }
      // silent network -> silent network (not allowed)
      for(int j=0;j<silent_network_states.size();j++) {
	int S2 = silent_network_states[j];
	GQ(S1,S2) = log_0;
      }
    }
  }
}
