#include <cmath>
#include "hmm.H"
#include "logsum.H"
#include "choose.H"

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
  for(int i=path.size()-1;i>=0;i--) {
    int S1 = path[i];

    if (silent_network(S1)) {
      int S_end = path[i+1];
      vector<int> extra_states;

      // Choose the extra states... S1->S2->.....>S_end
      do {
	vector<efloat_t> P(silent_network_states.size()+1);
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

efloat_t HMM::generalize_P_one(vector<int>::const_iterator s1,int n) const {
  efloat_t Pr = 1;

  int S_end = *(s1 + n);
  assert(not silent_network(S_end));

  for(int i=0;i<n;i++) {
    int S1 = *(s1 + i);

    vector<efloat_t> P(silent_network_states.size()+1);
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

efloat_t HMM::path_Q_path(const vector<int>& g_path) const {

  efloat_t Pr = 0.0;
  for(int S=0;S<nstates();S++)
    if (not silent(S))
      Pr += start_P[S] * Q(S,g_path[0]);

  for(int l=1;l<g_path.size();l++)
    Pr *= Q(g_path[l-1],g_path[l]);

  return Pr;
}

efloat_t HMM::path_GQ_path(const vector<int>& g_path) const {

  efloat_t Pr = 0.0;
  for(int S=0;S<nstates();S++)
    if (not silent(S))
      Pr += start_P[S] * GQ(S,g_path[0]);

  for(int l=1;l<g_path.size();l++)
    Pr *= GQ(g_path[l-1],g_path[l]);

  return Pr;
}

// IF (and only if) T > 1, then GQ(i,j) can be > 0....

eMatrix GQ_exit(const eMatrix& Q,const vector<int>& silent_network_states,const vector<int>& non_silent_network) {

  const int n_S = silent_network_states.size();
  const int n_NS = non_silent_network.size();


  //--------------- Create and fill the matrix G -----------------//
  eMatrix G(n_S,n_S+n_NS);
  for(int s1=0; s1<G.size1(); s1++) {
    int S1 = silent_network_states[s1];
    int j=0;
    for(;j < n_S; j++) {
      int S2 = silent_network_states[j];
      G(s1,j) = Q(S1,S2);
    }
    for(;j<G.size2();j++) {
      int NS = non_silent_network[j-n_S];
      G(s1,j) = Q(S1,NS);
    }
  }


  //------------ Compute first hitting probabilities -------------//

  // eliminate 'z' from destination states
  for(int z=0; z < G.size1(); z++) {
    // for each destination state
    for(int j=z+1; j < G.size2(); j++) {
      G(z,j) /= (1.0 - G(z,z));   // calculate G_(k+1)[z,j]
      for(int i=0;i<G.size1();i++) 
	if (i != z)
	  G(i,j) += G(i,z) * G(z,j); // calculate G_(k+1)[i,j]
    }
  }


  //--------------------- Resize the matrix ------------------------//
  eMatrix G2(n_S,n_NS);
  for(int i=0;i<G2.size1();i++)
    for(int j=0;j<G2.size2();j++)
      G2(i,j) = G(i,n_S + j);
  

  //---------------------------- Check G ---------------------------//
  for(int s1=0; s1<G2.size1(); s1++)
    for(int ns=0; ns<G2.size2(); ns++) {
      int S1 = silent_network_states[s1];
      int NS = non_silent_network[ns];
      if (Q(S1,NS) > 0.0)
	assert(G2(s1,ns) >= Q(S1,NS));
      for(int s2 =0; s2<n_S; s2++) {
	int S2 = silent_network_states[s2];
	if (Q(S1,S2) > 0.0 and Q(S2,NS) > 0.0)
	  assert(G2(s1,ns) >= Q(S1,S2)*Q(S2,NS));
      }
    }
  
  return G2;
}


void HMM::find_and_index_silent_network_states() {

  //------------ List silent nodes connected to silent nodes --------//
  for(int S1=0;S1<nstates()+1;S1++) {
    if (state_emit[S1])
      silent_network_[S1] = -1;
    else {
      bool connected=false;
      for(int S2=0;S2<nstates()+1;S2++) {
	if (connected_Q(S1,S2) and not state_emit[S2]) {
	  connected=true;
	  break;
	}
      }
      silent_network_[S1] = connected?1:-1;
    }
  }
  silent_network_[endstate()] = -1;

  //------------- Remove silent nodes if there are no cycles --------//
  bool changed=true;
  while(changed) {
    changed=false;
    for(int S1=0;S1<nstates()+1;S1++) {
      if (silent_network_[S1] == -1) continue;

      bool connected = false;
      for(int S2=0;S2<nstates()+1;S2++) {
	if (connected_Q(S1,S2) and silent_network_[S2] == 1)
	  connected = true;
      }
      silent_network_[S1] = connected?1:-1;
      if (not connected) changed = true;
    }
  }

  //------------ Compute index of silent network states -------------//
  for(int S1=0;S1<nstates();S1++) {
    if (silent_network(S1)) {
      silent_network_states.push_back(S1);
      silent_network_[S1] = silent_network_states.size()-1;  
    }
  }
}




// Don't scale Q and GQ until the end???
HMM::HMM(const vector<int>& v1,const vector<efloat_t>& v2,const eMatrix& M,double Temp)
  :silent_network_(v1.size()),
   T(Temp),
   Q(M),GQ(M),
   start_P(v2),state_emit(v1) 
{
  
  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++) {
      Q(i,j) = pow(Q(i,j),1.0/T);
      GQ(i,j) = pow(GQ(i,j),1.0/T);
    }

  //--------------------- Scale the prior probs....  --------------------//
  assert(start_P.size() == nstates());
  for(int i=0;i<start_P.size();i++)
    start_P[i] = pow(start_P[i],1.0/T);

  //--------------- Find and index nodes in silent networks ---------------//
  find_and_index_silent_network_states();


  //----------------------  Compute the state order -----------------------//
  vector<int> temp;
  for(int S1=0;S1<nstates()+1;S1++) {
    // FIXME - we don't deal with silent states that aren't part of the silent networks, except the end state
    assert(not silent(S1) or silent_network(S1) or S1 == endstate());

    if (silent(S1))
      temp.push_back(S1);
    else
      order_.push_back(S1);
  }
  order_.insert(order_.end(),temp.begin(),temp.end());


  if (not silent_network_states.size()) return;

  //------------ Compute the Generalized Transition Matrix, if we've got silent_network states

  //index the non silent network states
  vector<int> non_silent_network;
  for(int S=0;S<nstates()+1;S++)
    if (not silent_network(S))
      non_silent_network.push_back(S);
  
  //---------------- compute the probability of -------------------//

  eMatrix G = GQ_exit(Q,silent_network_states,non_silent_network);


  //---------------- Actually modify the elements of GQ ----------------//
  for(int s1=0; s1<silent_network_states.size(); s1++) {
    int S1 = silent_network_states[s1];
    
    // silent network -> not silent network
    for(int ns=0; ns<non_silent_network.size(); ns++) {
      int NS = non_silent_network[ns];
      GQ(S1,NS) = G(s1,ns);
    }
    
    // silent network -> silent network (not allowed)
    for(int s2=0; s2<silent_network_states.size(); s2++) {
      int S2 = silent_network_states[s2];
      GQ(S1,S2) = 0.0;
    }
  }

  //---------------------------- Check GQ --------------------------//
  for(int i=0;i<silent_network_states.size();i++) {
    int S1 = silent_network_states[i];
    for(int ns=0;ns<non_silent_network.size();ns++) {
      int NS = non_silent_network[ns];
	
      if (connected_Q(S1,NS))
	assert(GQ(S1,NS) >= Q(S1,NS));
      
      for(int s2 =0;s2<silent_network_states.size();s2++) {
	int S2 = silent_network_states[s2];
	if (connected_Q(S1,S2) and connected_Q(S2,NS)) 
	  assert(GQ(S1,NS) >= Q(S1,S2)*GQ(S2,NS));
	if (connected_Q(S1,S2) and connected(S2,NS))
	  assert(GQ(S1,NS) >= Q(S1,S2)*GQ(S2,NS));
      }
    }
  }

}
