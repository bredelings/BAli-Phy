#include "3way.H"
#include "bits.H"

// Returns the state, with the validity of sub-alignments 1,2,3 marked in bits 6,7,8
int A3::bits_to_states(int bits) {
  int S=(1<<6)|(1<<7)|(1<<8);
  if (not bitset(bits,0)) {
    if (bitset(bits,1))
      S |= (states::G1<<0);
    else
      S = clearbit(S,6);

    if (bitset(bits,2))
      S |= (states::G1<<2);
    else
      S = clearbit(S,7);

    if (bitset(bits,3))
      S |= (states::G1<<4);
    else
      S = clearbit(S,8);
  }
  else {
    if (bitset(bits,1))
      S |= (states::M<<0);
    else
      S |= (states::G2<<0);
      
    if (bitset(bits,2))
      S |= (states::M<<2);
    else
      S |= (states::G2<<2);

    if (bitset(bits,3))
      S |= (states::M<<4);
    else
      S |= (states::G2<<4);
  }
  return S;
}

inline int A3::getstates(int S) {
  assert(0 <= S and S<nstates+1);

  if (S==endstate)
    return (1<<12)|(1<<11)|(1<<10)|(states::E<<8)|(states::E<<6)|(states::E<<4);

  int bits=0;

  //--------- Set bits ---------
  if (S<8) {
    //internal node present
    bits |= (1<<0);

    // other nodes present according to S
    bits |= ((~S)&7)<<1;
  }
  else {
    S -=8;
    if (S/9 == 0) 
      bits |= (1<<1);
    else if (S/9 == 1) 
      bits |= (1<<2);
    else if (S/9 == 2) 
      bits |= (1<<3);
    S = S%9;
  }

  //-------- Get states --------
  int states = bits_to_states(bits);
  for(int i=0;i<3;i++)
    if (not bitset(states,6+i)) {
      int s = S%3;
      S /= 3;
      states |= (s<<(2*i));
    }

  //---------- Merge ----------
  return (states<<4)|bits;
}

inline int A3::findstate(int states) {
  unsigned int mask = ~((~0)<<10);
  for(int S=0;S<=nstates;S++) {
    if ((getstates(S)&mask) == (states&mask))
      return S;
  }
  //couldn't find it?
  assert(0);
}

using namespace A3;

int A3::di(int S) {
  S = getstates(S);
  if (S&(1<<1))
    return 1;
  else
    return 0;
}

int A3::dj(int S) {
  S = getstates(S);
  if (S&(1<<2))
    return 1;
  else
    return 0;
}

int A3::dk(int S) {
  S = getstates(S);
  if (S&(1<<3))
    return 1;
  else
    return 0;
}

int A3::dc(int S) {
  if (dj(S)==0 and dk(S)==0)
    return 0;
  else
    return 1;
}

int A3::dl(int S) {
  S = getstates(S);
  if (S&(1<<0))
    return 1;
  else
    return 0;
}

vector<int> get_path_3way(const alignment& A,int n0,int n1,int n2,int n3) {

  //----- Store whether or not characters are present -----//
  vector<int> present;
  for(int column=0;column<A.length();column++) {
    int bits=0;
    if (not A.gap(column,n0))
      bits |= (1<<0);
    if (not A.gap(column,n1))
      bits |= (1<<1);
    if (not A.gap(column,n2))
      bits |= (1<<2);
    if (not A.gap(column,n3))
      bits |= (1<<3);
    present.push_back(bits);
  }

  int A10 = states::M;
  int A20 = states::M;
  int A30 = states::M;

  vector<int> path;
  path.reserve(A.length()+1);
  for(int column=0;column<A.length();column++) {
    int bits = present[column];

    if (not bits) 
      continue;

    int states = bits_to_states(bits);
    if (states & (1<<6))
      A10 = (states>>0)&3;
    if (states & (1<<7))
      A20 = (states>>2)&3;
    if (states & (1<<8))
      A30 = (states>>4)&3;

    states |= (A30<<4)|(A20<<2)|(A10<<0);
    states = (states<<4)|bits;
    int S = findstate(states);
    path.push_back(S);
  }
  
  path.push_back(endstate);
  return path;
}

