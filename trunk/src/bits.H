#ifndef BITS_H
#define BITS_H

inline bool bitset(int m,int b) {
  return m&(1<<b);
}

inline bool bits_present(int i,int m) {
  return ((i&m) == m);
}

inline int clearbit(int m,int b) {
  return m&(~(1<<b));
}

inline int setbit(int m,int b) {
  return m|(1<<b);
}

inline int setbit(int m,int b,bool c) {
  if (c) m = setbit(m,b);
  return m;
}

inline int num_bits(int n) {
  int sum=0;
  for(int i=0;i<6;i++) 
    if (bitset(n,i)) sum++;
  return sum;
}

#endif
