#include <cassert>
#include "sequence.H"
#include "myexception.H"

int alphabet::operator[](char c) const {
  if (c=='-') 
    return alphabet::gap;
  for(int i=0;i<size();i++) {
    if (data[i]==c)
      return i;
  }
  std::cerr<<int(c)<<endl;
  throw myexception(string("Letter '") + c + string("' not in alphabet."));
}

vector<int> alphabet::operator() (const string& s) const{
  vector<int> v(s.size());
  for(int i=0;i<v.size();i++)
    v[i] = operator[](s[i]);
  return v;
}


char alphabet::lookup(int i) const {
  if (i == gap)
    return '-';
  else if (i == not_gap)
    return '*';
  assert(0 <=i && i < data.size());
  return data[i];
}


alphabet::alphabet(const char* s) {
  string letters(s);
  for(int i=0;i<letters.length();i++)
    data.push_back(letters[i]);
}



void sequence::load(const char* filename) {
  ifstream file(filename);
  int length;
  file>>length;
  for(int i=0;i<length;i++) {
    char c;
    file>>c;
    push_back((*a)[c]);
  }
}
