#include <cassert>
#include "sequence.H"

int alphabet::operator[](char c) const {
  if (c=='-') 
    return -1;
  for(int i=0;i<size();i++) {
    if (data[i]==c)
      return i;
  }
  assert(0);
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


alphabet::alphabet(const char* s,const char* filename) {
  string letters(s);
  for(int i=0;i<letters.length();i++)
    data.push_back(letters[i]);

  frequency.resize(size(),1.0/size());
  substitution.resize(size(),size());

  ifstream matrix_file(filename);

  for(int i=0;i<size();i++) 
    for(int j=0;j<size();j++) 
      matrix_file>>substitution(i,j);
      
  matrix_file.close();
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
