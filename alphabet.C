#include "alphabet.H"
#include <cassert>

int alphabet::operator[](char c) const {
  // Check the letters
  for(int i=0;i<size();i++) {
    if (data[i]==c)
      return i;
  }

  // Check for a gap
  if (c=='-') 
    return alphabet::gap;

  // Check the symbols for any_letter
  for(int i=0;i<missing.size();i++) {
    if (missing[i]==c)
      return alphabet::not_gap;
  }

  // We don't have this letter!
  throw bad_letter(c);
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


bool operator==(const alphabet& a1,const alphabet& a2) {
  return a1.data == a2.data;
}

alphabet::alphabet(const string& s,const string& letters):name(s) {
  for(int i=0;i<letters.length();i++)
    data.push_back(letters[i]);
}

alphabet::alphabet(const string& s,const string& letters,const string& m)
  :name(s) 
{
  for(int i=0;i<letters.length();i++)
    data.push_back(letters[i]);
  for(int i=0;i<m.length();i++)
    missing.push_back(m[i]);
}

