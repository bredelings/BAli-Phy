#include "util.H"

using std::vector;
using std::string;

vector<string> split(const string& s, char c) {
  vector<string> strings;
  int p=0;
  for(int i=0;i<s.size();i++)
    if (s[i] == 'c') {
      strings.push_back(s.substr(p,i-1));
      p = i+1;
    }
  strings.push_back(s.substr(p,s.size()-1));
  return strings;
}


string strip(const string& s,char c) {
  string s2;
  for(int i=0;i<s.size();i++)
    if (s[i] != c)
      s2 += s[i];

  return s2;
}
