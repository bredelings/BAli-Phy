#include "util.H"

using std::vector;
using std::string;

vector<string> split(const string& s, char c) {
  vector<string> strings;
  int length=0;
  for(int i=0;i<s.size();i++)
    if (s[i] == c) {
      strings.push_back(s.substr(i-length,length));
      length = 0;
    }
    else
      length++;
  
  strings.push_back(s.substr(s.size()-length,length));
  return strings;
}


string strip(const string& s,char c) {
  string s2;
  for(int i=0;i<s.size();i++)
    if (s[i] != c)
      s2 += s[i];

  return s2;
}


vector<int> invert(const vector<int>& mapping) {
  vector<int> imapping(mapping.size());

  for(int i=0;i<imapping.size();i++)
    imapping[mapping[i]] = i;

  return imapping;
}
