#include "util.H"

using std::vector;
using std::string;

string join(const vector<string>& v,char c) {
  string s;
  if (v.size())
    s = v[0];
  for(int i=1;i<v.size();i++) {
    s += string(1,c);
    s += v[i];
  }
  return s;
}


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


string strip(const string& s,const string& chars) {
  string s2;
  for(int i=0;i<s.size();i++) {
    bool found = false;
    for(int j=0;j<chars.size() and not found;j++) {
      if (s[i] == chars[j])
	found = true;
    }
    if (not found)
      s2 += s[i];
  }

  return s2;
}


vector<int> invert(const vector<int>& mapping) {
  vector<int> imapping(mapping.size());

  for(int i=0;i<imapping.size();i++)
    imapping[mapping[i]] = i;

  return imapping;
}

vector<int> compose(const vector<int>& mapping1,const vector<int>& mapping2) {
  assert(mapping1.size() == mapping2.size());

  vector<int> mapping(mapping1.size());

  for(int i=0;i<mapping.size();i++)
    mapping[i] = mapping2[mapping1[i]];

  return mapping;
}

std::vector<std::string> truncate_names(const std::vector<std::string>& names) {
  std::vector<std::string> names2 = names;

  std::sort(names2.begin(),names2.end(),lstr());

  vector<int> mapping = compute_mapping(names,names2);

  //--------------- Do the truncation --------------//
  for(int i=0;i<names2.size();i++)
      if (names2[i].size() > 10)
	names2[i] = names2[i].substr(0,10);

  //----------- Check for collisions --------------//
  vector<int> multiplicity(names2.size(),0);

  for(int i=0;i<names2.size();i++)
    for(int j=0;j<i;j++) {

      if (names2[i] != names2[j]) continue;

      //      cerr<<names2[i]<<" and "<<names2[j]<<" both map to "<<names2[i]<<endl;

      if (not multiplicity[j])
	multiplicity[j]++;	    

      multiplicity[j]++;
      
      string number = convertToString(multiplicity[j]);
      names2[i].replace( names2[i].size()-number.size(),number.size(),number);
      //      cerr<<"Rewriting "<<names[i]<<" => "<<names2[i]<<endl;
    }

  return apply_mapping(names2,invert(mapping));
}
