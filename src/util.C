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
      if (names2.size() >= 10)
	names2[i].replace( names2[i].size()-number.size(),number.size(),number);
      else
	//FIXME - this doesn't work, because if two name[i] objects are the same
	// then the mapping isn't computed correctly: we always go back to the first one.
	names2[i] = names2[i] + number;
      //      cerr<<"Rewriting "<<names[i]<<" => "<<names2[i]<<endl;
    }

  return apply_mapping(names2,invert(mapping));
}

bool contains_char(const string& s,char c) {
  for(int i=0;i<s.size();i++)
    if (s[i] == c)
      return true;
  return false;
}

bool get_word(string& word, int& i, const string& s,
	      const string& delimiters,const string& whitespace) 
{
  while(contains_char(whitespace,s[i])) {
    i++;
    if (i >= s.size()) 
      return false;
  }

  int start = i;
  if (contains_char(delimiters,s[i])) {
    word = s.substr(i,1);
    i++;
    return i < s.size();
  }

  do { i++; }
  while(not contains_char(delimiters,s[i]) and not contains_char(whitespace,s[i])
	and i < s.size());

  word = s.substr(start,i-start);

  return i < s.size();
}
