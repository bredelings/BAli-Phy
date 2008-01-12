#include "util.H"

using std::vector;
using std::string;

#include <iostream>
using std::cerr;
using std::endl;


std::istream& getline_handle_dos(std::istream& file,std::string& s)
{
  getline(file,s);
  while(s.size() and (s[s.size()-1] == char(13) or s[s.size()-1] == char(10)))
    s.erase(s.size()-1,1);

  return file;
}


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

string join(const vector<string>& v,const string& token) {
  string s;
  if (v.size())
    s = v[0];
  for(int i=1;i<v.size();i++) {
    s += token;
    s += v[i];
  }
  return s;
}


vector<string> split(const string& s, char c) 
{
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

vector<string> split(const string& s, const string& token)
{
  vector<string> strings;

  int p1 = 0;
  do {
    int p2 = s.find(token,p1);
    if (p2 == -1)
    {
      strings.push_back(s.substr(p1));
      break;
    }
    else 
    {
      assert(p2 < s.size());

      strings.push_back(s.substr(p1,p2-p1));
      p1 = p2 + token.size();

      assert(p1 <= s.size());
    }
  } while (true);
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


vector<int> invert(const vector<int>& mapping) 
{
  vector<int> imapping(mapping.size(),-1);

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

void scan_lines(std::istream& file,int skip,int subsample, int max, 
		accumulator<string>& op)
{
  int n_lines=0;
  string line;
  for(int line_number=0;getline_handle_dos(file,line);line_number++) 
  {
    // don't start if we haven't skipped enough trees
    if (line_number < skip) continue;

    // skip trees unless they are a multiple of 'subsample'
    if ((line_number-skip) % subsample != 0) continue;

    // quit if we've read in 'max' trees
    if (max >= 0 and n_lines == max) break;

    // should this be protected by a try { } catch(...) {} block?
    op(line);
    n_lines++;
  }
}

vector<string> load_lines(std::istream& file,int skip,int subsample, int max)
{
  vector_accumulator<string> lines;

  scan_lines(file,skip,subsample,max,lines);

  return lines;
}

std::string get_basename(std::string filename)
{
  // remove the pathname 
  while(filename.find('/') != -1) 
    filename = filename.substr(filename.find('/')+1);

  return filename;
}

std::string remove_extension(std::string filename)
{
  // remove the extension
  int dot = filename.rfind('.');
  string name = filename;
  if (dot != -1)
    name = filename.substr(0,dot);
  return name;
}
