#include "arguments.H"

using namespace std;

bool Arguments::set(const string& key) const {
  typeof(begin()) where = find(key);
  if (where == end())
    return false;
  else
    return true;
}


void Arguments::read(istream& input) {
  string line;
  if (not input)
    return;
  while(getline(input,line)) {
    if (!line.size()) continue;
    if (line[0] == '#') continue;
    parse(line);
  }
}

void Arguments::read(int argc,char* argv[]) {
  for(int i=1;i<argc;i++) 
    parse(string(argv[i]));
}

void Arguments::parse(const string& s) {
  int where = s.find('=');
  if (where == -1)
    add(s,s);
  else {
    string key = s.substr(0,where);
    string value = s.substr(where+1);
    add(key,value);
  }
}

void Arguments::add(const string& key, const string& value) {
  if (set(key))
    cerr<<"Warning: over-writing "<<key<<" = "<<(*this)[key]<<endl;
  (*this)[key] = value;
}


void Arguments::print(std::ostream& o) {
  for(typeof(begin()) i=begin();i!=end();i++)
    o<<i->first<<"="<<i->second<<std::endl;
  o<<std::endl;
}
