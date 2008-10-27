#include <cassert>
#include "sequence.H"
#include "myexception.H"
#include "util.H"

using namespace std;

void sequence::strip_gaps() {
  string ungapped;

  for(int i=0;i<size();i++) {
    char c = (*this)[i];

    // FIXME - this hardcodes the - and ? characters...
    if (c != '-' and c != '?')
      ungapped += c;
  }
  string::operator=(ungapped);
}

sequence::sequence(const string& n,const string& c)
  :name(n),comment(c) 
{}

bool operator==(const sequence& s1,const sequence& s2) {
  return s1.name == s2.name and
    (string&)s1 == (string&)s2;
}
