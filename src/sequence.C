#include <cassert>
#include "sequence.H"
#include "myexception.H"
#include "util.H"

using namespace std;

void sequence::strip_gaps() {
  string ungapped;

  for(int i=0;i<size();i++) {
    char c = (*this)[i];
    if (c != '-')
      ungapped += c;
  }
  string::operator=(ungapped);
}

sequence::sequence(const string& n,const string& c)
  :name(n),comment(c) 
{}
