#include <cassert>
#include "sequence.H"
#include "myexception.H"
#include "util.H"

void sequence::parse(const string& line,const string& letters) {
  /*------------ Delete '>' from label -----------*/
  assert(line[0] == '>');
  string label = line.substr(1);

  /*------Parse label with name and comments------*/
  name = label;
  comment = "";
  int where = label.find_first_of(" \t");
  if (where != -1) {
    name = label.substr(0,where);

    where = label.find_first_not_of(" \t",where+1);
    comment =  label.substr(where);
  }

  // Strip out space characters from the letters
  string letters2 = strip(letters," \t");

  /*--------- Load the actual letters ------------*/
  vector<int>::operator=((*a)(letters2));
}

void sequence::strip_gaps() {
  vector<int> ungapped;
  ungapped.reserve(size());
  for(int i=0;i<size();i++) {
    int l = (*this)[i];
    if (l != alphabet::gap)
      ungapped.push_back(l);
  }
  vector<int>::operator=(ungapped);
}

