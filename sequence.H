#ifndef SEQUENCE_H
#define SEQUENCE_H

#include "mytypes.H"
#include "alphabet.H"
#include "clone.H"

/***************** struct sequence ********************/
class sequence: public vector<int> {
  OwnedPointer<alphabet> a;
public:

  const alphabet& Alphabet() const {return *a;}

  string name;
  string comment;

  void parse(const string&,const string&);
  void read(const string&);

  void strip_gaps();

  sequence(const alphabet& a1): a(a1) {}
  //  sequence(const alphabet& a1,const string& filename): a(a1) {read(filename);}
  sequence(const alphabet& a1,const string& n,const string& c): a(a1),name(n),comment(c) {}
};

#endif
