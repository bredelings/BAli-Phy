#ifndef SEQUENCE_H
#define SEQUENCE_H

#include "mytypes.H"
#include <valarray>
#include "myexception.H"

/***************** struct alphabet ********************/
struct bad_letter:public myexception {
  char c;

  bad_letter(char c)
    :myexception(string("Letter '") + c + string("' not in alphabet."))
  {}

  virtual ~bad_letter() throw() {}
};

struct alphabet {

  vector<char> data;
  static const int gap = -1;
  static const int not_gap = -2;

  int operator[](char c) const;
  vector<int> operator()(const string&) const;
  char lookup(int) const;
  int size() const { return data.size(); }

  static bool letter(int l) {return (l>=0);}
  alphabet(const char*);
};

/***************** struct sequence ********************/
struct sequence:vector<int> {
  const alphabet* a;
  string name;

  void load(const char*);
  sequence(const alphabet& a1): a(&a1) {}
  sequence(const alphabet& a1,const char* s): a(&a1),name(s) {}
};

#endif
