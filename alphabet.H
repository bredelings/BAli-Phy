#ifndef ALPHABET_H
#define ALPHABET_H

#include "mytypes.H"
#include "myexception.H"

struct bad_letter:public myexception {
  char c;

  bad_letter(char c)
    :myexception(string("Letter '") + c + string("' not in alphabet."))
  {}
  virtual ~bad_letter() throw() {}
};

/***************** struct alphabet ********************/
struct alphabet {
  string name;

  vector<char> data;
  vector<char> missing;
  static const int gap = -1;
  static const int not_gap = -2;

  int operator[](char c) const;
  vector<int> operator()(const string&) const;
  char lookup(int) const;
  int size() const { return data.size(); }

  static bool letter(int l) {return (l>=0);}


  friend bool operator==(const alphabet&,const alphabet&);

  alphabet(const string&,const string&);
  alphabet(const string&,const string&,const string&);
};

inline bool operator!=(const alphabet& a1,const alphabet& a2) {return not (a1==a2);}

#endif
