#ifndef SEQUENCE_H
#define SEQUENCE_H

#include "mytypes.H"
#include <valarray>

/***************** struct alphabet ********************/
struct alphabet {

  vector<char> data;
  static const int gap = -1;
  static const int not_gap = -2;

  std::valarray<double> frequency;
  Matrix substitution;

  int operator[](char c) const;
  vector<int> operator()(const string&) const;
  char lookup(int) const;
  int size() const { return data.size(); }

  alphabet(const char*,const char*);
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
