#ifndef ALPHABET_H
#define ALPHABET_H

#include "mytypes.H"
#include "myexception.H"

struct bad_letter:public myexception {
  char c;

  bad_letter(char c);
  bad_letter(char c,string name);

  virtual ~bad_letter() throw() {}
};

/***************** struct alphabet ********************/

/// Converts between a list of letters, and their integer indices
struct alphabet {

  /// The name of the alphabet
  string name;

  /// The letters of the alphabet
  vector<char> data;

  /// Letters which mean "I don't know what this letter is"
  vector<char> missing;

  /// Index of a gap ('-')
  static const int gap = -1;

  /// Index of an unknown letter ('*')
  static const int not_gap = -2;

  /// Get the index for letter 'c'
  int operator[](char c) const;

  /// Translate a sequence of letters into indexes
  vector<int> operator()(const string&) const;

  /// Get the letter that corresponds to index 'i'
  char lookup(int i) const;

  /// How many letters in the alphabet?
  int size() const { return data.size(); }

  /// Is index 'l' a letter?
  static bool letter(int l) {return (l>=0);}

  friend bool operator==(const alphabet&,const alphabet&);

  alphabet(const string&,const string&);
  alphabet(const string&,const string&,const string&);
};

inline bool operator!=(const alphabet& a1,const alphabet& a2) {return not (a1==a2);}

#endif
