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


  friend bool operator==(const alphabet&,const alphabet&);

  alphabet(const char*);
};

inline bool operator!=(const alphabet& a1,const alphabet& a2) {return not (a1==a2);}

/***************** struct sequence ********************/
struct sequence: public vector<int> {
  alphabet a;
  string name;
  string comment;

  void parse(const string&,const string&);
  void read(const string&);

  void strip_gaps();

  sequence(const alphabet& a1): a(a1) {}
  sequence(const alphabet& a1,const string& filename): a(a1) {read(filename);}
  sequence(const alphabet& a1,const string& n,const string& c): a(a1),name(n),comment(c) {}
};

vector<sequence> load_fasta(const alphabet&,const std::string&);
vector<sequence> load_fasta(const alphabet&,std::istream& file);

#endif
