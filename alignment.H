#ifndef ALIGNMENT_H
#define ALIGNMENT_H

#include "mytypes.H"
#include "sequence.H"
#include "tree.H"
#include <iostream>

class alignment {
  vector<sequence> sequences;
  ublas::matrix<int> array;
  
  void resize(int,int);
  void pad(int);
  void remap(const vector<string>&);
public:
  const sequence& seq(int i) const { return sequences[i];}

  void add_sequence(const sequence&);
  void add_sequence(const alphabet& a,const string&, const string&);
  void add_sequence(const vector<int>&);
  void set_sequence(int,const vector<int>&);

  void load(std::istream&);
  void load_fasta(const alphabet&,std::istream&);
  void create_internal(const SequenceTree&);
  void print(std::ostream&) const;
  void print_fasta(std::ostream&) const;
  void print_phy(std::ostream&) const;


  bool changelength(int l);
  void delete_column(int i);

  int& operator()(int l,int s) {return array(l,s); }
  const int& operator()(int l,int s) const {return array(l,s); }

  alignment& operator=(const alignment&);

  int num_sequences() const { return sequences.size() ;}
  int size2() const {return array.size2();}
  int length() const {return array.size1();}

  const alphabet& get_alphabet() const {return *sequences[0].a;}

  alignment() {}
  alignment(const alignment& A) {operator=(A);}
};

inline std::ostream& operator<<(std::ostream& file,const alignment& A) {
  A.print(file);
  return file;
}

inline std::istream& operator>>(std::istream& file,alignment& A) {
  A.load(file);
  return file;
}

bool valid(const alignment& A);

#endif
