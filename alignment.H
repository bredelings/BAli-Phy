#ifndef ALIGNMENT_H
#define ALIGNMENT_H

#include "alphabet.H"
#include "sequence.H"
#include "tree.H"
#include <iostream>
#include "mytypes.H"

class alignment {
  vector<sequence> sequences;
  ublas::matrix<int> array;
  
  void resize(int,int);
  void pad(int);
  void remap(const vector<int>&);
  void remap(const vector<string>&);
  void gap_fixup(int,int,int);

  void add_row(const vector<int>&);
public:
  int index(const string&) const;

  const sequence& seq(int i) const { return sequences[i];}

  // Load sequences into the alignment
  void add_sequence(const sequence&);
  void load_fasta(const alphabet&,const std::string&);
  void load_fasta(const alphabet&,std::istream&);

  void load_phylip(const alphabet&,const std::string&);
  void load(const alphabet&,const std::string&);

  void create_internal(const SequenceTree&);
  void print(std::ostream&) const;
  void print_fasta(std::ostream&) const;
  void print_phylip(std::ostream&,bool=false) const;

  bool changelength(int l);
  void delete_column(int i);

  int& operator()(int l,int s) {return array(l,s); }
  const int& operator()(int l,int s) const {return array(l,s); }

  int gap(int i,int j) const {return array(i,j)==alphabet::gap;}

  alignment& operator=(const alignment&);

  //Number of leaf sequences
  int num_sequences() const { return sequences.size() ;}

  //Length of alignment
  int length() const {return array.size1();}

  //Number of total sequences
  int size2() const {return array.size2();}

  //Length of a particular sequence
  int seqlength(int) const;

  const alphabet& get_alphabet() const {return sequences[0].a;}

  alignment() {}
  alignment(const alignment& A) {operator=(A);}
};

inline std::ostream& operator<<(std::ostream& file,const alignment& A) {
  A.print_phylip(file);
  return file;
}

inline std::istream& operator>>(std::istream& file,alignment& A) {
  A.load_fasta(A.get_alphabet(),file);
  return file;
}

bool valid(const alignment&);

void remove_empty_columns(alignment&);

vector<int> get_path(const alignment&,int,int);

std::valarray<double> empirical_frequencies(const alignment& A);
#endif
