#ifndef ALIGNMENT_H
#define ALIGNMENT_H

#include "alphabet.H"
#include "sequence.H"
#include "sequence-format.H"
#include "tree.H"
#include <iostream>
#include "mytypes.H"
#include "clone.H"

/// A multiple alignment between a group of sequences
class alignment {
  vector<sequence> sequences;
  ublas::matrix<int> array;
  
  void clear();
  void resize(int,int);
  void pad(int);

  void add_row(const vector<int>&);

public:

  /// The order of the sequence named s
  int index(const string& s) const;

  /// The i-th sequence
  const sequence& seq(int i) const { return sequences[i];}
  /// The i-th sequence
  sequence& seq(int i) { return sequences[i];}

  /// Add sequence 's' to the alignment
  void add_sequence(const sequence& s);
  /// Add sequences called names, using a as the alphabet
  void load(const alphabet& a,const vector<string>& names, const vector<string>& sequences);
  /// Add sequences called names, using the first alphabet in alphabets that works
  void load(const vector<OwnedPointer<alphabet> >& alphabets,const vector<string>& names, const vector<string>& sequences);

  void load_sequences(const alphabet&,sequence_format::loader_t,std::istream&);
  void load_sequences(const vector<OwnedPointer<alphabet> >&,sequence_format::loader_t,std::istream&);

  /// Load PHYLIP or FASTA file 'filename', depending on extentime
  void load(const alphabet&,const std::string& filename);
  void load(const vector<OwnedPointer<alphabet> >&,const std::string& filename);

  void prepare_write(vector<string>&,vector<string>&,bool) const;
  void write_sequences(sequence_format::dumper_t,std::ostream&,bool) const;

  void print(std::ostream&) const;
  /// Print alignment in FASTA format
  void print_fasta(std::ostream&) const;
  /// Print alignment in PHYLIP format, optionally printing internal nodes
  void print_phylip(std::ostream&,bool=false) const;

  void changelength(int l);
  void delete_column(int i);

  /// The feature (letter,gap,non-gap) of sequence s in column l
  int& operator()(int l,int s) {return array(l,s); }
  /// The feature (letter,gap,non-gap) of sequence s in column l
  const int& operator()(int l,int s) const {return array(l,s); }

  /// Does sequence i have a gap at position j ?
  bool gap(int i,int j) const {return array(i,j)==alphabet::gap;}

  alignment& operator=(const alignment&);

  /// Number of leaf sequences (I think)
  int num_sequences() const { return sequences.size() ;}

  //Length of alignment
  int length() const {return array.size1();}

  /// Total number of sequences
  int size2() const {return array.size2();}

  ///Length of a particular sequence
  int seqlength(int) const;

  const alphabet& get_alphabet() const {return sequences[0].Alphabet();}

  alignment() {}
  alignment(const alphabet& a,const std::string& filename) { load(a,filename); }
  alignment(const alignment& A) {operator=(A);}
};

inline std::ostream& operator<<(std::ostream& file,const alignment& A) {
  A.print_phylip(file,true);
  return file;
}

inline std::istream& operator>>(std::istream& file,alignment& A) {
  A.load_sequences(A.get_alphabet(),sequence_format::read_fasta,file);
  return file;
}

bool valid(const alignment&);

bool all_gaps(const alignment& A,int column,const std::valarray<bool>& mask);
bool all_gaps(const alignment& A,int column);


void remove_empty_columns(alignment&);

vector<int> get_path(const alignment&,int,int);
vector<int> get_path_3way(const alignment&,int,int,int,int);

/// Generate a randomize alignment
alignment randomize(const alignment&,int n=-1);
#endif


