#ifndef ALIGNMENT_H
#define ALIGNMENT_H

#include "alphabet.H"
#include "sequence.H"
#include "sequence-format.H"
#include <iostream>
#include <vector>
#include "mytypes.H"
#include "clone.H"

/// A multiple alignment between a group of sequences
class alignment {
  std::vector<sequence> sequences;
  ublas::matrix<int> array;
  
  void clear();
  void pad(int);

  void add_row(const std::vector<int>&);

  OwnedPointer<alphabet> a;

  mutable std::vector<ublas::matrix<int> > notes;
  
public:

  friend alignment blank_copy(const alignment&,int length);

  int n_notes() const {return notes.size();}

  int add_note(int l) const;

  ublas::matrix<int>& note(int n) const {return notes[n];}

  int& note(int n,int i,int j) const {return notes[n](i,j);}

  /// The order of the sequence named s
  int index(const string& s) const;

  /// The i-th sequence
  const sequence& seq(int i) const { return sequences[i];}
  /// The i-th sequence
  sequence& seq(int i) { return sequences[i];}

  /// Remove sequence 'i' from the alignment
  void del_sequence(int);
  /// Add sequence 's' to the alignment
  void add_sequence(const sequence& s);

  /// Add sequences called names, using a as the alphabet
  void load(const std::vector<sequence>& sequences);
  /// Add sequences called names, using the first alphabet in alphabets that works
  void load(const std::vector<OwnedPointer<alphabet> >& alphabets,const std::vector<sequence>& sequences);

  void load(sequence_format::loader_t,std::istream&);
  void load(const std::vector<OwnedPointer<alphabet> >&,sequence_format::loader_t,std::istream&);

  /// Load PHYLIP or FASTA file 'filename', depending on extention
  void load(const std::string& filename);
  void load(const std::vector<OwnedPointer<alphabet> >&,const std::string& filename);

  std::vector<sequence> get_sequences() const;

  void write_sequences(sequence_format::dumper_t,std::ostream&) const;

  void print(std::ostream&) const;
  /// Print alignment in FASTA format
  void print_fasta(std::ostream&) const;
  /// Print alignment in PHYLIP format
  void print_phylip(std::ostream&) const;

  void changelength(int l);
  void delete_column(int i);

  /// The feature (letter,gap,non-gap) of sequence s in column l
  int& operator()(int l,int s) {return array(l,s); }
  /// The feature (letter,gap,non-gap) of sequence s in column l
  const int& operator()(int l,int s) const {return array(l,s); }

  /// Does sequence i have a gap at position j ?
  bool gap(int i,int j) const {return array(i,j)==alphabet::gap;}
  bool unknown(int i,int j) const {return array(i,j)==alphabet::unknown;}
  bool character(int i,int j) const {return not gap(i,j) and not unknown(i,j);}

  alignment& operator=(const alignment&);

  /// Number of columns
  int length() const {return array.size1();}

  /// Total number of sequences
  int n_sequences() const {assert(array.size2() == sequences.size()); return array.size2();}

  //FIXME!  I should cache the results of this
  ///Length of a particular sequence
  int seqlength(int) const;

  const alphabet& get_alphabet() const {return *a;}

  alignment() {}

  explicit alignment(const alphabet&);

  explicit alignment(const alphabet&,const std::string&);
};

std::ostream& operator<<(std::ostream& file,const alignment& A);

std::istream& operator>>(std::istream& file,alignment& A);

/// Does the alignment contain any empty columns?
bool valid(const alignment&);

/// Does the column contain only gaps for taxa in mask?
bool all_gaps(const alignment& A,int column,const std::valarray<bool>& mask);

/// Does the column contain only gaps?
bool all_gaps(const alignment& A,int column);

/// remove columns that contain only gaps
void remove_empty_columns(alignment&);

std::vector<int> get_path(const alignment&,int,int);
std::vector<int> get_path_3way(const alignment&,int,int,int,int);

std::vector<string> sequence_names(const alignment&);
std::vector<string> sequence_names(const alignment&,int);

/// Generate a randomized alignment
alignment randomize(const alignment&,int n=-1);
#endif


