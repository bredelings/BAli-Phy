#ifndef ALPHABET_H
#define ALPHABET_H

#include <vector>
#include <valarray>
#include <iostream>
#include <string>
#include <cassert>
#include "myexception.H"
#include "clone.H"

struct bad_letter:public myexception {
  std::string letter;

  bad_letter(const std::string& l);
  bad_letter(const std::string& l,const std::string& name);

  virtual ~bad_letter() throw() {}
};

/***************** struct alphabet ********************/

/// Converts between a list of letters, and their integer indices
class alphabet: public Cloneable {

protected:
  /// The letters of the alphabet
  std::vector<std::string> data;

public:

  virtual alphabet* clone() const=0;// {return new alphabet(*this);}

  /// The name of the alphabet
  std::string name;

  /// Letters which mean "I don't know what this letter is"
  std::vector<std::string> missing;

  std::string gap_letter;

  std::string unknown_letter;

  /// The number of real letters per letter of the alphabet
  int width() const {return data[0].size();}

  /// Index of a gap ('-')
  static const int gap = -1;

  /// Index of an unknown letter ('*')
  static const int not_gap = -2;

  /// Index of unknown: no information ('?')
  static const int unknown = -3;

  /// Do we contain the letter 'c'?
  bool contains(char c) const;
  /// Do we contain the letter 'c'?
  bool contains(const std::string&) const;

  /// Get the index for letter 'c'
  int operator[](char l) const;
  /// Get the index for letter 'c'
  int operator[](const std::string&) const;

  /// Translate a sequence of letters into indexes
  std::vector<int> operator()(const std::string&) const;

  /// Get the letter that corresponds to index 'i'
  std::string lookup(int i) const;

  /// How many letters in the alphabet?
  int size() const { return data.size(); }

  /// Is index 'l' a letter?
  static bool letter(int l) {return (l>=0);}

  friend bool operator==(const alphabet&,const alphabet&);

  virtual std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=1.0) const;

  /// Add a letter to the alphabet
  void insert(const std::string& l);

  /// Remove a letter from the alphabet
  void remove(const std::string& l);

  alphabet(const std::string&,const std::string&);
  alphabet(const std::string&,const std::string&,const std::string&);

  alphabet(const std::string&, const std::vector<std::string>&);
  alphabet(const std::string&, const std::vector<std::string>&,const std::vector<std::string>&);

  virtual ~alphabet() {};
};

/// An alphabet of nucleotides
class Nucleotides: public alphabet {
public:
  virtual Nucleotides* clone() const=0;

  /// Is the letter a purine?
  bool purine(int i) const {return i==0 or i==1;}

  /// Is the letter a pyrimadine?
  bool pyrimidine(int i) const {return i==2 or i==3;}

  /// Is i -> j a transition?
  bool transition(int i, int j) const {return (purine(i) and purine(j))
				   or (pyrimidine(i) and pyrimidine(j)) ;}

  /// Is i -> j a transversion?
  bool transversion(int i,int j) const {return not transition(i,j);}

  /// Get the index of A
  int A() const {return 0;}
  /// Get the index of G
  int G() const {return 1;}
  /// Get the index of T (or U)
  int T() const {return 2;}
  /// Get the index of C
  int C() const {return 3;}
  
  Nucleotides(const std::string& s,char c);
  Nucleotides(const std::string& s,char c,const std::string& m);

  ~Nucleotides() {};
};


/// The DNA alphabet
class DNA: public Nucleotides {
public:
  virtual DNA* clone() const {return new DNA(*this);}

  DNA();
  ~DNA() {};
};

/// The RNA alphabet
class RNA: public Nucleotides {
public:
  virtual RNA* clone() const {return new RNA(*this);}

  RNA();
  ~RNA() {};
};

/// An Amino Acid alphabet
class AminoAcids: public alphabet {
public:
  virtual AminoAcids* clone() const {return new AminoAcids(*this);}

  AminoAcids();
};

/// An Amino Acid alphabet that includes a "stop" amino acid
class AminoAcidsWithStop: public AminoAcids {
public:
  AminoAcidsWithStop();
};


/// An alphabet of triplets of nucleotides
class Triplets: public alphabet {
protected:
  OwnedPointer<Nucleotides> N;

  std::vector<std::vector<int> > sub_nuc_table;

  void setup_sub_nuc_table();
public:
  virtual Triplets* clone() const {return new Triplets(*this);}

  /// The alphabet of nucleotides that we construct triplets from
  const Nucleotides& getNucleotides() const {return *N;}

  /// The alphabet of nucleotides that we construct triplets from
  int sub_nuc(int codon,int pos) const;

  std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=1.0) const;

  Triplets(const Nucleotides& N);
  Triplets(const std::string& s,const Nucleotides& N);
};


/// A Triplets alphabet with a mapping to an AminoAcids alphabet
class Codons: public Triplets {
protected:
  OwnedPointer<AminoAcids> A;
  std::vector<int> translation_table;

  void setup_table(std::vector<std::string>,std::vector<std::string>);
  void setup_table(std::istream&);
  void setup_table(const std::string&);
public:
  virtual Codons* clone() const {return new Codons(*this);}

  /// Is letter codon a stop codon?
  bool stop_codon(int codon) const {
    assert(0 <= codon && codon <translation_table.size() );
    int aa = translation_table[codon];
    return A->lookup(aa) == "!";
  }

  /// What amino acid does codon map to?
  int translate(int codon) const {
    if (codon == alphabet::gap or codon==alphabet::not_gap)
      return codon;
    assert(codon >= 0 and codon < translation_table.size() );
    return translation_table[codon];
  }

  /// What AminoAcids alphabet are we using?
  const AminoAcids& getAminoAcids() const {return *A;}

  /// Construct a Codons alphabet from Nucleotides, AminoAcids, and a correspondance table.
  Codons(const Nucleotides& N1,const AminoAcids& A1,std::vector<std::string>,std::vector<std::string>);

  /// Construct a Codons alphabet from Nucleotides, AminoAcids, and a file handle.
  Codons(const Nucleotides& N1,const AminoAcids& A1,std::istream&);

  /// Construct a Codons alphabet from Nucleotides, AminoAcids, and a filename.
  Codons(const Nucleotides& N1,const AminoAcids& A1,const std::string&);
};


//------------------------------- Codons ------------------------------//


inline bool operator!=(const alphabet& a1,const alphabet& a2) {return not (a1==a2);}

std::valarray<double> get_nucleotide_counts_from_codon_counts(const Triplets& C,const std::valarray<double>& C_counts);

std::valarray<double> get_codon_frequencies_from_independant_nucleotide_frequencies(const Triplets& C,const std::valarray<double>& fN );

#endif
