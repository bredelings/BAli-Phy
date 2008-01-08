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
class alphabet: public Cloneable 
{
  /// The letters of the alphabet
  std::vector<std::string> letters_;

  /// The letters of the alphabet + letter classes
  std::vector<std::string> letter_classes_;

  /// The masks for the letter_classes
  std::vector<std::vector<bool> > letter_masks_;

protected:

  /// Add a letter to the alphabet
  void insert(const std::string& l);

  /// Remove a letter from the alphabet
  void remove(const std::string& l);
  
  /// Add letters to letter classes and set masks
  virtual void setup_letter_classes();

  /// Add a letter class to the alphabet
  void insert_class(const std::string& l,const std::string&);

  /// Add a letter class to the alphabet
  void insert_class(const std::string& l,const std::vector<std::string>&);

  /// Add a letter class to the alphabet
  void insert_class(const std::string& l,const std::vector<bool>&);

  /// Add a letter class to the alphabet
  void remove_class(const std::string& l);

public:

  virtual alphabet* clone() const=0;// {return new alphabet(*this);}

  /// The name of the alphabet
  std::string name;

  std::string gap_letter;

  /// Stands for any letter: no information
  std::string wildcard;

  std::string unknown_letter;

  /// The number of real letters per letter of the alphabet
  int width() const {return letters_[0].size();}

  /// Index of a gap ('-')
  static const int gap = -1;

  /// Index of an unknown letter ('+')
  static const int not_gap = -2;

  /// Index of unknown: no information ('?' - '+' or '-')
  static const int unknown = -3;


  /// The number of letters in the alphabet
  int n_letters() const {return letters_.size();}
  /// The letters of the alphabet
  const std::vector<std::string>& letters() const {return letters_;}
  /// The i-th letter of the alphabet
  const std::string& letter(int i) const {
    assert(i>=0 and i < letters_.size()); 
    return letters_[i];
  }


  /// The number of letter classes in the alphabet
  int n_letter_classes() const {return letter_classes_.size();}
  /// The letter classes of the alphabet
  const std::vector<std::string>& letter_classes() const {return letter_classes_;}
  /// The i-th letter class of the alphabet
  const std::string& letter_class(int i) const {
    assert(i>=0 and i < letter_classes_.size()); 
    return letter_classes_[i];
  }
  /// The i-th letter mask
  const std::vector<bool>& letter_mask(int i) const {
    assert(i>=0 and i < letter_masks_.size()); 
    assert(letter_masks_.size() == letter_classes_.size());
    return letter_masks_[i];
  }
  bool matches(int i1,int i2) const {
    if (i2 == not_gap)
      return true;
    assert(0 <= i2 and i2 < letter_masks_.size());
    assert(0 <= i1 and i1 < letter_masks_[i2].size());
    return letter_masks_[i2][i1];
  }


  /// Do we contain the letter 'c'?
  bool contains(char c) const;
  /// Do we contain the letter 'c'?
  bool contains(const std::string&) const;

  /// Get the index for letter 'c'
  int find_letter(char l) const;
  /// Get the index for letter 'c'
  int find_letter(const std::string& l) const;

  /// Get the index for letter 'c'
  int find_letter_class(char l) const;
  /// Get the index for letter 'c'
  int find_letter_class(const std::string& l) const;

  /// Get the index for letter 'c'
  int operator[](char l) const;
  /// Get the index for letter 'c'
  int operator[](const std::string&) const;

  /// Translate a sequence of letters into indexes
  std::vector<int> operator()(const std::string&) const;

  /// Get the letter that corresponds to index 'i'
  std::string lookup(int i) const;

  /// How many letters in the alphabet?
  int size() const { return n_letters(); }

  /// Is index 'l' a letter?
  bool is_letter(int l) const {return l>=0 and l<n_letters();}

  /// Is index 'l' a letter or class?
  static bool is_letter_class(int l) {return l>=0;}

  /// Is index 'l' a letter, class, or wildcard?
  static bool is_feature(int l) {return l==not_gap or is_letter_class(l);}

  friend bool operator==(const alphabet&,const alphabet&);

  virtual std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=1.0) const;

  alphabet(const std::string&);

  alphabet(const std::string&,const std::string&);
  alphabet(const std::string&,const std::string&,const std::string&);

  alphabet(const std::string&, const std::vector<std::string>&);
  alphabet(const std::string&, const std::vector<std::string>&,const std::string&);

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
protected:
  AminoAcids(const std::string& s,const std::string& letters);
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
  void setup_letter_classes();
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

  void setup_table(const std::vector<std::string>&,const std::vector<std::string>&);
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
  int translate(int codon) const; 

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
