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

  std::string gap_letter;

public:

  virtual alphabet* clone() const=0;// {return new alphabet(*this);}

  /// The name of the alphabet
  std::string name;

  /// Letters which mean "I don't know what this letter is"
  std::vector<std::string> missing;

  /// Index of a gap ('-')
  static const int gap = -1;

  /// Index of an unknown letter ('*')
  static const int not_gap = -2;

  /// Do we contain the letter 'c'
  bool contains(char c) const;
  bool contains(const std::string&) const;

  /// Get the index for letter 'c'
  int operator[](char l) const;
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

  virtual std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=10.0) const;

  void insert(const std::string& l);
  void remove(const std::string& l);

  alphabet(const std::string&,const std::string&);
  alphabet(const std::string&,const std::string&,const std::string&);

  alphabet(const std::string&, const std::vector<std::string>&);
  alphabet(const std::string&, const std::vector<std::string>&,const std::vector<std::string>&);

  virtual ~alphabet() {};
};

class Nucleotides: public alphabet {
public:
  virtual Nucleotides* clone() const=0;

  bool purine(int i) const {return i==0 or i==1;}
  bool pyrimidine(int i) const {return i==2 or i==3;}

  bool transition(int i, int j) const {return (purine(i) and purine(j))
				   or (pyrimidine(i) and pyrimidine(j)) ;}
  bool transversion(int i,int j) const {return not transition(i,j);}

  int A() const {return 0;}
  int G() const {return 1;}
  int T() const {return 2;}
  int C() const {return 3;}
  
  virtual std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=10.0) const;

  Nucleotides(const std::string& s,char c);
  Nucleotides(const std::string& s,char c,const std::string& m);

  ~Nucleotides() {};
};


//--------------------------------- DNA -------------------------------//
class DNA: public Nucleotides {
public:
  virtual DNA* clone() const {return new DNA(*this);}

  DNA();
  ~DNA() {};
};

//--------------------------------- RNA -------------------------------//
class RNA: public Nucleotides {
public:
  virtual RNA* clone() const {return new RNA(*this);}

  RNA();
  ~RNA() {};
};

//----------------------------- Amino Acids----------------------------//
class AminoAcids: public alphabet {
public:
  virtual AminoAcids* clone() const {return new AminoAcids(*this);}

  AminoAcids();
};

class AminoAcidsWithStop: public AminoAcids {
public:
  AminoAcidsWithStop();
};


//------------------------------- Codons ------------------------------//
class Codons: public alphabet {
  OwnedPointer<Nucleotides> N;
public:
  virtual Codons* clone() const {return new Codons(*this);}

  const Nucleotides& getNucleotides() const {return *N;}

  int sub_nuc(int codon,int pos) const;

  std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=10.0) const;

  Codons(const Nucleotides& N);
  Codons(const std::string& s,const Nucleotides& N);
};


//------------------------------- Codons ------------------------------//

class Translation_Table {
  OwnedPointer<Codons> C;
  OwnedPointer<AminoAcids> A;
  std::vector<int> table;

  void setup_table(std::vector<std::string>,std::vector<std::string>);
  void setup_table(std::istream&);
public:

  bool stop_codon(int codon) const {
    assert(0 <= codon && codon < C->size() );
    int aa = table[codon];
    return A->lookup(aa) == "!";
  }

  int operator[](int codon) const {
    if (codon == alphabet::gap or codon==alphabet::not_gap)
      return codon;
    assert(codon >= 0 and codon < table.size() );
    return table[codon];
  }

  const Nucleotides& getNucleotides() const {return getCodons().getNucleotides();}
  const Codons& getCodons() const {return *C;}
  const AminoAcids& getAminoAcids() const {return *A;}

  Translation_Table(const Codons& N1,const AminoAcids& A1,
		    const std::vector<std::string>&,const std::vector<std::string>&);

  Translation_Table(const Codons& N1,const AminoAcids& A1,
		    std::istream&);

  Translation_Table(const Codons& N1,const AminoAcids& A1,
		    const std::string&);

};

inline bool operator!=(const alphabet& a1,const alphabet& a2) {return not (a1==a2);}

std::valarray<double> get_nucleotide_counts_from_codon_counts(const Codons& C,const std::valarray<double>& C_counts);

std::valarray<double> get_codon_frequencies_from_independant_nucleotide_frequencies(const Codons& C,const std::valarray<double>& fN );

#endif
