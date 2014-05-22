/*
   Copyright (C) 2004-2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

/**
 * @file alphabet.H
 */

#ifndef ALPHABET_H
#define ALPHABET_H

#include <vector>
#include <valarray>
#include <iostream>
#include <string>
#include <cassert>
#include <boost/dynamic_bitset.hpp>
#include "myexception.H"
#include "object.H"
#include "owned-ptr.H"

/// An exception that is thrown upon reading a letter that is not in the current alphabet.
struct bad_letter:public myexception 
{
  /// The letter that was read
  std::string letter;

  /// Event: the letter "l" was not in the alphabet
  bad_letter(const std::string& l);
  /// Event: the letter "l" was not in alphabet "name"
  bad_letter(const std::string& l,const std::string& name);

  virtual ~bad_letter() throw() {}
};

/***************** struct alphabet ********************/

/// Converts between a list of letters, and their integer indices
class alphabet: public Object
{
public:
  typedef boost::dynamic_bitset<> bitmask_t;

private:

  /// The letters of the alphabet
  std::vector<std::string> letters_;

  /// The letters of the alphabet + letter classes
  std::vector<std::string> letter_classes_;

  /// The masks for the letter_classes
  std::vector<bitmask_t> letter_masks_;

protected:

  /// Add a letter to the alphabet
  void insert(const std::string& l);

  /// Remove a letter from the alphabet
  void remove(const std::string& l);
  
  /// Remove a letter from the alphabet
  void remove(int);
  
  /// Add letters to letter classes and set masks
  virtual void setup_letter_classes();

  /// Add a letter class to the alphabet
  void insert_class(const std::string& l,const std::string&);

  /// Add a letter class to the alphabet
  void insert_class(const std::string& l,const std::vector<std::string>&);

  /// Add a letter class to the alphabet
  void insert_class(const std::string& l, const bitmask_t&);

  /// Add a letter class to the alphabet
  void remove_class(const std::string& l);

public:

  virtual alphabet* clone() const=0;// {return new alphabet(*this);}

  /// The name of the alphabet
  std::string name;

  /// The string for a gap (often '-')
  std::string gap_letter;

  /// The string for a not_gap (often 'N' or 'X')
  std::string wildcard;

  /// The string for a unknown (often '?')
  std::string unknown_letter;

  /// The number of singlet letters per alphabet letter.
  int width() const {return letters_[0].size();}

  /// Index of a gap ('-')
  static const int gap = -1;

  /// Index of an letter whose value is missing ('N' or 'X')
  static const int not_gap = -2;

  /// Index of unknown ('?'): an ambiguous 'gap or not_gap' symbol.
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
  const bitmask_t& letter_mask(int i) const {
    assert(i>=0 and i < letter_masks_.size()); 
    assert(letter_masks_.size() == letter_classes_.size());
    return letter_masks_[i];
  }

  /// Returns true if the letter i1 is part of the letter class i2
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

  /// Compare two alphabets
  friend bool operator==(const alphabet&,const alphabet&);

  /// Estimate frequencies from counts in a way that uses alphabet-dependant pseudocounts
  virtual std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=1.0) const;

  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    const alphabet* a = dynamic_cast<const alphabet*>(&O);

    return (*this) == *a;
  }

  std::string print () const;

  /// Construct an alphabet with the given name
  alphabet(const std::string& name);

  /// Construct an alphabet with name 'n', letters 'l'
  alphabet(const std::string& n,const std::string& l);
  /// Construct an alphabet with name 'n', letters 'l'
  alphabet(const std::string& n, const std::vector<std::string>& l);

  /// Construct an alphabet with name 'n', letters 'l', and wildcard 'm'
  alphabet(const std::string& n,const std::string& l, const std::string& m);
  /// Construct an alphabet with name 'n', letters 'l', and wildcard 'm'
  alphabet(const std::string& n, const std::vector<std::string>& l,const std::string& m);

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

  int complement(int l) const;
  
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

  bool is_stop(int i) const;

  AminoAcids();
};

/// An Amino Acid alphabet that includes a "stop" amino acid
class AminoAcidsWithStop: public AminoAcids {
public:
  virtual AminoAcidsWithStop* clone() const {return new AminoAcidsWithStop(*this);}

  AminoAcidsWithStop();
};

/// An alphabet of triplets of nucleotides
class Triplets: public alphabet {
protected:
  owned_ptr<Nucleotides> N;

  /// (codon,position) -> nucleotide
  std::vector<std::vector<int> > sub_nuc_table;
  /// (n1,n2,n3) -> codon
  std::vector<std::vector<std::vector<int> > > codon_table;

  void setup_sub_nuc_table();
  void setup_letter_classes();
public:
  virtual Triplets* clone() const {return new Triplets(*this);}

  /// The alphabet of nucleotides that we construct triplets from
  const Nucleotides& getNucleotides() const {return *N;}

  /// The alphabet of nucleotides that we construct triplets from
  int sub_nuc(int codon,int pos) const;

  /// Find the triplet made of nucleotides n1, n2, and n3
  int get_triplet(int n1, int n2, int n3) const;

  std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=1.0) const;

  Triplets(const Nucleotides& N);
  Triplets(const std::string& s,const Nucleotides& N);
};

/// A Class that maps 3 nucleotides to an outcome in the amino_acid+stop alphabet
class Genetic_Code: public Object
{
  std::string name_;

  RNA rna;
  DNA dna;
  AminoAcidsWithStop A;

  std::vector< std::vector< std::vector<int> > > translation_table;

  void add_entry(char c1, char c2, char c3, char aa);

protected:
  void setup_table(const std::string&, const std::string&, const std::string&, const std::string&);

  void setup_table(const std::string&);

  void setup_table(std::istream&);

  void setup_table_from_file(const std::string& filename);

public:
  Genetic_Code* clone() const=0;

  const std::string& name() const {return name_;}

  const RNA& get_RNA() const {return rna;}
  const DNA& get_DNA() const {return dna;}

  const AminoAcidsWithStop& get_amino_acids() const {return A;}

  /// Find out which amino acid (or stop codon) the letter maps to.
  int translate(int,int,int) const;

  /// Is letter codon a stop codon?
  bool is_stop_codon(int, int, int) const;

  Genetic_Code(const std::string& name);

  Genetic_Code(const std::string& name, std::istream&);

  Genetic_Code(const std::string& name, const std::string& filename);
};


class Standard_Genetic_Code: public Genetic_Code
{
public:
  Standard_Genetic_Code* clone() const {return new Standard_Genetic_Code(*this);}

  Standard_Genetic_Code();
};

class Mt_Invertebrate_Genetic_Code: public Genetic_Code
{
public:
  Mt_Invertebrate_Genetic_Code* clone() const {return new Mt_Invertebrate_Genetic_Code(*this);}

  Mt_Invertebrate_Genetic_Code();
};

class Mt_Vertebrate_Genetic_Code: public Genetic_Code
{
public:
  Mt_Vertebrate_Genetic_Code* clone() const {return new Mt_Vertebrate_Genetic_Code(*this);}

  Mt_Vertebrate_Genetic_Code();
};

class Mt_Protozoan_Genetic_Code: public Genetic_Code
{
public:
  Mt_Protozoan_Genetic_Code* clone() const {return new Mt_Protozoan_Genetic_Code(*this);}

  Mt_Protozoan_Genetic_Code();
};

class Mt_Yeast_Genetic_Code: public Genetic_Code
{
public:
  Mt_Yeast_Genetic_Code* clone() const {return new Mt_Yeast_Genetic_Code(*this);}

  Mt_Yeast_Genetic_Code();
};

class Mt_Echinoderm_Genetic_Code: public Genetic_Code
{
public:
  Mt_Echinoderm_Genetic_Code* clone() const {return new Mt_Echinoderm_Genetic_Code(*this);}

  Mt_Echinoderm_Genetic_Code();
};

/// A Triplets alphabet with a mapping to an AminoAcids alphabet
class Codons: public Triplets {
protected:
  owned_ptr<AminoAcids> A;
  owned_ptr<Genetic_Code> G;
  std::vector<int> translation_table;

  void setup_table();

public:
  virtual Codons* clone() const {return new Codons(*this);}

  /// Is letter codon a stop codon?
  bool stop_codon(int codon) const {
    assert(0 <= codon && codon <translation_table.size() );
    int aa = translation_table[codon];
    return A->is_stop(aa);
  }

  /// What amino acid does codon map to?
  int translate(int codon) const; 

  /// What AminoAcids alphabet are we using?
  const AminoAcids& getAminoAcids() const {return *A;}

  const Genetic_Code& getGenetic_Code() const {return *G;}

  /// Construct a Codons alphabet from Nucleotides, AminoAcids, and a Genetic Code
  Codons(const Nucleotides& N1,const AminoAcids& A1, const Genetic_Code& G);
};


//------------------------------- Codons ------------------------------//


/// Check if two alphabets are unequal
inline bool operator!=(const alphabet& a1,const alphabet& a2) {return not (a1==a2);}

/// Compute nucleotide counts from codon counts
std::valarray<double> get_nucleotide_counts_from_codon_counts(const Triplets& C,const std::valarray<double>& C_counts);

/// Compute codon frequences from nucleotide frequencies if the nucleotide positions in the codon are independent
std::valarray<double> get_codon_frequencies_from_independent_nucleotide_frequencies(const Triplets& C,const std::valarray<double>& fN );

std::vector<object_ptr<const alphabet> > load_alphabets();

std::vector<object_ptr<const alphabet> > load_alphabets(const std::string&);

object_ptr<const Genetic_Code> get_genetic_code(const std::string& name);
#endif
