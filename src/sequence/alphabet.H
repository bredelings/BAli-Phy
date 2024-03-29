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

#include <memory>
#include <vector>
#include <valarray>
#include <iostream>
#include <string>
#include <filesystem>

#include "util/assert.hh"
#include <boost/dynamic_bitset.hpp>
#include "util/myexception.H"
#include "util/owned-ptr.H"

/// An exception that is thrown upon reading a letter that is not in the current alphabet.
struct bad_letter:public myexception 
{
    /// The letter that was read
    std::string letter;

    /// Event: the letter "l" was not in the alphabet
    bad_letter(const std::string& l);
    /// Event: the letter "l" was not in alphabet "name"
    bad_letter(const std::string& l,const std::string& name);

    virtual ~bad_letter() {}
};

/***************** struct alphabet ********************/

/// Converts between a list of letters, and their integer indices
class alphabet
{
public:
    typedef boost::dynamic_bitset<> bitmask_t;
    typedef std::vector<double> fmask_t;

private:

    /// The letters of the alphabet
    std::vector<std::string> letters_;

    /// The letters of the alphabet + letter classes
    std::vector<std::string> letter_classes_;

    /// The masks for the letter_classes
    std::vector<bitmask_t> letter_masks_;

    /// The 0.0 / 1.0 masks for the letter_classes
    std::vector<fmask_t> letter_fmasks_;

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
    std::string gap_letter = "-";

    /// The string for a not_gap (often 'N' or 'X')
    std::string wildcard = "+";

    /// The string for a unknown (often '?')
    std::vector<std::string> unknown_letters = {"?","="};

    const std::string& unknown_letter() const {return unknown_letters[0];}

    virtual std::string letter_name() const {return "letter";};
    virtual std::string letters_name() const {return letter_name()+"s";};

    /// The number of singlet letters per alphabet letter.
    int width() const {return letters_[0].size();}

    /// Index of a gap ('-')
    static constexpr int gap = -1;

    /// Index of an letter whose value is missing ('N' or 'X')
    static constexpr int not_gap = -2;

    /// Index of unknown ('?'): an ambiguous 'gap or not_gap' symbol.
    static constexpr int unknown = -3;

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

    /// The i-th letter mask
    const fmask_t& letter_fmask(int i) const {
	assert(i>=0 and i < letter_fmasks_.size()); 
	assert(letter_fmasks_.size() == letter_classes_.size());
	return letter_fmasks_[i];
    }

    /// Returns true if the letter i1 is part of the letter class i2
    bool matches(int i1,int i2) const {
	if (i2 == not_gap)
	    return true;
	assert(0 <= i2 and i2 < letter_masks_.size());
	assert(0 <= i1 and i1 < letter_masks_[i2].size());
	return letter_masks_[i2][i1];
    }

    bool consistent(int i1, int i2) const;

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
    virtual std::vector<int> operator()(const std::string&) const;

    /// Get the letter that corresponds to index 'i'
    std::string lookup(int i) const;

    /// How many letters in the alphabet?
    int size() const { return n_letters(); }

    /// Is index 'l' a letter?
    bool is_letter(int l) const {return l>=0 and l<n_letters();}

    /// Is index 'l' a letter or class?
    static constexpr bool is_letter_class(int l) {return l>=0;}

    /// Is index 'l' a letter, class, or wildcard?
    static constexpr bool is_feature(int l) {return l==not_gap or is_letter_class(l);}

    /// Compare two alphabets
    friend bool operator==(const alphabet&,const alphabet&);

    /// Estimate frequencies from counts in a way that uses alphabet-dependant pseudocounts
    virtual std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=1.0) const;

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

class Numeric: public alphabet {
public:
    virtual Numeric* clone() const {return new Numeric(*this);};
    Numeric(int n);
    Numeric(const std::string& name, int n);
};

/// An alphabet of nucleotides
class Nucleotides: public alphabet {
public:
    virtual Nucleotides* clone() const=0;

    virtual std::string letter_name() const {return "nucleotide";};

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
    /// Get the index of C
    int C() const {return 1;}
    /// Get the index of G
    int G() const {return 2;}
    /// Get the index of T (or U)
    int T() const {return 3;}

    bool is_watson_crick(int i, int j) const;
    bool is_mismatch(int i, int j) const;
    bool is_wobble_pair(int i, int j) const;

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
    virtual std::string letter_name() const {return "amino acid";};

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

std::shared_ptr<const alphabet> get_alphabet(const std::string& name);
std::shared_ptr<const Nucleotides> get_nucleotides(const std::string& name);

#endif
