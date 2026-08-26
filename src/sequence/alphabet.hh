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
 * @file alphabet.hh
 */

#ifndef ALPHABET_H
#define ALPHABET_H

#include <memory>
#include <optional>
#include <vector>
#include <valarray>
#include <iostream>
#include <string>
#include <filesystem>
#include <utility>

#include "util/assert.hh"
#include <boost/dynamic_bitset.hpp>
#include "util/myexception.hh"
#include "util/owned-ptr.hh"

/***************** struct alphabet ********************/

/// Converts between a list of letters, and their integer indices
class alphabet
{
public:
    typedef boost::dynamic_bitset<> bitmask_t;

private:

    /// The letters of the alphabet
    std::vector<std::string> letters_;

protected:

    /// Add a letter to the alphabet
    void insert(const std::string& l);

    /// Remove a letter from the alphabet
    void remove(int);
  
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

    /// First integer used for an ambiguity in a character-data-local database.
    static constexpr int first_ambiguity = -4;

    /// The number of letters in the alphabet
    int n_letters() const {return letters_.size();}
    /// The letters of the alphabet
    const std::vector<std::string>& letters() const {return letters_;}
    /// The i-th letter of the alphabet
    const std::string& letter(int i) const {
	assert(i>=0 and i < letters_.size()); 
	return letters_[i];
    }

    /// Do we contain the letter 'c'?
    bool contains(char c) const;
    /// Do we contain the letter 'c'?
    bool contains(const std::string&) const;

    /// Return the exact index for letter 'c', or nothing when it is absent.
    std::optional<int> find_letter(char l) const;
    /// Return the exact index for letter 'c', or nothing when it is absent.
    std::optional<int> find_letter(const std::string& l) const;

    /// Get the letter that corresponds to index 'i'
    std::string lookup(int i) const;

    /// Return the non-gap state set denoted by a symbol, or nothing if it is not recognized.
    virtual std::optional<bitmask_t> mask_for_symbol(const std::string& symbol) const;

    /// Spell a state set and report whether the spelling represents exactly that set.
    virtual std::pair<std::string, bool> lookup(const bitmask_t& mask) const;

    /// Compatibility error-path hook for diagnostics formerly emitted by product parsers.
    /// Remove once the shared encoder reports equivalent product-structure errors directly.
    virtual void validate_sequence(const std::string& sequence) const;

    /// How many letters in the alphabet?
    int size() const { return n_letters(); }

    /// Is index 'l' a letter?
    bool is_letter(int l) const {return l>=0 and l<n_letters();}

    /// Is index 'l' an ambiguity owned by the character data?
    static constexpr bool is_ambiguity(int l) {return l<=first_ambiguity;}

    /// Does 'l' represent a present character, including unconstrained non-gap?
    static constexpr bool is_character(int l) {return l>=0 or l==not_gap or is_ambiguity(l);}

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
    using alphabet::lookup;

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

    std::optional<bitmask_t> mask_for_symbol(const std::string& symbol) const override;

    std::pair<std::string, bool> lookup(const bitmask_t& mask) const override;
  
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
    using alphabet::lookup;

    virtual std::string letter_name() const {return "amino acid";};

    virtual AminoAcids* clone() const {return new AminoAcids(*this);}

    bool is_stop(int i) const;

    std::optional<bitmask_t> mask_for_symbol(const std::string& symbol) const override;

    std::pair<std::string, bool> lookup(const bitmask_t& mask) const override;

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
