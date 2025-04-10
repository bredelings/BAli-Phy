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

#include "alphabet.H"
#include "util/assert.hh"
#include <fstream>
#include "util/set.H"
#include "util/io.H"
#include "util/string/convert.H"
#include "util/cmdline.H"
#include "util/string/sanitize.H" // for sanitize_string( )

using std::vector;
using std::string;
using std::valarray;
using std::istream;
using std::shared_ptr;

bad_letter::bad_letter(const string& l)
    :myexception(string("Letter '") + sanitize_string(l) + string("' not in alphabet.")),letter(l)
{}

bad_letter::bad_letter(const string& l,const string& name)
    :myexception(string("Letter '") + sanitize_string(l) + string("' not in alphabet '") + name + "'."),letter(l)
{}

// Legally, we have to define this to give them a location.
// This only triggers an error in g++ if they aren't inlined.
const int alphabet::gap;
const int alphabet::not_gap;
const int alphabet::unknown;

bool alphabet::consistent(int i1, int i2) const
{
    if (i1 == i2) return true;

    if (i1 == alphabet::unknown or i2 == alphabet::unknown) return true;

    if (i1 == alphabet::gap or i2 == alphabet::gap) return false;

    if (i1 == alphabet::not_gap or i2 == alphabet::not_gap) return true;

    if (i1 == alphabet::not_gap and is_feature(i2)) return true;
    if (i2 == alphabet::not_gap and is_feature(i1)) return true;

    assert(is_letter_class(i1) and is_letter_class(i2));

    if (is_letter(i1) and is_letter(i2)) return false;

    if (is_letter(i1))
	return letter_masks_[i2][i1];

    if (is_letter(i2))
	return letter_masks_[i1][i2];

    return (letter_masks_[i1] & letter_masks_[i2]).any();
}


bool alphabet::contains(char l) const {
    string s(1U,l);
    return contains(s);
}

bool alphabet::contains(const std::string& l) const {
    return includes(letters_,l);
}

int alphabet::find_letter(char l) const {
    string s(1U,l);  
    return find_letter(s);
}

int alphabet::find_letter(const string& l) const {
    // Check the letters
    for(int i=0;i<size();i++) {
	if (letter(i)==l)
	    return i;
    }
    throw myexception()<<"Alphabet '"<<name<<"' doesn't contain letter '"<<sanitize_string(l)<<"'";
}


int alphabet::find_letter_class(char l) const {
    string s(1U,l);  
    return find_letter_class(s);
}

int alphabet::find_letter_class(const string& l) const {
    // Check the letters
    for(int i=0;i<n_letter_classes();i++) {
	if (letter_class(i)==l)
	    return i;
    }
    throw myexception()<<"Alphabet '"<<name<<"' doesn't contain letter class '"<<sanitize_string(l)<<"'";
}

int alphabet::operator[](char l) const {
    string s(1U,l);  
    return (*this)[s];
}

/// FIXME - use a hash table
int alphabet::operator[](const string& l) const 
{
    // Check for a gap
    if (l == gap_letter) 
	return alphabet::gap;

    // Check the letters
    for(int i=0;i<size();i++) {
	if (letter(i)==l)
	    return i;
    }

    // Check the letter_classes
    for(int i=size();i<n_letter_classes();i++) {
	if (letter_class(i) == l)
	    return i;
    }

    // Check for a wildcard
    if (l == wildcard) 
	return alphabet::not_gap;

    // Check for unknown
    for(auto& unknown_letter: unknown_letters)
        if (l == unknown_letter)
            return alphabet::unknown;

    // We don't have this letter!
    throw bad_letter(l,name);
}

// FIXME - this is somewhat wasteful...
vector<int> alphabet::operator() (const string& s) const
{
    const int lsize = width();

    if (s.size()%lsize != 0)
	throw myexception()<<"Number of letters should be a multiple of "<<lsize<<"!";

    vector<int> v(s.size()/lsize);

    for(int i=0;i<v.size();i++) {
	string temp = s.substr(i*lsize,lsize);
	v[i] = operator[](temp);
    }
    return v;
}


string alphabet::lookup(int i) const {
    if (i == gap)
	return gap_letter;
    else if (i == not_gap)
	return wildcard;
    else if (i == unknown)
	return unknown_letter();

    return letter_class(i);
}


bool operator==(const alphabet& a1,const alphabet& a2) {
    return a1.letters_ == a2.letters_;
}

void alphabet::insert(const string& l) 
{
    if (n_letter_classes() > n_letters()) 
	throw myexception()<<"Error: adding letters to alphabet after letter classes.";

    letters_.push_back(l);
    setup_letter_classes();
}

void alphabet::remove(const string& l)
{
    int index = find_letter(l);
    remove(index);
}

void alphabet::remove(int index)
{
    letters_.erase(letters_.begin()+index);
    setup_letter_classes();
}

void alphabet::setup_letter_classes()
{
    letter_classes_ = letters_;
  
    letter_masks_ = vector< bitmask_t >(n_letters(), bitmask_t(n_letters()) );
    for(int i=0;i<n_letters();i++)
	letter_masks_[i].set(i);

    letter_fmasks_ = vector< fmask_t >(n_letters(), fmask_t(n_letters(),0.0) );
    for(int i=0;i<n_letters();i++)
	letter_fmasks_[i][i] = 1.0;
}

void alphabet::insert_class(const string& l, const bitmask_t& mask) 
{
    if (includes(letters_,l))
	throw myexception()<<"Can't use letter name '"<<sanitize_string(l)<<"' as letter class name.";

    letter_classes_.push_back(l);
    letter_masks_.push_back(mask);

    fmask_t fmask(n_letters(), 0.0);
    for(int i=0;i<n_letters();i++)
	if (mask.test(i))
	    fmask[i] = 1.0;

    letter_fmasks_.push_back(fmask);
}

/// Add a letter class to the alphabet
void alphabet::insert_class(const std::string& l,const vector<string>& letters) 
{
    bitmask_t mask(size());

    for(const auto& letter: letters)
	mask.set(find_letter(letter));

    insert_class(l,mask);
}

/// Add a letter class to the alphabet
void alphabet::insert_class(const std::string& l,const string& letters) 
{
    vector<string> letters2;
    for(int i=0;i<letters.size();i++)
	letters2.push_back(letters.substr(i,1));

    insert_class(l,letters2);
}

/// Add a letter class to the alphabet
void alphabet::remove_class(const std::string& l)
{
    // Check the letters
    for(int i=size();i<n_letter_classes();i++) 
	if (letter_class(i) == l) {
	    letter_classes_.erase(letter_classes_.begin()+i);
	    return;
	}
    throw myexception()<<"Can't find letter class '"<<sanitize_string(l)<<"'";
}

valarray<double> alphabet::get_frequencies_from_counts(const valarray<double>& counts,double pseudocount) const {

    valarray<double> f = counts;
    for(int i=0;i<f.size();i++)
	f[i] += pseudocount;

    f /= f.sum();

    return f;
}

string alphabet::print () const {
    return "'" + name + "'";
}


alphabet::alphabet(const string& s)
    :name(s)
{
}

alphabet::alphabet(const string& s,const string& letters)
    :alphabet(s, letters, "+")
{ }

alphabet::alphabet(const string& s,const string& letters,const string& m)
    :name(s), wildcard(m)
{
    for(int i=0;i<letters.length();i++)
	insert(string(1U,letters[i]));
}

alphabet::alphabet(const string& s,const vector<string>& letters)
    :alphabet(s, letters, "+")
{ }

alphabet::alphabet(const string& s,const vector<string>& letters,const string& m) 
    :name(s),wildcard(m)
{
    for(int i=0;i<letters.size();i++)
	insert(letters[i]);
}

Numeric::Numeric(const string& s, int n)
    :alphabet(s, "", "X")
{
    for(int i=0;i<n;i++)
	insert(std::to_string(i));
}

Numeric::Numeric(int n)
    :Numeric(string("Numeric(")+std::to_string(n)+")", n)
{
}

bool Nucleotides::is_watson_crick(int l1, int l2) const
{
    assert(0 <= l1 and l1 < 4);
    assert(0 <= l2 and l2 < 4);

    return complement(l1) == l2;
}

bool Nucleotides::is_mismatch(int l1, int l2) const
{
    return not is_watson_crick(l1, l2);
}

bool Nucleotides::is_wobble_pair(int l1, int l2) const
{
    return (l1 == G() and l2 == T()) or (l1 == T() and l2 == G());
}

int Nucleotides::complement(int l) const
{
    assert(l >= -3);
    assert(l < n_letter_classes());

    switch (l) {
    case 0: // A
	return T();
    case 1: // C
	return G();
    case 2: // G
	return C();
    case 3: // T or U
	return A();
    }
    if (l < 0)
	return l;
    else 
	return alphabet::not_gap;
}

Nucleotides::Nucleotides(const string& s, char c)
    :alphabet(s,"","N")
{
    string t; t += c;

    insert("A");
    insert("C");
    insert("G");
    insert(t);

    insert_class("Y",t+"C");    // pYrimidine
    insert_class("R","AG");     // puRine
    insert_class("W",t+"A");    // Weak
    insert_class("S","GC");     // Strong
    insert_class("K",t+"G"); // Ketone
    insert_class("M","AC");  // aMino

    insert_class("B",t+"GC"); // not-A (B is after A)
    insert_class("D",t+"AG"); // not-C (D is after C)
    insert_class("H",t+"AC"); // not-G (H is after G)
    insert_class("V","GAC");  // not-T (V is after U)
}

DNA::DNA()
    :Nucleotides("DNA",'T')
{ }

RNA::RNA()
    :Nucleotides("RNA",'U')
{ }


bool AminoAcids::is_stop(int i) const
{
    // FIXME - this is incredibly slow and hackish.  Fix if ever on a fast path...
    return lookup(i) == "*";
}

AminoAcids::AminoAcids() 
    :alphabet("Amino-Acids","ARNDCQEGHILKMFPSTWYV","X")
{
    insert_class("B","DN");
    insert_class("Z","EQ");
    insert_class("J","IL");
}

AminoAcids::AminoAcids(const string& s, const string& letters) 
    :alphabet(s,string("ARNDCQEGHILKMFPSTWYV")+letters,"X")
{
    insert_class("B","DN");
    insert_class("Z","EQ");
    insert_class("J","IL");
}

AminoAcidsWithStop::AminoAcidsWithStop() 
    :AminoAcids("Amino-Acids+stop","*")
{ }


char convert_DNA_or_RNA_to(char c, Nucleotides& N)
{
    //---- Convert U to U/T ----//
    string T_letter = N.lookup(2);
    assert(T_letter.size() == 1);
    char T = T_letter[0];

    if (c == 'U' or c == 'T')
	return T;
    else
	return c;
}

shared_ptr<const Nucleotides> get_nucleotides(const string& name)
{
    if (name == "DNA")
	return shared_ptr<const Nucleotides>(new DNA);
    else if (name == "RNA")
	return shared_ptr<const Nucleotides>(new RNA);

    throw myexception()<<"'"<<name<<"' is not a valid nucleotides alphabet.  Please specify DNA or RNA.";
}

#include "doublets.H"
#include "RNAEdits.H"
#include "triplets.H"
#include "codons.H"

shared_ptr<const alphabet> get_alphabet(const string& name_)
{
    string name = name_;
    vector<string> arguments = get_arguments(name,'(',')');

    if (name == "Codons")
    {
	if (arguments.size() == 1)
	    arguments.push_back("standard");

	if (arguments.size() != 2 or arguments[0].empty() or arguments[1].empty())
	    throw myexception()<<"Codons needs two arguments specifying the nucleotide alphabet and the genetic code: e.g. Codons(DNA,standard).";

	auto N = get_nucleotides(arguments[0]);
	auto G = get_genetic_code(arguments[1]);

	return shared_ptr<const alphabet>(new Codons(*N, AminoAcids(), G));
    }
    else if (name == "Triplets")
    {
	if (arguments.size() != 1 or arguments[0].empty())
	    throw myexception()<<"Triplets needs one argument specifying the nucleotide alphabet: e.g. Triplets(DNA).";

	auto N = get_nucleotides(arguments[0]);

	return shared_ptr<const alphabet>(new Triplets(*N));
    }
    else if (name == "Doublets")
    {
	if (arguments.size() != 1 or arguments[0].empty())
	    throw myexception()<<"Doublets needs one argument specifying the nucleotide alphabet: e.g. Doublets(RNA).";

	auto N = get_nucleotides(arguments[0]);

	return shared_ptr<const alphabet>(new Doublets(*N));
    }
    else if (name == "RNAEdits")
    {
	if (arguments.size() != 1 or arguments[0].empty())
	    throw myexception()<<"RNAEdits needs one argument specifying the nucleotide alphabet: e.g. Doublets(RNA).";

	auto N = get_nucleotides(arguments[0]);

	return shared_ptr<const alphabet>(new RNAEdits(*N));
    }
    else if (name == "Numeric")
    {
	if (arguments.size() != 1 or arguments[0].empty())
	    throw myexception()<<"Numeric needs one argument specifying the number of states: e.g. Numeric(2).";
	int n = convertTo<int>(arguments[0]);

	return shared_ptr<const alphabet>(new Numeric(n));
    }
    else if (name == "DNA")
	return shared_ptr<const alphabet>(new DNA);
    else if (name == "RNA")
	return shared_ptr<const alphabet>(new RNA);
    else if (name == "Amino-Acids" or name == "AA")
	return shared_ptr<const alphabet>(new AminoAcids);
    else if (name == "Amino-Acids+stop" or name == "AA*")
	return shared_ptr<const alphabet>(new AminoAcidsWithStop);

    throw myexception()<<"I don't recognize alphabet '"<<name<<"'";
}
