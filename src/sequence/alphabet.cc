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

#include "alphabet.hh"
#include "util/assert.hh"
#include <fstream>
#include "util/set.hh"
#include "util/io.hh"
#include "util/string/convert.hh"
#include "util/cmdline.hh"
#include "util/string/sanitize.hh" // for sanitize_string( )

using std::vector;
using std::string;
using std::valarray;
using std::istream;
using std::shared_ptr;

// Legally, we have to define this to give them a location.
// This only triggers an error in g++ if they aren't inlined.
const int alphabet::gap;
const int alphabet::not_gap;
const int alphabet::unknown;

bool alphabet::contains(char l) const {
    string s(1U,l);
    return contains(s);
}

bool alphabet::contains(const std::string& l) const {
    return includes(letters_,l);
}

std::optional<int> alphabet::find_letter(char l) const {
    string s(1U,l);  
    return find_letter(s);
}

// Search the exact alphabet states without using exceptions for an absent result.
std::optional<int> alphabet::find_letter(const string& l) const
{
    for (int i = 0; i < size(); i++)
        if (letter(i) == l)
            return i;
    return {};
}

string alphabet::lookup(int i) const {
    if (i == gap)
	return gap_letter;
    else if (i == not_gap)
	return wildcard;
    else if (i == unknown)
	return unknown_letter();

    return letter(i);
}

// Exact letters and the wildcard are common to every alphabet; derived classes
// add their own readable ambiguity notation without assigning integer codes.
std::optional<alphabet::bitmask_t> alphabet::mask_for_symbol(const string& symbol) const
{
    if (auto state = find_letter(symbol))
    {
        bitmask_t mask(n_letters());
        mask.set(*state);
        return mask;
    }
    else if (symbol == wildcard)
    {
        bitmask_t mask(n_letters());
        mask.set();
        return mask;
    }
    return {};
}

// Spell masks common to every alphabet without retaining a mask-to-string table.
std::pair<string, bool> alphabet::lookup(const bitmask_t& mask) const
{
    assert(mask.size() == n_letters());
    assert(mask.any());
    if (mask.count() == 1)
        return {letter(mask.find_first()), true};
    if (mask.count() == n_letters())
        return {wildcard, true};

    return {wildcard, false};
}

// Ordinary alphabets need no second diagnostic pass after the shared encoder reports failure.
void alphabet::diagnose_sequence_encoding_failure(const string&) const
{}


bool operator==(const alphabet& a1,const alphabet& a2) {
    return a1.letters_ == a2.letters_;
}

void alphabet::insert(const string& l) 
{
    letters_.push_back(l);
}

void alphabet::remove(int index)
{
    letters_.erase(letters_.begin()+index);
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
    assert(l < n_letters());

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
    return l;
}

// Decode IUPAC nucleotide notation to the fixed A,C,G,T/U state ordering.
std::optional<alphabet::bitmask_t> Nucleotides::mask_for_symbol(const string& symbol) const
{
    if (auto mask = alphabet::mask_for_symbol(symbol))
        return mask;
    if (symbol.size() != 1)
        return {};

    unsigned long bits = 0;
    switch (symbol[0])
    {
    case 'Y': bits = 0b1010; break;
    case 'R': bits = 0b0101; break;
    case 'W': bits = 0b1001; break;
    case 'S': bits = 0b0110; break;
    case 'K': bits = 0b1100; break;
    case 'M': bits = 0b0011; break;
    case 'B': bits = 0b1110; break;
    case 'D': bits = 0b1101; break;
    case 'H': bits = 0b1011; break;
    case 'V': bits = 0b0111; break;
    default: return {};
    }
    return bitmask_t(n_letters(), bits);
}

// Use IUPAC notation for every proper nucleotide state set.
std::pair<string, bool> Nucleotides::lookup(const bitmask_t& mask) const
{
    auto result = alphabet::lookup(mask);
    if (result.second)
        return result;

    switch (mask.to_ulong())
    {
    case 0b1010: return {"Y", true};
    case 0b0101: return {"R", true};
    case 0b1001: return {"W", true};
    case 0b0110: return {"S", true};
    case 0b1100: return {"K", true};
    case 0b0011: return {"M", true};
    case 0b1110: return {"B", true};
    case 0b1101: return {"D", true};
    case 0b1011: return {"H", true};
    case 0b0111: return {"V", true};
    default: return result;
    }
}

Nucleotides::Nucleotides(const string& s, char c)
    :alphabet(s,"","N")
{
    string t; t += c;

    insert("A");
    insert("C");
    insert("G");
    insert(t);
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
{}

AminoAcids::AminoAcids(const string& s, const string& letters) 
    :alphabet(s,string("ARNDCQEGHILKMFPSTWYV")+letters,"X")
{}

// Decode the three standard two-state amino-acid ambiguity symbols.
std::optional<alphabet::bitmask_t> AminoAcids::mask_for_symbol(const string& symbol) const
{
    if (auto mask = alphabet::mask_for_symbol(symbol))
        return mask;

    std::optional<int> first;
    std::optional<int> second;
    if (symbol == "B")
    {
        first = find_letter("D");
        second = find_letter("N");
    }
    else if (symbol == "Z")
    {
        first = find_letter("E");
        second = find_letter("Q");
    }
    else if (symbol == "J")
    {
        first = find_letter("I");
        second = find_letter("L");
    }
    else
        return {};

    assert(first and second);
    bitmask_t mask(n_letters());
    mask.set(*first);
    mask.set(*second);
    return mask;
}

// Prefer the standard amino-acid ambiguity symbol when one denotes the mask.
std::pair<string, bool> AminoAcids::lookup(const bitmask_t& mask) const
{
    auto result = alphabet::lookup(mask);
    if (result.second or mask.count() != 2)
        return result;
    auto D = find_letter("D");
    auto N = find_letter("N");
    auto E = find_letter("E");
    auto Q = find_letter("Q");
    auto I = find_letter("I");
    auto L = find_letter("L");
    assert(D and N and E and Q and I and L);
    if (mask[*D] and mask[*N])
        return {"B", true};
    if (mask[*E] and mask[*Q])
        return {"Z", true};
    if (mask[*I] and mask[*L])
        return {"J", true};
    return result;
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

#include "doublets.hh"
#include "RNAEdits.hh"
#include "triplets.hh"
#include "codons.hh"

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
