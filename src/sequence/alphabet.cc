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

namespace fs = std::filesystem;

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
    :Numeric(string("Numeric[")+std::to_string(n)+"]", n)
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


void Doublets::setup_sub_nuc_table()
{
    auto n_singlet_letters = N->size();
    doublet_table = vector<vector<int> >(n_singlet_letters,vector<int>(n_singlet_letters,-1));

    sub_nuc_table.clear();
    sub_nuc_table.resize(size());

    assert(N->width() == 1);

    for(int i=0;i<sub_nuc_table.size();i++)
    {
	auto& doublet = letter(i);

	assert(doublet.length() == 2);
	sub_nuc_table[i].resize(2);

	int n0 = sub_nuc_table[i][0] = (*N)[ doublet.substr(0,1) ];
	int n1 = sub_nuc_table[i][1] = (*N)[ doublet.substr(1,1) ];

	doublet_table[n0][n1] = i;
    }
}

int Doublets::sub_nuc(int letter_index, int pos) const
{
    assert( 0 <= pos and pos <= 2);

    return sub_nuc_table[letter_index][pos];
}

vector<string> getDoublets(const vector<string>& v) 
{
    vector<string> w;
    for(int i=0;i<v.size();i++)
    {
	string s1 = v[i];
	for(int j=0;j<v.size();j++)
	{
	    string s2 = s1 + v[j];
	    w.push_back(s2);
	}
    }
    return w;
}

vector<string> getDoublets(const Nucleotides& a)
{
    vector<string> v;
    for(int i=0;i<a.size();i++)
	v.push_back(a.lookup(i));
    return getDoublets(v);
}


bool matches_doublet(const string& c1,const string& c2,const Nucleotides& N)
{
    assert(c1.size() == 2);
    assert(c1.size() == c2.size());

    for(int n=0;n<2;n++)
    {
	string l1 = c1.substr(n,1);
	string l2 = c2.substr(n,1);

	int i1 = N.find_letter(l1);
	int i2 = N[l2];

	if (not N.matches(i1,i2))
	    return false;
    }
    return true;
}

// alphabet: already set
// unknown_letters: already set
void Doublets::setup_letter_classes() 
{
    // clear masks and classes to just the letters
    alphabet::setup_letter_classes();

    // get nucleotide letters
    vector<string> v = N->letter_classes();
    v.push_back(N->wildcard);

    // construct letter classes names
    vector<string> w = getDoublets(v);
  
    // construct letter class masks
    bitmask_t empty_mask(size());
    bitmask_t mask(size());

    for(int i=0;i<w.size();i++)
    {
	if (contains(w[i])) continue;
	if (w[i] == wildcard) continue;

	mask = empty_mask;

	bool found = false;
	for(int j=0;j<mask.size();j++) {
	    if (::matches_doublet(letter(j),w[i],*N)) {
		mask[j] = true;
		found = true;
	    }
	}
	if (found)
	    insert_class(w[i],mask);
    }
}

valarray<double> get_nucleotide_counts_from_doublet_counts(const Doublets& D,const valarray<double>& D_counts)
{
    const Nucleotides& N = D.getNucleotides();

    valarray<double> N_counts(0.0, N.size());
    // For each codon type
    for(int i=0;i<D.size();i++) {
	// For each position in the codon
	for(int pos=0;pos<2;pos++)
	    // Count the nucleotides that occur there
	    N_counts[ D.sub_nuc(i,pos) ] += D_counts[i];
    }

    return N_counts;
}

valarray<double> get_doublet_frequencies_from_independent_nucleotide_frequencies(const Doublets& D,const valarray<double>& fN )
{
    valarray<double> fD(D.size());
    for(int i=0;i<fD.size();i++) {
	fD[i] = 1.0;
	for(int pos=0;pos<2;pos++)
	    fD[i] *= fN[ D.sub_nuc(i,pos) ];
    }

    fD /= fD.sum();
    return fD;
}

int Doublets::get_doublet(int n1, int n2) const
{
    if (N->is_feature(n1) and N->is_feature(n2)) 
    {
	if (N->is_letter(n1) and N->is_letter(n2))
	{
	    int index = doublet_table[n1][n2];
	    assert(index != -1);
	    return index;
	}
	else 
	    return alphabet::not_gap;
    }
    else if (n1 == alphabet::gap or n2 == alphabet::gap)
	return alphabet::gap;
    else
	return alphabet::unknown;
}

bool Doublets::is_watson_crick(int d) const
{
    int d1 = sub_nuc(d,0);
    int d2 = sub_nuc(d,1);
    return N->is_watson_crick(d1,d2);
}

bool Doublets::is_mismatch(int d) const
{
    int d1 = sub_nuc(d,0);
    int d2 = sub_nuc(d,1);
    return N->is_mismatch(d1,d2);
}

bool Doublets::is_wobble_pair(int d) const
{
    int d1 = sub_nuc(d,0);
    int d2 = sub_nuc(d,1);
    return N->is_wobble_pair(d1,d2);
}

int Doublets::n_changes(int l1, int l2) const
{
    int n = 0;
    for(int pos=0;pos<2;pos++)
	if (sub_nuc(l1,pos) != sub_nuc(l2,pos))
	    n++;
    return n;
}

valarray<double> Doublets::get_frequencies_from_counts(const valarray<double>& counts,double pseudocount) const {

    //--------- Level 1 pseudocount (nucleotides) ---------------//
    valarray<double> N_counts = get_nucleotide_counts_from_doublet_counts(*this,counts);
    valarray<double> fN = getNucleotides().get_frequencies_from_counts(N_counts);
    valarray<double> prior_f = get_doublet_frequencies_from_independent_nucleotide_frequencies(*this,fN);

    valarray<double> counts1 = counts + pseudocount*counts.size()*prior_f;

    valarray<double> f = counts1 /= counts1.sum();

    return f;
}

vector<int> Doublets::operator()(const string& letters) const
{
    const int letter_size = width();

    myexception e;

    // 1. First translate singlets.
    vector<int> singlets = (*N)(letters);

    // FIXME -- if ? gets here from a user who means N/X then things will get confusing fast.

    // 2. Second count the number of non-gap letters.
    int n_letters = 0;
    for(auto s: singlets)
	if (is_feature(s)) n_letters++;

    // 3. Try to load alignment row with gaps
    bool ok = true;
    vector<int> doublets(singlets.size()/letter_size);

    for(int i=0;i<doublets.size() and ok;i++)
    {
	int l1 = singlets[letter_size * i + 0];
	int l2 = singlets[letter_size * i + 1];

	if (is_feature(l1) and is_feature(l2))
	    doublets[i] = get_doublet(l1, l2);
	else if (l1 == alphabet::gap and l2 == alphabet::gap)
	    doublets[i] = alphabet::gap;
	else
	{
	    e<<" Sequence not aligned as "<<letters_name()<<"!  Column "<<i+1<<" has mixed gap/non-gap letter '"<<letters.substr(i*letter_size,letter_size)<<"'";
	    ok = false;
	}
    }

    // 4. Check if we have the right number of letters.
    if (n_letters%letter_size != 0)
    {
	if (not ok) e<<"\n";
	e<<" Sequence of "<<n_letters<<" "<<N->letters_name()<<" cannot be divided into "<<letters_name() <<": not a multiple of 2 "<<N->letters_name()<<"!";
	ok = false;
    }

    // 5. Check for extra columns we haven't been using
    if (singlets.size() % letter_size != 0)
    {
	if (not ok) e<<"\n";
	e<<" Alignment row of "<<letters.size()<<" columns cannot be divided into "<<letters_name() <<": not a multiple of 2 columns!";
	ok = false;
    }

    if (not ok)
	throw e;

    return doublets;
}

Doublets::Doublets(const string& s,const Nucleotides& a)
    :alphabet(s,getDoublets(a)),N(a)
{
    // compute our 'wildcard' letter
    wildcard = N->wildcard + N->wildcard;

    // compute our 'gap' letters
    gap_letter = N->gap_letter + N->gap_letter;

    // compute our 'unknown' letters
    unknown_letters.clear();
    for(auto& unknown_letter: N->unknown_letters)
        unknown_letters.push_back( unknown_letter + unknown_letter );

    setup_sub_nuc_table();

    setup_letter_classes();
}

Doublets::Doublets(const Nucleotides& a)
    :Doublets(string("Doublets[")+a.name+"]",a)
{ }

void Triplets::setup_sub_nuc_table()
{
    codon_table = vector<vector<vector<int>>>(4,vector<vector<int> >(4,vector<int>(4,-1)));

    sub_nuc_table.clear();
    sub_nuc_table.resize(size());

    assert(N->width() == 1);

    for(int i=0;i<sub_nuc_table.size();i++) {
	const string& codon = letter(i);

	assert(codon.length() == 3);
	sub_nuc_table[i].resize(3);

	int n0 = sub_nuc_table[i][0] = (*N)[ codon.substr(0,1) ];
	int n1 = sub_nuc_table[i][1] = (*N)[ codon.substr(1,1) ];
	int n2 = sub_nuc_table[i][2] = (*N)[ codon.substr(2,1) ];

	codon_table[n0][n1][n2] = i;
    }
}

int Triplets::sub_nuc(int codon,int pos) const {
    assert( 0 <= pos and pos <= 3);

    return sub_nuc_table[codon][pos];
}

vector<string> getTriplets(const vector<string>& v) 
{
    vector<string> w;
    for(int i=0;i<v.size();i++) {
	string s1 = v[i];
	for(int j=0;j<v.size();j++) {
	    string s2 = s1 + v[j];
	    for(int k=0;k<v.size();k++) {
		string s3 = s2 + v[k];
		w.push_back(s3);
	    }
	}
    }
    return w;
}

vector<string> getTriplets(const Nucleotides& a) {
    vector<string> v;
    for(int i=0;i<a.size();i++)
	v.push_back(a.lookup(i));
    return getTriplets(v);
}


bool matches(const string& c1,const string& c2,const Nucleotides& N)
{
    assert(c1.size() == 3);
    assert(c1.size() == c2.size());

    for(int n=0;n<3;n++) {
	string l1 = c1.substr(n,1);
	string l2 = c2.substr(n,1);

	int i1 = N.find_letter(l1);
	int i2 = N[l2];

	if (not N.matches(i1,i2))
	    return false;
    }
    return true;
}

// alphabet: already set
// unknown_letters: already set
void Triplets::setup_letter_classes() 
{
    // clear masks and classes to just the letters
    alphabet::setup_letter_classes();

    // get nucleotide letters
    vector<string> v = N->letter_classes();
    v.push_back(N->wildcard);

    // construct letter classes names
    vector<string> w = getTriplets(v);
  
    // construct letter class masks
    bitmask_t empty_mask(size());
    bitmask_t mask(size());

    for(int i=0;i<w.size();i++) {
	if (contains(w[i])) continue;
	if (w[i] == wildcard) continue;

	mask = empty_mask;

	bool found = false;
	for(int j=0;j<mask.size();j++) {
	    if (::matches(letter(j),w[i],*N)) {
		mask[j] = true;
		found = true;
	    }
	}
	if (found)
	    insert_class(w[i],mask);
    }
}

valarray<double> get_nucleotide_counts_from_codon_counts(const Triplets& C,const valarray<double>& C_counts) {
    const Nucleotides& N = C.getNucleotides();

    valarray<double> N_counts(0.0,N.size());
    // For each codon type
    for(int i=0;i<C.size();i++) {
	// For each position in the codon
	for(int pos=0;pos<3;pos++)
	    // Count the nucleotides that occur there
	    N_counts[ C.sub_nuc(i,pos) ] += C_counts[i];
    }

    return N_counts;
}

valarray<double> get_codon_frequencies_from_independent_nucleotide_frequencies(const Triplets& C,const valarray<double>& fN ) {
    valarray<double> fC(C.size());
    for(int i=0;i<fC.size();i++) {
	fC[i] = 1.0;
	for(int pos=0;pos<3;pos++)
	    fC[i] *= fN[ C.sub_nuc(i,pos) ];
    }

    fC /= fC.sum();
    return fC;
}

int Triplets::get_triplet(int n1, int n2, int n3) const
{
    if (N->is_feature(n1) and N->is_feature(n2) and N->is_feature(n3)) 
    {
	if (N->is_letter(n1) and N->is_letter(n2) and N->is_letter(n3))
	{
	    int index = codon_table[n1][n2][n3];
	    if (index == -1) 
		throw myexception()<<"get_triplet: Triplet is not in this alphabet";
	    return index;
	}
	else 
	    return alphabet::not_gap;
    }
    else if (n1 == alphabet::gap or n2 == alphabet::gap or n3 == alphabet::gap)
	return alphabet::gap;
    else
	return alphabet::unknown;
}

valarray<double> Triplets::get_frequencies_from_counts(const valarray<double>& counts,double pseudocount) const {

    //--------- Level 1 pseudocount (nucleotides) ---------------//
    valarray<double> N_counts = get_nucleotide_counts_from_codon_counts(*this,counts);
    valarray<double> fN = getNucleotides().get_frequencies_from_counts(N_counts);
    valarray<double> prior_f = get_codon_frequencies_from_independent_nucleotide_frequencies(*this,fN);

    valarray<double> counts1 = counts + pseudocount*counts.size()*prior_f;

    valarray<double> f = counts1 /= counts1.sum();

    return f;
}

#include "codons.H"

vector<int> Triplets::operator()(const string& letters) const
{
    const int letter_size = width();

    myexception e;

    // 1. First translate singlets.
    vector<int> singlets = (*N)(letters);

    // FIXME -- if ? gets here from a user who means N/X then things will get confusing fast.

    // 2. Second count the number of non-gap letters.
    int n_letters = 0;
    for(auto s: singlets)
	if (is_feature(s)) n_letters++;

    // 3. Try to load alignment row with gaps
    bool ok = true;
    vector<int> triplets(singlets.size()/letter_size);

    vector<int> stop_codons;
    for(int i=0;i<triplets.size() and ok;i++)
    {
	int l1 = singlets[letter_size * i + 0];
	int l2 = singlets[letter_size * i + 1];
	int l3 = singlets[letter_size * i + 2];

	if (is_feature(l1) and is_feature(l2) and is_feature(l3))
	{
	    try {
		triplets[i] = get_triplet(l1, l2, l3);
	    }
	    catch (...)
	    {
		stop_codons.push_back(i);
	    }
	}

	else if (l1 == alphabet::gap and l2 == alphabet::gap and l3 == alphabet::gap)
	    triplets[i] = alphabet::gap;
	else
	{
	    e<<" Sequence not aligned as "<<letters_name()<<"!  Column "<<i+1<<" has mixed gap/non-gap letter '"<<letters.substr(i*letter_size,letter_size)<<"'";
	    ok = false;
	}
    }

    // 4. Check if we have the right number of letters.
    if (n_letters%letter_size != 0)
    {
	if (not ok) e<<"\n";
	e<<" Sequence of "<<n_letters<<" "<<N->letters_name()<<" cannot be divided into "<<letters_name() <<": not a multiple of 3 "<<N->letters_name()<<"!";
	ok = false;
    }

    // 5. Check for extra columns we haven't been using
    if (singlets.size() % letter_size != 0)
    {
	if (not ok) e<<"\n";
	e<<" Alignment row of "<<letters.size()<<" columns cannot be divided into "<<letters_name() <<": not a multiple of 3 columns!";
	ok = false;
    }

    if (stop_codons.size())
    {
	if (not ok) e<<"\n";
	ok = false;
	auto C = dynamic_cast<const Codons*>(this);
	e<<" Sequence contains "<<stop_codons.size()<<" stop codons: not allowed!\n";

	int col = stop_codons[0];
	string stop = letters.substr(col*letter_size, letter_size);
	e<<"   First stop codon is '"<<stop<<"' at nucleotide column "<<3*col+1<<"   (genetic code = "<<C->getGenetic_Code().name()<<")";
    }

    if (not ok)
	throw e;

    return triplets;
}

Triplets::Triplets(const string& s,const Nucleotides& a)
    :alphabet(s,getTriplets(a)),N(a)
{
    // compute our 'wildcard' letter
    wildcard = N->wildcard+N->wildcard+N->wildcard;

    // compute our 'gap' letters
    gap_letter = N->gap_letter + N->gap_letter + N->gap_letter;

    // compute our 'unknown' letters
    unknown_letters.clear();
    for(auto& unknown_letter: N->unknown_letters)
        unknown_letters.push_back( unknown_letter + unknown_letter + unknown_letter);

    setup_sub_nuc_table();

    setup_letter_classes();
}

Triplets::Triplets(const Nucleotides& a)
    :Triplets(string("Triplets[")+a.name+"]",a)
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

void Genetic_Code::add_entry(char c1, char c2, char c3, char aa)
{
    int n1 = dna[ c1 ];
    int n2 = dna[ c2 ];
    int n3 = dna[ c3 ];

    if (not dna.is_letter(n1)) throw myexception()<<"add_entry( ): in codon '"<<c1<<c2<<c3<<"' -> '"<<aa<<"', '"<<c1<<" is not a nucleotide.";
    if (not dna.is_letter(n2)) throw myexception()<<"add_entry( ): in codon '"<<c1<<c2<<c3<<"' -> '"<<aa<<"', '"<<c2<<" is not a nucleotide.";
    if (not dna.is_letter(n3)) throw myexception()<<"add_entry( ): in codon '"<<c1<<c2<<c3<<"' -> '"<<aa<<"', '"<<c3<<" is not a nucleotide.";

    int aa_index = A[aa];

    if (not A.is_letter(aa_index)) throw myexception()<<"add_entry( ): in codon '"<<c1<<c2<<c3<<"' -> '"<<aa<<"', '"<<aa<<"' is not a valid amino acid.";

    translation_table[n1][n2][n3] = aa_index;
}

void Genetic_Code::setup_table(const string& n1, const string& n2, const string& n3, const string& aa)
{
    const int N = 64;
    assert(n1.size() == N);
    assert(n2.size() == N);
    assert(n3.size() == N);
    assert(aa.size() == N);

    translation_table = vector< vector< vector<int> > >(4,
							vector<vector<int> >(4,
									     vector<int>(4,-1)
							    )
	);

    for(int i=0;i<N;i++)
	add_entry(n1[i],n2[i],n3[i],aa[i]);

    for(int i=0;i<4;i++)
	for(int j=0;j<4;j++)
	    for(int k=0;k<4;k++)
		if (translation_table[i][j][k] == -1)
		    throw myexception()<<"Codon "<<i<<","<<j<<","<<k<<" has no translation!";
}

void Genetic_Code::setup_table(const string& aa)
{
    string n1 = "TTTTTTTTTTTTTTTTCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGG";
    string n2 = "TTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGG";
    string n3 = "TCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAG";
    setup_table(n1,n2,n3,aa);
}

void Genetic_Code::setup_table(std::istream& file)
{
    string aa;
    portable_getline(file,aa);

    string n1;
    portable_getline(file,n1);

    string n2;
    portable_getline(file,n2);

    string n3;
    portable_getline(file,n3);

    //---- Create the lookup table ----//
    setup_table(n1,n2,n3,aa);
}

void Genetic_Code::setup_table_from_file(const fs::path& filename)
{
    checked_ifstream file(filename,"genetic code file");
    setup_table(file);
}

int Genetic_Code::translate(int n1, int n2, int n3) const
{
    if (rna.is_feature(n1) and rna.is_feature(n2) and rna.is_feature(n3)) 
    {
	if (rna.is_letter(n1) and rna.is_letter(n2) and rna.is_letter(n3))
	{
	    int index = translation_table[n1][n2][n3];
	    if (index == -1) 
		throw myexception()<<"Genetic Code: "<<name()<<" has no entry for "<<n1<<","<<n2<<","<<n3;
	    return index;
	}
	else 
	    return alphabet::not_gap;
    }
    else if (n1 == alphabet::gap or n2 == alphabet::gap or n3 == alphabet::gap)
	return alphabet::gap;
    else
	return alphabet::unknown;
}

Genetic_Code::Genetic_Code(const string& n)
    :name_(n)
{
  
}

Genetic_Code::Genetic_Code(const string& n, istream& file)
    :name_(n)
{
    setup_table(file);
}

Genetic_Code::Genetic_Code(const string& n, const fs::path& filename)
    :name_(n)
{
    setup_table_from_file(filename);
}

Standard_Genetic_Code::Standard_Genetic_Code()
    :Genetic_Code("standard")
{
    setup_table("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG");
}

Mt_Invertebrate_Genetic_Code::Mt_Invertebrate_Genetic_Code()
    :Genetic_Code("mt-invert")
{
    setup_table("FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSSSVVVVAAAADDEEGGGG");
}

Mt_Vertebrate_Genetic_Code::Mt_Vertebrate_Genetic_Code()
    :Genetic_Code("mt-vert")
{
    setup_table("FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG");
}

Mt_Yeast_Genetic_Code::Mt_Yeast_Genetic_Code()
    :Genetic_Code("mt-yeast")
{
    setup_table("FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIMMTTTTNNKKSSRRVVVVAAAADDEEGGGG");
}

Mt_Protozoan_Genetic_Code::Mt_Protozoan_Genetic_Code()
    :Genetic_Code("mt-protozoan")
{
    setup_table("FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIMMTTTTNNKKSSRRVVVVAAAADDEEGGGG");
}

shared_ptr<const Genetic_Code> get_genetic_code(const string& name)
{
    if (name == "standard")
	return shared_ptr<const Genetic_Code>(new Standard_Genetic_Code());
    else if (name == "mt-vert")
	return shared_ptr<const Genetic_Code>(new Mt_Vertebrate_Genetic_Code());
    else if (name == "mt-invert")
	return shared_ptr<const Genetic_Code>(new Mt_Invertebrate_Genetic_Code());
    else if (name == "mt-yeast")
	return shared_ptr<const Genetic_Code>(new Mt_Yeast_Genetic_Code());
    else if (name == "mt-protozoan")
	return shared_ptr<const Genetic_Code>(new Mt_Protozoan_Genetic_Code());
    else
	throw myexception()<<"I don't recognize genetic code name '"<<name<<"'.\n"
	    "  Try one of 'standard', 'mt-vert', 'mt-invert', 'mt-protozoan', 'mt-yeast'.";
}

shared_ptr<const Nucleotides> get_nucleotides(const string& name)
{
    if (name == "DNA")
	return shared_ptr<const Nucleotides>(new DNA);
    else if (name == "RNA")
	return shared_ptr<const Nucleotides>(new RNA);

    throw myexception()<<"'"<<name<<"' is not a valid nucleotides alphabet.  Please specify DNA or RNA.";
}

shared_ptr<const alphabet> get_alphabet(const string& name_)
{
    string name = name_;
    vector<string> arguments = get_arguments(name,'[',']');

    if (name == "Codons")
    {
	if (arguments.size() != 2 or arguments[0].empty() or arguments[1].empty())
	    throw myexception()<<"Codons needs two arguments specifying the nucleotide alphabet and the genetic code: e.g. Codons[DNA,standard].";

	auto N = get_nucleotides(arguments[0]);
	auto G = get_genetic_code(arguments[1]);

	return shared_ptr<const alphabet>(new Codons(*N, AminoAcids(), *G));
    }
    else if (name == "Triplets")
    {
	if (arguments.size() != 1 or arguments[0].empty())
	    throw myexception()<<"Triplets needs one argument specifying the nucleotide alphabet: e.g. Triplets[DNA].";

	auto N = get_nucleotides(arguments[0]);

	return shared_ptr<const alphabet>(new Triplets(*N));
    }
    else if (name == "Doublets")
    {
	if (arguments.size() != 1 or arguments[0].empty())
	    throw myexception()<<"Doublets needs one argument specifying the nucleotide alphabet: e.g. Doublets[RNA].";

	auto N = get_nucleotides(arguments[0]);

	return shared_ptr<const alphabet>(new Doublets(*N));
    }
    else if (name == "Numeric")
    {
	if (arguments.size() != 1 or arguments[0].empty())
	    throw myexception()<<"Numeric needs one argument specifying the number of states: e.g. Numeric[2].";
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
