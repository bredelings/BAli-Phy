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
#include <cassert>
#include <fstream>
#include "util.H"
#include "io.H"

using std::vector;
using std::string;
using std::valarray;
using std::istream;

namespace {
string sanitize(const string& s1)
{
  string s2;
  for(int i=0;i<s1.size();i++) {
    char c = s1[i];
    if (c >=32 and c<= 126)
      s2 += c;
    else
      s2 = s2 + "[" + convertToString((int)c) + "]";
  }
  return s2;
}
}

bad_letter::bad_letter(const string& l)
  :myexception(string("Letter '") + sanitize(l) + string("' not in alphabet.")),letter(l)
{}

bad_letter::bad_letter(const string& l,const string& name)
  :myexception(string("Letter '") + sanitize(l) + string("' not in alphabet '") + name + "'."),letter(l)
{}

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
  throw myexception()<<"Alphabet '"<<name<<"' doesn't contain letter '"<<sanitize(l)<<"'";
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
  throw myexception()<<"Alphabet '"<<name<<"' doesn't contain letter class '"<<sanitize(l)<<"'";
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
    return unknown_letter;

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
    throw myexception()<<"Can't use letter name '"<<sanitize(l)<<"' as letter class name.";

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
  throw myexception()<<"Can't find letter class '"<<sanitize(l)<<"'";
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
  :name(s),gap_letter("-"),wildcard("+"),unknown_letter("?")
{
}

alphabet::alphabet(const string& s,const string& letters)
  :name(s),gap_letter("-"),wildcard("+"),unknown_letter("?")
{
  for(int i=0;i<letters.length();i++)
    insert(string(1U,s[i]));
}

alphabet::alphabet(const string& s,const string& letters,const string& m)
  :name(s),gap_letter("-"),wildcard(m),unknown_letter("?")
{
  for(int i=0;i<letters.length();i++)
    insert(string(1U,letters[i]));
}

alphabet::alphabet(const string& s,const vector<string>& letters)
  :name(s),gap_letter("-"),wildcard("+"),unknown_letter("?")
{
  for(int i=0;i<letters.size();i++)
    insert(letters[i]);
}

alphabet::alphabet(const string& s,const vector<string>& letters,const string& m) 
  :name(s),gap_letter("-"),wildcard(m),unknown_letter("?")
{
  for(int i=0;i<letters.size();i++)
    insert(letters[i]);
}

int Nucleotides::complement(int l) const
{
  assert(l >= -3);
  assert(l < n_letter_classes());

  switch (l) {
  case 0: // A
    return 2;
  case 1: // G
    return 3;
  case 2: // T or U
    return 0;
  case 3: // C
    return 1;
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
  insert("G");
  insert(t);
  insert("C");

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
  :Nucleotides("DNA nucleotides",'T') 
{ }

RNA::RNA()
  :Nucleotides("RNA nucleotides",'U') 
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


void Triplets::setup_sub_nuc_table() {
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
	// Cound the nucleotides that occur there
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

Triplets::Triplets(const Nucleotides& a)
  :alphabet(string("Triplets of ")+a.name,getTriplets(a)),N(a),
   codon_table(4,vector<vector<int> >(4,vector<int>(4,-1)))
{
  // compute our 'wildcard' letter
  wildcard = N->wildcard+N->wildcard+N->wildcard;

  // compute our 'gap' letter
  gap_letter = N->gap_letter + N->gap_letter + N->gap_letter;

  // compute our 'unknown' letter
  unknown_letter = N->unknown_letter + N->unknown_letter + N->unknown_letter;

  setup_sub_nuc_table();

  setup_letter_classes();
}

Triplets::Triplets(const string& s,const Nucleotides& a)
  :alphabet(s,getTriplets(a)),N(a)
{
  // compute our 'wildcard' letter
  wildcard = N->wildcard+N->wildcard+N->wildcard;

  // compute our 'gap' letters
  gap_letter = N->gap_letter + N->gap_letter + N->gap_letter;

  // compute our 'unknown' letter
  unknown_letter = N->unknown_letter + N->unknown_letter + N->unknown_letter;

  setup_sub_nuc_table();

  setup_letter_classes();
}

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

void Genetic_Code::setup_table_from_file(const std::string& filename)
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

Genetic_Code::Genetic_Code(const string& n, const string& filename)
  :name_(n)
{
  setup_table_from_file(filename);
}

Standard_Genetic_Code::Standard_Genetic_Code()
  :Genetic_Code("Standard_Genetic_Code")
{
  setup_table("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG");
}

Mt_Invertebrate_Genetic_Code::Mt_Invertebrate_Genetic_Code()
  :Genetic_Code("Mt_Invertebrate_Genetic_Code")
{
  setup_table("FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSSSVVVVAAAADDEEGGGG");
}

Mt_Vertebrate_Genetic_Code::Mt_Vertebrate_Genetic_Code()
  :Genetic_Code("Mt_Vertebrate_Genetic_Code")
{
  setup_table("FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG");
}

Mt_Yeast_Genetic_Code::Mt_Yeast_Genetic_Code()
  :Genetic_Code("Mt_Yeast_Genetic_Code")
{
  setup_table("FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIMMTTTTNNKKSSRRVVVVAAAADDEEGGGG");
}

Mt_Protozoan_Genetic_Code::Mt_Protozoan_Genetic_Code()
  :Genetic_Code("Mt_Protozoan_Genetic_Code")
{
  setup_table("FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIMMTTTTNNKKSSRRVVVVAAAADDEEGGGG");
}

void Codons::setup_table() 
{
  const AminoAcidsWithStop& GAA = G->get_amino_acids();

  // Remove codons/letters in (*this) do not map to amino acids in *A.
  for(int i=size()-1; i>=0; i--) 
  {
    int n1 = sub_nuc(i,0);
    int n2 = sub_nuc(i,1);
    int n3 = sub_nuc(i,2);

    string aa_letter = GAA.letter(G->translate(n1,n2,n3));
    if (not A->contains(aa_letter))
      remove(i);
  }

  translation_table.resize( size() );
  setup_sub_nuc_table();
  setup_letter_classes();

  // Compute the indices for the remaining ones
  for(int i=0;i<size();i++) 
  {
    int n1 = sub_nuc(i,0);
    int n2 = sub_nuc(i,1);
    int n3 = sub_nuc(i,2);

    translation_table[i] = (*A)[ GAA.letter(G->translate(n1,n2,n3)) ];
  }
}

/// What amino acid does codon map to?
int Codons::translate(int codon) const
{
  if (codon == alphabet::gap or codon == alphabet::not_gap)
    return codon;

  assert(codon >= 0 and codon < translation_table.size() );

  int aa = -1;
  if (is_letter(codon))
    aa = translation_table[codon];
  else if (is_letter_class(codon))
  {
    for(int i=0;i<size();i++)
      if (matches(i,codon)) {
	int aa_ = translate(i);
	if (aa != -1 and aa != aa_)
	  return alphabet::not_gap;
	aa = aa_;
      }
  }
  return aa;
}


Codons::Codons(const Nucleotides& N1,const AminoAcids& A1, const Genetic_Code& G_)
  :Triplets(N1),A(A1),G(G_)
{
  setup_table();
  setup_sub_nuc_table();
  setup_letter_classes();

  name = string("Codons of ") + getNucleotides().name + " -> " + A1.name + " [" + G->name() + "]";
}

vector<object_ptr<const alphabet> > load_alphabets()
{
  vector<object_ptr<const alphabet> > alphabets; 

  alphabets.push_back(object_ptr<const alphabet>(new DNA));
  alphabets.push_back(object_ptr<const alphabet>(new RNA));
  alphabets.push_back(object_ptr<const alphabet>(new AminoAcids));
  alphabets.push_back(object_ptr<const alphabet>(new AminoAcidsWithStop));

  return alphabets;
}

object_ptr<const Genetic_Code> get_genetic_code(const string& name)
{
  if (name == "standard")
    return object_ptr<const Genetic_Code>(new Standard_Genetic_Code());
  else if (name == "mt-vert")
    return object_ptr<const Genetic_Code>(new Mt_Vertebrate_Genetic_Code());
  else if (name == "mt-invert")
    return object_ptr<const Genetic_Code>(new Mt_Invertebrate_Genetic_Code());
  else if (name == "mt-yeast")
    return object_ptr<const Genetic_Code>(new Mt_Yeast_Genetic_Code());
  else if (name == "mt-protozoan")
    return object_ptr<const Genetic_Code>(new Mt_Protozoan_Genetic_Code());
  else
    throw myexception()<<"I don't recognize genetic code name '"<<name<<"'.\n"
      "  Try one of 'standard', 'mt-vert', 'mt-invert', 'mt-protozoan', 'mt-yeast'.";
}

vector<object_ptr<const alphabet> > load_alphabets(const string& name_)
{
  vector<object_ptr<const alphabet> > alphabets; 

  string name = name_;
  vector<string> arguments = get_arguments(name,'[',']');

  if (name == "Codons" or name == "Codons*" or name == "Codons+stop")
  {
    object_ptr<const AminoAcids> AA;
    if (name == "Codons")
      AA = object_ptr<const AminoAcids>(new AminoAcids);
    else
      AA = object_ptr<const AminoAcids>(new AminoAcidsWithStop);
    
    object_ptr<const Genetic_Code> G(new Standard_Genetic_Code());
    if (arguments.size())
      G = get_genetic_code(arguments[0]);

    alphabets.push_back(object_ptr<const alphabet>(new Codons(DNA(),*AA,*G)));
    alphabets.push_back(object_ptr<const alphabet>(new Codons(RNA(),*AA,*G)));
  }
  else if (name == "Triplets") {
    alphabets.push_back(object_ptr<const alphabet>(new Triplets(DNA())));
    alphabets.push_back(object_ptr<const alphabet>(new Triplets(RNA())));
  }
  else if (name == "DNA")
    alphabets.push_back(object_ptr<const alphabet>(new DNA()));
  else if (name == "RNA")
    alphabets.push_back(object_ptr<const alphabet>(new RNA()));
  else if (name == "Amino-Acids" or name == "AA")
    alphabets.push_back(object_ptr<const alphabet>(new AminoAcids()));
  else if (name == "Amino-Acids+stop" or name == "AA*")
    alphabets.push_back(object_ptr<const alphabet>(new AminoAcidsWithStop()));
  else 
    throw myexception()<<"I don't recognize alphabet '"<<name<<"'";

  return alphabets;
}

