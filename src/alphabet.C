#include "alphabet.H"
#include <cassert>
#include <fstream>
#include "util.H"

using namespace std;

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

void alphabet::remove(const string& l) {
  int index = find_letter(l);
  letters_.erase(letters_.begin()+index);
  setup_letter_classes();
}

void alphabet::setup_letter_classes() {
  letter_classes_ = letters_;
  
  letter_masks_ = vector< vector<bool> >(n_letters(), vector<bool>(n_letters(),false) );
  for(int i=0;i<n_letters();i++)
    letter_masks_[i][i] = true;
}


void alphabet::insert_class(const string& l,const vector<bool>& mask) {
  if (includes(letters_,l))
    throw myexception()<<"Can't use letter name '"<<sanitize(l)<<"' as letter class name.";

  letter_classes_.push_back(l);
  letter_masks_.push_back(mask);
}

/// Add a letter class to the alphabet
void alphabet::insert_class(const std::string& l,const vector<string>& letters) 
{
  vector<bool> mask(size(),false);
  for(int i=0;i<letters.size();i++) {
    int index = find_letter(letters[i]);

    mask[index] = true;
  }

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

Nucleotides::Nucleotides(const string& s, char c)
  :alphabet(s,"","N")
{
  string t; t += c;

  insert("A");
  insert("G");
  insert(t);
  insert("C");

  insert_class("Y",t+"C");
  insert_class("R","AG");
  insert_class("W",t+"A");
  insert_class("S","GC");
}

DNA::DNA()
  :Nucleotides("DNA nucleotides",'T') 
{ }

RNA::RNA()
  :Nucleotides("RNA nucleotides",'U') 
{ }


AminoAcids::AminoAcids() 
  :alphabet("Amino-Acids","ARNDCQEGHILKMFPSTWYV","X")
{
  insert_class("B","DN");
  insert_class("Z","EQ");
}

AminoAcids::AminoAcids(const string& s, const string& letters) 
  :alphabet(s,string("ARNDCQEGHILKMFPSTWYV")+letters,"X")
{
  insert_class("B","DN");
  insert_class("Z","EQ");
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

    sub_nuc_table[i].resize(codon.length());

    for(int j=0; j<codon.length(); j++) {
      sub_nuc_table[i][j] = (*N)[ codon.substr(j,1) ];
    }
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
  vector<bool> empty_mask(size(),false);
  vector<bool> mask(size());

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

valarray<double> get_codon_frequencies_from_independant_nucleotide_frequencies(const Triplets& C,const valarray<double>& fN ) {
    valarray<double> fC(C.size());
    for(int i=0;i<fC.size();i++) {
      fC[i] = 1.0;
      for(int pos=0;pos<3;pos++)
	fC[i] *= fN[ C.sub_nuc(i,pos) ];
    }

    fC /= fC.sum();
    return fC;
}

valarray<double> Triplets::get_frequencies_from_counts(const valarray<double>& counts,double pseudocount) const {

  //--------- Level 1 pseudocount (nucleotides) ---------------//
  valarray<double> N_counts = get_nucleotide_counts_from_codon_counts(*this,counts);
  valarray<double> fN = getNucleotides().get_frequencies_from_counts(N_counts);
  valarray<double> prior_f = get_codon_frequencies_from_independant_nucleotide_frequencies(*this,fN);

  valarray<double> counts1 = counts + pseudocount*counts.size()*prior_f;

  valarray<double> f = counts1 /= counts1.sum();

  return f;
}

Triplets::Triplets(const Nucleotides& a)
  :alphabet(string("Triplets of ")+a.name,getTriplets(a)),N(a)
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

void Codons::setup_table(const vector<string>& cc,const vector<string>& aa) 
{
  // check that we actually have a one-to-one and onto mapping
  assert(cc.size() == aa.size());
  translation_table.clear();

  // Remove codons/letters in (*this) that map to amino acids not in *A.
  for(int i=cc.size()-1; i>=0; i--) 
  {
    // check that cc[i] is in the alphabet
    if (not contains(cc[i]))
      throw myexception()<<"Codon table has entry: "<<cc[i]<<" -> "<<aa[i]
			 <<", but alphabet does not contain "<<cc[i]<<".";

    // remove cc[i] if we don't recognize its amino acid
    if (not A->contains(aa[i]))
      remove(cc[i]);
  }

  translation_table.resize( size() );
  // Compute the indices for the remaining ones
  for(int i=0;i<translation_table.size();i++) 
  {
    if (not includes(cc,lookup(i)))
	throw myexception()<<"Codon table has no entry for codon '"<<lookup(i)<<"'!";

    int entry = find_index(cc,lookup(i));
    translation_table[i] = (*A)[ aa[entry] ];
  }
}

void Codons::setup_table(istream& file) 
{
  //------ Load the file ------//
  vector<string> cc;
  vector<string> aa;
  for(int i=0;i<size();i++) {
    string temp;
    file>>temp;
    cc.push_back(temp);
    
    file>>temp;
    aa.push_back(temp);
  }
  
  //---- Convert U to U/T ----//
  string TT = N->lookup(2);
  assert(TT.size() == 1);
  char T = TT[0];

  for(int i=0;i<cc.size();i++)
    for(int j=0;j<cc[i].size();j++)
      if (cc[i][j] == 'U')
	cc[i][j] = T;

  //---- Setup the table ----//
  setup_table(cc,aa);
}
				    
void Codons::setup_table(const string& filename) {
  ifstream genetic_code(filename.c_str());
  if (not genetic_code)
    throw myexception()<<"Couldn't open file '"<<filename<<"'";

  setup_table(genetic_code);

  genetic_code.close();
}


/// What amino acid does codon map to?
int Codons::translate(int codon) const
{
  if (codon == alphabet::gap or codon == alphabet::not_gap)
    return codon;

  assert(codon >= 0 and codon < translation_table.size() );
  return translation_table[codon];
}


Codons::Codons(const Nucleotides& N1,const AminoAcids& A1,
	       const vector<string> cc,const vector<string> aa) 
  :Triplets(N1),A(A1)
{
  setup_table(cc,aa);
  setup_sub_nuc_table();

  name = string("Codons of ") + getNucleotides().name + " -> " + A1.name;
}


Codons::Codons(const Nucleotides& N1,const AminoAcids& A1,
	       istream& file) 
  :Triplets(N1),A(A1)
{
  setup_table(file);
  setup_sub_nuc_table();

  name = string("Codons of ") + getNucleotides().name + " -> " + A1.name;
}

Codons::Codons(const Nucleotides& N1,const AminoAcids& A1,
	       const string& filename) 
  :Triplets(N1),A(A1)
{
  setup_table(filename);
  setup_sub_nuc_table();

  name = string("Codons of ") + getNucleotides().name + " -> " + A1.name;
}
