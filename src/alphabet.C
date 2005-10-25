#include "alphabet.H"
#include <cassert>
#include <fstream>
#include "util.H"

using namespace std;

bad_letter::bad_letter(const string& l)
  :myexception(string("Letter '") + l + string("' not in alphabet.")),letter(l)
{}
bad_letter::bad_letter(const string& l,const string& name)
  :myexception(string("Letter '") + l + string("' not in alphabet '") + name + "'."),letter(l)
{}

bool alphabet::contains(char l) const {
  string s(1U,l);
  return contains(s);
}

bool alphabet::contains(const std::string& l) const {
  // Check the letters
  for(int i=0;i<size();i++) {
    if (data[i]==l)
      return true;
  }
  return false;
}


int alphabet::operator[](char l) const {
  string s(1U,l);  
  return (*this)[s];
}

int alphabet::operator[](const string& l) const {
  // Check the letters
  for(int i=0;i<size();i++) {
    if (data[i]==l)
      return i;
  }

  // Check for a gap
  if (l == gap_letter) 
    return alphabet::gap;

  // Check the symbols for any_letter
  for(int i=0;i<missing.size();i++) {
    if (missing[i] == l)
      return alphabet::not_gap;
  }

  // Check for unknown
  if (l == unknown_letter)
    return alphabet::unknown;

  // We don't have this letter!
  throw bad_letter(l,name);
}

vector<int> alphabet::operator() (const string& s) const{
  const int lsize = data[0].size();

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
    return missing.front();
  else if (i == unknown)
    return unknown_letter;

  assert(0 <=i && i < data.size());
  return data[i];
}


bool operator==(const alphabet& a1,const alphabet& a2) {
  return a1.data == a2.data;
}

void alphabet::insert(const string& l) {
  data.push_back(l);
  throw myexception()<<"Not implemented yet: adding letters to alphabets.";
}

void alphabet::remove(const string& l) {
  int index = operator[](l);
  data.erase(data.begin()+index);
}

valarray<double> alphabet::get_frequencies_from_counts(const valarray<double>& counts,double pseudocount) const {

  valarray<double> f = counts;
  for(int i=0;i<f.size();i++)
    f[i] += pseudocount;

  f /= f.sum();

  return f;
}


alphabet::alphabet(const string& s,const string& letters)
  :name(s),gap_letter("-"),unknown_letter("?")
{
  for(int i=0;i<letters.length();i++)
    data.push_back(string(1U,s[i]));

  missing.push_back("*");
}

alphabet::alphabet(const string& s,const string& letters,const string& m)
  :name(s),gap_letter("-"),unknown_letter("?")
{
  for(int i=0;i<letters.length();i++)
    data.push_back(string(1U,letters[i]));

  for(int i=0;i<m.length();i++) 
    missing.push_back(string(1U,m[i]));

  missing.push_back("*");
}

alphabet::alphabet(const string& s,const vector<string>& letters)
  :name(s),gap_letter("-"),unknown_letter("?")
{
  for(int i=0;i<letters.size();i++)
    data.push_back(letters[i]);

  missing.push_back("*");
}

alphabet::alphabet(const string& s,const vector<string>& letters,const vector<string>& m) 
  :name(s),gap_letter("-"),unknown_letter("?")
{
  for(int i=0;i<letters.size();i++)
    data.push_back(letters[i]);

  for(int i=0;i<m.size();i++) 
    missing.push_back(m[i]);

  missing.push_back("*");

}

Nucleotides::Nucleotides(const string& s, char c)
  :alphabet(s,"AGTC")
{
  data[T()] = string(1U,c);
}

Nucleotides::Nucleotides(const string& s, char c,const string& m)
  :alphabet(s,"AGTC",m)
{
  data[T()] = string(1U,c);
}

DNA::DNA()
  :Nucleotides("DNA nucleotides",'T',"NYR") 
{}

RNA::RNA()
  :Nucleotides("RNA nucleotides",'U',"NYR") 
{}


AminoAcids::AminoAcids() 
  :alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X")
{}

AminoAcidsWithStop::AminoAcidsWithStop() 
{
  name = "Amino Acids + stop";
  data.push_back("!");
}


void Triplets::setup_sub_nuc_table() {
  sub_nuc_table.clear();
  sub_nuc_table.resize(size());

  assert(N->width() == 1);

  for(int i=0;i<sub_nuc_table.size();i++) {
    const string& codon = data[i];

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

vector<string> getTriplets(const Nucleotides& a) {
  vector<string> v;
  for(int i=0;i<a.size();i++) {
    string s1 = a.lookup(i);
    for(int j=0;j<a.size();j++) {
      string s2 = s1 + a.lookup(j);
      for(int k=0;k<a.size();k++) {
	string s3 = s2 + a.lookup(k);
	v.push_back(s3);
      }
    }
  }
  return v;
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
  // compute our 'missing' letters
  missing.resize(N->missing.size());
  for(int i=0;i<missing.size();i++)
    missing[i] = N->missing[i]+N->missing[i]+N->missing[i];

  // compute our 'gap' letter
  gap_letter = N->gap_letter + N->gap_letter + N->gap_letter;

  // compute our 'unknown' letter
  unknown_letter = N->unknown_letter + N->unknown_letter + N->unknown_letter;

  setup_sub_nuc_table();
}

Triplets::Triplets(const string& s,const Nucleotides& a)
  :alphabet(s,getTriplets(a)),N(a)
{
  // compute our 'missing' letters
  missing.resize(N->missing.size());
  for(int i=0;i<missing.size();i++)
    missing[i] = N->missing[i]+N->missing[i]+N->missing[i];

  // compute our 'gap' letters
  gap_letter = N->gap_letter + N->gap_letter + N->gap_letter;

  // compute our 'unknown' letter
  unknown_letter = N->unknown_letter + N->unknown_letter + N->unknown_letter;

  setup_sub_nuc_table();
}

// FIXME - should I make a separate class that removes stop codons?
void Codons::setup_table(vector<string> cc,vector<string> aa) 
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

void Codons::setup_table(istream& file) {
  vector<string> cc;
  vector<string> aa;
  for(int i=0;i<size();i++) {
    string temp;
    file>>temp;
    cc.push_back(temp);
    
    file>>temp;
    aa.push_back(temp);
  }
  
  setup_table(cc,aa);
}
				    
void Codons::setup_table(const string& filename) {
  ifstream genetic_code(filename.c_str());
  if (not genetic_code)
    throw myexception()<<"Couldn't open file '"<<filename<<"'";

  setup_table(genetic_code);

  genetic_code.close();
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
