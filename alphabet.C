#include "alphabet.H"
#include <cassert>

using namespace std;

bad_letter::bad_letter(const string& l)
  :myexception(string("Letter '") + l + string("' not in alphabet.")),letter(l)
{}
bad_letter::bad_letter(const string& l,const string& name)
  :myexception(string("Letter '") + l + string("' not in alphabet '") + name + "'."),letter(l)
{}

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
    return missing.back();

  assert(0 <=i && i < data.size());
  return data[i];
}


bool operator==(const alphabet& a1,const alphabet& a2) {
  return a1.data == a2.data;
}

alphabet::alphabet(const string& s,const string& letters)
  :gap_letter("-"),name(s) 
{
  for(int i=0;i<letters.length();i++)
    data.push_back(string(1U,s[i]));

  missing.push_back("*");
}

alphabet::alphabet(const string& s,const string& letters,const string& m)
  :gap_letter("-"),name(s) 
{
  for(int i=0;i<letters.length();i++)
    data.push_back(string(1U,letters[i]));

  for(int i=0;i<m.length();i++) 
    missing.push_back(string(1U,m[i]));

  missing.push_back("*");
}

alphabet::alphabet(const string& s,const vector<string>& letters)
  :gap_letter("-"),name(s) 
{
  for(int i=0;i<letters.size();i++)
    data.push_back(letters[i]);

  missing.push_back("*");
}

alphabet::alphabet(const string& s,const vector<string>& letters,const vector<string>& m) 
  :gap_letter("-"),name(s) 
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


int Codons::sub_nuc(int codon,int pos) const {
  codon >>= pos;
  codon &= 4;
  return codon;
}

vector<string> getCodons(const Nucleotides& a) {
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


Codons::Codons(const Nucleotides& a)
  :alphabet(string("Codons of ")+a.name,getCodons(a)),N(a.clone())
{
  missing.back() = "***";
  gap_letter = "---";
}

Codons::Codons(const string& s,const Nucleotides& a)
  :alphabet(s,getCodons(a)),N(a.clone())
{
  missing.back() = "***";
  gap_letter = "---";
}


void Translation_Table::setup_table(const vector<string>& cc,const vector<string>& aa) {
  assert(cc.size() == aa.size());
  assert(cc.size() == C->size());
  for(int i=0;i<table.size();i++) {
    int cc_index = (*C)[ cc[i] ];
    int aa_index = (*A)[ aa[i] ];
    table[cc_index] = aa_index;
  }
}

Translation_Table::Translation_Table(const Codons& C1,const AminoAcids& A1,
				     const vector<string>& cc,const vector<string>& aa) 
  :C(C1),A(A1),table(C->size())
{
  setup_table(cc,aa);
}


Translation_Table::Translation_Table(const Codons& C1,const AminoAcids& A1,
				     istream& file) 
  :C(&C1),A(A1),table(C->size())
{
  vector<string> cc;
  vector<string> aa;
  for(int i=0;i<C->size();i++) {
    string temp;
    file>>temp;
    cc.push_back(temp);
    
    file>>temp;
    aa.push_back(temp);
  }
  
  setup_table(cc,aa);
}
