#include <algorithm>
#include <sstream>
#include "alignment.H"
#include "myexception.H"
#include "util.H"
#include "rng.H"

using std::string;

bool all_gaps(const alignment& A,int column,const std::valarray<bool>& mask) {
  for(int i=0;i<A.size2();i++)
    if (mask[i] and not A.gap(column,i))
      return false;
  return true;
}

bool all_gaps(const alignment& A,int column) {
  for(int i=0;i<A.size2();i++)
    if (not A.gap(column,i))
      return false;
  return true;
}

bool valid(const alignment& A) {
  for(int column=0;column<A.length();column++)
    if (all_gaps(A,column))
      return false;
  return true;
}

void alignment::clear() {
  sequences.clear();
  array.resize(0,0);
}

void alignment::resize(int s1,int s2) {
  ublas::matrix<int> array2(s1,s2);
  
  for(int i=0;i<array2.size1();i++)
    for(int j=0;j<array2.size2();j++)
      array2(i,j) = alphabet::gap;

  for(int i=0;i<array.size1() and i<array2.size1();i++)
    for(int j=0;j< array.size2() and j<array2.size2();j++)
      array2(i,j) = array(i,j);

  array.swap(array2);
}

int alignment::index(const string& s) const {
  for(int i=0;i<sequences.size();i++) 
    if (sequences[i].name == s) return i;

  return -1;
}

void alignment::changelength(int l) {
  resize(l,array.size2());
}

void alignment::delete_column(int column) {
  for(int i=0;i<size2();i++) 
    assert(array(column,i) == alphabet::gap);

  ublas::matrix<int> array2(array.size1()-1,array.size2());
  
  for(int i=0;i<array2.size1();i++)
    for(int j=0;j<array2.size2();j++) {
      int c = i;
      if (c>=column) c++;
      array2(i,j) = array(c,j);
    }
  
  array.swap(array2);
}

int alignment::seqlength(int i) const {
  int count =0;
  for(int column=0;column<length();column++) {
    if (not gap(column,i))
      count++;
  }
  return count;
}

alignment& alignment::operator=(const alignment& A) {
  letters_OK(*this,"operator=(): in");
  letters_OK(A,"operator=(): before");
  sequences = A.sequences;

  array.resize(A.array.size1(),A.array.size2());
  array = A.array;
  letters_OK(A,"operator=(): after");
  letters_OK(*this,"operator=(): out");

  return *this;
}

void alignment::add_row(const vector<int>& v) {
  int new_length = std::max(length(),(int)v.size());

  resize(new_length,size2()+1);

  for(int position=0;position<v.size();position++)
    array(position,size2()-1) = v[position];
}


void alignment::del_sequence(int ds) {
  assert(0 <= ds and ds < size2());

  //----------- Fix the sequence list -------------//
  sequences.erase(sequences.begin()+ds);

  //-------------- Alter the matrix ---------------//
  ublas::matrix<int> array2(array.size1(),array.size2()-1);
  
  for(int i=0;i<array2.size1();i++)
    for(int j=0;j<array2.size2();j++) {
      int s = j;
      if (s>=ds) s++;
      array2(i,j) = array(i,s);
    }
  
  array.swap(array2);
}

void alignment::add_sequence(const sequence& s) {
  if (sequences.size()>1)   // All the sequences should have the same alphabet
    assert(s.Alphabet() == get_alphabet());

  add_row(s);

  sequences.push_back(s);
  sequences.back().strip_gaps();
}

void alignment::load(const alphabet& a,const vector<string>& names, const vector<string>& sequences) {
  clear();

  // load and check number of taxa
  int ntaxa = names.size();
  assert(sequences.size() == ntaxa);

  // Add the sequences to the alignment
  for(int i=0;i<ntaxa;i++) {
    sequence s(a);
    s.parse(string(">")+names[i],sequences[i]);
    add_sequence(s);
  }
}

void alignment::load(const vector<OwnedPointer<alphabet> >& alphabets,const vector<string>& names, const vector<string>& sequences) {

  bool success = false;
  for(int i=0;i<alphabets.size();i++) {
    try {
      load(*alphabets[i],names,sequences);
      success=true;
      break;
    }
    catch (bad_letter& e) {
      if (i<alphabets.size()-1)
	;//	std::cerr<<"Exception: "<<e.what()<<endl;
      else
	throw e;
    }
  }
}

void alignment::load_sequences(const alphabet& a, sequence_format::loader_t loader,
		    std::istream& file) {

  if (not file)
    throw myexception()<<"Reading sequences: bad file!";

  // read file
  vector<string> names;
  vector<string> sequences;
  loader(file,names,sequences);

  // load sequences into alignment
  load(a,names,sequences);
}


void alignment::load_sequences(const vector<OwnedPointer<alphabet> >& alphabets, sequence_format::loader_t loader,
		    std::istream& file) {

  if (not file)
    throw myexception()<<"Reading sequences: bad file!";

  // read file
  vector<string> names;
  vector<string> sequences;
  loader(file,names,sequences);

  // load sequences into alignment
  load(alphabets,names,sequences);
}


void alignment::load(const alphabet& a,const std::string& filename) {
  std::ifstream file(filename.c_str());

  if (not file)
    throw myexception()<<"Couldn't open file '"<<filename<<"'";

  string extension = filename.substr(filename.size()-4);
  if (extension == ".phy")
    load_sequences(a,sequence_format::read_phylip,file);
  else if (extension == ".fasta")
    load_sequences(a,sequence_format::read_fasta,file);
  else
    throw myexception()<<"Alignment file \""<<filename<<"\" doesn't have a recognized extension.";

  file.close();
}

string get_extension(const string& s) {
  int pos = s.rfind('.');
  if (pos == -1)
    return "";
  else
    return s.substr(pos);
}

void alignment::load(const vector<OwnedPointer<alphabet> >& alphabets,const std::string& filename) {
  std::ifstream file(filename.c_str());

  if (not file)
    throw myexception()<<"Couldn't open file '"<<filename<<"'";

  string extension = get_extension(filename);
  if (extension == ".phy")
    load_sequences(alphabets,sequence_format::read_phylip,file);
  else if (extension == ".fasta")
    load_sequences(alphabets,sequence_format::read_fasta,file);
  else
    throw myexception()<<"Alignment file \""<<filename<<"\" doesn't have a recognized extension.";

  file.close();
}

void alignment::print(std::ostream& file) const{
  const alphabet& a = get_alphabet();
  file<<length()<<endl;
  for(int start = 0;start<length();) {

    int end = start + 80;
    if (end > length()) end = length();

    for(int i=0;i<array.size2();i++) {
      for(int column=start;column<end;column++) {
	file<<a.lookup(array(column,i));
      }
      file<<endl;
    }

    start = end;
    file<<endl<<endl;
  }
}

void alignment::prepare_write(vector<string>& names,vector<string>& letters,bool othernodes) const {
  const alphabet& a = get_alphabet();

  int N = num_sequences();
  if (not othernodes)
    N = N/2+1;

  for(int i=0;i<N;i++) {
    string letters_i;
    for(int column=0;column<length();column++) 
      letters_i += a.lookup((*this)(column,i));

    names.push_back(sequences[i].name);
    letters.push_back(letters_i);
  }
}

void alignment::write_sequences(sequence_format::dumper_t method,std::ostream& file,bool othernodes) const {
  vector<string> names;
  vector<string> sequences;
  prepare_write(names,sequences,othernodes);
  (*method)(file,names,sequences);
}

void alignment::print_fasta(std::ostream& file) const {
  write_sequences(sequence_format::write_fasta,file,true);
}

void alignment::print_phylip(std::ostream& file,bool othernodes) const {
  write_sequences(sequence_format::write_phylip,file,othernodes);
}

vector<int> get_path(const alignment& A,int node1, int node2) {
  vector<int> state;

  state.reserve(A.length()+1);
  for(int column=0;column<A.length();column++) {
    if (A.gap(column,node1)) {
      if (A.gap(column,node2)) 
	continue;
      else
	state.push_back(1);
    }
    else {
      if (A.gap(column,node2))
	state.push_back(2);
      else
	state.push_back(0);
    }
  }
  
  state.push_back(3);
  return state;
}

void remove_empty_columns(alignment& A) {
  for(int column=A.length()-1;column>=0;column--) {
    if (all_gaps(A,column)) {
      A.delete_column(column);
      std::cerr<<"Deleted a column: all gaps"<<std::endl;
    }
  }
}
