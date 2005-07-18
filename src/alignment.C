#include <algorithm>
#include <sstream>
#include "alignment.H"
#include "myexception.H"
#include "util.H"
#include "rng.H"

using std::string;

void resize(ublas::matrix<int>& M1,int s1,int s2,int clear=0)
{
  ublas::matrix<int> M2(s1,s2);

  for(int i=0;i<M2.size1();i++)
    for(int j=0;j<M2.size2();j++)
      M2(i,j) = clear;

  for(int i=0;i<M1.size1() and i<M2.size1();i++)
    for(int j=0;j< M1.size2() and j<M2.size2();j++)
      M2(i,j) = M1(i,j);

  M1.swap(M2);
}

int alignment::add_note(int l) const {
  notes.push_back(ublas::matrix<int>(length()+1,l));
  return notes.size()-1;
}

bool all_gaps(const alignment& A,int column,const std::valarray<bool>& mask) {
  for(int i=0;i<A.n_sequences();i++)
    if (mask[i] and not A.gap(column,i))
      return false;
  return true;
}

bool all_gaps(const alignment& A,int column) {
  for(int i=0;i<A.n_sequences();i++)
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

int alignment::index(const string& s) const {
  for(int i=0;i<sequences.size();i++) 
    if (sequences[i].name == s) return i;

  return -1;
}

void alignment::changelength(int l) 
{
  array.resize(l,array.size2());

  for(int i=0;i<notes.size();i++)
    notes[i].resize(l+1,notes[i].size2());
}

void alignment::delete_column(int column) {
  for(int i=0;i<n_sequences();i++) 
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
  a = A.a;

  sequences = A.sequences;

  array.resize(A.array.size1(),A.array.size2());
  array = A.array;

  notes = A.notes;

  return *this;
}

void alignment::add_row(const vector<int>& v) {
  int new_length = std::max(length(),(int)v.size());

  ::resize(array,new_length,n_sequences()+1,-1);

  for(int position=0;position<v.size();position++)
    array(position,array.size2()-1) = v[position];
}


void alignment::del_sequence(int ds) {
  assert(0 <= ds and ds < n_sequences());

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

void alignment::add_sequence(const sequence& s) 
{
  add_row((*a)(s));

  sequences.push_back(s);
  sequences.back().strip_gaps();
}

void alignment::load(const vector<sequence>& seqs) {
  clear();

  // Add the sequences to the alignment
  for(int i=0;i<seqs.size();i++)
    add_sequence(seqs[i]);
}

void alignment::load(const vector<OwnedPointer<alphabet> >& alphabets,const vector<sequence>& seqs) {
  string errors = "Sequences don't fit any of the alphabets:";
  for(int i=0;i<alphabets.size();i++) {
    try {
      a = alphabets[i];
      load(seqs);
      break;
    }
    catch (bad_letter& e) {
      a = NULL;
      errors += "\n";
      errors += e.what();
      if (i<alphabets.size()-1)
	;
      else
	throw myexception(errors);
    }
  }
}

void alignment::load(sequence_format::loader_t loader,std::istream& file) 
{
  // read file
  vector<sequence> seqs = loader(file);

  // load sequences into alignment
  load(seqs);
}


void alignment::load(const vector<OwnedPointer<alphabet> >& alphabets, sequence_format::loader_t loader,
			       std::istream& file) 
{
  // read file
  vector<sequence> seqs = loader(file);

  // load sequences into alignment
  load(alphabets,seqs);
}


string get_extension(const string& s) {
  int pos = s.rfind('.');
  if (pos == -1)
    return "";
  else
    return s.substr(pos);
}

void alignment::load(const string& filename) 
{
  // read from file
  vector<sequence> seqs = sequence_format::load_from_file(filename);

  // load sequences into alignment
  load(seqs);
}

void alignment::load(const vector<OwnedPointer<alphabet> >& alphabets,const string& filename) {
  // read from file
  vector<sequence> seqs = sequence_format::load_from_file(filename);

  // load sequences into alignment
  load(alphabets,seqs);
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

vector<sequence> alignment::get_sequences() const 
{

  vector<sequence> seqs(n_sequences());

  for(int i=0;i<seqs.size();i++) {
    seqs[i].name = sequences[i].name;
    seqs[i].comment = sequences[i].comment;

    string& letters = seqs[i];
    letters = "";
    for(int c=0;c<length();c++)
      letters += a->lookup( (*this)(c,i) );
  }

  return seqs;
}

void alignment::write_sequences(sequence_format::dumper_t method,std::ostream& file) const {
  vector<sequence> seqs = get_sequences();
  (*method)(file,seqs);
}

void alignment::print_fasta(std::ostream& file) const {
  write_sequences(sequence_format::write_fasta,file);
}

void alignment::print_phylip(std::ostream& file) const {
  write_sequences(sequence_format::write_phylip,file);
}

alignment::alignment(const alphabet& a1) 
  :a(a1)
{}

alignment::alignment(const alphabet& a1,const string& filename) 
    :a(a1)
{ 
  load(filename); 
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

std::ostream& operator<<(std::ostream& file,const alignment& A) {
  A.print_fasta(file);
  return file;
}

std::istream& operator>>(std::istream& file,alignment& A) {
  A.load(sequence_format::read_guess,file);
  return file;
}

