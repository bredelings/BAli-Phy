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
  sequences = A.sequences;

  array.resize(A.array.size1(),A.array.size2());
  array = A.array;

  return *this;
}

void alignment::add_row(const vector<int>& v) {
  int new_length = std::max(length(),(int)v.size());

  resize(new_length,size2()+1);

  for(int position=0;position<v.size();position++)
    array(position,size2()-1) = v[position];
}


void alignment::add_sequence(const sequence& s) {
  if (sequences.size()>1)   // All the sequences should have the same alphabet
    assert(s.a == get_alphabet());

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

void alignment::load(const vector<alphabet>& alphabets,const vector<string>& names, const vector<string>& sequences) {

  bool success = false;
  for(int i=0;i<alphabets.size();i++) {
    try {
      load(alphabets[i],names,sequences);
      success=true;
      break;
    }
    catch (bad_letter& e) {
      if (i<alphabets.size()-1)
	std::cerr<<"Exception: "<<e.what()<<endl;
      else
	throw e;
    }
  }
}

void alignment::load_sequences(const alphabet& a, sequence_format::loader_t loader,
		    std::istream& file) {

  // read file
  vector<string> names;
  vector<string> sequences;
  loader(file,names,sequences);

  // load sequences into alignment
  load(a,names,sequences);
}


void alignment::load_sequences(const vector<alphabet>& alphabets, sequence_format::loader_t loader,
		    std::istream& file) {

  // read file
  vector<string> names;
  vector<string> sequences;
  loader(file,names,sequences);

  // load sequences into alignment
  load(alphabets,names,sequences);
}


void alignment::load(const alphabet& a,const std::string& filename) {
  std::ifstream file(filename.c_str());

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

void alignment::load(const vector<alphabet>& alphabets,const std::string& filename) {
  std::ifstream file(filename.c_str());

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

void alignment::print_phylip(std::ostream& file,bool othernodes) const {
  const alphabet& a = get_alphabet();

  // Write header
  file<<num_sequences()<<" "<<length()<<endl;

  // Find length of longest name
  int max_name_length=0;
  for(int i=0;i<sequences.size();i++)
    if (sequences[i].name.size() > max_name_length)
      max_name_length = sequences[i].name.size();

  const int header_length = std::max(10,max_name_length+2);
  const int line_length = std::max(70,header_length+60);
  int nsequences = num_sequences();
  if (not othernodes)
    nsequences = nsequences/2+1;

  int pos=0;
  while(pos<length()) {
    int start = pos;
    int end = pos + (line_length - header_length);

    for(int seq = 0;seq < nsequences;seq++) {

      // get the line header (e.g. sequence name or spaces)
      string header = string(header_length,' ');
      if ((pos == 0) and seq<num_sequences()) {
	string name = sequences[seq].name;
	assert(name.size() <= header_length-2);

	header = name + string(header_length-name.size(),' ');
      }

      // write out the line
      file<<header;
      for(int column=start;column<end and column<length();column++)
	file<<a.lookup(array(column,seq));
      file<<endl;
    }
    // write one blank line;
    file<<endl;
    pos = end;
  }
}

void alignment::print_fasta(std::ostream& file) const {
  const alphabet& a = get_alphabet();
  for(int i=0;i<sequences.size();i++) {
    file<<">"<<sequences[i].name<<endl;
    int column=0;
    while(column < length()) {
      int start = column;
      for(;column < length() and column < start+50;column++)
	file<<a.lookup(array(column,i));
      file<<endl;
    }
  }
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

std::valarray<double> empirical_frequencies(const alignment& A) {
  std::valarray<double> f(0.0,A.get_alphabet().size());

  double total=0;
  for(int i=0;i<A.length();i++) {
    for(int j=0;j<A.size2();j++) {
      if (alphabet::letter(A(i,j))) {
	total++;
	f[A(i,j)]++;
      }
    }
  }

  // Use Pseudo-counts to stability the estimates
  const double count = 10;
  for(int i=0;i<f.size();i++)
    f[i] += count/f.size();
  total += count;


  f /= total;
  return f;
}

void remove_empty_columns(alignment& A) {
  for(int column=A.length()-1;column>=0;column--) {
    if (all_gaps(A,column)) {
      A.delete_column(column);
      std::cerr<<"Deleted a column: all gaps"<<std::endl;
    }
  }
}

alignment randomize(const alignment& A) {
  int maxlength = -1;
  for(int s=0;s<A.size2();s++) {
    if (A.seqlength(s) > maxlength)
      maxlength = A.seqlength(s);
  }

  alignment A2 = A;
  int newlength = int( maxlength + 2 + 0.1*maxlength*(A2.size2()-1) );
  A2.changelength(newlength);

  const int temp = alphabet::gap;
  for(int i=0;i<A.num_sequences();i++) {
    vector<int> s = A.seq(i);
    while(s.size() < newlength) {
      int pos = myrandom(s.size()+1);
      s.insert(s.begin()+pos,temp);
    }
    for(int c=0;c<A2.length();c++)
      A2(c,i) = s[c];
  }

  remove_empty_columns(A2);
  std::cerr<<A2<<endl;
  return A2;
}
