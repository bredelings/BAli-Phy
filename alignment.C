#include <algorithm>
#include <sstream>
#include "alignment.H"
#include "myexception.H"
#include "util.H"

int num_non_gaps(const alignment& A,int column) {
  int count=0;
  for(int i=0;i<A.size2();i++) 
    if (not A.gap(column,i))
      count++;
  return count;
}

bool valid(const alignment& A) {
  for(int column=0;column<A.length();column++)
    if (num_non_gaps(A,column)==0) {
      assert(0);
      return false;
    }
  return true;
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

bool alignment::changelength(int l) {
  bool can_do=true;
  if (l < length()) {
    for(int i=0;i<sequences.size();i++)
      if (l < sequences[i].size())
	can_do = false;
  }
  if (not can_do) return false;

  resize(l,array.size2());

  return true;
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

// i -> mapping[i]
void alignment::remap(const vector<int>& mapping) {
  ublas::matrix<int> old_array = array;
  for(int column=0;column<array.size1();column++) {
    for(int i=0;i<sequences.size();i++)
      array(column,mapping[i]) = old_array(column,i);
  }

  vector<sequence> old_order = sequences;
  
  for(int i=0;i<sequences.size();i++)
    sequences[mapping[i]] = old_order[i];
}

void alignment::remap(const vector<string>& order) {
  if (order.size() > sequences.size())
    throw myexception("Too many sequences in tree");

  vector<int> mapping(sequences.size());
  for(int i=0;i<mapping.size();i++) {
    int target = -1;
    for(int j=0;j<order.size() and target == -1;j++) {
      if (order[j] == sequences[i].name)
	target = j;
    }
    if (target == -1)
      throw myexception(string("Couldn't find sequence \"")+sequences[i].name+"\" in tree");
    mapping[i] = target;
  }

  remap(mapping);
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

void alignment::load_fasta(const alphabet& a,const std::string& filename) {
  vector<sequence> sequences = ::load_fasta(a,filename);
  for(int i=0;i<sequences.size();i++)
    add_sequence(sequences[i]);
}

void alignment::load_fasta(const alphabet& a,std::istream& file) {
  vector<sequence> sequences = ::load_fasta(a,file);
  for(int i=0;i<sequences.size();i++)
    add_sequence(sequences[i]);
}

bool phylip_section(std::istream& file,int ntaxa, vector<string>& names,vector<string>& letters) {
  names.clear();
  letters.clear();
  int indent = -1;
  for(int i=0;i<ntaxa;i++) {
    string line;
    getline(file,line);
    while(i==0 and line.size()==0 and file.good())
      getline(file,line);
    if (not file) return false;

    int pos = line.find_first_not_of(" \t");
    if (pos >9)
      names.push_back(string(""));
    else {
      int name_start = pos;
      pos = line.find_first_of(" \t",pos);
      names.push_back(line.substr(name_start,pos));
    
      pos = std::max(pos,9);
    }
    pos = line.find_first_not_of(" \t",pos+1);
    if (indent == -1)
      indent = pos;
    else
      assert(indent == pos);
    letters.push_back(line.substr(indent));
  }
  return file;
}

void alignment::load_phylip(const alphabet& a,const std::string& filename) {
  ifstream file(filename.c_str());

  string line;
  getline(file,line);
  int ntaxa = -1;
  int length = -1;
  {
    std::istringstream linestream(line);
    linestream>>ntaxa;
    linestream>>length;
  }

  vector<string> sequences;
  vector<string> letters;
  vector<string> names;
  vector<string> names2;
  phylip_section(file,ntaxa,names,sequences);
  while(phylip_section(file,ntaxa,names2,letters)) {
    for(int i=0;i<ntaxa;i++)
      sequences[i] += letters[i];
  }

  for(int i=0;i<ntaxa;i++) 
    sequences[i] = strip(sequences[i],' ');

  for(int i=0;i<ntaxa;i++) {
    sequence s(a);
    s.parse(string(">")+names[i],sequences[i]);
    add_sequence(s);
  }
    
  if (length > 0 and length != this->length())
    throw myexception(string("Sequences in file '") + filename + "' have length " + convertToString(this->length()) + " instead of specified length " + convertToString(length) + ".");

  for(int i=0;i<ntaxa;i++) {
    std::cerr<<">"<<names[i]<<endl;
    std::cerr<<sequences[i]<<endl;
    std::cerr<<endl;
  }
}

void alignment::load(const alphabet& a,const std::string& filename) {
  if (filename.substr(filename.size()-4) == ".phy")
    load_phylip(a,filename);
  else
    load_fasta(a,filename);
}

void alignment::gap_fixup(int n1,int n2,int g1,int g2,int m) {
  std::cerr<<"FIXME: in sequences "<<n1<<" and "<<n2<<" we have \
  a g2 @ "<<g2<<" followed by a g1 @ "<<g1<<std::endl;
  assert(0);
}

//FIXME - make all internal nodes present - see how that does
//FIXME - also try un-aligning everything - see if we can rediscover
//        the right alignment
//FIXME - make this not a member function
void alignment::create_internal(const SequenceTree& T) {
  remap(T.get_sequences());

  resize(array.size1(),T.num_nodes()-1);

  // Create Internal Nodes
  for(int column=0;column< length();column++) {
    vector<int> present_leaf(T.leaves());
    for(int i=0;i<T.leaves();i++)
      present_leaf[i] = not gap(column,i);
    TreeFunc<int> present = mark_tree(present_leaf,T);
    for(int i=T.leaves();i<array.size2();i++) {
      if (present(i))
	array(column,i) = alphabet::not_gap;
      else
	array(column,i) = alphabet::gap;
    }
  }

  // Make sure there are no G2->G1 transitions
  for(int b=0;b<T.branches();b++) {
    int n1 = T.branch(b).parent();
    int n2 = T.branch(b).child();

    int g1 = -1,g2 = -1;
    for(int column=0;column<length();column++) {
      if (gap(column,n1) and not gap(column,n2)) { //G1
	if (g1 != -1) 
	  g1 = column;
      }
      else if (not gap(column,n1) and gap(column,n2)) { //G2
	if (g2 != -1) 
	  g2 = column;
      }    
      else {
	if (g1 != -1 and g2 != -1 and g1 > g2) {
	  gap_fixup(n1,n2,g1,g2,column);
	}
	
	g1 = -1;
	g2 = -1;
      }
    }
  }

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
#ifndef NDEBUG
  for(int i=0;i<sequences.size();i++) {
    vector<int> s;
    for(int column=0;column<length();column++)
      if (not gap(column,i))
	s.push_back(array(column,i));

    assert(s == sequences[i]);
  }
#endif
}

string shorten_name(const string& s,int good_length = 9) {
  string s2 = s;
  int pos = s2.find('.');
  if (s.size() > good_length and pos != -1 and pos > 1) {
    int species_length = s.size()-pos;
    int genuslength = good_length - species_length;
    if (genuslength<2)
      genuslength=2;
    
    s2 = s2.substr(0,genuslength) + s2.substr(pos);
    
  }
  return s2;
}

void alignment::print_phylip(std::ostream& file,bool othernodes) const {
  const alphabet& a = get_alphabet();
  file<<num_sequences()<<" "<<length()<<endl;

  const int header_length = 15;
  const int line_length = 70;

  bool always_print_names = true;

  int pos=0;
  while(pos<length()) {
    int start = pos;
    int end = pos + (line_length - header_length);
    int nsequences = num_sequences();
    if (othernodes)
      nsequences = size2();
    for(int seq = 0;seq < nsequences;seq++) {
      string header = string(header_length,' ');
      if ((pos == 0 or always_print_names) and seq<num_sequences()) {
	string name = shorten_name(sequences[seq].name,header_length-2);
	if (name.size() > (header_length-2))
	  name = name.substr(0,header_length-2);
	header = name + string(header_length-name.size(),' ');
      }
      file<<header;
      for(int column=start;column<end and column<length();column++)
	file<<a.lookup(array(column,seq));
      file<<endl;
    }
    file<<endl<<endl;
    pos = end;
  }

#ifndef NDEBUG
  for(int i=0;i<sequences.size();i++) {
    vector<int> s;
    for(int column=0;column<length();column++)
      if (not gap(column,i))
	s.push_back(array(column,i));

    assert(s == sequences[i]);
  }
#endif
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

#ifndef NDEBUG
  for(int i=0;i<sequences.size();i++) {
    vector<int> s;
    for(int column=0;column<length();column++)
      if (not gap(column,i))
	s.push_back(array(column,i));

    assert(s == sequences[i]);
  }
#endif
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

  int total=0;
  for(int i=0;i<A.length();i++) {
    for(int j=0;j<A.size2();j++) {
      if (alphabet::letter(A(i,j))) {
	total++;
	f[A(i,j)]++;
      }
    }
  }
  f /= total;
  return f;
}

void remove_empty_columns(alignment& A) {
  for(int column=A.length()-1;column>=0;column--) {
    bool only_internal = (num_non_gaps(A,column) == 0);

    if (only_internal) {
      A.delete_column(column);
      std::cerr<<"Deleted a column!"<<std::endl;
    }
  }
}
