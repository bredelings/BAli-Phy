#include <algorithm>
#include "alignment.H"
#include "myexception.H"

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

// use 'swap' instead of '=' at end?  initialize array2 with '-'?
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

void alignment::print_phylip(std::ostream& file,bool othernodes) const {
  const alphabet& a = get_alphabet();
  file<<num_sequences()<<" "<<length()<<endl;

  const int header_length = 10;
  const int line_length = 70;

  int pos=0;
  while(pos<length()) {
    int start = pos;
    int end = pos + (line_length - header_length);
    int nsequences = num_sequences();
    if (othernodes)
      nsequences = size2();
    for(int seq = 0;seq < nsequences;seq++) {
      string header = string(header_length,' ');
      if (pos == 0 and seq<num_sequences()) {
	string name = sequences[seq].name;
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
