#include <algorithm>
#include "alignment.H"
#include "possibilities.H"

void alignment::pad(int column) {
  for(int i=0;i<sequences.size();i++)
    for(int j=column;j<length();j++)
      array(j,i) = alphabet::gap;
}


void alignment::resize(int s1,int s2) {
  ublas::matrix<int> array2(s1,s2);
  
  for(int i=0;i<array.size1() && i<array2.size1();i++)
    for(int j=0;j< array.size2() && j<array2.size2();j++)
      array2(i,j) = array(i,j);
  
  array.resize(array2.size1(),array2.size2());
  array = array2;
}

void alignment::add_sequence(const sequence& s) {
  sequences.push_back(s);

  add_sequence(reinterpret_cast<const vector<int>& >(s));
}


void alignment::add_sequence(const alphabet& a,const string& name,const string& letters) {
  vector<int> v = a(letters);
  add_sequence(v);

  sequence s(a,name.c_str());
  for(int i=0;i<v.size();i++) {
    if (v[i] != alphabet::gap)
      s.push_back(v[i]);
  }
  sequences.push_back(s);
}

void alignment::add_sequence(const vector<int>& v) {
  int old_length = length();
  int new_length = std::max(length(),(int)v.size());

  resize(new_length,sequences.size()+1);

  set_sequence(array.size2()-1,v);

  pad(old_length);
}


void alignment::set_sequence(int index,const vector<int>& v) {
  int position=0;
  for(;position<v.size();position++)
    array(position,index) = v[position];

  for(;position<length();position++)
    array(position,index) = alphabet::gap;
}

bool alignment::changelength(int l) {
  bool can_do=true;
  if (l < length()) {
    for(int i=0;i<sequences.size();i++)
      if (l < sequences[i].size())
	can_do = false;
  }
  if (!can_do) return false;

  resize(l,array.size2());

  return true;
}

void alignment::delete_column(int column) {
  for(int i=0;i<size2();i++) 
    if (i<sequences.size()) 
      assert(array(column,i) == alphabet::gap);

  ublas::matrix<int> array2(array.size1()-1,array.size2());
  
  for(int i=0;i<array2.size1();i++)
    for(int j=0;j<array2.size2();j++) {
      int c = i;
      if (c>=column) c++;
      array2(i,j) = array(c,j);
    }
  
  array.resize(array2.size1(),array2.size2());
  array = array2;
}


void alignment::remap(const vector<string>& order) {
  assert(sequences.size() == order.size());

  vector<sequence> old_order = sequences;
  
  bool mismatch = false;
  for(int i=0;i<sequences.size() && !mismatch;i++) {
    if (sequences[i].name != order[i]) 
      mismatch = true;
  }

  if (!mismatch) return;

  vector<int> mapping(sequences.size());
  for(int i=0;i<mapping.size();i++) {
    int target = -1;
    for(int j=0;j<order.size() && target == -1;j++) {
      if (order[j] == sequences[i].name)
	target = j;
    }
    assert(target != -1);
    mapping[i] = target;
  }

  ublas::matrix<int> old_array = array;
  for(int column=0;column<array.size1();column++) {
    for(int i=0;i<sequences.size();i++)
      array(column,mapping[i]) = old_array(column,i);
  }

  for(int i=0;i<sequences.size();i++)
    sequences[mapping[i]] = old_order[i];
}


alignment& alignment::operator=(const alignment& A) {
  sequences.clear();
  for(int i=0;i<A.num_sequences();i++)
    sequences.push_back(A.seq(i));

  // This copy could be sped up a lot
  array.resize(A.length(),A.size2());
  for(int species=0;species<array.size2();species++)
    for(int column=0;column<array.size1();column++)
      array(column,species) = A(column,species);

  return *this;
}


void alignment::load_fasta(const alphabet& a,std::istream& file) {
  char temp[128];

  string current;
  string name;

  while(file.getline(temp,128)) {
    string line(temp);

    if (temp[0] != '>') {
      current += line;
      continue;
    }
    
    if (!name.empty()) 
      add_sequence(a,name,current);
    
    name = string(line.c_str()+1);
    current.clear();
    continue;
  }
  if (!name.empty()) 
    add_sequence(a,name,current);
}

void alignment::create_internal(const SequenceTree& T) {
  remap(T.get_sequences());

  resize(array.size1(),T.num_nodes()-1);

  for(int column=0;column< length();column++) {
    vector<int> present_leaf(T.leaves());
    for(int i=0;i<T.leaves();i++)
      present_leaf[i] = array(column,i)!=alphabet::gap;
    TreeFunc<int> present = mark_tree(present_leaf,T);
    for(int i=T.leaves();i<array.size2();i++) {
      if (present(i))
	array(column,i) = alphabet::not_gap;
      else
	array(column,i) = alphabet::gap;
    }
  }
}

void alignment::load(std::istream& file) {
  const alphabet& a = get_alphabet();

  int temp;
  file>>temp;
  array.resize(temp,sequences.size());

  for(int i=0;i<sequences.size();i++) {
    for(int column=0;column<length();column++) {
      char c;
      file>>c;
      array(column,i) = a[c];
    }
  }
}

void alignment::print(std::ostream& file) const{
  const alphabet& a = get_alphabet();
  file<<length()<<endl;
  for(int i=0;i<array.size2();i++) {
    for(int column=0;column<array.size1();column++) {
      file<<a.lookup(array(column,i));
    }
    file<<endl;
  }

#ifndef NDEBUG
  for(int i=0;i<sequences.size();i++) {
    vector<int> s;
    for(int column=0;column<length();column++)
      if (array(column,i) != alphabet::gap)
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
      if (array(column,i) != alphabet::gap)
	s.push_back(array(column,i));

    assert(s == sequences[i]);
  }
#endif
}

