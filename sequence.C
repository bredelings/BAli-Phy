#include <cassert>
#include "sequence.H"
#include "myexception.H"

void sequence::parse(const string& line,const string& letters) {
  /*------------ Delete '>' from label -----------*/
  assert(line[0] == '>');
  string label = line.substr(1);

  /*------Parse label with name and comments------*/
  name = label;
  comment = "";
  int where = label.find_first_of(" \t");
  if (where != -1) {
    name = label.substr(0,where);

    where = label.find_first_not_of(" \t",where+1);
    comment =  label.substr(where);
  }

  /*--------- Load the actual letters ------------*/
  vector<int>::operator=(a(letters));
}

void sequence::read(const string& filename) {
  vector<sequence> sequences = load_fasta(a,filename);
  if (not sequences.size())
    throw myexception(string("There are no sequences in file '") + filename + "'!");

  operator=(sequences[0]);
}

void sequence::strip_gaps() {
  vector<int> ungapped;
  ungapped.reserve(size());
  for(int i=0;i<size();i++) {
    int l = (*this)[i];
    if (l != alphabet::gap)
      ungapped.push_back(l);
  }
  vector<int>::operator=(ungapped);
}

vector<sequence> load_fasta(const alphabet& a,std::istream& file) {
  vector<sequence> sequences;

  string label;
  string letters;

  string line;
  while(getline(file,line)) {
    if (!line.size()) continue;

    if (line[0] != '>') {
      letters += line;
      continue;
    }
    
    if (not label.empty()) {
      sequence s(a);
      s.parse(label,letters);
      sequences.push_back(s);
    }
    
    label = line;
    letters.clear();
    continue;
  }

  if (not label.empty()) {
    sequence s(a);
    s.parse(label,letters);
    sequences.push_back(s);
  }

  return sequences;
}

vector<sequence> load_fasta(const alphabet& a,const string& filename) {
  ifstream file(filename.c_str());
  if (not file)
    throw myexception(string("Couldn't open file '")+filename+"'");
  vector<sequence> sequences = load_fasta(a,file);
  file.close();
  return sequences;
}


