#include <cassert>
#include "sequence.H"
#include "myexception.H"
#include "util.H"

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

/// Read the first phylip section, including names
bool phylip_section(std::istream& file,int ntaxa, vector<string>& names,vector<string>& letters) {
  names.clear();
  letters.clear();

  int indent = -1;
  for(int i=0;i<ntaxa;i++) {
    string line;
    getline(file,line);
    assert(file);

    // Read the name from beginning of line
    int pos = line.find_first_not_of(" \t");
    if (pos == -1)
      throw myexception(string("[Reading PHYLIP alignment] Expected ") + convertToString(ntaxa) + " species, only found " + 
			convertToString(i) + ".");

    if (pos >9) // (note we're allowing empty names)
      names.push_back(string(""));
    else {
      int name_start = pos;
      pos = line.find_first_of(" \t",pos);
      names.push_back(line.substr(name_start,pos-name_start));
    
      pos = std::max(pos+1,10);
    }

    // find how much to indent
    pos = line.find_first_not_of(" \t",pos);
    assert(pos != -1);
    if (indent == -1)
      indent = pos;
    assert(indent == pos);
    line = line.substr(indent);

    // Strip out space characters from the letters
    line = strip(line," \t");

    // Add line to letters
    letters.push_back(line);
  }

  for(int i=1;i<letters.size();i++) 
    assert(letters[i].size() == letters[0].size());

  return file;
}

/// Read the second and following phylip sections - no names
bool phylip_section(std::istream& file,int ntaxa,vector<string>& letters) {
  letters.clear();
  string line;
  for(int i=0;i<ntaxa;i++) {
    getline(file,line);
    assert(file);

    // Strip out space characters from the letters
    line = strip(line," \t");

    // Add line to letters
    letters.push_back(line);
  }

  return file.good();
}

bool read_phylip(std::istream& file,vector<string>& names,vector<string>& sequences) {

  // parse phylip header
  string line;
  getline(file,line);
  int ntaxa = -1;
  int length = -1;
  {
    std::istringstream linestream(line);
    linestream>>ntaxa;
    linestream>>length;
  }

  // Get the letters and names from first section
  phylip_section(file,ntaxa,names,sequences);

  // Get the letters from following sections
  vector<string> letters;
  while(length <= 0 or sequences[0].size() < length) {
    string line;
    getline(file,line);
    if (not file.good() or line.size() != 0) 
      return false;

    if (not phylip_section(file,ntaxa,letters))
      break;

    for(int i=0;i<ntaxa;i++)
      sequences[i] += letters[i];
  }

  if (length > 0) ;

  // Check that the length matches the supplied length
  if (length > 0 and length != sequences[0].size())
    throw myexception(
		      string("Sequences have length ") + 
		      convertToString(sequences[0].size()) + 
		      " instead of specified length " + 
		      convertToString(length) + 
		      "."
		      );
  return true;
}



