#ifndef SEQUENCE_H
#define SEQUENCE_H

#include "mytypes.H"
#include "alphabet.H"

/***************** struct sequence ********************/
struct sequence: public vector<int> {
  alphabet a;
  string name;
  string comment;

  void parse(const string&,const string&);
  void read(const string&);

  void strip_gaps();

  sequence(const alphabet& a1): a(a1) {}
  sequence(const alphabet& a1,const string& filename): a(a1) {read(filename);}
  sequence(const alphabet& a1,const string& n,const string& c): a(a1),name(n),comment(c) {}
};

vector<sequence> load_fasta(const alphabet&,const std::string&);
vector<sequence> load_fasta(const alphabet&,std::istream& file);

/// Read an alignments letters and names from a file in phylip format
bool read_phylip(std::istream& file, vector<string>& names, vector<string>& sequences);


#endif
