#ifndef SEQUENCE_FORMAT_H
#define SEQUENCE_FORMAT_H

#include <vector>
#include <istream>
#include <string>

#include "sequence.H"
#include "alphabet.H"

namespace sequence_format {

  std::vector<sequence> load_fasta(const alphabet&,const std::string&);
  std::vector<sequence> load_fasta(const alphabet&,std::istream& file);

  typedef void (loader_t)(std::istream&, std::vector<std::string>&, std::vector<std::string>&);

  /// Read an alignments letters and names from a file in phylip format
  void read_phylip(std::istream& file, std::vector<std::string>& names, std::vector<std::string>& sequences);
  
  /// Read an alignments letters and names from a file in fasta format
  void read_fasta(std::istream& file, std::vector<std::string>& names, std::vector<std::string>& sequences);

}
#endif
