#ifndef SEQUENCE_FORMAT_H
#define SEQUENCE_FORMAT_H

#include <vector>
#include <iostream>
#include <string>

#include "sequence.H"
#include "alphabet.H"

//FIXME - make 'sequence' contain only letters, not indices?

namespace sequence_format {

  std::vector<sequence> load_fasta(const alphabet&,const std::string&);
  std::vector<sequence> load_fasta(const alphabet&,std::istream& file);

  typedef std::vector<sequence> (loader_t)(std::istream& file);

  /// Read an alignments letters and names from a file in phylip format
  std::vector<sequence> read_phylip(std::istream& file);
  
  /// Read an alignments letters and names from a file in fasta format
  std::vector<sequence> read_fasta(std::istream& file);

  typedef void (dumper_t)(std::ostream&, const std::vector<sequence>&);

  /// Read an alignments letters and names from a file in phylip format
  void write_phylip(std::ostream& file,const std::vector<sequence>& names);
  
  /// Read an alignments letters and names from a file in fasta format
  void write_fasta(std::ostream& file, const std::vector<sequence>& names);
}
#endif
