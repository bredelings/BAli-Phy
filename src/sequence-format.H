#ifndef SEQUENCE_FORMAT_H
#define SEQUENCE_FORMAT_H

#include <vector>
#include <iostream>
#include <string>

#include "sequence.H"

namespace sequence_format {

  typedef std::vector<sequence> (loader_t)(std::istream& file);

  /// Read an alignments letters and names from a file in phylip format
  std::vector<sequence> read_guess(std::istream& file);

  /// Read an alignments letters and names from a file in phylip format
  std::vector<sequence> read_phylip(std::istream& file);
  
  /// Read an alignments letters and names from a file in fasta format
  std::vector<sequence> read_fasta(std::istream& file);



  typedef void (dumper_t)(std::ostream&, const std::vector<sequence>&);

  /// Read an alignments letters and names from a file in phylip format
  void write_phylip(std::ostream& file,const std::vector<sequence>& sequences);
  
  /// Read an alignments letters and names from a file in fasta format
  void write_fasta(std::ostream& file, const std::vector<sequence>& sequences);



  // load a format from a file.
  std::vector<sequence> load_from_file(loader_t loader,const std::string&);

  std::vector<sequence> load_from_file(const std::string&);

  // write a format to a file.
  void write_to_file(loader_t loader,const std::vector<sequence>&,const std::string&);
}
#endif
