/*
   Copyright (C) 2004-2005,2007,2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

/**
 * @file sequence-format.H
 */

#ifndef SEQUENCE_FORMAT_H
#define SEQUENCE_FORMAT_H

#include <vector>
#include <iostream>
#include <string>

#include "sequence.H"

/// A namespace containing functions for parsing and writing sequence formats
namespace sequence_format 
{
  /// A typedef for functions that reads sequences from a file
  typedef std::vector<sequence> (loader_t)(std::istream& file);

  /// Read an alignments letters and names from a file in phylip format
  std::vector<sequence> read_guess(std::istream& file);

  /// Read an alignments letters and names from a file in phylip format
  std::vector<sequence> read_phylip(std::istream& file);
  
  /// Read an alignments letters and names from a file in fasta format
  std::vector<sequence> read_fasta(std::istream& file);

  /// Read an alignments letters and names from a file in fasta format
  std::vector<sequence> read_fasta_entire_file(std::istream& file);

  /// A typedef for functions that write sequences to a file
  typedef void (dumper_t)(std::ostream&, const std::vector<sequence>&);

  /// Read an alignments letters and names from a file in phylip format
  void write_phylip(std::ostream& file,const std::vector<sequence>& sequences);
  
  /// Read an alignments letters and names from a file in fasta format
  void write_fasta(std::ostream& file, const std::vector<sequence>& sequences);



  /// load a format from a file with name filename
  std::vector<sequence> load_from_file(loader_t loader,const std::string& filename);

  /// load a format from a file with name filename
  std::vector<sequence> load_from_file(const std::string& filename);

  /// write a format to a file.
  void write_to_file(loader_t loader,const std::vector<sequence>&,const std::string&);
}
#endif
