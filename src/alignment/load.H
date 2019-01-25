#ifndef ALIGNMENT_LOAD_H
#define ALIGNMENT_LOAD_H

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include <boost/program_options.hpp>
#include "sequence/sequence.H"
#include "sequence/alphabet.H"
#include "alignment.H"
#include "util/object.H"

std::string get_alphabet_name(const boost::program_options::variables_map& args);

/// Load an alignment from command line args --align=filename
alignment load_A(const boost::program_options::variables_map& args,bool keep_internal=true);

/// Load an alignment from file
std::vector<sequence> load_sequences_with_range(const std::string& filename, const std::string&_range);

/// Load an alignment from file
alignment load_alignment_with_range(const std::string& filename, const std::string&_range, const std::string& alph_name = "");

/// Load an alignment from file
alignment load_alignment(const std::string& filename, const std::string& alph_name = "");

/// Load an alignment from file
alignment load_alignment(const std::vector<sequence>& sequences, const std::string& alph_name = "");

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<std::string>& names, const alphabet& a, int skip, int maxalignments);

std::list<alignment> load_alignments(std::istream& ifile, const std::string& alph_name, int skip, int maxalignments);

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<std::string>& names, const std::string& alph_name, int skip, int maxalignments);

std::vector<alignment> load_alignments(std::istream& ifile, const std::string& names);

std::optional<std::vector<sequence>> find_load_next_sequences(std::istream& ifile);

alignment find_first_alignment(std::istream& ifile, const std::string& alph_name);

alignment find_last_alignment(std::istream& ifile, const std::string& alph_name);

#endif
