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
#include "util/file-readers.H"

std::string get_alphabet_name(const boost::program_options::variables_map& args);

/// Load an alignment from command line args --align=filename
alignment load_A(const boost::program_options::variables_map& args,bool keep_internal=true, bool remove_empty_columns=true);

/// Load an alignment from file
std::vector<sequence> load_sequences_with_range(const std::string& filename, const std::string&_range);

/// Load an alignment from file
alignment load_alignment_with_range(const std::string& filename, const std::string&_range, const std::string& alph_name = "");

/// Load an alignment from file
alignment load_alignment(const std::string& filename, const std::string& alph_name = "", bool remove_empty_columns = true);

/// Load an alignment from file
alignment load_alignment(const std::vector<sequence>& sequences, const std::string& alph_name = "", bool remove_empty_columns = true);

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<std::string>& names, const alphabet& a, int skip, int maxalignments);

std::list<alignment> load_alignments(std::istream& ifile, const std::string& alph_name, int skip, int maxalignments);

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<std::string>& names, const std::string& alph_name, int skip, int maxalignments);

std::vector<alignment> load_alignments(std::istream& ifile, const std::string& names);

std::optional<std::vector<sequence>> find_load_next_sequences(std::istream& ifile);

std::istream& find_alignment(std::istream& ifile);

alignment load_next_alignment(std::istream& ifile, const std::string&);

alignment load_next_alignment(std::istream& ifile, const alphabet& a);

alignment load_next_alignment(std::istream& ifile, const alphabet& a, const std::vector<std::string>& names);

alignment find_first_alignment(std::istream& ifile, const std::string& alph_name);

alignment find_last_alignment(std::istream& ifile, const std::string& alph_name);

class alignment_reader: public file_reader<std::string>
{
public:
    std::optional<std::string> next_one();

    alignment_reader(std::istream&);
};

/*
#include "range/v3/all.hpp"

class alignments_range: public ranges::view_facade<alignments_range>
{
    std::istream& file;
    std::optional<alignment> current;
    friend ranges::range_access;
    std::string const & read() const;
    bool equal(ranges::default_sentinel_t) const;
    void next();
public:
    explicit alignments_range(std::istream&);
};
*/

#endif
