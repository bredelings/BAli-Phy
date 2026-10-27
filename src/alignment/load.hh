#ifndef ALIGNMENT_LOAD_H
#define ALIGNMENT_LOAD_H

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include <filesystem>
#include "sequence/sequence.hh"
#include "sequence/alphabet.hh"
#include "alignment.hh"
#include "util/file-readers.hh"

/// Load an alignment from file
std::vector<sequence> load_sequences_with_range(const std::filesystem::path& filename, const std::string&_range);

/// Load an alignment from file
alignment load_alignment_with_range(const std::filesystem::path& filename, const std::string&_range, const std::string& alph_name = "");

/// Load an alignment from file
alignment load_alignment(const std::filesystem::path& filename, const std::string& alph_name = "", bool remove_empty_columns = true);

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

class alignment_reader: public file_reader<std::string>
{
    std::string current;
public:
    const std::string& read() const {assert(not done()); return current;}
    void next();

    alignment_reader(std::istream&);
};

#endif
