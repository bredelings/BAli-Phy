#ifndef ALIGNMENT_LOAD_H
#define ALIGNMENT_LOAD_H

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include <boost/program_options.hpp>
#include "sequence/alphabet.H"
#include "alignment.H"
#include "object.H"

std::vector<object_ptr<const alphabet> > load_alphabets(const boost::program_options::variables_map& args);

/// Load an alignment from command line args --align=filename
alignment load_A(const boost::program_options::variables_map& args,bool keep_internal=true);

/// Load an alignment from file
alignment load_alignment(const std::string& filename, const std::vector<object_ptr<const alphabet> >& alphabets);

/// Load an alignment from file
alignment load_alignment(const std::string& filename);

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<std::string>& names, const alphabet& a, int skip, int maxalignments);

std::list<alignment> load_alignments(std::istream& ifile,
				     const std::vector<object_ptr<const alphabet> >& alphabets,
				     int skip, int maxalignments);

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<std::string>& names, 
				     const std::vector<object_ptr<const alphabet> >& alphabets, 
				     int skip, int maxalignments);

std::vector<alignment> load_alignments(std::istream& ifile, const std::vector<object_ptr<const alphabet> >& alphabets);

alignment find_first_alignment(std::istream& ifile, const std::vector<object_ptr<const alphabet> >& alphabets);

alignment find_last_alignment(std::istream& ifile, const std::vector<object_ptr<const alphabet> >& alphabets);

#endif
