#ifndef ALIGNMENTUTIL_H
#define ALIGNMENTUTIL_H

#include "alignment.H"
#include "alphabet.H"
#include <vector>
#include <istream>
#include <string>

std::vector<alignment> load_alignments(std::istream& ifile, const std::string& tag,
				  const std::vector<alphabet>& alphabets, int maxalignments);

alignment find_last_alignment(std::istream& ifile, const string& tag,
			       const vector<alphabet>& alphabets);

#endif
