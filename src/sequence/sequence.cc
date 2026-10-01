/*
  Copyright (C) 2004-2005,2008 Benjamin Redelings

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

#include "util/assert.hh"
#include "sequence.hh"
#include "util/myexception.hh"
#include "util/cmdline.hh"
#include <cstddef>
#include <algorithm>

using std::vector;
using std::string;

sequence_info::sequence_info(const string& n)
    :name(n)
{}

sequence_info::sequence_info(const string& n,const string& c)
    :name(n),comment(c) 
{}

int sequence::seq_length() const
{
    int total = 0;
    for(char c: (*this))
	if (c != '-' and c != '?' and c != '=')
	    total++;

    return total;
}

void sequence::strip_gaps()
{
    string ungapped;

    for(int i=0;i<size();i++) {
	char c = (*this)[i];

	// FIXME - this hardcodes the -, ?, and = characters...
	if (c != '-' and c != '?' and c != '=')
	    ungapped += c;
    }
    string::operator=(ungapped);
}

sequence::sequence(const sequence_info& si)
    :sequence_info(si)
{}

sequence::sequence(const string& n,const string& c)
    :sequence_info(n,c)
{}

bool operator==(const sequence& s1,const sequence& s2) {
    return s1.name == s2.name and
	static_cast<const string&>(s1) == static_cast<const string&>(s2);
}

std::size_t total_length(const vector<std::size_t>& letter_counts)
{
    std::size_t count = 0;
    for(auto letter_count: letter_counts)
	count += letter_count;
    return count;
}

std::size_t letter_count(const string& letters, const vector<std::size_t>& letter_counts)
{
    std::size_t count = 0;
    for(unsigned char c: letters)
	count += letter_counts[c];
    return count;
}

double letter_fraction(const string& letters, const string& gaps, const vector<std::size_t>& letter_counts)
{
    auto count = letter_count(letters, letter_counts);
    auto total = total_length(letter_counts);
    auto excluded = letter_count(gaps, letter_counts);

    if (total <= excluded)
        return 0.0;
    else
        return double(count)/(total - excluded);
}

std::vector<std::size_t> count_letters(const vector<sequence>& sequences)
{
    std::vector<std::size_t> counts(256, 0);

    for(auto& sequence: sequences)
        for(unsigned char c: sequence)
            counts[c]++;

    return counts;
}

string guess_alphabet(const vector<sequence>& sequences)
{
    // Error model:
    // * If the sequences contain a few typos, we don't have to catch them here.
    //   The first letter than doesn't fit the alphabet will be reported later.
    // * If there are broad errors in letter frequency, report them here
    //   where we have better diagnostics.

    auto letter_counts = count_letters(sequences);

    // If there are no informative letters, maybe we should call DNA?
    if (total_length(letter_counts) <= 0)
	throw myexception()<<"Can't guess alphabet from 0 letters!";

    if (total_length(letter_counts) <= letter_count("-?=", letter_counts) )
	throw myexception()<<"Can't guess alphabet from only '-', '?', and '='!";

    double ATGCN  = letter_fraction("ATGCN",  "-?=", letter_counts);
    double AUGCN  = letter_fraction("AUGCN",  "-?=", letter_counts);
    double AUTGC  = letter_fraction("AUTGC",  "-?=", letter_counts);

    // two-letter code show up both with data ambiguity for 1 letter, and in heterozygous samples
    double dna_two_letters = letter_fraction("ACGTNYRWSKM",      "-?=", letter_counts);
    double rna_two_letters = letter_fraction("ACGUNYRWSKM",      "-?=", letter_counts);

    double aa         = letter_fraction("ARNDCQEGHILKMFPSTWYVX", "*-?=", letter_counts);
    double aa_not_nuc = letter_fraction("QEILFPJZ*",             "-?=",  letter_counts); // X used in DNA masking?

    double digits = letter_fraction("0123456789","-?X=",letter_counts);

    // PROBLEM: If each column is numeric but has a different number of characters, then we should
    // maybe choose a "Numeric" alphabet that doesn't specify an upper bound??
    if (digits > 0.95) return "Numeric(2)"; // 0123

    // ATGCP -> Amino-Acids
    // ATGCP* -> Amino-Acids+stop
    // ACGTNYRWSM -> DNA, because no QEILFPXJZ*
    if (aa > 0.9 and aa_not_nuc > 0.005)
    {
        bool is_protein = (AUTGC < 0.5 or aa_not_nuc > 0.01) and (AUTGC < 0.8 or aa_not_nuc > 0.02);

        if (is_protein)
            return (letter_counts['*'] > 0) ? "Amino-Acids+stop" : "Amino-Acids";
    }

    if (ATGCN > 0.95 and AUGCN <= ATGCN) return "DNA"; // T, A, N
    if (AUGCN > 0.95 and AUGCN >= ATGCN) return "RNA"; // U

    if (ATGCN > 0.8 and dna_two_letters > 0.95 and AUGCN < ATGCN) return "DNA"; // YAGCT
    if (AUGCN > 0.8 and rna_two_letters > 0.95 and AUGCN > ATGCN) return "RNA"; // YAGCU

    double T = letter_fraction("T", "-?=", letter_counts);
    double U = letter_fraction("U", "-?=", letter_counts);
    double AUTGCN = letter_fraction("AUTGCN", "-?=", letter_counts);

    myexception e;
    e<<"Can't guess alphabet!\n"
     <<"   AUTGCN="<<int(AUTGCN*100)<<"%    T = "<<int(T*100)<<"%   U = "<<int(U*100)<<"%\n"
     <<"   ARNDCQEGHILKMFPSTWYVX="<<int(aa*100)<<"%   QEILFPJZ* = "<<int(aa_not_nuc*100)<<"%\n"
     <<"   0123456789="<<int(digits*100)<<"%";

    throw e;
}

// File readers uppercase sequence letters. Count T and U across all rows, ignoring other symbols;
// DNA wins ties, including empty input. Decoding subsequently validates the chosen alphabet.
string guess_nucleotides_for(const vector<sequence>& sequences)
{
    const auto letter_counts = count_letters(sequences);
    return letter_counts['U'] > letter_counts['T'] ? "RNA" : "DNA";
}

string guess_alphabet(const string& name_, const vector<sequence>& sequences)
{
    if (name_.empty())
	return guess_alphabet(sequences);

    string name = name_;
    vector<string> arguments = get_arguments(name,'(',')');

    // Preserve excess arguments so get_alphabet can report its usual argument-count error.
    if ((name == "Codons" and arguments.size() > 2) or
        ((name == "Doublets" or name == "RNAEdits" or name == "Triplets") and arguments.size() > 1))
        return name_;

    if (name == "Codons")
    {
	if (arguments.size() < 2) arguments.resize(2);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(sequences);
	if (arguments[1].empty()) arguments[1] = "standard";
	return "Codons(" + arguments[0] + "," + arguments[1] + ")";
    }
    else if (name == "Doublets")
    {
	if (arguments.size() < 1) arguments.resize(1);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(sequences);
	return "Doublets(" + arguments[0] + ")";
    }
    else if (name == "RNAEdits")
    {
	if (arguments.size() < 1) arguments.resize(1);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(sequences);
	return "RNAEdits(" + arguments[0] + ")";
    }
    else if (name == "Triplets")
    {
	if (arguments.size() < 1) arguments.resize(1);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(sequences);
	return "Triplets(" + arguments[0] + ")";
    }
    else
	return name_;
}

// Select in the requested order, preserving duplicates. Treat absent trailing positions as gaps
// so that every output row has the same column correspondence, even for reordered selections.
vector<sequence> select(const vector<sequence>& s,const vector<int>& columns)
{
    std::size_t L = 0;
    for(const auto& sequence: s)
        L = std::max(L, sequence.size());
    for(int column: columns)
        if (column < 0 or column >= L)
            throw myexception()<<"Column index "<<column<<" is outside an alignment of length "<<L<<".";

    //------- Start with empty sequences --------//
    vector<sequence> S;
    S.reserve(s.size());
    for(const auto& sequence: s)
        S.emplace_back(static_cast<const sequence_info&>(sequence));

    //------- Append columns to sequences -------//
    for(int j=0;j<s.size();j++)
    {
        S[j].reserve(columns.size());
        for(int column: columns)
            S[j] += column < s[j].size() ? s[j][column] : '-';
    }

    return S;
}

vector<sequence> select(const vector<sequence>& s,const string& range)
{
    if (range.empty()) return s;
    if (s.empty())
        throw myexception()<<"Cannot select columns from an empty sequence collection.";

    auto L = s[0].size();
    for(int i=0;i<s.size();i++)
	L = std::max(L, s[i].size());

    vector<int> columns = parse_multi_range(range, L);

    return select(s,columns);
}

void pad_to_same_length(vector<sequence>& s)
{
    // find total alignment length
    std::size_t AL = 0;
    for(const auto& sequence: s)
        AL = std::max(AL, sequence.size());

    // pad sequences if they are less than this length
    for(auto& sequence: s)
        sequence.resize(AL, '-');
}

