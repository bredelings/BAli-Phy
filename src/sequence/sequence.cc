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
#include "sequence.H"
#include "util/myexception.H"
#include "util/cmdline.H"
#include <range/v3/algorithm/max.hpp>

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
	if (c != '-' and c != '?')
	    total++;

    return total;
}

void sequence::strip_gaps()
{
    string ungapped;

    for(int i=0;i<size();i++) {
	char c = (*this)[i];

	// FIXME - this hardcodes the - and ? characters...
	if (c != '-' and c != '?')
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
	(string&)s1 == (string&)s2;
}

int total_length(const vector<int>& letter_counts)
{
    int count = 0;
    for(int letter_count: letter_counts)
	count += letter_count;
    return count;
}

int letter_count(const string& letters, const vector<int>& letter_counts)
{
    int count = 0;
    for(char c: letters)
	count += letter_counts[c];
    return count;
}

double letter_fraction(const string& letters, const string& gaps, const vector<int>& letter_counts)
{
    int count = letter_count(letters, letter_counts);
    int total = total_length(letter_counts) - letter_count(gaps, letter_counts);

    if (total <= 0)
        return 0.0;
    else
        return double(count)/total;
}

std::vector<int> count_letters(const vector<sequence>& sequences)
{
    std::vector<int> counts(256, 0);

    for(auto& sequence: sequences)
        for(char c: sequence)
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
     <<"   ARNDCQEGHILKMFPSTWYVX="<<int(aa*100)<<"%   QEILKFPJZ* = "<<int(aa_not_nuc*100)<<"%\n"
     <<"   0123456789="<<int(digits*100)<<"%";

    throw e;
}

string guess_nucleotides_for(const string& name, const vector<sequence>& sequences)
{
    auto a = guess_alphabet(sequences);
    if (a != "DNA" and a != "RNA")
	throw myexception()<<"Can't guess nucleotide alphabet for '"<<name<<"': sequences appear to be '"<<a<<"'";
    return a;
}

string guess_alphabet(const string& name_, const vector<sequence>& sequences)
{
    if (name_.empty())
	return guess_alphabet(sequences);

    string name = name_;
    vector<string> arguments = get_arguments(name,'(',')');

    if (name == "Codons")
    {
	if (arguments.size() < 2) arguments.resize(2);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(name_, sequences);
	if (arguments[1].empty()) arguments[1] = "standard";
	return "Codons(" + arguments[0] + "," + arguments[1] + ")";
    }
    else if (name == "Doublets")
    {
	if (arguments.size() < 1) arguments.resize(1);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(name_, sequences);
	return "Doublets(" + arguments[0] + ")";
    }
    else if (name == "RNAEdits")
    {
	if (arguments.size() < 1) arguments.resize(1);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(name_, sequences);
	return "RNAEdits(" + arguments[0] + ")";
    }
    else if (name == "Triplets")
    {
	if (arguments.size() < 1) arguments.resize(1);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(name_, sequences);
	return "Triplets(" + arguments[0] + ")";
    }
    else
	return name_;
}

vector<sequence> select(const vector<sequence>& s,const vector<int>& columns)
{
    //------- Start with empty sequences --------//
    vector<sequence> S = s;
    for(int i=0;i<s.size();i++)
	S[i].string::operator=("");

    //------- Append columns to sequences -------//
    for(int j=0;j<s.size();j++)
	for(int i=0;i<columns.size() and columns[i] < s[j].size();i++)
	    S[j] += s[j][columns[i]];

    return S;
}

vector<sequence> select(const vector<sequence>& s,const string& range)
{
    if (range.empty()) return s;

    auto L = s[0].size();
    for(int i=0;i<s.size();i++)
	L = std::max(L, s[i].size());

    vector<int> columns = parse_multi_range(range, L);

    return select(s,columns);
}

void pad_to_same_length(vector<sequence>& s)
{
    // find total alignment length
    vector<unsigned> L;
    for(int i=0;i<s.size();i++)
	L.push_back(s[i].size());
    unsigned AL = ranges::max(L);

    // pad sequences if they are less than this length
    for(int i=0;i<s.size();i++)
	if (L[i] < AL)
	    (string&)s[i] = (string&)s[i] + string(AL-L[i],'-');
}

