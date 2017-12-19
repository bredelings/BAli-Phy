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
#include "myexception.H"
#include "util.H"

using namespace std;


sequence_info::sequence_info(const string& n)
  :name(n)
{}

sequence_info::sequence_info(const string& n,const string& c)
  :name(n),comment(c) 
{}

void sequence::strip_gaps() {
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

int total_length(const vector<sequence>& sequences)
{
    int count = 0;
    for(const auto& sequence: sequences)
	count += sequence.size();
    return count;
}

int letter_count(const string& letters, const string& s)
{
    int count = 0;
    for(int i=0;i<s.size();i++)
	if (letters.find(s[i]) != std::string::npos)
	    count++;
    return count;
}

double letter_count(const string& letters, const vector<sequence>& sequences)
{
    int count = 0;
    for(const auto& sequence: sequences)
	count += letter_count(letters, sequence);
    return count;
}

double letter_fraction(const string& letters, const string& gaps, const vector<sequence>& sequences)
{
    int count = letter_count(letters, sequences);
    int total = total_length(sequences) - letter_count(gaps, sequences);

    return double(count)/total;
}

string guess_alphabet(const vector<sequence>& sequences)
{
    // FIXME - we should maybe count things one time into a map from char -> int.
    double ATGCN = letter_fraction("ATGCN","-?",sequences);
    double AUGCN = letter_fraction("AUGCN","-?",sequences);
    double AUTGCN = letter_fraction("AUTGCN","-?",sequences);
    if (ATGCN > 0.95 and AUGCN <= ATGCN)
	return "DNA";
    else if (AUGCN > 0.95)
	return "RNA";

    if (AUTGCN > 0.95)
    {
	double T = letter_fraction("T","-?",sequences);
	double U = letter_fraction("U","-?",sequences);
	throw myexception()<<"Can't guess alphabet!\n Seems to be DNA or RNA but contains both U and T:\n  AUTGCN="<<int(AUTGCN*100)<<"%   T="<<int(T*100)<<"%   U="<<int(U*100)<<"%";
    }

    double digits = letter_fraction("0123456789","-?X",sequences);
    // FIXME - We can check the largest number ... but each column might have a different highest number.
    if (digits > 0.95)
	return "Numeric[2]";

    double aa = letter_fraction("ARNDCQEGHILKMFPSTWYVX","-?",sequences);
    if (letter_fraction("ARNDCQEGHILKMFPSTWYVX","-?",sequences) > 0.9 and AUTGCN<0.5)
    {
	if (letter_count("*",sequences) > 0)
	    return "Amino-Acids+stop";
	else
	    return "Amino-Acids";
    }

    throw myexception()<<"Can't guess alphabet!\n AUTGCN="<<int(AUTGCN*100)<<"%   0123456789="<<int(digits*100)<<"%   ARNDCQEGHILKMFPSTWYVX="<<int(aa*100)<<"%";
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
    vector<string> arguments = get_arguments(name,'[',']');

    if (name == "Codons")
    {
	if (arguments.size() < 2) arguments.resize(2);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(name_, sequences);
	if (arguments[1].empty()) arguments[1] = "standard";
	return "Codons[" + arguments[0] + "," + arguments[1] + "]";
    }
    else if (name == "Triplets")
    {
	if (arguments.size() < 1) arguments.resize(1);
	if (arguments[0].empty()) arguments[0] = guess_nucleotides_for(name_, sequences);
	return "Triplets[" + arguments[0] + "]";
    }
    else
	return name_;
}

