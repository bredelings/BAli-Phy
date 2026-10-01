/*
   Copyright (C) 2004-2008,2010 Benjamin Redelings

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

#include <iostream>
#include <string>
#include <array>
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <vector>
#include "util/myexception.hh"
#include "sequence/genetic_code.hh"
#include "alignment/alignment.hh"
#include "alignment/alignment-util.hh"
#include <CLI/CLI.hpp>

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::string;

// Flush buffered output before reporting success, including help and untranslated sequences.
static void check_output()
{
    cout.flush();
    if (not cout)
        throw myexception()<<"Failed writing standard output.";
}

// Exact codons retain the direct table lookup. An ambiguous codon is the
// Cartesian product of its three nucleotide sets; translating all matching
// exact codons gives precisely the amino-acid set represented by the output code.
int translate_codon(int n0, int n1, int n2, const Genetic_Code& code, const AminoAcidsWithStop& amino_acids,
                    const ambiguity_database& input_ambiguities, ambiguity_database& output_ambiguities)
{
    if (n0 >= 0 and n1 >= 0 and n2 >= 0)
        return code.translate(n0, n1, n2);
    if (n0 == alphabet::gap or n1 == alphabet::gap or n2 == alphabet::gap)
        return alphabet::gap;
    if (n0 == alphabet::unknown or n1 == alphabet::unknown or n2 == alphabet::unknown)
        return alphabet::unknown;

    std::array<int, 3> observations{n0, n1, n2};
    std::array<alphabet::bitmask_t, 3> nucleotide_masks{
        alphabet::bitmask_t(4), alphabet::bitmask_t(4), alphabet::bitmask_t(4)};
    for (int position = 0; position < 3; position++)
    {
        int observation = observations[position];
        if (observation >= 0)
            nucleotide_masks[position].set(observation);
        else if (alphabet::is_ambiguity(observation))
            nucleotide_masks[position] = input_ambiguities.mask(observation);
        else
        {
            assert(observation == alphabet::not_gap);
            nucleotide_masks[position].set();
        }
    }

    alphabet::bitmask_t amino_acid_mask(amino_acids.n_letters());
    for (int first = 0; first < 4; first++)
        for (int second = 0; second < 4; second++)
            for (int third = 0; third < 4; third++)
                if (nucleotide_masks[0][first] and nucleotide_masks[1][second] and nucleotide_masks[2][third])
                    amino_acid_mask.set(code.translate(first, second, third));

    return output_ambiguities.encode_mask(amino_acid_mask);
}

//FIXME - make this handle un-aligned gaps...
// diagnose sequences which are not a multiple of 3
// look for reading frames?  start codons?
// translate just the sequences before translating
// the ALIGNMENT of the sequences to print out

int main(int argc,char* argv[]) 
{ 

  try {
    //---------- Parse command line  -------//
    CLI::App app{"Translate a DNA/RNA alignment into amino acids.", "alignment-translate"};
    app.usage("Usage: alignment-translate [OPTIONS] < sequence-file > output-file");
    app.get_formatter()->long_option_alignment_ratio(0.2f);
    string genetic_code = "standard";
    int frame = 1;
    bool do_reverse = false, do_complement = false, translate = true;

    app.add_option("-g,--genetic-code", genetic_code, "Specify alternate genetic code")
        ->type_name("CODE")->capture_default_str();
    app.add_option("-f,--frame", frame, "Frame 1, 2, 3, -1, -2, or -3")
        ->type_name("FRAME")->capture_default_str();
    app.add_flag("-r,--reverse", do_reverse, "Reverse alignment columns before translation");
    app.add_flag("-c,--complement", do_complement, "Complement nucleotides before translation");
    app.add_option("-t,--translate", translate, "Translate the sequences; --translate=no disables translation")
        ->type_name("BOOL")->default_str("yes");

    // The examples are preformatted; preserve their indentation and line breaks.
    app.get_formatter()->enable_footer_formatting(false);
    app.footer("Examples:\n\n"
               "  Translate DNA or RNA to amino acids in reading frame 1:\n"
               "    alignment-translate < dna.fasta > aa.fasta\n\n"
               "  Give the reverse complement without translation:\n"
               "    alignment-translate -rc --translate=no < dna.fasta > dna2.fasta\n\n"
               "  The following commands are identical:\n"
               "    alignment-translate --frame=-2 < dna.fasta > aa2.fasta\n"
               "    alignment-translate -rc --frame=2 < dna.fasta > aa2.fasta\n");
    try
    {
      app.parse(argc, argv);
    }
    catch (const CLI::ParseError& error)
    {
      // Let CLI11 print help or diagnostics, while retaining this tool's 0/1 exit statuses.
      app.exit(error);
      if (error.get_exit_code() != 0)
        return 1;
      check_output();
      return 0;
    }

    //------- Validate the reading frame before consuming input --------//
    if (frame < -3 or frame > 3 or frame == 0)
      throw myexception()<<"You may only specify frame 1, 2, 3, -1, -2, or -3: "<<frame<<" is right out.";
    const bool do_reverse_complement = frame < 0;

    // Frames +/-1, +/-2, and +/-3 start at zero-based column offsets 0, 1, and 2.
    const int column_offset = std::abs(frame) - 1;

    //------- Try to load sequences --------//
    vector<sequence> sequences = sequence_format::read_guess(std::cin);

    if (sequences.size() == 0)
      throw myexception()<<"Alignment file read from STDIN  didn't contain any sequences!";
    
    //--------- Load alignment & determine RNA or DNA ----------//
    alignment A1{DNA()};
    try
    {
      A1.load(sequences);
    }
    catch (const myexception& dna_error)
    {
      const string dna_message = dna_error.what();
      // Discard any partially loaded DNA data before trying RNA. If neither alphabet works,
      // retain both diagnostics rather than guessing which alphabet the input was meant to use.
      A1 = alignment(RNA());
      try
      {
        A1.load(sequences);
      }
      catch (const myexception& rna_error)
      {
        throw myexception()<<"Could not read the alignment as DNA or RNA.\n"
                           <<"DNA: "<<dna_message<<"\nRNA: "<<rna_error.what();
      }
    }

    //------------------ Reverse Complement? -------------------//

    if (do_reverse and do_complement)
      A1 = reverse_complement(A1);
    else if (do_reverse)
      A1 = reverse(A1);
    else if (do_complement)
      A1 = complement(A1);

    if (not translate) {
      cout<<A1;
      check_output();
      return 0;
    }
      
    if (do_reverse_complement)
      A1 = reverse_complement(A1);

    //------- Construct the alphabets that we are using  --------//
    auto G = get_genetic_code(genetic_code);

    AminoAcidsWithStop AA;

    //------- Convert sequence codons to amino acids  --------//
    const int translated_length = std::max(0, A1.length() - column_offset) / 3;

    vector<sequence> translated_sequences(A1.n_sequences());
    for(int i=0;i<A1.n_sequences();i++)
    {
      translated_sequences[i].name = A1.seq(i).name;
      translated_sequences[i].comment = A1.seq(i).comment;
    }
    alignment A2(AA, translated_sequences, translated_length);

    for(int i=0;i<A1.n_sequences();i++)
    {
      int output_column = 0;
      for(int column=column_offset;column<A1.length()-2;column+=3)
      {
	int n0 = A1(column,i);
	int n1 = A1(column+1,i);
	int n2 = A1(column+2,i);

	int aa = translate_codon(n0, n1, n2, G, AA, A1.get_ambiguities(), A2.get_ambiguities());
	A2.set_value(output_column++, i, aa);
	// Keep gaps and unknowns in the matrix, but omit them from the ungapped sequence
	// string, as alignment::load does. Ambiguous amino acids remain in both.
	if (alphabet::is_character(aa))
	    A2.seq(i) += A2.lookup(aa);
      }
    }

    cout<<A2;
    check_output();
  }
  catch (std::exception& e) {
    cerr<<"alignment-translate: Error! "<<e.what()<<endl;
    return 1;
  }
  return 0;

}
