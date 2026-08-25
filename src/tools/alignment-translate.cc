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
#include <fstream>
#include <string>
#include <array>
#include "sequence/genetic_code.hh"
#include "alignment/alignment.hh"
#include "alignment/alignment-util.hh"
#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::string;
using std::shared_ptr;

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

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help,h", "Produce help message")
    ("genetic-code,g",value<string>()->default_value("standard"),"Specify alternate genetic code.")
    ("frame,f",value<int>()->default_value(1),"Frame 1, 2, 3, -1, -2, or -3")
    ("reverse,r","Just return the reverse")
    ("complement,c","Just return the complement")
    ("translate,t",value<bool>()->default_value(true,"yes"),"Translate the sequences")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Translate a DNA/RNA alignment into amino acids.\n\n";
    cout<<"Usage: alignment-translate [OPTIONS] < sequence-file [OPTIONS]\n";
    cout<<all<<"\n";
    cout<<" Examples:\n\n";
    cout<<"  Translate DNA or RNA to amino acids in reading frame 1:\n";
    cout<<"    % alignment-translate < dna.fasta > aa.fasta\n\n";
    cout<<"  Give the reverse complement without translation:\n";
    cout<<"    % alignment-translate -rc --translate=no < dna.fasta > dna2.fasta\n\n";
    cout<<"  The following commands are identical:\n";
    cout<<"    % alignment-translate --frame=-2 < dna.fasta > aa2.fasta\n";
    cout<<"    % alignment-translate -rc --frame=2 < dna.fasta > aa2.fasta\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) 
{ 

  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //------- Try to load sequences --------//
    vector<sequence> sequences = sequence_format::read_guess(std::cin);

    if (sequences.size() == 0)
      throw myexception()<<"Alignment file read from STDIN  didn't contain any sequences!";

    //------- Convert sequences to specified reading frame --------//
    int frame = args["frame"].as<int>();

    if (frame < -3 or frame > 3 or frame == 0)
      throw myexception()<<"You may only specify frame 1, 2, 3, -1, -2, or -3: "<<frame<<" is right out.";
    bool do_reverse = (frame < 0);

    // shift to the 0,1,2 scale
    frame = (std::abs(frame)+2)%3;
    
    //--------- Load alignment & determine RNA or DNA ----------//
    alignment A1;
    try
    {
	DNA d;
	alignment A(d);
	A.load(sequences);
	A1 = A;
    }
    catch (...)
    {
	RNA r;
	alignment A(r);
	A.load(sequences);
	A1 = A;
    }

    owned_ptr<Nucleotides> N(dynamic_cast<const Nucleotides&>(A1.get_alphabet()));
    assert(N);

    //------------------ Reverse Complement? -------------------//

    if (args.count("reverse") and args.count("complement"))
      A1 = reverse_complement(A1);
    else if (args.count("reverse"))
      A1 = reverse(A1);
    else if (args.count("complement"))
      A1 = complement(A1);

    if (not args["translate"].as<bool>()) {
      cout<<A1;
      exit(0);
    }
      
    if (do_reverse) 
      A1 = reverse_complement(A1);

    //------- Construct the alphabets that we are using  --------//
    auto G = get_genetic_code(args["genetic-code"].as<string>());

    AminoAcidsWithStop AA;

    //------- Convert sequence codons to amino acids  --------//
    int translated_length = 0;
    for(int column=frame;column<A1.length()-2;column+=3)
      translated_length++;

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
      for(int column=frame;column<A1.length()-2;column+=3) 
      {
	int n0 = A1(column,i);
	int n1 = A1(column+1,i);
	int n2 = A1(column+2,i);

	int aa = translate_codon(n0, n1, n2, G, AA, A1.get_ambiguities(), A2.get_ambiguities());
	A2.set_value(output_column++, i, aa);
	A2.seq(i) += A2.lookup(aa);
      }
    }

    cout<<A2;
  }
  catch (std::exception& e) {
    cerr<<"alignment-translate: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
