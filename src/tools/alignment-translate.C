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
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"
#include "setup.H"
#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::string;

using boost::shared_ptr;

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
    cout<<"Usage: alignment-translate [OPTIONS] < sequence-file [OPTIONS]\n";
    cout<<"Translate a DNA/RNA alignment into amino acids.\n\n";
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
    vector<object_ptr<const alphabet> > alphabets;
    alphabets.push_back(object_ptr<const alphabet>(new DNA));
    alphabets.push_back(object_ptr<const alphabet>(new RNA));
    A1.load(alphabets, sequences);

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
    boost::object_ptr<const Genetic_Code> G = get_genetic_code(args["genetic-code"].as<string>());

    AminoAcidsWithStop AA;

    //------- Convert sequence codons to amino acids  --------//
    alignment A2(AA);

    for(int i=0;i<A1.n_sequences();i++) 
    {
      sequence S;
      S.name = A1.seq(i).name;
      S.comment = A1.seq(i).comment;

      for(int column=frame;column<A1.length()-2;column+=3) 
      {
	int n0 = A1(column,i);
	int n1 = A1(column+1,i);
	int n2 = A1(column+2,i);

	int aa = G->translate(n0,n1,n2);

	S += AA.lookup(aa);
      }
      A2.add_sequence(S);
    }

    cout<<A2;
  }
  catch (std::exception& e) {
    cerr<<"alignment-translate: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
