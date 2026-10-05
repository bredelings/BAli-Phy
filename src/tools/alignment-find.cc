/*
   Copyright (C) 2004-2008 Benjamin Redelings

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
#include <vector>
#include <string>
#include "sequence/alphabet.hh"
#include "alignment/alignment.hh"
#include "alignment/load.hh"

#include <CLI/CLI.hpp>

using namespace std;

// Select the first or last FASTA alignment from stdin and write it to stdout.
int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    CLI::App app{"Find the first or last FASTA alignment in a stream.", "alignment-find"};
    app.usage("Usage: alignment-find [OPTIONS] < alignments-file > alignment.fasta");
    app.get_formatter()->long_option_alignment_ratio(0.2f);
    string alphabet;
    bool first = false, last = false;
    app.add_option("--alphabet", alphabet,
                   "Alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop")
        ->type_name("ALPHABET");
    app.add_flag("--first", first, "Select the first alignment");
    app.add_flag("--last", last, "Select the last alignment (default)");
    try
    {
      app.parse(argc, argv);
    }
    catch (const CLI::ParseError& error)
    {
      // Let CLI11 print help or diagnostics, retaining the tool's 0/1 exit statuses.
      app.exit(error);
      return error.get_exit_code() == 0 ? 0 : 1;
    }

    if (app.count("--first") and app.count("--last"))
      throw myexception()<<"Cannot give both --first and --last.";

    //--------------- Find the alignment ----------------//
    alignment A;
    if (first)
      A = find_first_alignment(std::cin, alphabet);
    else
      A = find_last_alignment(std::cin, alphabet);

    //------------------ Print it out -------------------//
    std::cout<<A;
  }
  catch (std::exception& e) {
    std::cerr<<"alignment-find: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
