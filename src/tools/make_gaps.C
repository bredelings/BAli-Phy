/*
   Copyright (C) 2004,2008 Benjamin Redelings

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

#include "alignment/alignment.H"
#include "arguments.H"
#include "tree/tree.H"
#include "rng.H"
#include "myexception.H"

int main(int argc,char* argv[]) { 
  try {
    /*--------------Load the Sequences and Tree------------------*/

    Arguments args;
    args.read(argc,argv);
  
    if (not args.set("align"))
      throw myexception("Alignment file not specified! (align=<filename>)");

    alphabet dna("DNA nucleotides","AGTC","N");
    alignment A(dna,args["align"]);

    if (not args.set("tree"))
      throw myexception("Alignment file not specified! (tree=<filename>)");
      
    SequenceTree T;
    T.read(args["tree"]);

    /*--------------Make the alignment here------------------*/
    matrix<char> A;
    
  }
  catch (std::exception& e) {
    std::cerr<<"make_gaps: Error! "<<e.what()<<endl;
    return 1;
  }
  return 0;
}
