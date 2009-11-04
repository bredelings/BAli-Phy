/*
   Copyright (C) 2004 Benjamin Redelings

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


int main(int argc,char* argv[]) {


  /* ----- Alphabets to try ------ */
  vector<alphabet> alphabets;
  alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
  alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
  alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));
                                                                                
  /* ----- Try to load template alignment -----*/
  if (not args.set("align"))
    throw myexception("Alignment file not specified! (align=<filename>)");
 
  ifstream ifile(args["align"].c_str());
  A.load_phylip(alphabets,ifile);
                                                                                
  ifile.close();

}
