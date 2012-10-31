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

#include <iostream>
#include <string>
#include <vector>
#include <cmath>

#include "tree/sequencetree.H"
#include "arguments.H"
#include "util.H"
#include "tree-dist.H"

using namespace std;

void write_header(const SequenceTree& T,bool leaves) {

  vector<string> names;

  if (leaves) {
    for(int n1=0;n1<T.leaves();n1++)
      names.push_back(string("L")+convertToString(n1));
  }
  else {
    for(int n1=0;n1<T.n_nodes()-1;n1++)
      for(int n2=0;n2<n1;n2++) {
	string field = string("D")+convertToString(n1)+"-"+convertToString(n2);
	names.push_back(field);
      }
  }

  
  std::cout<<join(names,',')<<endl;
}

void write_out(const SequenceTree& T,bool leaves) {

  vector<string> lengths;
  if (leaves) {
    for(int n1=0;n1<T.leaves();n1++)
      lengths.push_back(convertToString(T.branch(n1).length()));
  }
  else {
    for(int n1=0;n1<T.n_nodes()-1;n1++)
      for(int n2=0;n2<n1;n2++)
	lengths.push_back(convertToString(T.distance(n1,n2)));
  }
  std::cout<<join(lengths,',')<<endl;
}


int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    bool topology_only = false;
    if (args.set("topology"))
      topology_only = true;


    bool leaves = args.set("leaves_only");
    bool header=false;

    // read in the trees
    string line;
    while(getline(cin,line)) {
      SequenceTree T = standardized(line); 
      if (topology_only) 
	for(int b=0;b<T.branches();b++)
	  T.branch(b).length() = 1.0;

      if (not header) {
	header=true;
	write_header(T,leaves);
      }
      write_out(T,leaves);
    }
  }
  catch (std::exception& e) {
    std::cerr<<"tree-dist-cvars: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
