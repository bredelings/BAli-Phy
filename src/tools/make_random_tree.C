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

#include "tree/tree.H"
#include "arguments.H"
#include "util.H"
#include "rng.H"

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  unsigned long seed =0;
  if (args.set("seed")) {
    seed = convertTo<unsigned long>(args["seed"]);
    myrand_init(seed);
  }
  else
    seed = myrand_init();

  assert(args.set("names"));
  vector<string> names = split(args["names"],':');

  double branch_mean = 0.1;
  if (args.set("mean"))
    branch_mean = convertTo<double>(args["mean"]);

  SequenceTree T = RandomTree(names,branch_mean);

  std::cout<<T.write()<<std::endl;
}
