/*
   Copyright (C) 2004-2007,2009 Benjamin Redelings

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

#include "monitor.H"

#include <cstdlib>
#include <sstream>

/*
#ifdef HAVE_SYS_RESOURCE_H
extern "C" {
#include <sys/resource.h>
}
#endif
*/

#include <valarray>
#include "substitution/substitution.H"
#include "util.H"
#include "alignment/alignment.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"

using std::valarray;
using std::vector;
using std::endl;

void show_frequencies(std::ostream& o,const alphabet& a,const std::valarray<double>& f) {
    for(int i=0;i<a.size();i++)
      o<<"f"<<a.lookup(i)<<" = "<<f[i]<<"\n";
}

void show_frequencies(std::ostream& o,const data_partition& P)
{
  const alphabet& a = P.get_alphabet();

  auto F = P.FrequencyMatrix();

  int n_base_models = F.size1();
  assert(a.size() == F.size2());

  if (n_base_models == 1) {
    for(int i=0;i<a.size();i++)
      o<<"f"<<a.lookup(i)<<" = "<<F(0,i)<<"\n";
  }
  else {
    auto WF = P.WeightedFrequencyMatrix();
    for(int i=0;i<a.size();i++) {
      double total = 0;
      for(int m=0;m<n_base_models;m++) {
	o<<"f"<<a.lookup(i)<<m+1<<" = "<<F(m,i)<<"     ";
	total += WF(m,i);
      }
      o<<"f"<<a.lookup(i)<<" = "<<total<<"\n";
    }
  }
}

void show_smodel(std::ostream& o, const data_partition& P)
{
  //  for(int i=0;i<P.n_base_models();i++)
  //    o<<"    rate"<<i<<" = "<<convert<const substitution::ReversibleAdditiveObject>(P.base_model(i))->rate();
  //  o<<"\n\n";
  
  for(int i=0;i<P.n_base_models();i++)
    o<<"    fraction"<<i<<" = "<<P.distribution()[i];
  o<<"\n\n";

  o<<"frequencies = "<<"\n";
  show_frequencies(o,P);
}

void show_smodels(std::ostream& o, const Parameters& P)
{
  for(int m=0;m<P.n_data_partitions();m++) {
    o<<"data partition"<<m+1<<endl;
    show_smodel(o,P[m]);
  }
}

