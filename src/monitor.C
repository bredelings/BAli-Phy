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
#include "likelihood.H"
#include "util.H"
#include "setup.H"
#include "alignment/alignment.H"
#include "alignment/alignment-util.H"

using std::valarray;
using std::vector;
using std::endl;

void show_frequencies(std::ostream& o,const alphabet& a,const std::valarray<double>& f) {
    for(int i=0;i<a.size();i++)
      o<<"f"<<a.lookup(i)<<" = "<<f[i]<<"\n";
}

void show_frequencies(std::ostream& o,const Mat_Cache& MC)
{
  const alphabet& a = MC.get_alphabet();

  if (MC.n_base_models() == 1) {
    vector<double> f = MC.frequencies(0);
    for(int i=0;i<a.size();i++)
      o<<"f"<<a.lookup(i)<<" = "<<f[i]<<"\n";
  }
  else {

    for(int i=0;i<a.size();i++) {
      double total = 0;
      for(int m=0;m<MC.n_base_models();m++) {
	vector<double> f = MC.frequencies(m);
	o<<"f"<<a.lookup(i)<<m+1<<" = "<<f[i]<<"     ";
	total += MC.distribution()[m] * f[i];
      }
      o<<"f"<<a.lookup(i)<<" = "<<total<<"\n";
    }
  }
}

void show_smodel(std::ostream& o, const Mat_Cache& MC)
{
  //  for(int i=0;i<MC.n_base_models();i++)
  //    o<<"    rate"<<i<<" = "<<convert<const substitution::ReversibleAdditiveObject>(MC.base_model(i))->rate();
  //  o<<"\n\n";
  
  for(int i=0;i<MC.n_base_models();i++)
    o<<"    fraction"<<i<<" = "<<MC.distribution()[i];
  o<<"\n\n";

  o<<"frequencies = "<<"\n";
  show_frequencies(o,MC);
}

void show_smodels(std::ostream& o, const Parameters& P)
{
  for(int m=0;m<P.n_data_partitions();m++) {
    o<<"data partition"<<m+1<<endl;
    show_smodel(o,P[m]);
  }
}

void print_stats(std::ostream& o, const Parameters& P, bool /* print_alignment */) 
{
  log_double_t Pr_prior = P.prior();
  log_double_t Pr_likelihood = P.likelihood();
  log_double_t Pr = Pr_prior * Pr_likelihood;

  o<<"    prior = "<<Pr_prior;
  for(int i=0;i<P.n_data_partitions();i++) 
    o<<"   prior_A"<<i+1<<" = "<<P[i].prior_alignment();

  o<<"    likelihood = "<<Pr_likelihood<<"    logp = "<<Pr
   <<"    beta = " <<P.get_beta()  <<"\n";

  o<<"\n";
  show_parameters(o,P);
  o.flush();

  //  This takes too much disk space!
  //  show_smodels(o,P);
  //  o.flush();

  // The leaf sequences should NOT change during alignment
#ifndef NDEBUG
  for(int i=0;i<P.n_data_partitions();i++)
    check_alignment(P[i].A(), P.T(), "print_stats:end");
#endif
}

void report_mem() {
#if !defined(_MSC_VER) && !defined(__MINGW32__)
/*
  struct rusage usage;
  std::cerr<<getrusage(RUSAGE_SELF,&usage);
  std::cerr<<"Maximum resident set size: "<<usage.ru_maxrss<<"\n";
  std::cerr<<"Integral shared memory size: "<<usage.ru_ixrss<<"\n";
  std::cerr<<"Integral unshared data size: "<<usage.ru_idrss<<"\n";
  std::cerr<<"Integral unshared stack size: "<<usage.ru_isrss<<"\n";
  std::cerr<<"Number of swaps: "<<usage.ru_nswap<<"\n";
  std::cerr.flush();
*/
  int pid = getpid();
  std::ostringstream cmd;
  cmd<<"cat /proc/"<<pid<<"/status | grep Vm 1>&2";
  system(cmd.str().c_str());
  std::cerr.flush();
#endif
}

