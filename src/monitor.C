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
#include "substitution.H"
#include "likelihood.H"
#include "setup.H"
#include "alignment-util.H"

using std::valarray;

void show_frequencies(std::ostream& o,const alphabet& a,const std::valarray<double>& f) {
    for(int i=0;i<a.size();i++)
      o<<"f"<<a.lookup(i)<<" = "<<f[i]<<"\n";
}


void show_frequencies(std::ostream& o,const substitution::MultiModel& MModel) {
  const alphabet& a = MModel.Alphabet();

  if (MModel.n_base_models() == 1) {
    const valarray<double>& f = MModel.base_model(0).frequencies();
    for(int i=0;i<a.size();i++)
      o<<"f"<<a.lookup(i)<<" = "<<f[i]<<"\n";
  }
  else {

    for(int i=0;i<a.size();i++) {
      double total = 0;
      for(int m=0;m<MModel.n_base_models();m++) {
	const valarray<double>& f = MModel.base_model(m).frequencies();
	o<<"f"<<a.lookup(i)<<m+1<<" = "<<f[i]<<"     ";
	total += MModel.distribution()[m] * f[i];
      }
      o<<"f"<<a.lookup(i)<<" = "<<total<<"\n";
    }
  }
}

void print_stats(std::ostream& o,std::ostream& trees,
		 const alignment& A,const Parameters& P,
		 bool print_alignment) 
{
   efloat_t Pr_prior = P.basic_prior(A,P);
  efloat_t Pr_likelihood = P.basic_likelihood(A,P);
  efloat_t Pr = Pr_prior * Pr_likelihood;

  o<<"    prior = "<<Pr_prior<<"    likelihood = "<<Pr_likelihood<<"    logp = "<<Pr
   <<"    beta = " <<P.beta[0]  <<"\n";

  if (print_alignment)
    o<<standardize(A,P.T)<<"\n";
  
  trees<<P.T<<std::endl;
  trees.flush();
  
  o<<"\n";
  show_parameters(o,P);
  o.flush();

  for(int i=0;i<P.SModel().n_base_models();i++)
    o<<"    rate"<<i<<" = "<<P.SModel().base_model(i).rate();
  o<<"\n\n";

  for(int i=0;i<P.SModel().n_base_models();i++)
    o<<"    fraction"<<i<<" = "<<P.SModel().distribution()[i];
  o<<"\n\n";

  o<<"frequencies = "<<"\n";
  show_frequencies(o,P.SModel());
  o<<"\n\n";
  o.flush();

  // The leaf sequences should NOT change during alignment
#ifndef NDEBUG
  check_alignment(A,P.T,"print_stats:end");
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

