#include "monitor.H"

#include <cstdlib>
#include <sstream>
extern "C" {
#include <sys/resource.h>
}

#include <valarray>
#include "substitution.H"
#include "likelihood.H"
#include "setup.H"
#include "alignment-util.H"

using std::valarray;

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

void show_parameters(std::ostream& o,const Model& M) {
  for(int i=0;i<M.parameters().size();i++) {
    o<<"    ";
    if (M.fixed(i)) 
      o<<"*";
    o<<M.parameter_name(i)<<" = "<<M.parameters()[i];
  }
  o<<"\n";
}

void print_stats(std::ostream& o,std::ostream& trees,std::ostream& pS,std::ostream& pI,
		 const alignment& A,const Parameters& P,const string& tag,bool print_alignment) {
  
  o<<"\n";
  o<<" no A  ["<<substitution::Pr_unaligned(A,P)<<"]     ";
  o<<" single sequence ["<<substitution::Pr_single_sequence(A,P)<<"]"<<"\n";
  o<<" sgsl  ["<<Pr_sgaps_sletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<"\n";
  o<<" sg    ["<<Pr_sgaps_tletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<"\n";
  o<<" sl    ["<<Pr_tgaps_sletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<"\n";
  o<<" Full  ["<<Pr_tgaps_tletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<"\n";
  
  efloat_t Pr_prior = P.basic_prior(A,P);
  efloat_t Pr_likelihood = P.basic_likelihood(A,P);
  efloat_t Pr = Pr_prior * Pr_likelihood;

  o<<"    prior = "<<Pr_prior<<"    likelihood = "<<Pr_likelihood<<"    logp = "<<Pr
   <<"    temp = " <<P.Temp  <<"    weight = "    <<pow(Pr,1.0-1.0/P.Temp)<<"\n";

  if (print_alignment) {
    o<<"align["<<tag<<"] = "<<"\n";
    o<<standardize(A,P.T)<<"\n\n";
  }
  
  trees<<P.T<<std::endl;
  
  pS<<  "    mu = "<<P.branch_mean<<"   ";
  show_parameters(pS,P.SModel());
  pS.flush();

  show_parameters(pI,P.IModel());
  pI.flush();
  
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
}

