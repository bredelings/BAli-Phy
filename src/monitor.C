#include "monitor.H"

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
  for(int i=0;i<M.parameters().size();i++)
    o<<"    "<<M.parameter_name(i)<<" = "<<M.parameters()[i];
  o<<"\n";
}

void print_stats(std::ostream& o,std::ostream& trees,std::ostream& pS,std::ostream& pI,
		 const alignment& A,const Parameters& P,const string& tag,bool print_alignment) {
  
  o<<"\n";
  o<<" no A  ["<<substitution::Pr_unaligned(A,P)<<"]"<<"\n";
  o<<" sgsl  ["<<Pr_sgaps_sletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<"\n";
  o<<" sg    ["<<Pr_sgaps_tletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<"\n";
  o<<" sl    ["<<Pr_tgaps_sletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<"\n";
  o<<" Full  ["<<Pr_tgaps_tletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<"\n";
  
  double Pr_prior = P.basic_prior(A,P);
  double Pr_likelihood = P.basic_likelihood(A,P);
  double Pr = Pr_prior + Pr_likelihood;

  o<<"    prior = "<<Pr_prior
   <<"    likelihood = "<<Pr_likelihood
   <<"    logp = "<<Pr
   <<"    temp = "<<P.Temp
   <<"    weight = "<<Pr*(1.0-1.0/P.Temp)
   <<"\n";

  if (print_alignment) {
    o<<"align["<<tag<<"] = "<<"\n";
    o<<standardize(A,P.T)<<"\n\n";
  }
  
  trees<<P.T<<"\n";
  
  pS<<  "    mu = "<<P.branch_mean<<"   ";
  show_parameters(pS,P.SModel());

  show_parameters(pI,P.IModel());
  
  for(int i=0;i<P.SModel().n_base_models();i++)
    o<<"    rate"<<i<<" = "<<P.SModel().base_model(i).rate();
  o<<"\n\n";

  o<<"frequencies = "<<"\n";
  show_frequencies(o,P.SModel());
  o<<"\n\n";

  // The leaf sequences should NOT change during alignment
#ifndef NDEBUG
  check_alignment(A,P.T,"print_stats:end");
#endif
}

