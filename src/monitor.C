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
      o<<"f"<<a.lookup(i)<<" = "<<f[i]<<endl;
  }
  else {

    for(int i=0;i<a.size();i++) {
      double total = 0;
      for(int m=0;m<MModel.n_base_models();m++) {
	const valarray<double>& f = MModel.base_model(m).frequencies();
	o<<"f"<<a.lookup(i)<<m+1<<" = "<<f[i]<<"     ";
	total += MModel.distribution()[m] * f[i];
      }
      o<<"f"<<a.lookup(i)<<" = "<<total<<std::endl;
    }
  }
}

void show_parameters(std::ostream& o,const Model& M) {
  for(int i=0;i<M.parameters().size();i++)
    o<<"    "<<M.parameter_name(i)<<" = "<<M.parameters()[i];
  o<<endl;
}

void print_stats(std::ostream& o,std::ostream& trees,std::ostream& pS,std::ostream& pI,
		 const alignment& A,const Tree& T,const string& tag,bool print_alignment) {
  
  check_alignment(A,T,"print_stats:in");

    o<<"align["<<tag<<"] = "<<endl;
    //    alignment A2 = standardize(A,P.T);
    alignment A2 = A;
    alignment A3 = A2;
    alignment A4 = A3;
    check_alignment(A,T,"print_stats:2a");
    check_alignment(A2,T,"print_stats:2b");
    check_alignment(A3,T,"print_stats:2c");
    check_alignment(A4,T,"print_stats:2d");
    o<<A4<<endl<<endl;
  
  check_alignment(A,T,"print_stats:out");
}

