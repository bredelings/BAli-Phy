#include "probability.H"

using std::valarray;

double dirichlet_log_pdf(const valarray<double>& p,const valarray<double>& n) {
  assert(p.size() == n.size());

  double Pr = 0;
  for(int i=0;i<p.size();i++) {
    Pr += n[i]*log(p[i]);

    // This is because we our proposal is symmetric on the scale of log(p[i]).
    Pr += p[i];
  }

  // This term is constant in p
  //  Pr += log_gamma(n.sum()+n.size());
  //  for(int i=0;i<p.size();i++)
  //    Pr -= log_gamma(n[i]+1);

  return Pr;
}

double dirichlet_log_pdf(const valarray<double>& p,const valarray<double>& q,
			 double N) {
  return dirichlet_log_pdf(p,q*N);
}

