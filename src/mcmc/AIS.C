#include "AIS.H"

using std::vector;

// AIS samples sequences of points x[n] ... x[0], where x[0] is from the cold chain.
// * Each sequence then gets a weight.
// * Its possible to weight subsequences x[n] ... x[i] also.
// This yields weighted sample w(x[n]...x[i]) at level x[i], including i=0.

void AIS_Sampler::sample_from_beta(double beta, owned_ptr<Probability_Model>& P, int n, MCMC::Sampler& S0)
{
  P->set_beta(beta);
  for(int i=0;i<n;i++) {
    MCMC::MoveStats Stats;
    S0.iterate(P,Stats);
  }
}

void show_weights(const vector<vector<efloat_t> >& weights, std::ostream& o)
{
  o<<std::endl;
  for(int l=0;l<weights.size();l++)
  {
    const vector<efloat_t> w = weights[l];
    efloat_t sum = 0;
    efloat_t sum2 = 0;
    for(int i=0;i<w.size();i++)
    {
      sum += w[i];
      sum2 += w[i]*w[i];
    }
    sum /= w.size();
    sum2 /= w.size();

    efloat_t var_plus_1 = sum2/(sum*sum);
    efloat_t ESS = double(w.size())/var_plus_1;
    o<<"level = "<<l<<"   Pmarg = "<<sum<<"   var = "<<double(var_plus_1)-1.0<<"   ESS = "<<double(ESS)<<std::endl;
  }
  o<<std::endl;
}


void AIS_Sampler::go(owned_ptr<Probability_Model>& P, std::ostream& o, std::vector<double> beta, int n)
{
  o<<"Starting AIS:\n";
  assert(beta.size());
  assert(beta[0] == 1);
  assert(beta.back() == 0);

  vector<MCMC::Sampler> Samplers(beta.size(), S);

  // Try to forget the starting position
  sample_from_beta(beta.back(), P, 100, Samplers.back());

  // Generate our sequences
  vector<vector<efloat_t> > weights(beta.size()-1);
  vector<owned_ptr<Probability_Model> > X;
  for(int i=0;i<1000;i++)
  {
    owned_ptr<Probability_Model> P2;
    efloat_t weight = 1;
    for(int level=beta.size()-1; level >=0 ;level--)
    {
      if (level == beta.size() - 1) {
	sample_from_beta(beta[level], P, 100, Samplers[level]);
	P2 = P;
	continue;
      }

      sample_from_beta(beta[level], P2, n, Samplers[level]);
      
      efloat_t L = P2->likelihood();

      o<<"i = "<<i<<"  level = "<<level<<"  beta = "<<beta[level]<<" L = "<<L<<" w = "<<weight<<std::endl;
      weight *= pow(L,beta[level]-beta[level+1]);
      weights[level].push_back(weight);
    }
    X.push_back(P2);

    o<<"i = "<<i<<"  w = "<<weight<<std::endl;
    show_weights(weights,o);
  }
}
