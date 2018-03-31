#include "AIS.H"

using std::vector;

// AIS samples sequences of points x[n] ... x[0], where x[0] is from the cold chain.
// * Each sequence then gets a weight.
// * Its possible to weight subsequences x[n] ... x[i] also.
// This yields weighted sample w(x[n]...x[i]) at level x[i], including i=0.

void AIS_Sampler::sample_from_beta(double beta, owned_ptr<Model>& P, int n, MCMC::Sampler& S0)
{
    P->set_beta(beta);
    for(int i=0;i<n;i++) {
	MCMC::MoveStats Stats;
	S0.iterate(P,Stats);
	if (log_verbose)
	{
	    std::cerr<<"sample_from_beta: "<<i+1<<"/"<<n<<"\n";
	    show_parameters(std::cerr,*P);
	}
    }
}

void show_weights(const vector<vector<log_double_t> >& weights, std::ostream& o)
{
    o<<std::endl;
    for(int l=0;l<weights.size();l++)
    {
	const vector<log_double_t> w = weights[l];
	log_double_t sum = 0;
	log_double_t sum2 = 0;
	for(int i=0;i<w.size();i++)
	{
	    sum += w[i];
	    sum2 += w[i]*w[i];
	}
	sum /= w.size();
	sum2 /= w.size();

	log_double_t var_plus_1 = sum2/(sum*sum);
	log_double_t ESS = double(w.size())/var_plus_1;
	o<<"level = "<<l<<"   Pmarg = "<<sum<<"   var = "<<double(var_plus_1)-1.0<<"   ESS = "<<double(ESS)<<std::endl;
    }
    o<<std::endl;
}


void AIS_Sampler::go(owned_ptr<Model>& P, std::ostream& o, std::vector<double> beta)
{
    o<<"Starting AIS:\n";
    assert(beta.size());
    assert(beta[0] == 1);
    assert(beta.back() == 0);

    const int reps = P->load_value("AIS:reps", 10);
    const int n_particles  = P->load_value("AIS:n_particles", 20);

    vector<MCMC::Sampler> Samplers(beta.size(), S);

    // Try to forget the starting position
    sample_from_beta(beta.back(), P, 100, Samplers.back());

    // Generate our sequences
    vector<vector<log_double_t> > weights(beta.size()-1);
    vector<owned_ptr<Model> > X;
    for(int iterations=0; iterations<n_particles; iterations++)
    {
	owned_ptr<Model> P2 = P;
	log_double_t weight = 1;
	for(int level=beta.size()-1; level >=0 ;level--)
	{
	    sample_from_beta(beta[level], P2, reps, Samplers[level]);
	    
	    // Generate a new sample at the hottest level to start the next temperature chain from
	    if (level == beta.size() - 1)
	    {
		P = P2;
	    }
	    else
	    {
		log_double_t L = P2->likelihood();

		o<<"iter = "<<iterations<<"  level = "<<level<<"  beta = "<<beta[level]<<" L = "<<L<<" w = "<<weight<<std::endl;
		weight *= pow(L,beta[level]-beta[level+1]);
		weights[level].push_back(weight);
	    }
	    S.run_loggers(*P2, iterations);
	}
	// Record the cold temperature particles
	X.push_back(P2);

	o<<"i = "<<iterations<<"  w = "<<weight<<std::endl;
	show_weights(weights,o);
    }
}
