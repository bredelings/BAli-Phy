#include "rates.H"
#include "smodel/operations.H"
#include "distribution-operations.H"

using boost::shared_ptr;
using std::vector;
using std::valarray;
using std::string;
using boost::shared_ptr;

namespace substitution
{
  using namespace probability;

  shared_ptr<ReversibleFrequencyModelObject> Plus_gwF_Function(const alphabet& a, double f, const vector<double>& pi)
  {
    assert(a.size() == pi.size());

    shared_ptr<ReversibleFrequencyModelObject> R( new ReversibleFrequencyModelObject(a) );

    // compute frequencies
    R->pi = pi;
    normalize(R->pi);
    
    // compute transition rates
    valarray<double> pi_f(a.size());
    for(int i=0;i<a.size();i++)
      pi_f[i] = pow(R->pi[i],f);

    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++)
	R->R(i,j) = pi_f[i]/R->pi[i] * pi_f[j];

    // diagonal entries should have no effect
    for(int i=0;i<a.size();i++)
      R->R(i,i) = 0;

    return R;
  }

  expression_ref Plus_gwF(const alphabet& a)
  {
    return lambda_expression( Plus_gwF_Op(a) );
  }

  shared_ptr<ExchangeModelObject> SimpleExchangeFunction(double rho, int n)
  {
    shared_ptr<ExchangeModelObject> R (new ExchangeModelObject(n));

    for(int i=0;i<n;i++) {
      for(int j=0;j<n;j++)
	R->S(i,j) = rho;

      R->S(i,i) = 0;       // this is NOT a rate away.
    }

    return R;
  }

  shared_ptr<ExchangeModelObject> EQU_Exchange_Function(const alphabet& a)
  {
    shared_ptr<ExchangeModelObject> R ( new ExchangeModelObject(a.size()) );

    // Calculate S matrix
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++)
	R->S(i,j) = 1;

    return R;
  }

  shared_ptr<AlphabetExchangeModelObject> HKY_Function(const Nucleotides& a, double kappa)
  {
    assert(a.size()==4);

    shared_ptr<AlphabetExchangeModelObject> R ( new AlphabetExchangeModelObject(a) );

    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++) {
	if (i==j) continue;
	if (a.transversion(i,j))
	  R->S(i,j) = 1;
	else
	  R->S(i,j) = kappa;
      }

    return R;
  }

  formula_expression_ref HKY_Model(const alphabet& a)
  {
    expression_ref kappa = parameter("HKY::kappa");
    formula_expression_ref R(lambda_expression(HKY_Op())(a,kappa));
    R.add_expression(kappa);
    R.add_expression(default_value(kappa,2.0));
    R.add_expression(bounds(kappa,lower_bound(0.0)));
    R.add_expression(distributed_as(log_laplace_dist, kappa, Tuple(2)(log(2), 0.25) ) );
    
    return R;
  }
  
  shared_ptr<ExchangeModelObject> TN_Function(const Nucleotides& a, double kappa1, double kappa2)
  {
    assert(a.size()==4);
  
    shared_ptr<AlphabetExchangeModelObject> R ( new AlphabetExchangeModelObject(a) );
  
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++) {
	if (i==j) continue;
	if (a.transversion(i,j))
	  R->S(i,j) = 1;
	else if (a.purine(i))
	  R->S(i,j) = kappa1;
	else
	  R->S(i,j) = kappa2;
      }
  
    return R;
  }

  formula_expression_ref TN_Model(const alphabet& a)
  {
    expression_ref kappa1 = parameter("TN::kappa(pur)");
    expression_ref kappa2 = parameter("TN::kappa(pyr)");

    formula_expression_ref R(lambda_expression(TN_Op())(a,kappa1,kappa2));

    R.add_expression(kappa1);
    R.add_expression(kappa2);

    R.add_expression(default_value(kappa1,2.0));
    R.add_expression(default_value(kappa2,2.0));

    R.add_expression(bounds(kappa1,lower_bound(0.0)));
    R.add_expression(bounds(kappa2,lower_bound(0.0)));

    R.add_expression(distributed_as(log_laplace_dist, kappa1, Tuple(2)(log(2), 0.25) ) );
    R.add_expression(distributed_as(log_laplace_dist, kappa2, Tuple(2)(log(2), 0.25) ) );
    
    return R;
  }
  
  shared_ptr<AlphabetExchangeModelObject> INV_Exchange_Function(const alphabet& a)
  {
    shared_ptr<AlphabetExchangeModelObject> R ( new AlphabetExchangeModelObject(a) );

    // Calculate S matrix
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++)
	R->S(i,j) = 0;

    return R;
  }

  shared_ptr<AlphabetExchangeModelObject> GTR_Function(const Nucleotides& a, 
						       double AG, double AT, double AC,
						       double GT, double GC, 
						       double TC)
  {
    assert(a.size()==4);

    shared_ptr<AlphabetExchangeModelObject> R ( new AlphabetExchangeModelObject(a) );

    double total = AG + AT + AC + GT + GC + TC;

    R->S(0,1) = AG/total;
    R->S(0,2) = AT/total;
    R->S(0,3) = AC/total;

    R->S(1,2) = GT/total;
    R->S(1,3) = GC/total;

    R->S(2,3) = TC/total;

    return R;
  }

  formula_expression_ref GTR_Model(const alphabet& a)
  {
    expression_ref AG = parameter("GTR::AG");
    expression_ref AT = parameter("GTR::AT");
    expression_ref AC = parameter("GTR::AC");
    expression_ref GT = parameter("GTR::GT");
    expression_ref GC = parameter("GTR::GC");
    expression_ref TC = parameter("GTR::TC");

    formula_expression_ref R(lambda_expression(GTR_Op())(a)(AG)(AT)(AC)(GT)(GC)(TC));

    R.add_expression(AG);
    R.add_expression(AT);
    R.add_expression(AC);
    R.add_expression(GT);
    R.add_expression(GC);
    R.add_expression(TC);

    R.add_expression(default_value(AG,2.0/8));
    R.add_expression(default_value(AT,1.0/8));
    R.add_expression(default_value(AC,1.0/8));
    R.add_expression(default_value(GT,1.0/8));
    R.add_expression(default_value(GC,1.0/8));
    R.add_expression(default_value(GC,1.0/8));
    R.add_expression(default_value(TC,2.0/8));

    R.add_expression(bounds(AG,between(0.0,1.0)));
    R.add_expression(bounds(AT,between(0.0,1.0)));
    R.add_expression(bounds(AC,between(0.0,1.0)));
    R.add_expression(bounds(GT,between(0.0,1.0)));
    R.add_expression(bounds(GC,between(0.0,1.0)));
    R.add_expression(bounds(GC,between(0.0,1.0)));
    R.add_expression(bounds(TC,between(0.0,1.0)));

    R.add_expression(distributed_as(dirichlet_dist, 
				    Tuple(6)(AG)(AT)(AC)(GT)(GC)(TC), 
				    Tuple(6)(8.0)(4.0)(4.0)(4.0)(4.0)(8.0)
				    )
		     );
    
    return R;
  }
  
  shared_ptr<AlphabetExchangeModelObject> SingletToTripletExchangeFunction(const Triplets& T, const ExchangeModelObject& S2)
  {
    shared_ptr<AlphabetExchangeModelObject> R ( new AlphabetExchangeModelObject(T) );
    ublas::symmetric_matrix<double>& S = R->S;

    for(int i=0;i<T.size();i++)
      for(int j=0;j<i;j++) 
      {
	int nmuts=0;
	int pos=-1;
	for(int p=0;p<3;p++)
	  if (T.sub_nuc(i,p) != T.sub_nuc(j,p)) {
	    nmuts++;
	    pos=p;
	  }
	assert(nmuts>0);
	assert(pos >= 0 and pos < 3);
	
	S(i,j) = 0;

	if (nmuts == 1) {

	  int l1 = T.sub_nuc(i,pos);
	  int l2 = T.sub_nuc(j,pos);
	  assert(l1 != l2);

	  S(i,j) = S2(l1,l2);
	}
      }

    return R;
  }

  formula_expression_ref Plus_gwF_Model(const alphabet& a, const valarray<double>& pi)
  {
    assert(a.size() == pi.size());
    shared_ptr<Formula> F (new Formula);
    
    vector<expression_ref> parameters;
    vector<expression_ref> Vars;
    vector<expression_ref> N;
    expression_ref f = parameter("f");
    F->add_expression( f );
    F->add_expression( default_value( f, 1.0) );
    F->add_expression( bounds( f, between(0.0,1.0) ) );
    F->add_expression( distributed_as( prob_density("Uniform",uniform_density()), 
				       f,
				       Tuple(2)(0.0,1.0)
				       ) 
		       );


    for(int i=0;i<a.size();i++)
    {
      string pname = string("pi") + a.letter(i);
      Vars.push_back( parameter(pname) );
      N.push_back( Double(1.0) );
      F->add_expression( parameter(pname) );
      F->add_expression( default_value( parameter(pname) , pi[i] ) );
      F->add_expression( bounds( parameter(pname) , between(0.0, 1.0) ) );
    }

    F->add_expression( distributed_as( dirichlet_dist,
				       Tuple(a.size())(Vars), 
				       Tuple(a.size())(N)
				       ) 
		       );

    return formula_expression_ref(F,Plus_gwF(a)(f)(Vars));
  }

  formula_expression_ref Plus_gwF_Model(const alphabet& a)
  {
    valarray<double> pi (1.0/a.size(), a.size());
    return Plus_gwF_Model(a,pi);
  }

  shared_ptr<ReversibleMarkovModelObject> Q_from_S_and_R_Function(const ExchangeModelObject& S, const ReversibleFrequencyModelObject& F)
  {
    shared_ptr<ReversibleMarkovModelObject> R ( new ReversibleMarkovModelObject(F.Alphabet()) );

    R->Alphabet();
    // This doesn't work for Modulated markov models
    assert(F.n_states() == F.Alphabet().size());

    // The exchange model and the frequency model should have the same number of states, if not the same alphabet
    assert(S.n_states() == F.n_states());

    const unsigned N = S.n_states();

    // recompute rate matrix
    Matrix& Q = R->Q;

    for(int i=0;i<N;i++) {
      double sum=0;
      for(int j=0;j<N;j++) {
	if (i==j) continue;
	Q(i,j) = S(i,j) * F(i,j);
	sum += Q(i,j);
      }
      Q(i,i) = -sum;
    }

    R->invalidate_eigensystem();
    
    R->pi = F.pi;

    return R;
  }

  expression_ref Q_from_S_and_R = lambda_expression( Q_from_S_and_R_Op() );

  shared_ptr<AlphabetExchangeModelObject> M0_Function(const Codons& C, const ExchangeModelObject& S2,double omega)
  {
    shared_ptr<AlphabetExchangeModelObject> R ( new AlphabetExchangeModelObject(C) );
    ublas::symmetric_matrix<double>& S = R->S;

    for(int i=0;i<C.size();i++) 
    {
      for(int j=0;j<i;j++) {
	int nmuts=0;
	int pos=-1;
	for(int p=0;p<3;p++)
	  if (C.sub_nuc(i,p) != C.sub_nuc(j,p)) {
	    nmuts++;
	    pos=p;
	  }
	assert(nmuts>0);
	assert(pos >= 0 and pos < 3);

	double rate=0.0;

	if (nmuts == 1) {

	  int l1 = C.sub_nuc(i,pos);
	  int l2 = C.sub_nuc(j,pos);
	  assert(l1 != l2);

	  rate = S2(l1,l2);

	  if (C.translate(i) != C.translate(j))
	    rate *= omega;	
	}

	S(i,j) = S(j,i) = rate;
      }
    }

    return R;
  }


  expression_ref M0E = lambda_expression( M0_Op() );

  formula_expression_ref Reversible_Markov_Model(const formula_expression_ref& FS, const formula_expression_ref& FR)
  {
    formula_expression_ref S = prefix_formula("S",FS);
    formula_expression_ref R = prefix_formula("R",FR);
    
    return Q_from_S_and_R(S)(R);
  }

  formula_expression_ref Simple_gwF_Model(const formula_expression_ref& FS, const alphabet& a)
  {
    formula_expression_ref S = prefix_formula("S",FS);
    formula_expression_ref R = prefix_formula("R",Plus_gwF_Model(a));
    
    return Q_from_S_and_R(S)(R);
  }

  formula_expression_ref Simple_gwF_Model(const formula_expression_ref& FS, const alphabet& a, const valarray<double>& pi)
  {
    formula_expression_ref S = prefix_formula("S",FS);
    formula_expression_ref R = prefix_formula("R",Plus_gwF_Model(a,pi));
    
    return Q_from_S_and_R(S)(R);
  }

  boost::shared_ptr<DiscreteDistribution> DiscretizationFunction(const Distribution& D, Int n)
  {
    // Make a discretization - not uniform.
    Discretization d(n,D);

    double ratio = d.scale()/D.mean();
    
    // this used to affect the prior
    //    bool good_enough = (ratio > 1.0/1.5 and ratio < 1.5);

    // problem - this isn't completely general
    d.scale(1.0/ratio);
    
    boost::shared_ptr<DiscreteDistribution> R( new DiscreteDistribution(n) );
    R->fraction = d.f;

    for(int i=0;i<n;i++)
    {
      Double V = d.r[i];
      R->values[i] = boost::shared_ptr<Double>( V.clone() );
    }

    return R;
  }

  expression_ref Discretize = lambda_expression( DiscretizationOp() );

  shared_ptr<MultiModelObject> MultiParameterFunction(const ModelFunction& F, const DiscreteDistribution& D)
  {
    shared_ptr<MultiModelObject> R;

    for(int i=0;i<D.fraction.size();i++)
    {
      shared_ptr<const MultiModelObject> M = dynamic_pointer_cast<const MultiModelObject>(F(D.values[i]));

      if (not R) R = shared_ptr<MultiModelObject>(new MultiModelObject);
      
      for(int j=0;j<M->n_base_models();j++)
      {
	R->fraction.push_back( D.fraction[i] * M->distribution()[j] );
	R->base_models.push_back( const_ptr( M->base_model(j) ) );
      }
    }

    return R;
  }

  MultiModelObject MultiRateFunction(const MultiModelObject& M_, const DiscreteDistribution& D)
  {
    shared_ptr<MultiModelObject> M = ptr(M_);

    int N = M->n_base_models() * D.size();

    MultiModelObject R;

    // recalc fractions and base models
    R.resize(N);

    for(int m=0;m<R.n_base_models();m++) 
    {
      int i = m / M->n_base_models();
      int j = m % M->n_base_models();

      R.fraction[m] = D.fraction[i]*M->distribution()[j];

      Double value = dynamic_cast<const Double&>(*D.values[i]);
      M->set_rate( value );

      R.base_models[m] = ptr( M->base_model(j) );
    }

    return R;
  }

  expression_ref MultiParameter = lambda_expression( MultiParameterOp() );

  expression_ref MultiRate = lambda_expression( MultiRateOp() );

  // We want Q(mi -> mj) = Q[m](i -> j)   for letter exchange
  //         Q(mi -> ni) = R(m->n)        for model exchange
  // and     Q(mi -> nj) = 0              for all other pairs

  // We assume that R(m->n) = S(m,n) * M->distribution()[n]

  // This should result in a Markov chain where the frequencies are
  //  frequencies()[mi] = pi[i] * f[m] 
  // with pi = M->frequencies() 
  // and   f = M->distribution()

  // PROBLEM: I don't have a good way of defining the switching rate.
  // Right now, I have S(m,n) = rho, S(m,m) = 0
  // But, the S(m,n) do not correspond to switching rates exactly.
  // Instead, the switching rate is now rho*f[n], which is going to
  // be something like rho*(n-1)/n if there are n categories.
  
  // ADDITIONALLY, depending on how fine-grained the categories are,
  // a switching rate has a different interpretation.

  // HOWEVER, I think the current approach works for now, because it
  // approximates the model that at rate 'rho' the rate is randomly
  // re-drawn from the underlying distribution.  A lot of the time it
  // will fall in the same bin, giving a lower observed switching rate
  // when the discrete approximation to the continuous distribution has
  // low resolution.

  shared_ptr<ReversibleMarkovModelObject> Modulated_Markov_Function(const ExchangeModelObject& S,MultiModelObject M)
  {
    // Make a copy and use this.
    M.set_rate(1);

    unsigned T = 0;
    for(int m=0; m < M.n_base_models(); m++) 
    {
      const ReversibleMarkovModelObject* RM = dynamic_cast<const ReversibleMarkovModelObject*>(&M.base_model(m).part(0));
      if (not RM)
	throw myexception()<<"Can't construct a modulated Markov model from non-Markov model"; // what is the name?
      T += RM->n_states();
    }

    shared_ptr<ReversibleMarkovModelObject> R ( new ReversibleMarkovModelObject(*M.get_alphabet(), T) );

    // calculate the state_letters() map here!

    T = 0;
    for(int m=0; m < M.n_base_models(); m++) 
    {
      unsigned N = M.base_model(m).n_states();
      for(int i=0; i<N; i++)
	R->state_letters_[T+i] = M.base_model(m).state_letters()[i];

      T += N;
    }

    const int n_models = M.n_base_models();


    const valarray<double>& M_pi = M.frequencies();
    const vector<double>&   M_f  = M.distribution();

    // calculate pi[ ] for each state
    T = 0;
    for(int m=0; m < n_models; m++) {
      unsigned N = M.base_model(m).n_states();
      for(int s=0; s < N; s++) 
	R->pi[T+s] = M_pi[s] * M_f[m];
      T += N;
    }
    

    // initially zero out the matrix
    for(int i=0;i<R->Q.size1();i++)
      for(int j=0;j<R->Q.size2();j++)
	R->Q(i,j) = 0;

    // rates for within-model transitions
    T=0;
    for(int m=0; m < n_models; m++) 
    {
      const ReversibleMarkovModelObject* RM = dynamic_cast<const ReversibleMarkovModelObject*>(&M.base_model(m).part(0));
      if (not RM)
	throw myexception()<<"Can't construct a modulated Markov model from non-Markov model"; // what is the name?

      unsigned N = RM->n_states();
      
      for(int s1=0; s1 < N; s1++) 
	for(int s2=0; s2 < N; s2++)
	  R->Q(T+s1,T+s2) = RM->Q(s1,s2);

      T += N;
    }

    // rates for between-model transitions
    unsigned T1=0;
    for(int m1=0; m1 < n_models; m1++) 
    {
      const ReversibleMarkovModelObject* RM1 = dynamic_cast<const ReversibleMarkovModelObject*>(&M.base_model(m1).part(0));
      unsigned N1 = RM1->n_states();

      unsigned T2=0;
      for(int m2=0; m2 < n_models; m2++) 
      {
	const ReversibleMarkovModelObject* RM2 = dynamic_cast<const ReversibleMarkovModelObject*>(&M.base_model(m2).part(0));
	unsigned N2 = RM2->n_states();
	assert(N1 == N2);

	if (m1 != m2) {
	  double S12 = S(m1,m2);
	  for(int s1=0;s1<N1;s1++)
	    R->Q(T1+s1,T2+s1) = S12*M_f[m2];
	}

	T2 += N2;
      }
      T1 += N1;
    }

    // recompute diagonals 
    for(int i=0;i<R->Q.size1();i++) 
    {
      double sum=0;
      for(int j=0;j<R->Q.size2();j++)
	if (i!=j)
	  sum += R->Q(i,j);
      R->Q(i,i) = -sum;
    }

    R->invalidate_eigensystem();

    return R;
  }

  expression_ref Modulated_Markov_E = lambda_expression( Modulated_Markov_Op() );

  shared_ptr< DiscreteDistribution > M2_Function(Double f1, Double f2, Double f3, Double omega)
  {
    shared_ptr< DiscreteDistribution > R ( new DiscreteDistribution(3) );
    R->fraction[0] = f1;
    R->fraction[1] = f2;
    R->fraction[2] = f3;

    R->values[0] = ptr( Double(0) );
    R->values[1] = ptr( Double(1) );
    R->values[2] = ptr( omega );

    return R;
  }

}
