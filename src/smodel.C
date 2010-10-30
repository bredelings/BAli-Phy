/*
   Copyright (C) 2004-2009 Benjamin Redelings

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

#include <algorithm>
#include <fstream>
#include <sstream>
#include "smodel.H"
#include "exponential.H"
#include "rng.H"
#include "util.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include "logsum.H"
#include "likelihood.H"
#include "probability.H"
#include "proposals.H"

using std::vector;
using std::valarray;
using std::string;
using std::cerr;
using std::endl;
using std::istringstream;

namespace substitution {

  string s_parameter_name(int i,int n) {
    if (i>=n)
      throw myexception()<<"substitution model: referred to parameter "<<i<<" but there are only "<<n<<" parameters.";
    return string("pS") + convertToString(i);
  }

  template <typename T>
  valarray<T> get_varray(const vector<T>& v1,int start, int n) 
  {
    assert(start>=0);
    assert(n>0);
    assert(start + n <= v1.size());

    valarray<T> v2(n);
    for(int i=0;i<v2.size();i++)
      v2[i] = v1[start+i];
    return v2;
  }

  template <typename T>
  inline valarray<T> get_varray(const vector<T>& v1) 
  {
    return get_varray(v1,0,v1.size());
  }

  template <typename T>
  void set_varray(vector<T>& v1,int start,const valarray<T>& v2) 
  {
    assert(start>=0);
    assert(v2.size() > 0);
    assert(start + v2.size() <= v1.size());

    //copy from valarray
    for(int i=0;i<v2.size();i++)
      v1[start + i] = v2[i];
  }

  template <typename T>
  inline valarray<T> set_varray(const vector<T>& v1,const valarray<T>& v2) 
  {
    return set_varray(v1,v2);
  }


  efloat_t dirichlet_pdf(const vector<double>& p1,int start, int n, const valarray<double>& q)
  {
    valarray<double> p2 = get_varray(p1,start,n);

    return ::dirichlet_pdf(p2,q);
  }

  efloat_t dirichlet_pdf(const vector<double>& p1,const valarray<double>& q)
  {
    return dirichlet_pdf(p1,0,p1.size(),q);
  }

  efloat_t dirichlet_pdf(const vector<double>& p1,int start, int n, double N)
  {
    valarray<double> p2 = get_varray(p1,start,n);

    return ::dirichlet_pdf(p2,N);
  }

  efloat_t dirichlet_pdf(const vector<double>& p1,double N)
  {
    return dirichlet_pdf(p1,0,p1.size(),N);
  }



  ExchangeModel::ExchangeModel(unsigned n)
    :S(n,n)
  {}

  efloat_t SimpleExchangeModel::prior() const {
    return laplace_pdf(log(rho()), -3, 1)/rho();
  }

  void SimpleExchangeModel::recalc(const vector<int>&)
  {
    double r = rho();
    for(int i=0;i<n_states();i++) {
      for(int j=0;j<n_states();j++)
	S(i,j) = r;

      S(i,i) = 0;       // this is NOT a rate away.
    }
  }

  string SimpleExchangeModel::name() const 
  {
    return "SimpleExchangeModel";
  }

  SimpleExchangeModel::SimpleExchangeModel(unsigned n)
    :ExchangeModel(n)
  {
    add_parameter(Parameter("rho",exp(-4),lower_bound(0)));
    recalc_all();
  }


  AlphabetExchangeModel::AlphabetExchangeModel(const alphabet& a)
    :ExchangeModel(a.size())
  {}

  //----------------------- Frequency Models ------------------------//

  ReversibleFrequencyModel::ReversibleFrequencyModel(const alphabet& a)
    :R(a.size(),a.size()),
     pi(1.0/a.size(),a.size())
  { }

  void UniformFrequencyModel::frequencies(const valarray<double>&) 
  { }

  void UniformFrequencyModel::recalc(const vector<int>&)
  {
    for(int i=0;i<n_letters();i++)
      pi[i] = 1;
    pi /= pi.sum();
    
    // compute transition rates
    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = 1;

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  string UniformFrequencyModel::name() const {
    return "UF";
  }
  
  UniformFrequencyModel::UniformFrequencyModel(const alphabet& a)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    // initialize everything
    recalc_all();
  }

  UniformFrequencyModel::UniformFrequencyModel(const alphabet& a,const valarray<double>&)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    // initialize everything
    recalc_all();
  }


  void SimpleFrequencyModel::frequencies(const valarray<double>& pi2) 
  {
    assert(pi2.size() == n_letters());

    // set the frequency parameters
    for(int i=0;i<n_letters();i++)
      parameters_[i+1].value = pi2[i];

    // recompute everything
    recalc_all();
  }

  void SimpleFrequencyModel::recalc(const vector<int>&)
  {
    // compute frequencies
    pi = get_varray(get_parameter_values(),1,n_letters());
    pi /= pi.sum();
    
    // compute transition rates
    valarray<double> pi_f(n_letters());
    for(int i=0;i<n_letters();i++)
      pi_f[i] = pow(pi[i],f());

    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = pi_f[i]/pi[i] * pi_f[j];

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  efloat_t SimpleFrequencyModel::prior() const 
  {
    // uniform prior on f
    efloat_t Pr = 1;

    // uniform - 1 observeration per letter
    return dirichlet_pdf(get_parameter_values(), 1, n_letters(), 1.0);
  }

  string SimpleFrequencyModel::name() const 
  {
    if (is_fixed(0) and get_parameter_value(0) == 1.0)
      return "F";
    else
      return "gwF";
  }
  
  SimpleFrequencyModel::SimpleFrequencyModel(const alphabet& a)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    // Start with *f = 1
    add_parameter(Parameter("f",1.0,between(0, 1)));
    parameters_[0].fixed = true;

    for(int i=0;i<n_letters();i++) {
      string pname = string("pi") + Alphabet().letter(i);
      add_parameter(Parameter(pname, 1.0/n_letters(), between(0, 1)));
    }

    // initialize everything
    recalc_all();
  }

  SimpleFrequencyModel::SimpleFrequencyModel(const alphabet& a,const valarray<double>& pi)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    if (pi.size() != a.size())
      throw myexception()<<"Constructing a frequency model on alphabet '"<<a.name<<"' but got frequencies of "<<pi.size()<<" letters instead of the expected "<<a.size();

    add_parameter(Parameter("f", 1.0, between(0, 1)));
    // Start with *f = 1
    // set_fixed(0,true);

    valarray<double> f = pi;
    f /= f.sum();
    for(int i=0;i<n_letters();i++) {
      string pname = string("pi") + Alphabet().letter(i);
      add_parameter(Parameter(pname, f[i], between(0, 1)));
    }

    // initialize everything
    recalc_all();
  }


  //------------------- Triplet Frequency Model -----------------//
  TripletFrequencyModel::TripletFrequencyModel(const Triplets& T)
    :ReversibleFrequencyModel(T),
     ModelWithAlphabet<Triplets>(T)
  { }
    
  valarray<double> triplet_from_singlet_frequencies(const Triplets& T,const SimpleFrequencyModel& N)
  {
    if (not dynamic_cast<const Nucleotides*>(&N.Alphabet()))
      throw myexception()<<"Singlet frequencies are not nucleotide frequencies.";

    valarray<double> sub_pi = N.frequencies();

    valarray<double> pi(1.0,T.size());

    for(int i=0;i<T.size();i++) 
      for(int j=0;j<3;j++)
	pi[i] *= sub_pi[T.sub_nuc(i,j)];

    pi /= pi.sum();
    
    return pi;
  }

  void IndependentNucleotideFrequencyModel::recalc(const vector<int>&)
  {
    //------------------ compute triplet frequencies ------------------//
    pi = triplet_from_singlet_frequencies(Alphabet(),SubModels(0));

    vector<double> sub_parameters = SubModels(0).get_parameter_values();

    vector<double> triplet_parameters(n_letters()+1);
    triplet_parameters[0] = sub_parameters[0];
    set_varray(triplet_parameters,1,pi);

    triplets->set_parameter_values(triplet_parameters);

    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = (*triplets)(i,j);
  }

  string IndependentNucleotideFrequencyModel::name() const
  {
    return "F1x4";
  }

  
  IndependentNucleotideFrequencyModel::IndependentNucleotideFrequencyModel(const Triplets& T) 
    : TripletFrequencyModel(T),
      triplets(SimpleFrequencyModel(T))
  {
    insert_submodel("1",SimpleFrequencyModel(T.getNucleotides()));
    recalc_all();
  }


  void TripletsFrequencyModel::recalc(const vector<int>&)
  {
    valarray<double> nu = get_varray(get_parameter_values(), 1, n_letters());

    //------------- compute frequencies ------------------//
    pi = triplet_from_singlet_frequencies(Alphabet(),SubModels(0));

    pi *= nu;

    pi /= pi.sum();


    //------------ compute transition rates -------------//
    double g = get_parameter_value(0);

    valarray<double> nu_g(n_letters());
    for(int i=0;i<n_letters();i++)
      nu_g[i] = pow(nu[i],g);


    // FIXME - can we really handle two mutations?
    // Should the restriction on 1 mutation be HERE?
    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++) {
	R(i,j) = nu_g[i]/nu[i]*nu_g[j];
	for(int k=0;k<3;k++) {
	  int n1 = Alphabet().sub_nuc(i,k);
	  int n2 = Alphabet().sub_nuc(j,k);
	  if (n1 != n2)
	    R(i,j) *= SubModels(0)(n1,n2);
	}
      }

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  efloat_t TripletsFrequencyModel::super_prior() const 
  {
    return dirichlet_pdf(get_parameter_values(),1,n_letters(),4.0);
  }

  string TripletsFrequencyModel::name() const 
  {
    return "FF=triplets";
  }

  TripletsFrequencyModel::TripletsFrequencyModel(const Triplets& T)
    : TripletFrequencyModel(T)
  {
    add_super_parameter(Parameter("g", 1, between(0, 1) ));

    for(int i=0;i<n_letters();i++) {
      string pname = string("v") + Alphabet().letter(i);
      add_super_parameter(Parameter(pname, 1.0/n_letters(), between(0, 1) ));
    }

    insert_submodel("1",SimpleFrequencyModel(T.getNucleotides()));

    read();
    recalc_all();
  }

  //------------------- Codon Frequency Model -----------------//
  CodonFrequencyModel::CodonFrequencyModel(const Codons& C)
    :ReversibleFrequencyModel(C),
     ModelWithAlphabet<Codons>(C)
  { }


  void AACodonFrequencyModel::recalc(const vector<int>&)
  {
    //----------- get amino acid frequencies and counts ------------//
    valarray<double> f_aa = SubModels(0).frequencies();
    vector<int> n_aa(aa_size(),0);
    for(int i=0;i<Alphabet().size();i++) {
      int aa = Alphabet().translate(i);
      n_aa[aa]++;
    }
      
    //------------------ compute triplet frequencies ------------------//
    for(int i=0;i<pi.size();i++) {
      int aa = Alphabet().translate(i);
      pi[i] = f_aa[aa]/n_aa[aa];
    }

    vector<double> codon_parameters(n_letters()+1);
    codon_parameters[0] = SubModels(0).get_parameter_value(0);
    set_varray(codon_parameters,1,pi);

    codons->set_parameter_values(codon_parameters);

    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = (*codons)(i,j);
  }

  string AACodonFrequencyModel::name() const
  {
    return "FF=amino-acids";
  }

  
  AACodonFrequencyModel::AACodonFrequencyModel(const Codons& C) 
    : CodonFrequencyModel(C),
      codons(SimpleFrequencyModel(C))
  {
    insert_submodel("1",SimpleFrequencyModel(C.getAminoAcids()));

    recalc_all();
  }




  //------------------- Codons Frequency Model -----------------//

  void CodonsFrequencyModel::recalc(const vector<int>&)
  {
    double c = get_parameter_value(0);

    //------------- compute frequencies ------------------//
    valarray<double> aa_pi = get_varray(get_parameter_values(), 2, aa_size());

    // get codon frequencies of sub-alphabet
    valarray<double> sub_pi = SubModels(0).frequencies();

    // get aa frequencies of sub-alphabet
    valarray<double> sub_aa_pi(0.0,aa_size());
    for(int i=0;i<n_letters();i++)
      sub_aa_pi[Alphabet().translate(i)] += sub_pi[i];

    // get factors by which to multiply sub-alphabet frequencies
    valarray<double> factor(n_letters());
    for(int i=0;i<n_letters();i++) 
    {
      int j = Alphabet().translate(i);
      factor[i] = pow(aa_pi[j]/sub_aa_pi[j],c);
    }

    // compute aa-aware codon frequencies
    for(int i=0;i<n_letters();i++) 
      pi[i] = sub_pi[i] * factor[i];

    // scale so as to sum to 1
    pi /= pi.sum();


    //------------ compute transition rates -------------//
    double h = get_parameter_value(1);

    valarray<double> factor_h(n_letters());
    for(int i=0;i<n_letters();i++)
      factor_h[i] = pow(factor[i],h);


    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = SubModels(0)(i,j) * factor_h[i]/factor[i]*factor_h[j];

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  efloat_t CodonsFrequencyModel::super_prior() const 
  {
    return dirichlet_pdf(get_parameter_values(), 2, aa_size(), 2.0);
  }

  string CodonsFrequencyModel::name() const 
  {
    return "FF=codons1";
  }

  CodonsFrequencyModel::CodonsFrequencyModel(const Codons& C)
    : CodonFrequencyModel(C)
  {
    add_super_parameter(Parameter("c", 0.5, between(0, 1)));
    add_super_parameter(Parameter("h", 0.5, between(0, 1)));

    for(int i=0;i<C.getAminoAcids().size();i++) {
      string pname = string("b_") + Alphabet().getAminoAcids().letter(i);
      add_super_parameter(Parameter(pname, 1.0/C.getAminoAcids().size(), between(0, 1)));
    }

    insert_submodel("1",TripletsFrequencyModel(C));

    read();
    recalc_all();
  }

  //------------------- Codons Frequency Model 2 -----------------//

  void CodonsFrequencyModel2::recalc(const vector<int>&)
  {
    //------------- compute frequencies ------------------//
    valarray<double> aa_pref_ = get_varray(get_parameter_values(), 1, aa_size());

    valarray<double> aa_pref(n_letters());
    for(int i=0;i<n_letters();i++)
      aa_pref[i] = aa_pref_[Alphabet().translate(i)];

    // get codon frequencies of sub-alphabet
    valarray<double> sub_pi = SubModels(0).frequencies();

    // scale triplet frequencies by aa prefs
    for(int i=0;i<n_letters();i++) 
      pi[i] = sub_pi[i] * aa_pref[i];

    // scale so as to sum to 1
    pi /= pi.sum();


    //------------ compute transition rates -------------//
    double h = get_parameter_value(0);

    valarray<double> aa_pref_h(n_letters());
    for(int i=0;i<n_letters();i++)
      aa_pref_h[i] = pow(aa_pref[i], h);


    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = SubModels(0)(i,j) * aa_pref_h[i]/aa_pref[i] * aa_pref_h[j];

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  efloat_t CodonsFrequencyModel2::super_prior() const 
  {
    return dirichlet_pdf(get_parameter_values(), 1, aa_size(), 2.0);
  }

  string CodonsFrequencyModel2::name() const 
  {
    return "FF=codons2";
  }

  CodonsFrequencyModel2::CodonsFrequencyModel2(const Codons& C)
    : CodonFrequencyModel(C)
  {
    add_super_parameter(Parameter("h", 0.5, between(0, 1)));

    for(int i=0;i<C.getAminoAcids().size();i++) {
      string pname = string("b_") + Alphabet().getAminoAcids().letter(i);
      add_super_parameter(Parameter(pname, 1.0/C.getAminoAcids().size(), between(0, 1)));
    }

    insert_submodel("1",TripletsFrequencyModel(C));

    read();
    recalc_all();
  }


  /// Construct a Makov model on alphabet 'a'
  MarkovModel::MarkovModel(const alphabet& a)
    :Q(a.size(),a.size()),state_letters_(a.size())
  {
    for(int i=0;i<a.size();i++)
      state_letters_[i] = i;
  }
  
  //----------------------- ReversibleMarkovModel --------------------------//
  // Q(i,j) = S(i,j)*pi[j]   for i!=j
  // Q(i,i) = -sum_{i!=j} S(i,j)*pi[j]

  // We want to set S(i,i) so that Q(i,j) = S(i,j)*pi[j] for all i,j
  // Then Q = S*D, and we can easily compute the exponential
  // So, S(i,j) = Q(i,i)/pi[i]

  double ReversibleMarkovModel::rate() const 
  {
    const unsigned N = n_states();
    
    double scale=0;

    if (N == Alphabet().size()) 
    {
      for(int i=0;i<Q.size1();i++) 
	scale -= frequencies()[i]*Q(i,i);
    }
    else 
    {
      const vector<unsigned>& smap = state_letters();

      for(int s1=0;s1<N;s1++)
      {
	double temp = 0;
	for(int s2=0;s2<N;s2++)
	  if (smap[s1] != smap[s2])
	    temp += Q(s1,s2);

	scale += temp*frequencies()[s1];
      }
    }

    return scale/Alphabet().width();
  }

  void ReversibleMarkovModel::set_rate(double r)  
  {
    if (r == rate()) return;

    if (rate() == 0 and r != 0)
      throw myexception()<<"Model rate is 0, can't set it to "<<r<<".";

    double scale = r/rate();
    Q *= scale;
    for(int i=0;i<eigensystem.Diagonal().size();i++)
      eigensystem.Diagonal()[i] *= scale ;
  }

  /*
   * 1. pi[i]*Q(i,j) = pi[j]*Q(j,i)         - Because Q is reversible
   * 2. Q(i,j)/pi[j] = Q(j,i)/pi[i] = S1(i,j)
   * 3. pi[i]^1/2 * Q(j,i) / pi[j]^1/2 = S2(i,j)
   * 4. exp(Q) = pi^-1.2 * exp(pi^1/2 * Q * pi^-1/2) * pi^1/2
   *           = pi^-1.2 * exp(S2) * pi^1/2
   */

  void ReversibleMarkovModel::recalc_eigensystem()
  {
    const unsigned n = n_states();

#ifdef DEBUG_RATE_MATRIX
    cerr<<"scale = "<<rate()<<endl;

    assert(std::abs(frequencies().sum()-1.0) < 1.0e-6);
    for(int i=0;i<n;i++) {
      double sum = 0;
      for(int j=0;j<n;j++)
	sum += Q(i,j);
      assert(abs(sum) < 1.0e-6);
    }
#endif

    //--------- Compute pi[i]**0.5 and pi[i]**-0.5 ----------//
    vector<double> sqrt_pi(n);
    vector<double> inverse_sqrt_pi(n);
    for(int i=0;i<n;i++) {
      sqrt_pi[i] = sqrt(frequencies()[i]);
      inverse_sqrt_pi[i] = 1.0/sqrt_pi[i];
    }

    //--------------- Calculate eigensystem -----------------//
    ublas::symmetric_matrix<double> S(n,n);
    for(int i=0;i<n;i++)
      for(int j=0;j<=i;j++) {
	S(i,j) = Q(i,j) * sqrt_pi[i] * inverse_sqrt_pi[j];

#ifdef DEBUG_RATE_MATRIX
	// check reversibility of rate matrix
	if (i != j) {
	  assert (S(i,j) >= 0);
	  double p12 = Q(i,j)*frequencies()[i];
	  double p21 = Q(j,i)*frequencies()[j];
	  assert (abs(p12-p21) < 1.0e-12*(1.0+abs(p12)));
	}
	else
	  assert (Q(i,j) <= 0);
#endif
      }

    //---------------- Compute eigensystem ------------------//
    eigensystem = EigenValues(S);
  }

  Matrix ReversibleMarkovModel::transition_p(double t) const 
  {
    vector<double> pi(n_states());
    const valarray<double> f = frequencies();
    assert(pi.size() == f.size());
    for(int i=0;i<pi.size();i++)
      pi[i] = f[i];
    return exp(eigensystem,pi,t);
  }

  ReversibleMarkovModel::ReversibleMarkovModel(const alphabet& a)
    :MarkovModel(a), 
     eigensystem(a.size())
  { }

  //------------------------ F81 Model -------------------------//

  void F81_Model::recalc(const vector<int>&)
  {
    const int N = n_states();
    assert(N == n_letters());

    pi = get_varray(get_parameter_values(),0,N);
    pi /= pi.sum();

    for(int i=0;i<N;i++)
      for(int j=0;j<N;j++)
	Q(i,j) = (pi[j] - ((i==j)?1:0))*alpha_;
  }

  double  F81_Model::rate() const
  {
    const unsigned N = n_states();

    double sum=0;
    for(int i=0;i<N;i++)
      sum += pi[i]*(1.0-pi[i]);

    return sum*alpha_;
  }

  void F81_Model::set_rate(double r)
  {
    if (r == rate()) return;

    if (rate() == 0 and r != 0)
      throw myexception()<<"Model rate is 0, can't set it to "<<r<<".";

    double scale = r/rate();

    Q *= scale;

    alpha_ *= scale;
  }
  
  Matrix F81_Model::transition_p(double t) const
  {
    const unsigned N = n_states();

    Matrix E(N,N);

    const double exp_a_t = exp(-alpha_ * t);

    for(int i=0;i<N;i++)
      for(int j=0;j<N;j++)
	E(i,j) = pi[j] + (((i==j)?1.0:0.0) - pi[j])*exp_a_t;

    return E;
  }

  efloat_t F81_Model::prior() const
  {
    // uniform prior on f
    efloat_t Pr = 1;

    // uniform - 1 observeration per letter
    return dirichlet_pdf(get_parameter_values(), 0, n_letters(), 1.0);
  }

  string F81_Model::name() const
  {
    return string("F81[")+Alphabet().name+"]";
  }

  F81_Model::F81_Model(const alphabet& a)
    :ReversibleMarkovModel(a),ModelWithAlphabet<alphabet>(a),alpha_(1),pi(a.size())
  {
    for(int i=0;i<n_letters();i++) {
      string pname = string("pi") + Alphabet().letter(i);
      add_parameter(Parameter(pname, 1.0/n_letters(), between(0, 1)));
    }

    recalc_all();
  }

  F81_Model::F81_Model(const alphabet& a,const valarray<double>& f)
    :ReversibleMarkovModel(a),ModelWithAlphabet<alphabet>(a),alpha_(1),pi(a.size())
  {
    assert(f.size() == n_letters());

    for(int i=0;i<n_letters();i++) {
      string pname = string("pi") + Alphabet().letter(i);
      add_parameter(Parameter(pname, f[i], between(0, 1)));
    }

    recalc_all();
  }

  void ReversibleMarkovSuperModel::recalc(const vector<int>&)
  {
    const unsigned N = n_states();

    // recompute rate matrix
    for(int i=0;i<N;i++) {
      double sum=0;
      for(int j=0;j<N;j++) {
	if (i==j) continue;
	Q(i,j) = (*S)(i,j) * (*R)(i,j);
	sum += Q(i,j);
      }
      Q(i,i) = -sum;
    }

    recalc_eigensystem();
  }

  string ReversibleMarkovSuperModel::name() const {
    return S->name() + "+" + R->name();
  }

  /// Construct a reversible Markov model on alphabet 'a'
  ReversibleMarkovSuperModel::ReversibleMarkovSuperModel(const AlphabetExchangeModel& S1,const ReversibleFrequencyModel& R1)
    :ReversibleMarkovModel(S1.Alphabet()),
     ModelWithAlphabet<alphabet>(S1.Alphabet()),
     S(S1),
     R(R1)
  {
    add_submodel("S",*S);
    add_submodel("R",*R);

    read();
    recalc_all();
  }
    


  void SimpleReversibleMarkovModel::frequencies(const valarray<double>& pi) 
  {
    SimpleFrequencyModel* R2 = dynamic_cast<SimpleFrequencyModel*>(R.get());
    R2->frequencies(pi);
    read();
    recalc_all();
  }

  SimpleReversibleMarkovModel::SimpleReversibleMarkovModel(const AlphabetExchangeModel& E)
      :ReversibleMarkovSuperModel(E,SimpleFrequencyModel(E.Alphabet()))
  { }

  SimpleReversibleMarkovModel::
  SimpleReversibleMarkovModel(const AlphabetExchangeModel& E,const valarray<double>& pi)
      :ReversibleMarkovSuperModel(E,SimpleFrequencyModel(E.Alphabet(),pi))
  { }

  //---------------------- INV_Model --------------------------//

  string INV_Model::name() const 
  {
    return "INV";
  }

  INV_Model::INV_Model(const alphabet& a)
    :AlphabetExchangeModel(a),ModelWithAlphabet<alphabet>(a)
  {
    add_parameter(Parameter("INV::f", 1, between(0, 1)));

    // Calculate S matrix
    for(int i=0;i<S.size1();i++)
      for(int j=0;j<S.size2();j++)
	S(i,j) = 0;
  }
      
  //----------------------- EQU -------------------------//

  string EQU::name() const {
    return "EQU";
  }

  EQU::EQU(const alphabet& a) 
    :AlphabetExchangeModel(a),ModelWithAlphabet<alphabet>(a)
  {
    for(int i=0;i<n_states();i++)
      for(int j=0;j<n_states();j++)
	S(i,j) = 1;
  }

  //----------------------- Empirical -------------------------//

  void Empirical::load_file(const string& filename) 
  {
    name_ = string("Empirical[") + get_basename(filename) + "]";

    std::ifstream ifile(filename.c_str());

    if (not ifile)
      throw myexception(string("Couldn't open file '")+filename+"'");

    load_file(ifile);

    ifile.close();
    // the file has frequencies as well... where would we put them?
  }

  void Empirical::load_file(std::istream& file)
  {
    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<i;j++) {
	file>>S(i,j);
	S(j,i) = S(i,j);
      }

    // the file has frequencies as well... where would we put them?
  }

  /// Construct an Empirical model on alphabet \a a
  Empirical::Empirical(const alphabet& a) 
    :AlphabetExchangeModel(a),ModelWithAlphabet<alphabet>(a)
  { }

  /// Construct an Empirical model on alphabet \a a with name \n
  Empirical::Empirical(const alphabet& a,const string& n) 
    :AlphabetExchangeModel(a),ModelWithAlphabet<alphabet>(a),name_(n)
  { }

  PAM::PAM()
    :Empirical(AminoAcids(),"PAM")
  {
    istringstream file(
"27                                                                         \
 98  32                                                                     \
120   0 905                                                                 \
 36  23   0   0                                                             \
 89 246 103 134   0                                                         \
198   1 148 1153  0 716                                                     \
240   9 139 125  11  28  81                                                 \
 23 240 535  86  28 606  43  10                                             \
 65  64  77  24  44  18  61   0   7                                         \
 41  15  34   0   0  73  11   7  44 257                                     \
 26 464 318  71   0 153  83  27  26  46  18                                 \
 72  90   1   0   0 114  30  17   0 336 527 243                             \
 18  14  14   0   0   0   0  15  48 196 157   0  92                         \
250 103  42  13  19 153  51  34  94  12  32  33  17  11                     \
409 154 495  95 161  56  79 234  35  24  17  96  62  46 245                 \
371  26 229  66  16  53  34  30  22 192  33 136 104  13  78 550             \
  0 201  23   0   0   0   0   0  27   0  46   0   0  76   0  75   0         \
 24   8  95   0  96   0  22   0 127  37  28  13   0 698   0  34  42  61     \
208  24  15  18  49  35  37  54  44 889 175  10 258  12  48  30 157   0  28 \
");
    load_file(file);
  }

  JTT::JTT()
    :Empirical(AminoAcids(),"JTT1992")
  {
    istringstream file(
" 58                                                                        \
 54  45                                                                    \
 81  16 528                                                                \
 56 113  34  10                                                            \
 57 310  86  49   9                                                        \
105  29  58 767   5 323                                                    \
179 137  81 130  59  26 119                                                \
 27 328 391 112  69 597  26  23                                            \
 36  22  47  11  17   9  12   6  16                                        \
 30  38  12   7  23  72   9   6  56 229                                    \
 35 646 263  26   7 292 181  27  45  21  14                                \
 54  44  30  15  31  43  18  14  33 479 388  65                            \
 15   5  10   4  78   4   5   5  40  89 248   4  43                        \
194  74  15  15  14 164  18  24 115  10 102  21  16  17                    \
378 101 503  59 223  53  30 201  73  40  59  47  29  92 285                \
475  64 232  38  42  51  32  33  46 245  25 103 226  12 118 477            \
  9 126   8   4 115  18  10  55   8   9  52  10  24  53   6  35  12        \
 11  20  70  46 209  24   7   8 573  32  24   8  18 536  10  63  21  71    \
298  17  16  31  62  20  45  47  11 961 180  14 323  62  23  38 112  25  16 \
");

    load_file(file);
  }

  WAG::WAG()
    :Empirical(AminoAcids(),"WAG2001")
  {
    istringstream file(
"0.551571 \
0.509848  0.635346 \
0.738998  0.147304  5.429420 \
1.027040  0.528191  0.265256  0.0302949 \
0.908598  3.035500  1.543640  0.616783  0.0988179 \
1.582850  0.439157  0.947198  6.174160  0.021352  5.469470 \
1.416720  0.584665  1.125560  0.865584  0.306674  0.330052  0.567717 \
0.316954  2.137150  3.956290  0.930676  0.248972  4.294110  0.570025  0.249410 \
0.193335  0.186979  0.554236  0.039437  0.170135  0.113917  0.127395  0.0304501 0.138190 \
0.397915  0.497671  0.131528  0.0848047 0.384287  0.869489  0.154263  0.0613037 0.499462  3.170970 \
0.906265  5.351420  3.012010  0.479855  0.0740339 3.894900  2.584430  0.373558  0.890432  0.323832  0.257555 \
0.893496  0.683162  0.198221  0.103754  0.390482  1.545260  0.315124  0.174100  0.404141  4.257460  4.854020  0.934276 \
0.210494  0.102711  0.0961621 0.0467304 0.398020  0.0999208 0.0811339 0.049931  0.679371  1.059470  2.115170  0.088836  1.190630 \
1.438550  0.679489  0.195081  0.423984  0.109404  0.933372  0.682355  0.243570  0.696198  0.0999288 0.415844  0.556896  0.171329  0.161444 \
3.370790  1.224190  3.974230  1.071760  1.407660  1.028870  0.704939  1.341820  0.740169  0.319440  0.344739  0.967130  0.493905  0.545931  1.613280 \
2.121110  0.554413  2.030060  0.374866  0.512984  0.857928  0.822765  0.225833  0.473307  1.458160  0.326622  1.386980  1.516120  0.171903  0.795384  4.378020 \
0.113133  1.163920  0.0719167 0.129767  0.717070  0.215737  0.156557  0.336983  0.262569  0.212483  0.665309  0.137505  0.515706  1.529640  0.139405  0.523742  0.110864 \
0.240735  0.381533  1.086000  0.325711  0.543833  0.227710  0.196303  0.103604  3.873440  0.420170  0.398618  0.133264  0.428437  6.454280  0.216046  0.786993  0.291148  2.485390 \
2.006010  0.251849  0.196246  0.152335  1.002140  0.301281  0.588731  0.187247  0.118358  7.821300  1.800340  0.305434  2.058450  0.649892  0.314887  0.232739  1.388230  0.365369  0.314730 \
");
    
    load_file(file);
  }

  LG::LG()
    :Empirical(AminoAcids(),"LG2008")
  {
    istringstream file(
"0.425093 \
0.276818 0.751878 \
0.395144 0.123954 5.076149 \
2.489084 0.534551 0.528768 0.062556 \
0.969894 2.807908 1.695752 0.523386 0.084808 \
1.038545 0.363970 0.541712 5.243870 0.003499 4.128591 \
2.066040 0.390192 1.437645 0.844926 0.569265 0.267959 0.348847 \
0.358858 2.426601 4.509238 0.927114 0.640543 4.813505 0.423881 0.311484 \
0.149830 0.126991 0.191503 0.010690 0.320627 0.072854 0.044265 0.008705 0.108882 \
0.395337 0.301848 0.068427 0.015076 0.594007 0.582457 0.069673 0.044261 0.366317 4.145067 \
0.536518 6.326067 2.145078 0.282959 0.013266 3.234294 1.807177 0.296636 0.697264 0.159069 0.137500 \
1.124035 0.484133 0.371004 0.025548 0.893680 1.672569 0.173735 0.139538 0.442472 4.273607 6.312358 0.656604 \
0.253701 0.052722 0.089525 0.017416 1.105251 0.035855 0.018811 0.089586 0.682139 1.112727 2.592692 0.023918 1.798853 \
1.177651 0.332533 0.161787 0.394456 0.075382 0.624294 0.419409 0.196961 0.508851 0.078281 0.249060 0.390322 0.099849 0.094464 \
4.727182 0.858151 4.008358 1.240275 2.784478 1.223828 0.611973 1.739990 0.990012 0.064105 0.182287 0.748683 0.346960 0.361819 1.338132 \
2.139501 0.578987 2.000679 0.425860 1.143480 1.080136 0.604545 0.129836 0.584262 1.033739 0.302936 1.136863 2.020366 0.165001 0.571468 6.472279 \
0.180717 0.593607 0.045376 0.029890 0.670128 0.236199 0.077852 0.268491 0.597054 0.111660 0.619632 0.049906 0.696175 2.457121 0.095131 0.248862 0.140825 \
0.218959 0.314440 0.612025 0.135107 1.165532 0.257336 0.120037 0.054679 5.306834 0.232523 0.299648 0.131932 0.481306 7.803902 0.089613 0.400547 0.245841 3.151815 \
2.547870 0.170887 0.083688 0.037967 1.959291 0.210332 0.245034 0.076701 0.119013 10.649107 1.702745 0.185202 1.898718 0.654683 0.296501 0.098369 2.188158 0.189510 0.249313 \
");
    
    load_file(file);
  }

  // mtzoa Rota Stabelli 2009
  // mtart Rota Stabelli 2009

  //------------------------- HKY -----------------------------//
  string HKY::name() const {
    return "HKY";
  }

  efloat_t HKY::prior() const 
  {
    return laplace_pdf(log(kappa()), log(2), 0.25)/kappa();
  }

  void HKY::recalc(const vector<int>&) {
    assert(Alphabet().size()==4);

    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<Alphabet().size();j++) {
	if (i==j) continue;
	if (Alphabet().transversion(i,j))
	  S(i,j) = 1;
	else
	  S(i,j) = kappa();
      }
  }

  /// Construct an HKY model on alphabet 'a'
  HKY::HKY(const Nucleotides& N)
    : NucleotideExchangeModel(N)
  { 
    add_parameter(Parameter("HKY::kappa", 2, lower_bound(0)));
    recalc_all();
  }

  //------------------------- TN -----------------------------//
  string TN::name() const {
    return "TN";
  }


  efloat_t TN::prior() const 
  {
    efloat_t P = 1;
    P *= laplace_pdf(log(kappa1()), log(2), 0.25)/kappa1();
    P *= laplace_pdf(log(kappa2()), log(2), 0.25)/kappa2();
    return P;
  }

  void TN::recalc(const vector<int>&) { 
    assert(Alphabet().size()==4);

    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<Alphabet().size();j++) {
	if (i==j) continue;
	if (Alphabet().transversion(i,j))
	  S(i,j) = 1;
	else if (Alphabet().purine(i))
	  S(i,j) = kappa1();
	else
	  S(i,j) = kappa2();
      }
  }

  /// Construct a TN model on alphabet 'a'
  TN::TN(const Nucleotides& N)
    : NucleotideExchangeModel(N)
  { 
    add_parameter(Parameter("TN::kappa(pur)",2, lower_bound(0)));
    add_parameter(Parameter("TN::kappa(pyr)",2, lower_bound(0)));
    recalc_all();
  }

  string GTR::name() const {
    return "GTR";
  }

  // This should be OK - the increments are linear combinations of gaussians...

  /// return the LOG of the prior
  efloat_t GTR::prior() const 
  {
    valarray<double> n(6);

    n[0] = 2; // AG - transition

    n[1] = 1; // AT - transversion

    n[2] = 1; // AC - transversion

    n[3] = 1; // GT - transversion

    n[4] = 1; // GC - transversion

    n[5] = 2; // TC - transition

    n *= 4;

    return dirichlet_pdf(get_parameter_values(), n);
  }

  void GTR::recalc(const vector<int>&) 
  {
    assert(Alphabet().size()==4);

    double total = 0;
    for(int i=0;i<6;i++)
      total += get_parameter_value(i);

    for(int i=0;i<6;i++)
      parameters_[i].value /= total;

    S(0,1) = get_parameter_value(0); // AG
    S(0,2) = get_parameter_value(1); // AT
    S(0,3) = get_parameter_value(2); // AC

    S(1,2) = get_parameter_value(3); // GT
    S(1,3) = get_parameter_value(4); // GC

    S(2,3) = get_parameter_value(5); // TC
  }

  GTR::GTR(const Nucleotides& N)
      : NucleotideExchangeModel(N)
    { 
      add_parameter(Parameter("GTR::AG", 2.0/8, between(0, 1)));
      add_parameter(Parameter("GTR::AT", 1.0/8, between(0, 1)));
      add_parameter(Parameter("GTR::AC", 1.0/8, between(0, 1)));
      add_parameter(Parameter("GTR::GT", 1.0/8, between(0, 1)));
      add_parameter(Parameter("GTR::GC", 1.0/8, between(0, 1)));
      add_parameter(Parameter("GTR::TC", 2.0/8, between(0, 1)));

      recalc_all();
    }

  //------------------------ Triplet Models -------------------//

  TripletExchangeModel::TripletExchangeModel(const Triplets& T)
    :AlphabetExchangeModel(T),ModelWithAlphabet<Triplets>(T)
  { }

  void SingletToTripletExchangeModel::recalc(const vector<int>&)
  {
    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<i;j++) 
      {
	int nmuts=0;
	int pos=-1;
	for(int p=0;p<3;p++)
	  if (Alphabet().sub_nuc(i,p) != Alphabet().sub_nuc(j,p)) {
	    nmuts++;
	    pos=p;
	  }
	assert(nmuts>0);
	assert(pos >= 0 and pos < 3);
	
	S(i,j) = 0;

	if (nmuts == 1) {

	  int l1 = Alphabet().sub_nuc(i,pos);
	  int l2 = Alphabet().sub_nuc(j,pos);
	  assert(l1 != l2);

	  S(i,j) = SubModels(0)(l1,l2);
	}
      }
  }

  string SingletToTripletExchangeModel::name() const {
    string n = SubModels(0).name();
    n += "x3";
    return n;
  }
  
  SingletToTripletExchangeModel::SingletToTripletExchangeModel(const Triplets& T,const NucleotideExchangeModel& N)
    :TripletExchangeModel(T)
  { 
    insert_submodel("1",N);

    read();
    recalc_all();
  }

  //------------------------ Codon Models -------------------//

  CodonAlphabetExchangeModel::CodonAlphabetExchangeModel(const Codons& C)
    :AlphabetExchangeModel(C),ModelWithAlphabet<Codons>(C)
  { }

  /// Get the parameter 'omega' (non-synonymous/synonymous rate ratio)
  double M0::omega() const {
    return get_parameter_value(0);
  }

  /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
  void M0::omega(double w) {
    set_parameter_value(0,w);
  }

  void M0::recalc(const vector<int>&)
  {
    for(int i=0;i<Alphabet().size();i++) 
    {
      for(int j=0;j<i;j++) {
	int nmuts=0;
	int pos=-1;
	for(int p=0;p<3;p++)
	  if (Alphabet().sub_nuc(i,p) != Alphabet().sub_nuc(j,p)) {
	    nmuts++;
	    pos=p;
	  }
	assert(nmuts>0);
	assert(pos >= 0 and pos < 3);

	double rate=0.0;

	if (nmuts == 1) {

	  int l1 = Alphabet().sub_nuc(i,pos);
	  int l2 = Alphabet().sub_nuc(j,pos);
	  assert(l1 != l2);

	  rate = SubModels(0)(l1,l2);

	  if (AminoAcid(i) != AminoAcid(j))
	    rate *= omega();	
	}

	S(i,j) = S(j,i) = rate;
      }
    }
  }

  efloat_t M0::super_prior() const {
    return laplace_pdf(log(omega()), 0, 0.1)/omega();
  }

  efloat_t M0::prior() const {
    return SuperModelOver<NucleotideExchangeModel>::prior();
  }

  string M0::name() const {
    return string("M0[") + SubModels(0).name() + "]";
  }

  M0::M0(const Codons& C,const NucleotideExchangeModel& N)
    :CodonAlphabetExchangeModel(C)
  { 
    add_super_parameter(Parameter("M0::omega", 1.0, lower_bound(0)));
    insert_submodel("1",N);
    recalc_all();
  }

  M0::~M0() {}

  //--------------- MultiRate Models ----------------//

  double MultiModel::rate() const {
    double r=0;
    for(int m=0;m<n_base_models();m++)
      r += distribution()[m]*base_model(m).rate();
    return r;
  }

  void MultiModel::set_rate(double r)  {
    double scale = r/rate();
    for(int m=0;m<n_base_models();m++) {
      base_model(m).set_rate(base_model(m).rate()*scale);
    }
  }

  // This is per-branch, per-column - doesn't pool info about each branches across columns
  Matrix MultiModel::transition_p(double t) const {
    Matrix P = distribution()[0] * transition_p(t,0);
    for(int m=1;m<n_base_models();m++)
      P += distribution()[m] * transition_p(t,m);
    return P;
  }

  Matrix frequency_matrix(const MultiModel& M) {
    Matrix f(M.n_base_models(),M.n_states());
    for(int m=0;m<f.size1();m++)
      for(int l=0;l<f.size2();l++)
	f(m,l) = M.base_model(m).frequencies()[l];
    return f;
  }

  string UnitModel::name() const {
    return string("[") + SubModels(0).name() + "]";
  }

  UnitModel::UnitModel(const Base_Model_t& M)
    :ReversibleWrapperOver<Base_Model_t>(M)
  { }

  //---------------------- MultiFrequencyModel -----------------------//
  efloat_t MultiFrequencyModel::super_prior() const 
  {
    const int n = fraction.size();

    efloat_t Pr = 1;

    for(int l=0;l<Alphabet().size();l++) 
    {
      valarray<double> a_l(n);
      for(int m=0;m<a_l.size();m++)
	a_l[m] = a(m,l);

      Pr *= ::dirichlet_pdf(a_l, n/2.0);
    }

    return Pr;
  }

  const MultiModel::Base_Model_t& MultiFrequencyModel::base_model(int i) const {
    return *sub_parameter_models[i];
  }

  MultiModel::Base_Model_t& MultiFrequencyModel::base_model(int i) {
    return *sub_parameter_models[i];
  }
  
  vector<double> MultiFrequencyModel::distribution() const {
    return fraction;
  }

  /// Get the equilibrium frequencies
  const std::valarray<double>& MultiFrequencyModel::frequencies() const {
    return SubModel().frequencies();
  }

  void MultiFrequencyModel::recalc(const vector<int>&) 
  {
    // get underlying frequencies from our submodel
    valarray<double> f = frequencies();

    // calculate probability of each sub-model
    for(int m=0;m<fraction.size();m++) 
    {
      // Pr(m) = sum_l Pr(m|l)*Pr(l)
      fraction[m] = 0;
      for(int l=0;l<Alphabet().size();l++)
	fraction[m] += a(m,l)*f[l];
    }

    if (std::abs(sum(fraction) - 1.0) > 1.0e-5) cerr<<"ERROR: sum(fraction) = "<<sum(fraction)<<endl;

    // recalculate sub-models
    valarray<double> fm(Alphabet().size());
    for(int m=0;m<fraction.size();m++) 
    {
      // Pr(l|m) = Pr(m|l)*Pr(l)/Pr(m)
      for(int l=0;l<fm.size();l++)
	fm[l] = a(m,l)*f[l]/fraction[m];

      if (std::abs(fm.sum() - 1.0) > 1.0e-5) cerr<<"ERROR[m="<<m<<"]: fm.sum() = "<<fm.sum()<<endl;

      // get a new copy of the sub-model and set the frequencies
      sub_parameter_models[m] = SubModel();
      sub_parameter_models[m]->frequencies(fm);
    }
  }

  string MultiFrequencyModel::name() const {
    return SubModels(0).name() + " + multi_freq[" + 
      convertToString(fraction.size()) + "]";
  }

  MultiFrequencyModel::MultiFrequencyModel(const AlphabetExchangeModel& E,int n)
    :ReversibleWrapperOver<SimpleReversibleMarkovModel>(SimpleReversibleMarkovModel(E)),
     fraction(n)
  { 
    sub_parameter_models.resize(n);
    for(int i=0;i<n;i++)
      sub_parameter_models[i] = SubModel();

    // Set up variable names
    //   - initial probability that a letter l is in a submodel of type m = 1/n
    for(int l=0;l<Alphabet().size();l++) 
    {
      string letter = Alphabet().lookup(l);

      for(int m=0;m<n;m++) 
      {
	string index = convertToString(m+1);
	string pname = string("a") + letter + index;
	add_super_parameter(Parameter(pname, 1.0/n, between(0, 1)));
      }
    }

    read();
    recalc_all();
  }

  //---------------------- CAT_FixedFrequencyModel -----------------------//

  efloat_t CAT_FixedFrequencyModel::prior() const
  {
    valarray<double> f(fraction.size());
    for(int i=0;i<f.size();i++)
      f[i] = prior_fraction[i];

    valarray<double> x(fraction.size());
    for(int i=0;i<x.size();i++)
      x[i] = fraction[i];

    return ::dirichlet_pdf(x,safe_count(f*10.0));
  }

  void CAT_FixedFrequencyModel::recalc(const std::vector<int>&)
  {
    for(int i=0;i<fraction.size();i++)
      fraction[i] = get_parameter_value(i);
  }

  const MultiModel::Base_Model_t& CAT_FixedFrequencyModel::base_model(int i) const {
    return *sub_parameter_models[i];
  }

  MultiModel::Base_Model_t& CAT_FixedFrequencyModel::base_model(int i) {
    return *sub_parameter_models[i];
  }
  
  vector<double> CAT_FixedFrequencyModel::distribution() const {
    return fraction;
  }

  /// Get the equilibrium frequencies
  const std::valarray<double>& CAT_FixedFrequencyModel::frequencies() const {
    cerr<<"CAT model with fixed frequences does not HAVE an 'overall' frequencies method."<<endl;
    std::abort();
  }

  void CAT_FixedFrequencyModel::load_file(std::istream& file)
  {
    const alphabet& a = Alphabet();
    string line;

    //------- 1: Model name --------------//
    portable_getline(file,name_);

    //------- 2: # of categories ---------//
    portable_getline(file,line);
    const int n_cat = convertTo<int>(line);

    //------- 3: category weights --------//
    portable_getline(file,line);
    fraction = prior_fraction = split<double>(line,' ');
    if (fraction.size() != n_cat) 
      throw myexception()<<"In reading CAT-Fixed model '"<<name_<<"' expected weights for "<<n_cat<<" categories, but got "<<fraction.size();

    //------- 4: alphabet order ---------//
    portable_getline(file,line);
    vector<int> letter(a.size(),-1);
    vector<string> unordered_letters = split(line,' ');
    if (unordered_letters.size() != a.size())
      throw myexception()<<"In reading CAT-Fixed model '"<<name_<<"' expected "<<a.size()<<" letters, but got "<<unordered_letters.size();

    // I could put a try,catch block here to re-throw with context information.
    for(int i=0;i<unordered_letters.size();i++)
      letter[i] = a[unordered_letters[i]];

    //------- 5-end: frequencies of the actual classes ----//
    for(int i=0;i<n_cat;i++)
    {
      portable_getline(file,line);

      if (file.bad())
	throw myexception()<<"Failed to read frequencies for category "<<i+1;

      vector<double> f = split<double>(line,' ');

      valarray<double> f_ordered(a.size());
      for(int j=0;j<f_ordered.size();j++)
	f_ordered[letter[j]] = f[j];

      sub_parameter_models.push_back(F81_Model(a,f_ordered));
      sub_parameter_models.back()->set_rate(1);
    }

    //------- 6: Create the parameters for fiddling --------//
    for(int i=0;i<n_cat;i++)
    {
      string name = "CAT::f"+convertToString(i+1);
      add_parameter(Parameter(name,prior_fraction[i],between(0,1)));
    }

    recalc_all();
  }

  void CAT_FixedFrequencyModel::load_file(const string& filename)
  {
    std::ifstream file(filename.c_str());

    if (not file)
      throw myexception(string("Couldn't open file '")+filename+"'");

    load_file(file);

    file.close();
  }

  CAT_FixedFrequencyModel::CAT_FixedFrequencyModel(const alphabet& a)
    :ModelWithAlphabet<alphabet>(a)
  { }

  CAT_FixedFrequencyModel::CAT_FixedFrequencyModel(const alphabet& a, const string& n)
    :ModelWithAlphabet<alphabet>(a),name_(n)
  { }

  C20_CAT_FixedFrequencyModel::C20_CAT_FixedFrequencyModel()
    :CAT_FixedFrequencyModel(AminoAcids())
  {
    istringstream file(
"C20\n\
20\n\
0.0559911 0.0514825 0.0812922 0.0721977 0.0556719 0.0331003 0.0589502 0.0263757 0.0307584 0.0376701 0.0303058 0.0808776 0.0263349 0.0579101 0.0371248 0.0586868 0.0561479 0.0349811 0.0544937 0.0596472\n\
A C D E F G H I K L M N P Q R S T V W Y\n\
0.0862413 0.0130505 0.0329909 0.0184527 0.00441553 0.0366905 0.0108013 0.00979071 0.0220195 0.0112826 0.00878215 0.0791293 0.0189273 0.0169047 0.0171944 0.317815 0.27117 0.0179753 0.00153173 0.00483429\n\
0.203558 0.0348667 0.00316561 0.00708594 0.0112429 0.0195236 0.0024392 0.115257 0.00423808 0.0789777 0.0309187 0.00770524 0.0164189 0.00640441 0.00509808 0.0496777 0.111895 0.284906 0.00177626 0.00484482\n\
0.0211547 0.00481886 0.000549287 0.00145396 0.0128252 0.00114309 0.00113464 0.392846 0.00135799 0.125064 0.0209789 0.0012755 0.00202472 0.00123288 0.00149462 0.00262407 0.0171914 0.386068 0.00115911 0.0036028\n\
0.0376904 0.00640738 0.0109469 0.0358365 0.00363498 0.0191107 0.0329514 0.0101712 0.289763 0.0237496 0.00965289 0.0365411 0.0105337 0.0893564 0.28852 0.0356314 0.0355927 0.0144622 0.00279252 0.00665572\n\
0.00845978 0.0084909 0.00244879 0.00250555 0.342046 0.00242771 0.0433214 0.0097713 0.0026741 0.0380507 0.00807248 0.00725259 0.00214187 0.00427815 0.00535899 0.00804189 0.00553221 0.012141 0.049484 0.4375\n\
0.17599 0.00175587 0.130126 0.218217 0.0025277 0.0409535 0.0130708 0.00856221 0.0542946 0.0159531 0.00540458 0.0332846 0.037102 0.0707184 0.0290429 0.0793481 0.0540083 0.0249553 0.00105921 0.00362591\n\
0.16344 0.00886599 0.0374273 0.0220612 0.00306413 0.529672 0.00900061 0.00175694 0.0167118 0.00611563 0.00293908 0.0438702 0.0126458 0.0137555 0.0195541 0.0829343 0.0142836 0.00579857 0.00286407 0.00323983\n\
0.0917469 0.0284015 0.0133819 0.0196876 0.0998479 0.0249899 0.0449766 0.0583556 0.0164916 0.115501 0.0395995 0.0290699 0.0209916 0.0255085 0.0265853 0.0736483 0.0661518 0.0831856 0.0246464 0.0972327\n\
0.0646701 0.00771176 0.0168734 0.0544978 0.0219148 0.0148894 0.0313852 0.0505983 0.0907931 0.184428 0.077484 0.0228907 0.0105004 0.0996415 0.0988016 0.0321196 0.0411766 0.0505824 0.0084303 0.0206106\n\
0.0135994 0.010009 0.00079517 0.00180118 0.264097 0.00267946 0.00724019 0.0814027 0.00251581 0.366142 0.0734965 0.00184694 0.00389941 0.00464208 0.00434084 0.00436688 0.00752485 0.0573473 0.0261565 0.0660971\n\
0.147804 0.00488258 0.0534743 0.0727246 0.00299039 0.0907726 0.0262289 0.00357811 0.105166 0.0126777 0.00596218 0.072663 0.0156558 0.0757166 0.0842845 0.14599 0.0634877 0.00927198 0.00159285 0.00507607\n\
0.0186377 0.00549689 0.00083297 0.00202485 0.0385383 0.00217135 0.0023666 0.202081 0.00291207 0.437038 0.124186 0.00198652 0.00406723 0.00658901 0.00420552 0.00461774 0.0149904 0.118938 0.00268717 0.00563241\n\
0.0477624 0.00757917 0.0141349 0.0462688 0.0130691 0.00523279 0.0165352 0.17415 0.0577575 0.112125 0.0330288 0.0209574 0.0124375 0.0429297 0.0505743 0.0264989 0.0951755 0.20937 0.00316605 0.0112466\n\
0.416419 0.0406938 0.00451317 0.00632298 0.00484384 0.0946185 0.00310574 0.00764432 0.00389418 0.00998854 0.00693232 0.00917014 0.0187841 0.00613205 0.00561008 0.236077 0.0746275 0.0459225 0.00121726 0.00348258\n\
0.0402296 0.0124783 0.0365524 0.0372197 0.0459095 0.0233618 0.210831 0.00934787 0.0482411 0.0360561 0.010029 0.103665 0.0098504 0.0826558 0.0735203 0.0533383 0.0310209 0.015248 0.0140077 0.106438\n\
0.0323453 0.00359763 0.24315 0.0710274 0.00244293 0.101607 0.0366225 0.00314108 0.0470129 0.00519805 0.00240287 0.252045 0.00948378 0.0330831 0.0236283 0.0848355 0.0359083 0.00487046 0.000873093 0.00672477\n\
0.147626 0.00323272 0.0403052 0.0576893 0.00471772 0.0330851 0.0146393 0.0108267 0.0451351 0.0256201 0.00586514 0.0211973 0.347371 0.0371554 0.0334507 0.0892065 0.0485899 0.0282336 0.00163587 0.00441772\n\
0.103145 0.00617625 0.0386402 0.0923369 0.00676664 0.0202338 0.0246762 0.0376904 0.0921699 0.0376284 0.0161883 0.0435172 0.0128302 0.0786603 0.0717748 0.095145 0.137857 0.0740454 0.00221447 0.00830416\n\
0.0837543 0.00207351 0.0804871 0.194776 0.00230634 0.022903 0.0268459 0.00740798 0.145929 0.019025 0.00673952 0.0518811 0.0085616 0.14565 0.0899383 0.045574 0.0451081 0.0150303 0.00107713 0.00493253\n\
0.0578736 0.00111308 0.294674 0.34021 0.00170349 0.0293911 0.0139817 0.00305257 0.0363365 0.00626119 0.0027296 0.0491422 0.0156106 0.059825 0.0138314 0.0358045 0.0249942 0.00876742 0.000866434 0.0038313\n\
");
    load_file(file);
  }

  C10_CAT_FixedFrequencyModel::C10_CAT_FixedFrequencyModel()
    :CAT_FixedFrequencyModel(AminoAcids())
  {
    istringstream file(
"C10\n\
10\n\
0.119134 0.0874372 0.103711 0.0922585 0.107049 0.132995 0.0538028 0.0691986 0.131994 0.10242\n\
A C D E F G H I K L M N P Q R S T V W Y\n\
0.408257 0.0349388 0.00698709 0.00978467 0.00616043 0.122161 0.00391518 0.0125784 0.00596702 0.0158339 0.00813132 0.00962854  0.0394156 0.00752797 0.0081783 0.168245 0.0658133 0.0604427 0.00187516 0.00415797\n\
0.102776 0.0149663 0.0155944 0.0419667 0.0180729 0.0138806 0.0158865 0.106608 0.0436344 0.113194 0.04378 0.0213272 0.0223251 0.0440685 0.0418664 0.0529608 0.108174 0.160665 0.00451472 0.0137374\n\
0.0351766 0.00787065 0.000676874 0.00196868 0.0126221 0.00224206 0.00128783 0.351582 0.00188565 0.127818 0.0242632 0.00165915 0.00297716 0.00165596 0.00196786 0.00499981 0.0255378 0.388864 0.00119078 0.00375393\n\
0.0408514 0.00376029 0.233381 0.0901239 0.00251082 0.115833 0.0373197 0.00255236 0.0485017 0.00521646 0.00225718 0.218565 0.0108334 0.0380451 0.0269887 0.0804527 0.030288 0.00444811 0.00108153 0.00698909\n\
0.0185493 0.00704165 0.000977506 0.00248916 0.073333 0.00289529 0.0040104 0.163242 0.00435709 0.444308 0.120282 0.00248957 0.00488276 0.00835394 0.00623624 0.00516424 0.0131807 0.0968581 0.00687598 0.0144734\n\
0.110675 0.00148349 0.163644 0.263846 0.00232568 0.0325228 0.0163804 0.00683349 0.0677158 0.014068 0.00489881 0.0405186 0.0298982 0.0877962 0.035219 0.0562888 0.0426922 0.0181079 0.0010339 0.00405223\n\
0.0522658 0.0143325 0.0297745 0.0388387 0.0624033 0.0228101 0.155164 0.0187406 0.0439469 0.065378 0.0207189 0.0714837 0.0145475 0.073654 0.0668295 0.0549018 0.037014 0.0267512 0.0193757 0.111069\n\
0.0116587 0.0105341 0.00217425 0.00242511 0.365099 0.00347091 0.0366787 0.0187185 0.00266947 0.067649 0.0143535 0.00640111 0.00311599 0.00402037 0.00509901 0.00948485 0.00737139 0.0206341 0.0509565 0.357486\n\
0.0627196 0.00526629 0.0236193 0.0686285 0.00391818 0.0256175 0.0332612 0.0128968 0.227084 0.0305628 0.0124037 0.0428629 0.0140441 0.109811 0.203878 0.0483152 0.0463378 0.0197063 0.00251435 0.00655211\n\
0.114552 0.00985495 0.0416192 0.0364908 0.0046606 0.0503818 0.0165233 0.00929495 0.0423027 0.0139154 0.00822408 0.0750615 0.0379222 0.0339625 0.0324009 0.261065 0.184583 0.0195769 0.0017549 0.00585383\n\
");
    load_file(file);
  }

  //---------------------------- class MultiModel --------------------------//
  const MultiModel::Base_Model_t& MultiParameterModel::base_model(int m) const {
    int i = m / SubModel().n_base_models();
    int j = m % SubModel().n_base_models();

    return sub_parameter_models[i]->base_model(j);
  }

  MultiModel::Base_Model_t& MultiParameterModel::base_model(int m) {
    int i = m / SubModel().n_base_models();
    int j = m % SubModel().n_base_models();

    return sub_parameter_models[i]->base_model(j);
  }
  
  vector<double> MultiParameterModel::distribution() const {
    vector<double> dist(n_base_models());

    for(int m=0;m<dist.size();m++) {
      int i = m / SubModel().n_base_models();
      int j = m % SubModel().n_base_models();

      dist[m] = fraction[i]*sub_parameter_models[i]->distribution()[j];
    }

    return dist;
  }

    /// Get the equilibrium frequencies
  const std::valarray<double>& MultiParameterModel::frequencies() const {
    return SubModel().frequencies();
  }

  // Um, summed-over parameter lives on as its MEAN

  void MultiParameterModel::recalc_submodel_instances()
  {
    // recalc sub-models
    vector<double> params = SubModel().get_parameter_values();
    for(int b=0;b<fraction.size();b++) {
      sub_parameter_models[b] = &SubModel();

      if (p_change == -1)
	sub_parameter_models[b]->set_rate(p_values[b]);
      else {
	params[p_change] = p_values[b];
	sub_parameter_models[b]->set_parameter_values(params);
      }
    }
  }

  MultiParameterModel::MultiParameterModel(const MultiModel& M,int p,int n) 
    :ReversibleWrapperOver<MultiModel>(M),
     sub_parameter_models(vector<owned_ptr<MultiModel> >(n,M)),
     fraction(n),
     p_change(p),
     p_values(n)
  { 
    // start with sane values for p_values
    for(int m=0;m<p_values.size();m++)
      if (p_change == -1)
	p_values[m] = M.rate();
      else
	p_values[m] = M.get_parameter_value(p_change);

    if (p_change != -1)
      SubModel().set_fixed(p_change,true);
  }

  //--------------- Dirichlet-based Model----------------//

  efloat_t DirichletParameterModel::super_prior() const
  {
    efloat_t Pr  = 1;

    // Prior on the fractions
    double n_f = 1.0 + p_values.size()/2.0;
    Pr *= dirichlet_pdf(get_parameter_values(),0              ,p_values.size(),n_f);

    // Prior on the rates
    double n_r = 2.0; // + p_values.size()/2.0;
    Pr *= dirichlet_pdf(get_parameter_values(),p_values.size(),p_values.size(),n_r);

    return Pr;
  }

  void DirichletParameterModel::recalc(const vector<int>&) 
  {
    /*
    // sort bins to enforce monotonically increasing order
    vector<double> values;
    for(int i=0;i<p_values.size();i++)
      values.push_back(parameters_[i+p_values.size()]);
    
    vector<int> order = iota<int>(p_values.size());

    sort(order.begin(), order.end(), sequence_order<double>(values));

    vector<double> p2 = parameters_;
    for(int i=0;i<p_values.size();i++) {
      p2[i] = parameters_[order[i]];
      p2[i+p_values.size()] = parameters_[order[i]+p_values.size()];
    }
    parameters_ = p2;
    */

    // write parameter values to fraction / p_values
    for(int i=0;i<p_values.size();i++)
    {
      fraction[i] = get_parameter_value(i);
      p_values[i] = get_parameter_value(i+p_values.size());
    }
    
    // We need to do this when either P_values changes, or the SUBMODEL changes
    recalc_submodel_instances();
  }

  string DirichletParameterModel::name() const {
    string p_name = "rate";
    if (p_change > -1)
      p_name = SubModels(0).parameter_name(p_change);

    string dist_name = p_name + "~Dirichlet[" + convertToString(p_values.size()) + "]";
    return SubModels(0).name() + " + " + dist_name;
  }

  DirichletParameterModel::DirichletParameterModel(const MultiModel& M, int p, int n)
    :MultiParameterModel(M,p,n)
  {
    // bin frequencies
    for(int i=0;i<n;i++) {
      string pname = "DP::f" + convertToString(i+1);
      add_super_parameter(Parameter(pname, 1.0/n, between(0, 1)));
    }

    // bin values
    string p_name = "DP::rate";
    if (p >= 0) p_name = string("DP::") + M.parameter_name(p);
    for(int i=0;i<n;i++)
      add_super_parameter(Parameter(p_name+convertToString(i+1), 1.0, between(0,n)));

    read();
    recalc_all();
  }

  //--------------- Distribution-based Model----------------//

  efloat_t DistributionParameterModel::super_prior() const
  {
    if (not good_enough)
      return 0.0;
    else
      return 1.0;
  }

  Distribution& DistributionParameterModel::D()
  {
    return SubModelAs<Distribution>(1);
  }

  const Distribution& DistributionParameterModel::D() const
  {
    return SubModelAs<Distribution>(1);
  }

  void DistributionParameterModel::recalc(const vector<int>&) 
  {
    // We only need to do this when the DISTRIBUTION changes (?)
    Discretization d(p_values.size(),D());
    double ratio = d.scale()/D().mean();

    good_enough = (ratio > 1.0/1.5 and ratio < 1.5);

    d.scale(1.0/ratio);

    fraction = d.f;
    p_values = d.r;
    
    // We need to do this when either P_values changes, or the SUBMODEL changes
    recalc_submodel_instances();
  }

  string DistributionParameterModel::name() const {
    string p_name = "rate";
    if (p_change > -1)
      p_name = SubModels(0).parameter_name(p_change);

    string dist_name = p_name + "~" + D().name() + "(" + convertToString(p_values.size()) + ")";
    return SubModels(0).name() + " + " + dist_name;
  }

  DistributionParameterModel::DistributionParameterModel(const MultiModel& M,const Distribution& RD, int p, int n)
    :MultiParameterModel(M,p,n),
     good_enough(false)
  {
    insert_submodel("DIST",RD);

    read();
    recalc_all();
  }

  /*--------------- Gamma Sites Model----------------*/

  GammaParameterModel::GammaParameterModel(const MultiModel& M,int n)
    :DistributionParameterModel(M,Gamma(),-1,n)
  {}


  /*--------------- LogNormal Sites Model----------------*/

  LogNormalParameterModel::LogNormalParameterModel(const MultiModel& M,int n)
    :DistributionParameterModel(M,LogNormal(),-1,n)
  {}


  //--------------- Invariant Sites Model----------------//

  const double WithINV::inv_frac_mean = 0.1;
  const double WithINV::max_inv_rate = 0.01;

  /// Get the equilibrium frequencies
  const std::valarray<double>& WithINV::frequencies() const {
    return SubModel().frequencies();
  }

  void WithINV::recalc(const vector<int>&) {
    INV->frequencies(SubModel().frequencies());
  }

  string WithINV::name() const {
    return SubModel().name() + " + INV";
  }

  efloat_t WithINV::super_prior() const {
    double p = get_parameter_value(0);

    return beta_pdf(p, 1, 2);
  }

    /// Access the base models
  const MultiModel::Base_Model_t& WithINV::base_model(int m) const {
    if (m<SubModel().n_base_models())
      return SubModel().base_model(m);
    else
      return *INV;
  }

  MultiModel::Base_Model_t& WithINV::base_model(int m) {
    if (m<SubModel().n_base_models())
      return SubModel().base_model(m);
    else
      return *INV;
  }

  vector<double> WithINV::distribution() const {
    double p = get_parameter_value(0);

    vector<double> dist = SubModel().distribution();
    for(int i=0;i<dist.size();i++)
      dist[i] *= (1-p);

    dist.push_back(p);
    return dist;
  }

  WithINV::WithINV(const MultiModel& M)
    :ReversibleWrapperOver<MultiModel>(M),
     INV(SimpleReversibleMarkovModel(INV_Model(M.Alphabet())))
  {
    add_super_parameter(Parameter("INV::p", 0.01, between(0, 1)));

    read();
    recalc_all();
  }


  //--------------- Invariant Sites Model 2 ----------------//

  const double WithINV2::inv_frac_mean = 0.1;
  const double WithINV2::max_inv_rate = 0.01;

  /// Get the equilibrium frequencies
  //  const std::valarray<double>& WithINV2::frequencies() const {
  //    return VAR->frequencies();
  //  }

  const MultiModel& WithINV2::VAR() const {
    return SubModelAs<MultiModel>(0);
  }

  MultiModel& WithINV2::VAR() {
    return SubModelAs<MultiModel>(0);
  }

  const SimpleReversibleMarkovModel& WithINV2::INV() const {
    return SubModelAs<SimpleReversibleMarkovModel>(1);
  }

  SimpleReversibleMarkovModel& WithINV2::INV() {
    return SubModelAs<SimpleReversibleMarkovModel>(1);
  }

  void WithINV2::recalc(const vector<int>&) 
  {
    double p = get_parameter_value(0);

    freq = (1-p)*VAR().frequencies() + p*INV().frequencies();
  }

  string WithINV2::name() const 
  {
    return VAR().name() + " + INV2";
  }

  efloat_t WithINV2::super_prior() const 
  {
    double p = get_parameter_value(0);

    return beta_pdf(p, 1, 2);
  }

    /// Access the base models
  const MultiModel::Base_Model_t& WithINV2::base_model(int m) const {
    if (m<VAR().n_base_models())
      return VAR().base_model(m);
    else
      return INV();
  }

  MultiModel::Base_Model_t& WithINV2::base_model(int m) {
    if (m<VAR().n_base_models())
      return VAR().base_model(m);
    else
      return INV();
  }

  vector<double> WithINV2::distribution() const {
    double p = get_parameter_value(0);

    vector<double> dist = VAR().distribution();
    for(int i=0;i<dist.size();i++)
      dist[i] *= (1-p);

    dist.push_back(p);
    return dist;
  }

  const valarray<double>& WithINV2::frequencies() const {
    return freq;

  }

//  NOTE: Shouldn't we have a generic 'frequencies' calculation if we need one?
//  ????: And do we need one?

  WithINV2::WithINV2(const MultiModel& M)
    :freq(M.frequencies())
  {
    add_super_parameter(Parameter("INV::p", 0.01, between(0, 1)));
    insert_submodel("VAR", M);
    insert_submodel("INV", SimpleReversibleMarkovModel(INV_Model(M.Alphabet())));

    read();
    recalc_all();
  }



  //-------------------- M2 --------------------//
  void M2::recalc(const vector<int>&) 
  {
    fraction[0] = get_parameter_value(0);
    fraction[1] = get_parameter_value(1);
    fraction[2] = get_parameter_value(2);

    p_values[0] = 0;
    p_values[1] = 1;
    p_values[2] = get_parameter_value(3);

    recalc_submodel_instances();
  }

  efloat_t M2::super_prior() const 
  {
    // prior on frequencies
    valarray<double> n(3);
    n[0] = 1;
    n[1] = 98;
    n[2] = 1;
    efloat_t P = dirichlet_pdf(get_parameter_values(), 0, 3, n);

    // prior on omega
    double omega = get_parameter_value(3);
    P *= exponential_pdf(log(omega),0.05)/omega;
    return P;
  }

  string M2::name() const {
    return SubModels(0).name() + " + M2";
  }

  M2::M2(const M0& M1,const ReversibleFrequencyModel& R) 
    :MultiParameterModel(UnitModel(ReversibleMarkovSuperModel(M1,R)),0,3)
  {
    add_super_parameter(Parameter("M2::f[AA INV]",   1.0/3, between(0, 1)));
    add_super_parameter(Parameter("M2::f[Neutral]",  1.0/3, between(0, 1)));
    add_super_parameter(Parameter("M2::f[Selected]", 1.0 - get_parameter_value(0) - get_parameter_value(1), between(0, 1)));
    add_super_parameter(Parameter("M2::omega", 1.0, lower_bound(0)));

    read();
    recalc_all();
  }

  int any_set(const vector<bool>& mask,int i1,int i2) 
  {
    int inc = (i2 > i1)?1:-1;
      
    for(int i=i1;i!=i2;i+=inc) {
      if (mask[i])
	return i;
    }
    return -1;
  }

  //M3

  double M3::omega(int i) const {
    return get_parameter_value(fraction.size() + i);
  }

  /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
  void M3::omega(int i,double w) {
    set_parameter_value(fraction.size()+i,w);
  }

  // NOTE: we only enforce order in the LOGGING of the omegas

  void M3::recalc(const vector<int>&) 
  {
    for(int i=0;i<fraction.size();i++)
      fraction[i] = get_parameter_value(i);

    for(int i=0;i<fraction.size();i++)
      p_values[i] = get_parameter_value(fraction.size()+i);

    recalc_submodel_instances();
  }

  // FIXME: Conditional on one omega being small, the probability of the other ones being
  //        small too should be higher.
  //
  //        We could require more evidence for conservation if we required it only once.
  //
  //        But how can we enforce the idea that at least one of the categories has an
  //        omega near 1?
  //
  //        Goal: Prior should place weak support on neutrality, and medium support on
  //        some fraction of sites being neutral.  Given sites conserved at level w
  //        we should place a decent level of support for other omegas being similar
  //        to w.
  //
  //        Perhaps M3 is not quite the model for this, in a Bayesian context.
  //        Instead: fix one omega to 1.0, and put a uniform prior on its frequency.
  //        Place a uniform distribution on the other omegas, conditional on not being 1.0.
  //        The user should decrease the number of omegas if they cannot be reliably estimated.
  //
  //        Make model that puts a uniform on (conserved,neutral) or (conserved,neutral,positive)
  //        and then has the possibility of [n] conserved rates with a UNIFORM prior.
  //        (The Uniform seems like a good analogue of the dirichlet.)

  efloat_t M3::super_prior() const 
  {
    efloat_t P = 1;
    int n = fraction.size();

    if (n <= 1) return P;

    // prior on frequencies
    P *= dirichlet_pdf(get_parameter_values(), 0, n, 4.0);

    // prior on omegas
    double f = 0.05/n;
    for(int i=0; i < n; i++) 
    {
      double w = omega(i);
      P *= ((1-f)*exponential_pdf(-log(w),0.05)/w + f*exponential_pdf(log(w),0.05)/w);
    }

    return P;
  }

  string M3::name() const {
    return SubModels(0).name() + " + M3[" + convertToString(fraction.size()) + "]";
  }

  M3::M3(const M0& M1,const ReversibleFrequencyModel& R, int n) 
    :MultiParameterModel(UnitModel(ReversibleMarkovSuperModel(M1,R)),0,n)
  {
    // p
    for(int i=0;i<n;i++) {
      string pname = "M3::f" + convertToString(i+1);
      add_super_parameter(Parameter(pname, 1.0/n, between(0, 1)));
    }

    // omega
    for(int i=0;i<n;i++) {
      string pname = "M3::omega" + convertToString(i+1);
      add_super_parameter(Parameter(pname, 1.0, lower_bound(0)));
    }

    read();
    recalc_all();
  }



  M7::M7(const M0& M1,const ReversibleFrequencyModel& R, int n) 
    :DistributionParameterModel(UnitModel(ReversibleMarkovSuperModel(M1,R)),
				Beta(),0,n)
  { 
  }

  int MixtureModel::n_base_models() const 
  {
    int total=0;
    for(int i=0; i<n_submodels(); i++)
      total += SubModels(i).n_base_models();
    return total;
  }


  void MixtureModel::recalc(const vector<int>&) 
  {
    //recalculate pi
    pi = 0;
    int sm_total = n_submodels();
    for(int sm=0;sm<sm_total;sm++)
      pi += get_parameter_value(sm)*SubModels(sm).frequencies();
  }

  efloat_t MixtureModel::super_prior() const 
  {
    valarray<double> p = get_varray(get_parameter_values(),0,n_submodels());
    valarray<double> q = get_varray(get_parameter_values(),n_submodels(),n_submodels());

    return dirichlet_pdf(get_parameter_values(), 0, n_submodels(), 10.0*q);
  }

  const MultiModel::Base_Model_t& MixtureModel::base_model(int m) const 
  {
    assert(m >= 0);

    int sm_total = n_submodels();
    for(int sm=0;sm<sm_total;sm++) {
      if (m < SubModels(sm).n_base_models())
	return SubModels(sm).base_model(m);
      else
	m -= SubModels(sm).n_base_models();
    }

    // we don't even have that many base models...
    std::abort();
  }

  MultiModel::Base_Model_t& MixtureModel::base_model(int m) 
  {
    assert(m >= 0);

    int sm_total = n_submodels();
    for(int sm=0; sm<sm_total; sm++) {
      if (m < SubModels(sm).n_base_models())
	return SubModels(sm).base_model(m);
      else
	m -= SubModels(sm).n_base_models();
    }

    // we don't even have that many base models...
    std::abort();
  }

  vector<double> MixtureModel::distribution() const 
  {
    int sm_total = n_submodels();

    vector<double> dist(n_base_models());

    for(int sm=0,m=0; sm<sm_total; sm++) {
      double f = get_parameter_value(sm);
      for(int i=0;i<SubModels(sm).n_base_models();i++)
	dist[m++] = f*SubModels(0).distribution()[i];
    }
    
    return dist;
  }

  string MixtureModel::name() const {
    string name = "MixtureModel(";
    int n = n_submodels();
    for(int i=0;i<n;i++) {
      name += SubModels(i).name();
      if (i != n-1)
	name += ", ";
    }
    name += ")";

    return name;
  }

  MixtureModel::MixtureModel(const std::vector<owned_ptr<MultiModel> >& models)
  {
    for(int i=0;i<models.size();i++) {
      string pname = string("Mixture::p") + convertToString(i+1);
      add_super_parameter(Parameter(pname, 1.0/models.size(), between(0, 1)));
      insert_submodel(string("M")+convertToString(i+1),*models[i]);
    }

    for(int i=0;i<models.size();i++) {
      string pname = string("Mixture::prior") + convertToString(i+1);
      add_super_parameter(Parameter(pname, 1.0/models.size()));
    }

    pi.resize(Alphabet().size());

    read();
    recalc_all();
  }


  string ModulatedMarkovModel::name() const 
  {
    return M->name() + " + " + S->name();
  }

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

  void ModulatedMarkovModel::recalc(const vector<int>&) 
  {
    const int n_models = M->n_base_models();

    M->set_rate(1);

    const valarray<double>& M_pi = M->frequencies();
    const vector<double>&   M_f  = M->distribution();

    // calculate pi[ ] for each state
    unsigned T = 0;
    for(int m=0; m < n_models; m++) {
      unsigned N = M->base_model(m).n_states();
      for(int s=0; s < N; s++) 
	pi[T+s] = M_pi[s] * M_f[m];
      T += N;
    }
    

    // initially zero out the matrix
    for(int i=0;i<Q.size1();i++)
      for(int j=0;j<Q.size2();j++)
	Q(i,j) = 0;

    // rates for within-model transitions
    T=0;
    for(int m=0; m < n_models; m++) 
    {
      const ReversibleMarkovModel* RM = dynamic_cast<const ReversibleMarkovModel*>(&M->base_model(m));
      if (not RM)
	throw myexception()<<"Can't construct a modulated Markov model from non-Markov model '"<<M->base_model(m).name()<<"'";

      unsigned N = RM->n_states();
      

      const Matrix& QM = RM->transition_rates();
      for(int s1=0; s1 < N; s1++) 
	for(int s2=0; s2 < N; s2++)
	  Q(T+s1,T+s2) = QM(s1,s2);

      T += N;
    }

    // rates for between-model transitions
    unsigned T1=0;
    for(int m1=0; m1 < n_models; m1++) 
    {
      const ReversibleMarkovModel* RM1 = dynamic_cast<const ReversibleMarkovModel*>(&M->base_model(m1));
      unsigned N1 = RM1->n_states();

      unsigned T2=0;
      for(int m2=0; m2 < n_models; m2++) 
      {
	const ReversibleMarkovModel* RM2 = dynamic_cast<const ReversibleMarkovModel*>(&M->base_model(m2));
	unsigned N2 = RM2->n_states();
	assert(N1 == N2);

	if (m1 != m2) {
	  double S12 = (*S)(m1,m2);
	  for(int s1=0;s1<N1;s1++)
	    Q(T1+s1,T2+s1) = S12*M_f[m2];
	}

	T2 += N2;
      }
      T1 += N1;
    }

    // recompute diagonals 
    for(int i=0;i<Q.size1();i++) 
    {
      double sum=0;
      for(int j=0;j<Q.size2();j++)
	if (i!=j)
	  sum += Q(i,j);
      Q(i,i) = -sum;
    }

    recalc_eigensystem();
  }

  ModulatedMarkovModel::ModulatedMarkovModel(const MultiModel& MM, const ExchangeModel& EM)
    :ReversibleMarkovModel(MM.Alphabet()),M(MM),S(EM)
  {
    unsigned T = 0;
    for(int m=0; m < M->n_base_models(); m++) 
    {
      const ReversibleMarkovModel* RM = dynamic_cast<const ReversibleMarkovModel*>(&M->base_model(m));
      if (not RM)
	throw myexception()<<"Can't construct a modulated Markov model from non-Markov model '"<<M->base_model(m).name()<<"'";
      T += RM->n_states();
    }

    // resize for larger state set
    pi.resize(T);

    Q.resize(T,T);

    state_letters_.resize(T);

    // calculate the state_letters() map here!

    T = 0;
    for(int m=0; m < M->n_base_models(); m++) 
    {
      unsigned N = M->base_model(m).n_states();
      for(int i=0; i<N; i++)
	state_letters_[T+i] = M->base_model(m).state_letters()[i];

      T += N;
    }

    add_submodel("M",*M);
    add_submodel("S",*S);

    read();
    recalc_all();
  }
  
}
