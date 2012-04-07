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
#include "probability.H"
#include "io.H"
#include "computation/expression.H"
#include "computation/formula_expression.H"
#include "smodel/operations.H"

using std::vector;
using std::valarray;
using std::string;
using std::cerr;
using std::endl;
using std::istringstream;

using boost::shared_ptr;
using boost::dynamic_pointer_cast;

namespace substitution {

  string s_parameter_name(int i,int n) {
    if (i>=n)
      throw myexception()<<"substitution model: referred to parameter "<<i<<" but there are only "<<n<<" parameters.";
    return string("pS") + convertToString(i);
  }

  template <typename T>
  efloat_t dirichlet_pdf(const vector<T>& p1,const valarray<double>& q)
  {
    valarray<double> p2 = get_varray<double>(p1);

    return ::dirichlet_pdf(p2,q);
  }

  template <typename T>
  efloat_t dirichlet_pdf(const vector<T>& p1,double N)
  {
    valarray<double> p2 = get_varray<double>(p1);

    return ::dirichlet_pdf(p2,N);
  }

  efloat_t SimpleExchangeModel::prior() const 
  {
    if (is_fixed(0))
      return 1;
    else
      return laplace_pdf(log(rho()), -3, 1)/rho();
  }

  shared_ptr<const Object> SimpleExchangeModel::result() const
  {
    Double rho = get_parameter_value_as<Double>(0);
    Int n = get_parameter_value_as<Int>(1);

    return SimpleExchangeFunction(rho, n);
  }

  string SimpleExchangeModel::name() const 
  {
    return "SimpleExchangeModel";
  }

  SimpleExchangeModel::SimpleExchangeModel(unsigned n)
  {
    add_parameter(Parameter("rho",Double(exp(-4)),lower_bound(0)));
    add_parameter(Parameter("n_states",Int(n)));
  }

  //----------------------- Frequency Models ------------------------//

  ReversibleFrequencyModel::ReversibleFrequencyModel(const alphabet& a)
  { 
    add_parameter(Parameter("alphabet",a));
  }

  boost::shared_ptr<const Object>  UniformFrequencyModel::result() const
  {
    const alphabet& a = get_parameter_value_as<alphabet>(0);

    shared_ptr<ReversibleFrequencyModelObject> R( new ReversibleFrequencyModelObject(a) );

    R->pi = std::vector<double>(a.size(), 1.0/a.size());
    
    // compute transition rates
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++)
	R->R(i,j) = 1;

    // diagonal entries should have no effect
    for(int i=0;i<a.size();i++)
      R->R(i,i) = 0;

    return R;
  }

  string UniformFrequencyModel::name() const {
    return "UF";
  }
  
  UniformFrequencyModel::UniformFrequencyModel(const alphabet& a)
    :ReversibleFrequencyModel(a)
  {
    // initialize everything
    recalc_all();
  }

  UniformFrequencyModel::UniformFrequencyModel(const alphabet& a,const valarray<double>&)
    :ReversibleFrequencyModel(a)
  {
    // initialize everything
    recalc_all();
  }

  shared_ptr<const Object> SimpleFrequencyModel::result() const
  {
    const alphabet& a = get_parameter_value_as<alphabet>(0);
    double f = get_parameter_value_as<Double>(1);
    vector<double> pi = get_vector<double>( get_parameter_values_as<Double>( range<int>(2,a.size()) ) );

    return Plus_gwF_Function(a,f,pi);
  }

  efloat_t SimpleFrequencyModel::prior() const 
  {
    const alphabet& a = get_parameter_value_as<alphabet>(0);

    // uniform prior on f
    efloat_t Pr = 1;

    // uniform - 1 observeration per letter
    Pr *= dirichlet_pdf(get_parameter_values_as<Double>( range<int>(2, a.size() ) ), 1.0);

    return Pr;
  }

  string SimpleFrequencyModel::name() const 
  {
    const alphabet& a = get_parameter_value_as<alphabet>(0);
    if (is_fixed(1) and get_parameter_value_as<Double>(1) == 1.0)
      return "F["+a.name+"]";
    else
      return "gwF["+a.name+"]";
  }
  
  SimpleFrequencyModel::SimpleFrequencyModel(const alphabet& a)
    :ReversibleFrequencyModel(a)
  {
    // Start with *f = 1
    add_parameter(Parameter("f",Double(1.0),between(0, 1)));
    //    parameters_[0].fixed = true;

    for(int i=0;i<a.size();i++) {
      string pname = string("pi") + a.letter(i);
      add_parameter(Parameter(pname, Double(1.0/a.size()), between(0, 1)));
    }

    // initialize everything
    recalc_all();
  }

  SimpleFrequencyModel::SimpleFrequencyModel(const alphabet& a,const valarray<double>& pi)
    :ReversibleFrequencyModel(a)
  {
    if (pi.size() != a.size())
      throw myexception()<<"Constructing a frequency model on alphabet '"<<a.name<<"' but got frequencies of "<<pi.size()<<" letters instead of the expected "<<a.size();

    add_parameter(Parameter("f", Double(1.0), between(0, 1)));
    // Start with *f = 1
    // set_fixed(0,true);

    valarray<double> f = pi;
    f /= f.sum();
    for(int i=0;i<a.size();i++) {
      string pname = string("pi") + a.letter(i);
      add_parameter(Parameter(pname, Double(f[i]), between(0, 1)));
    }

    // initialize everything
    recalc_all();
  }

  //------------------- Triplet Frequency Model -----------------//
  TripletFrequencyModel::TripletFrequencyModel(const Triplets& T)
    :ReversibleFrequencyModel(T)
  { }
    
  valarray<double> triplet_from_singlet_frequencies(const Triplets& T,const ReversibleFrequencyModelObject& N)
  {
    if (not dynamic_pointer_cast<const Nucleotides>(N.get_alphabet()))
      throw myexception()<<"Singlet frequencies are not nucleotide frequencies.";

    valarray<double> pi(1.0,T.size());

    for(int i=0;i<T.size();i++) 
      for(int j=0;j<3;j++)
	pi[i] *= N.pi[T.sub_nuc(i,j)];

    pi /= pi.sum();
    
    return pi;
  }


  shared_ptr<const Object> IndependentNucleotideFrequencyModel::result() const
  {
    const Triplets& T = get_parameter_value_as<Triplets>(0);

    shared_ptr<ReversibleFrequencyModelObject> R( new ReversibleFrequencyModelObject(T) );

    //------------------ compute triplet frequencies ------------------//

    // 1. Get the nucleotide frequencies
    shared_ptr<const ReversibleFrequencyModelObject> nucs = SubModels(0).result_as<ReversibleFrequencyModelObject>();

    // 2. Then get the corresponding triplet frequencies
    R->pi = get_vector<double>( triplet_from_singlet_frequencies(T, *nucs) );

    // 3. Then construct a +F triplets model from them
    //    (?? hey, shouldn't I need to specify e.g. Muse&Gaut or Yang-type here?)
    shared_ptr<const ReversibleFrequencyModelObject> triplets;
    triplets = SimpleFrequencyModel(T,get_varray<double>(R->pi) ).result_as<ReversibleFrequencyModelObject>();

    for(int i=0;i<T.size();i++)
      for(int j=0;j<T.size();j++)
	R->R(i,j) = (*triplets)(i,j);

    return R;
  }

  string IndependentNucleotideFrequencyModel::name() const
  {
    return "F1x4";
  }

  
  IndependentNucleotideFrequencyModel::IndependentNucleotideFrequencyModel(const Triplets& T) 
    : TripletFrequencyModel(T)
  {
    // problem: TripletFrequency model won't have done this for its alphabet parameter
    model_slots_for_index.push_back(vector<model_slot>());

    insert_submodel("1",SimpleFrequencyModel(T.getNucleotides()));
    recalc_all();
  }


  shared_ptr<const Object> TripletsFrequencyModel::result() const
  {
    const Triplets& T = get_parameter_value_as<Triplets>(0);

    shared_ptr<ReversibleFrequencyModelObject> R( new ReversibleFrequencyModelObject(T) );

    valarray<double> nu = get_varray<double>( get_parameter_values_as<Double>( range<int>(1,T.size()) ) );

    //------------- compute frequencies ------------------//

    // 1. Get the nucleotide frequencies
    shared_ptr<const ReversibleFrequencyModelObject> nucs = SubModels(0).result_as<ReversibleFrequencyModelObject>();

    // 2. Then get the corresponding triplet frequencies
    R->pi = get_vector<double>( triplet_from_singlet_frequencies(T, *nucs) );

    for(int i=0;i<T.size();i++)
      R->pi[i] *= nu[i];

    normalize(R->pi);


    //------------ compute transition rates -------------//
    double g = get_parameter_value_as<Double>(0);

    valarray<double> nu_g(T.size());
    for(int i=0;i<T.size();i++)
      nu_g[i] = pow(nu[i],g);


    // FIXME - can we really handle two mutations?
    // Should the restriction on 1 mutation be HERE?
    for(int i=0;i<T.size();i++)
      for(int j=0;j<T.size();j++) {
	R->R(i,j) = nu_g[i]/nu[i]*nu_g[j];
	for(int k=0;k<3;k++) {
	  int n1 = T.sub_nuc(i,k);
	  int n2 = T.sub_nuc(j,k);
	  if (n1 != n2)
	    R->R(i,j) *= (*nucs)(n1,n2);
	}
      }

    // diagonal entries should have no effect
    for(int i=0;i<T.size();i++)
      R->R(i,i) = 0;

    return R;
  }

  efloat_t TripletsFrequencyModel::super_prior() const 
  {
    const Triplets& T = get_parameter_value_as<Triplets>(0);

    return dirichlet_pdf(get_parameter_values_as<Double>( range<int>(2,T.size() ) ), 4.0);
  }

  string TripletsFrequencyModel::name() const 
  {
    return "FF=triplets";
  }

  TripletsFrequencyModel::TripletsFrequencyModel(const Triplets& T)
    : TripletFrequencyModel(T)
  {
    // problem: TripletFrequency model won't have done this for its alphabet parameter
    model_slots_for_index.push_back(vector<model_slot>());

    add_super_parameter(Parameter("g", Double(1), between(0, 1) ));

    for(int i=0;i<T.size();i++) {
      string pname = string("v") + T.letter(i);
      add_super_parameter(Parameter(pname, Double(1.0/T.size()), between(0, 1) ));
    }

    insert_submodel("1",SimpleFrequencyModel(T.getNucleotides()));

    recalc_all();
  }

  //------------------- Codon Frequency Model -----------------//
  CodonFrequencyModel::CodonFrequencyModel(const Codons& C)
    :ReversibleFrequencyModel(C)
  { }

  // FIXME - these frequency models are a bit messed up.
  // (i)  I need to think more clearly about the use of +gwF parameters.
  // (ii) I need to put priors on the +gwF parameters here, instead of abusing SubModels for that purpose.
  //  - I can make an frequency model OBJECT w/o constructing a MODEL.
  //  - I should make a +gwF FUNCTION that is used by SimpleFrequencyModel.
  //  - I should make a +F FUNCTION as well.
  //  - I would not need a submodel anymore.
  // (iii) I should 

  shared_ptr<const Object> AACodonFrequencyModel::result() const
  {
    const Codons& C = get_parameter_value_as<Codons>(0);

    shared_ptr<ReversibleFrequencyModelObject> R( new ReversibleFrequencyModelObject(C) );

    //----------- get amino acid frequencies and counts ------------//
    // 1. Get unweighted codon frequencies
    shared_ptr<const ReversibleFrequencyModelObject> amino_acids = SubModels(0).result_as<ReversibleFrequencyModelObject>();

    vector<double> f_aa = amino_acids->pi;
    vector<int> n_aa(f_aa.size(),0);
    for(int i=0;i<C.size();i++) {
      int aa = C.translate(i);
      n_aa[aa]++;
    }
      
    //------------------ compute triplet frequencies ------------------//
    for(int i=0;i<C.size();i++) {
      int aa = C.translate(i);
      R->pi[i] = f_aa[aa]/n_aa[aa];
    }

    //    vector<Double> codon_parameters(n_letters()+1);
    //    codon_parameters[0] = SubModels(0).get_parameter_value_as<Double>(0);
    //    set_varray(codon_parameters,1,pi);
    SimpleFrequencyModel CM(C,get_varray<double>(R->pi) );
    CM.set_parameter_value("f", SubModels(0).get_parameter_value("f"));
      
    shared_ptr<const ReversibleFrequencyModelObject> codons = CM.result_as<ReversibleFrequencyModelObject>();
    //    (copy the "f" from the amino-acid model)

    for(int i=0;i<C.size();i++)
      for(int j=0;j<C.size();j++)
	R->R(i,j) = (*codons)(i,j);

    return R;
  }

  string AACodonFrequencyModel::name() const
  {
    return "FF=amino-acids";
  }

  
  AACodonFrequencyModel::AACodonFrequencyModel(const Codons& C) 
    : CodonFrequencyModel(C)
  {
    // problem: CodonFrequency model won't have done this for its alphabet parameter
    model_slots_for_index.push_back(vector<model_slot>());

    insert_submodel("1",SimpleFrequencyModel(C.getAminoAcids()));

    recalc_all();
  }




  //------------------- Codons Frequency Model -----------------//

  shared_ptr<const Object> CodonsFrequencyModel::result() const
  {
    const Codons& C = get_parameter_value_as<Codons>(0);
    int aa_size = C.getAminoAcids().size();

    shared_ptr<ReversibleFrequencyModelObject> R( new ReversibleFrequencyModelObject(C) );

    double c = get_parameter_value_as<Double>(1);

    //------------- compute frequencies ------------------//
    valarray<double> aa_pi = get_varray<double>(get_parameter_values_as<Double>( range<int>(2,aa_size) ) );

    // get codon frequencies of sub-alphabet
    shared_ptr<const ReversibleFrequencyModelObject> M = SubModels(0).result_as<ReversibleFrequencyModelObject>();
    valarray<double> sub_pi = get_varray<double>( M->pi );

    // get aa frequencies of sub-alphabet
    valarray<double> sub_aa_pi(0.0,aa_size);
    for(int i=0;i<C.size();i++)
      sub_aa_pi[C.translate(i)] += sub_pi[i];

    // get factors by which to multiply sub-alphabet frequencies
    valarray<double> factor(C.size());
    for(int i=0;i<C.size();i++) 
    {
      int j = C.translate(i);
      factor[i] = pow(aa_pi[j]/sub_aa_pi[j],c);
    }

    // compute aa-aware codon frequencies
    for(int i=0;i<C.size();i++) 
      R->pi[i] = sub_pi[i] * factor[i];

    // scale so as to sum to 1
    normalize(R->pi);

    //------------ compute transition rates -------------//
    double h = get_parameter_value_as<Double>(1);

    valarray<double> factor_h(C.size());
    for(int i=0;i<C.size();i++)
      factor_h[i] = pow(factor[i],h);


    for(int i=0;i<C.size();i++)
      for(int j=0;j<C.size();j++)
	R->R(i,j) = (*M)(i,j) * factor_h[i]/factor[i]*factor_h[j];

    // diagonal entries should have no effect
    for(int i=0;i<C.size();i++)
      R->R(i,i) = 0;

    return R;
  }

  efloat_t CodonsFrequencyModel::super_prior() const 
  {
    const Codons& C = get_parameter_value_as<Codons>(0);
    return dirichlet_pdf(get_parameter_values_as<Double>( range<int>(2, C.getAminoAcids().size() ) ), 2.0);
  }

  string CodonsFrequencyModel::name() const 
  {
    return "FF=codons1";
  }

  CodonsFrequencyModel::CodonsFrequencyModel(const Codons& C)
    : CodonFrequencyModel(C)
  {
    // problem: CodonFrequency model won't have done this for its alphabet parameter
    model_slots_for_index.push_back(vector<model_slot>());

    add_super_parameter(Parameter("c", Double(0.5), between(0, 1))); // 1
    add_super_parameter(Parameter("h", Double(0.5), between(0, 1))); // 2

    for(int i=0;i<C.getAminoAcids().size();i++) {
      string pname = string("b_") + C.getAminoAcids().letter(i);
      add_super_parameter(Parameter(pname,  Double(1.0/C.getAminoAcids().size()), between(0, 1)));
    }

    insert_submodel("1",TripletsFrequencyModel(C));

    recalc_all();
  }

  //------------------- Codons Frequency Model 2 -----------------//

  shared_ptr<const Object>  CodonsFrequencyModel2::result() const
  {
    const Codons& C = get_parameter_value_as<Codons>(0);

    shared_ptr<ReversibleFrequencyModelObject> R( new ReversibleFrequencyModelObject(C) );

    //------------- compute frequencies ------------------//
    valarray<double> aa_pref_ = get_varray<double>(get_parameter_values_as<Double>( range<int>(1,C.getAminoAcids().size()) ) );

    valarray<double> aa_pref(C.size());
    for(int i=0;i<C.size();i++)
      aa_pref[i] = aa_pref_[C.translate(i)];

    // get codon frequencies of sub-alphabet
    shared_ptr<const ReversibleFrequencyModelObject> M = SubModels(0).result_as<ReversibleFrequencyModelObject>();
    valarray<double> sub_pi = get_varray<double>( M->pi );

    // scale triplet frequencies by aa prefs
    for(int i=0;i<C.size();i++) 
      R->pi[i] = sub_pi[i] * aa_pref[i];

    // scale so as to sum to 1
    normalize(R->pi);


    //------------ compute transition rates -------------//
    double h = get_parameter_value_as<Double>(0);

    valarray<double> aa_pref_h(C.size());
    for(int i=0;i<C.size();i++)
      aa_pref_h[i] = pow(aa_pref[i], h);


    for(int i=0;i<C.size();i++)
      for(int j=0;j<C.size();j++)
	R->R(i,j) = (*M)(i,j) * aa_pref_h[i]/aa_pref[i] * aa_pref_h[j];

    // diagonal entries should have no effect
    for(int i=0;i<C.size();i++)
      R->R(i,i) = 0;

    return R;
  }

  efloat_t CodonsFrequencyModel2::super_prior() const 
  {
    const Codons& C = get_parameter_value_as<Codons>(0);

    return dirichlet_pdf(get_parameter_values_as<Double>( range<int>(1, C.getAminoAcids().size() ) ), 2.0);
  }

  string CodonsFrequencyModel2::name() const 
  {
    return "FF=codons2";
  }

  CodonsFrequencyModel2::CodonsFrequencyModel2(const Codons& C)
    : CodonFrequencyModel(C)
  {
    add_super_parameter(Parameter("h", Double(0.5), between(0, 1)));

    for(int i=0;i<C.getAminoAcids().size();i++) {
      string pname = string("b_") + C.getAminoAcids().letter(i);
      add_super_parameter(Parameter(pname, Double(1.0/C.getAminoAcids().size()), between(0, 1)));
    }

    insert_submodel("1",TripletsFrequencyModel(C));

    recalc_all();
  }

  //----------------------- ReversibleMarkovModel --------------------------//

  shared_ptr<const Object> F81_Model::result() const
  {
    const alphabet& a = get_parameter_value_as<alphabet>(0);

    shared_ptr<ReversibleFrequencyModelObject> R( new ReversibleFrequencyModelObject(a) );

    const int N = a.size();

    valarray<double> pi = get_varray<double>( get_parameter_values_as<Double>( range<int>(1,N) ) );
    pi /= pi.sum();

    return shared_ptr<const F81_Object>(new F81_Object(a, pi) );
  }

  efloat_t F81_Model::prior() const
  {
    const alphabet& a = get_parameter_value_as<alphabet>(0);
    // uniform prior on f
    efloat_t Pr = 1;

    // uniform - 1 observeration per letter
    Pr *= dirichlet_pdf(get_parameter_values_as<Double>( range<int>(1, a.size()) ), 1.0);

    return Pr;
  }

  string F81_Model::name() const
  {
    const alphabet& a = get_parameter_value_as<alphabet>(0);
    return string("F81[")+a.name+"]";
  }

  F81_Model::F81_Model(const alphabet& a)
  {
    add_parameter(Parameter("alphabet",a));

    int N = a.size();

    for(int i=0;i<N;i++) {
      string pname = string("pi") + a.letter(i);
      add_parameter(Parameter(pname, Double(1.0/N), between(0, 1)));
    }

    recalc_all();
  }

  F81_Model::F81_Model(const alphabet& a,const valarray<double>& f)
  {
    add_parameter(Parameter("alphabet",a));

    int N = a.size();

    assert(f.size() == N);

    for(int i=0;i<N;i++) {
      string pname = string("pi") + a.letter(i);
      add_parameter(Parameter(pname, Double(f[i]), between(0, 1)));
    }

    recalc_all();
  }

  //---------------------- INV_Model --------------------------//

  shared_ptr<const Object> INV_Model::result() const
  {
    return INV_Exchange_Function(get_parameter_value_as<alphabet>(0), get_parameter_value_as<alphabet>(0).size());
  }

  string INV_Model::name() const 
  {
    return "INV";
  }

  INV_Model::INV_Model(const alphabet& a)
  {
    add_parameter(Parameter("alphabet",a));
  }
      
  //----------------------- EQU -------------------------//

  shared_ptr<const Object> EQU::result() const
  {
    return EQU_Exchange_Function(get_parameter_value_as<alphabet>(0));
  }

  string EQU::name() const {
    return "EQU";
  }

  EQU::EQU(const alphabet& a) 
  {
    add_parameter(Parameter("alphabet",a));
  }

  //----------------------- Empirical -------------------------//

  shared_ptr<const Object> Empirical::result() const
  {
    return get_parameter_value(1);
  }

  void Empirical::load_file(const string& filename) 
  {
    name_ = string("Empirical[") + get_basename(filename) + "]";

    checked_ifstream ifile(filename, "empirical rate matrix file");

    load_file(ifile);

    // the file has frequencies as well... where would we put them?
  }

  void Empirical::load_file(std::istream& file)
  {
    const alphabet& a = get_parameter_value_as<alphabet>(0);

    shared_ptr<ExchangeModelObject> R (new AlphabetExchangeModelObject(a));

    for(int i=0;i<a.size();i++)
      for(int j=0;j<i;j++) {
	file>>R->S(i,j);
	R->S(j,i) = R->S(i,j);
      }

    set_parameter_value(1, R);
    // the file has frequencies as well... where would we put them?
  }

  /// Construct an Empirical model on alphabet \a a
  Empirical::Empirical(const alphabet& a) 
  { 
    add_parameter(Parameter("alphabet",a));
  }

  /// Construct an Empirical model on alphabet \a a with name \n
  Empirical::Empirical(const alphabet& a,const string& n) 
    :name_(n)
  { 
    add_parameter(Parameter("alphabet",a));
    add_parameter(Parameter("S"));
  }

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
    if (is_fixed(1))
      return 1;
    else
      return laplace_pdf(log(kappa()), log(2), 0.25)/kappa();
  }

  shared_ptr<const Object> HKY::result() const
  {
    const Nucleotides& N = get_parameter_value_as<Nucleotides>(0);
    Double kappa = get_parameter_value_as<Double>(1);

    return HKY_Function(N, kappa);
  }

  /// Construct an HKY model on alphabet 'a'
  HKY::HKY(const Nucleotides& N)
  { 
    add_parameter(Parameter("alphabet",N));
    add_parameter(Parameter("HKY::kappa", Double(2), lower_bound(0)));
  }

  //------------------------- TN -----------------------------//
  string TN::name() const {
    return "TN";
  }


  efloat_t TN::prior() const 
  {
    efloat_t P = 1;
    if (not is_fixed(1))
      P *= laplace_pdf(log(kappa1()), log(2), 0.25)/kappa1();
    if (not is_fixed(2))
      P *= laplace_pdf(log(kappa2()), log(2), 0.25)/kappa2();
    return P;
  }

  shared_ptr<const Object> TN::result() const
  {
    const Nucleotides& N = get_parameter_value_as<Nucleotides>(0);
    Double kappa1 = get_parameter_value_as<Double>(1);
    Double kappa2 = get_parameter_value_as<Double>(2);
    
    return TN_Function(N, kappa1, kappa2);
  }
  
  /// Construct a TN model on alphabet 'a'
  TN::TN(const Nucleotides& N)
  { 
    add_parameter(Parameter("alphabet",N));
    add_parameter(Parameter("TN::kappa(pur)",Double(2), lower_bound(0)));
    add_parameter(Parameter("TN::kappa(pyr)",Double(2), lower_bound(0)));
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

    return dirichlet_pdf( get_parameter_values_as<Double>( range<int>(1,6) ), n);
  }


  shared_ptr<const Object> GTR::result() const
  {
    const Nucleotides& N = get_parameter_value_as<Nucleotides>(0);

    Double AG = get_parameter_value_as<Double>(1);
    Double AT = get_parameter_value_as<Double>(2);
    Double AC = get_parameter_value_as<Double>(3);

    Double GT = get_parameter_value_as<Double>(4);
    Double GC = get_parameter_value_as<Double>(5);

    Double TC = get_parameter_value_as<Double>(6);

    return GTR_Function(N, AG, AT, AC, GT, GC, TC);
  }

  GTR::GTR(const Nucleotides& N)
  { 
    add_parameter(Parameter("alphabet",N));
    add_parameter(Parameter("GTR::AG", Double(2.0/8), between(0, 1)));
    add_parameter(Parameter("GTR::AT", Double(1.0/8), between(0, 1)));
    add_parameter(Parameter("GTR::AC", Double(1.0/8), between(0, 1)));
    add_parameter(Parameter("GTR::GT", Double(1.0/8), between(0, 1)));
    add_parameter(Parameter("GTR::GC", Double(1.0/8), between(0, 1)));
    add_parameter(Parameter("GTR::TC", Double(2.0/8), between(0, 1)));
  }

  //------------------------ Triplet Models -------------------//

  TripletExchangeModel::TripletExchangeModel(const Triplets& T)
  {
    add_parameter(Parameter("alphabet",T));
  }

  shared_ptr<const Object> SingletToTripletExchangeModel::result() const
  {
    return SingletToTripletExchangeFunction(get_parameter_value_as<Triplets>(0), *SubModels(0).result_as<ExchangeModelObject>() );
  }

  string SingletToTripletExchangeModel::name() const {
    string n = SubModels(0).name();
    n += "x3";
    return n;
  }
  
  SingletToTripletExchangeModel::SingletToTripletExchangeModel(const Triplets& T,const ::Model& N)
    :TripletExchangeModel(T)
  { 
    add_parameter(Parameter("alphabet",T));
    insert_submodel("1",N);
  }

  //---------------------- CAT_FixedFrequencyModel -----------------------//
  const alphabet& CAT_FixedFrequencyModel::Alphabet() const
  {
    return get_parameter_value_as<alphabet>(0);
  }


  efloat_t CAT_FixedFrequencyModel::prior() const
  {
    valarray<double> f = get_varray<double>(prior_fraction);

    valarray<double> x = get_varray<double>( get_parameter_values_as<Double>( range<int>(2, prior_fraction.size()) ) );

    return ::dirichlet_pdf(x,safe_count(f*10.0));
  }

  shared_ptr<const Object> CAT_FixedFrequencyModel::result() const
  {
    shared_ptr<MultiModelObject> R = ptr( get_parameter_value_as<MultiModelObject>(1) );

    for(int i=0;i<prior_fraction.size();i++)
      R->fraction[i] = get_parameter_value_as<Double>(2+i);

    return R;
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
    prior_fraction = split<double>(line,' ');
    if (prior_fraction.size() != n_cat) 
      throw myexception()<<"In reading CAT-Fixed model '"<<name_<<"' expected weights for "<<n_cat<<" categories, but got "<<prior_fraction.size();

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
    shared_ptr<MultiModelObject> M(new MultiModelObject);

    for(int i=0;i<n_cat;i++)
    {
      portable_getline(file,line);

      if (file.bad())
	throw myexception()<<"Failed to read frequencies for category "<<i+1;

      vector<double> f = split<double>(line,' ');

      valarray<double> f_ordered(a.size());
      for(int j=0;j<f_ordered.size();j++)
	f_ordered[letter[j]] = f[j];

      M->fraction.push_back(prior_fraction[i]);
      M->base_models.push_back(F81_Model(a,f_ordered).result_as<const ReversibleAdditiveObject>() );

      // FIXME! M->base_models.back()->set_rate(1);
    }
    add_parameter(Parameter("CAT_frequencies",M));

    //------- 6: Create the parameters for fiddling --------//
    for(int i=0;i<n_cat;i++)
    {
      string name = "CAT::f"+convertToString(i+1);
      add_parameter(Parameter(name,Double(prior_fraction[i]),between(0,1)));
    }

    recalc_all();
  }

  void CAT_FixedFrequencyModel::load_file(const string& filename)
  {
    checked_ifstream file(filename,"CAT fixed frequency model file");

    if (not file)
      throw myexception(string("Couldn't open file '")+filename+"'");

    load_file(file);
  }

  CAT_FixedFrequencyModel::CAT_FixedFrequencyModel(const alphabet& a)
  { 
    add_parameter(Parameter("alphabet",a));
  }

  CAT_FixedFrequencyModel::CAT_FixedFrequencyModel(const alphabet& a, const string& n)
    :name_(n)
  { 
    add_parameter(Parameter("alphabet",a));
  }

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
}
