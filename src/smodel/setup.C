/*
   Copyright (C) 2004-2010 Benjamin Redelings

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

///
/// \file setup-smodel.C
///
/// \brief Create substitution models from strings of the form model+...+model.
///

#include <vector>
#include <boost/program_options.hpp>
#include "setup.H"
#include "smodel/smodel.H"
#include "util.H"
#include "rates.H"
#include "myexception.H"
#include "smodel/operations.H"
#include "computation/operations.H"
#include "distribution-operations.H"
#include "computation/prelude.H"
#include "smodel/functions.H"

using std::vector;
using std::valarray;
using namespace substitution;
using boost::program_options::variables_map;
using boost::shared_ptr;
using boost::dynamic_pointer_cast;

/// \brief Take a string of the form \a s[\a arg] off the top of \a sstack, if its present
///
/// \param sstack A stack of strings that represent a substitution model.
/// \param s The model name to match.
/// \param args The possible argument.
///
bool match(vector<string>& sstack,const string& s, vector<string>& args) {
  if (not sstack.size())
    return false;
  
  string name = sstack.back();

  args = get_arguments(name,'[',']');

  if (name != s) return false;

  sstack.pop_back();

  return true;
}

/// \brief Take a string of the form \a s[\a arg] off the top of \a sstack, if its present
///
/// \param sstack A stack of strings that represent a substitution model.
/// \param s The model name to match.
/// \param arg A possible argument.
///
bool match(vector<string>& sstack,const string& s,string& arg) 
{
  vector<string> arguments;

  if (not match(sstack,s,arguments)) return false;

  if (arguments.size() == 0)
    arg = "";
  else if (arguments.size() > 0)
    arg = arguments[0];

  return true;
}

/// \brief Return the default substitution model name for alphabet \a a, and "" if there is no default.
string default_markov_model(const alphabet& a) 
{
  if (dynamic_cast<const Nucleotides*>(&a))
    return "TN";
  else if (dynamic_cast<const AminoAcidsWithStop*>(&a))
    return "";
  else if (dynamic_cast<const AminoAcids*>(&a))
    return "LG";
  else if (dynamic_cast<const Codons*>(&a))
    return "M0";
  else if (dynamic_cast<const Triplets*>(&a))
    return "TNx3";
  else
    return "";
}

formula_expression_ref
get_smodel_(const string& smodel,const object_ptr<const alphabet>& a, const shared_ptr<const valarray<double> >&);

formula_expression_ref
get_smodel_(const string& smodel,const object_ptr<const alphabet>& a);

formula_expression_ref
get_smodel_(const string& smodel);

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param model_stack The list of models that may be used as components of the current model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
bool process_stack_Markov(vector<string>& string_stack,
			  vector<formula_expression_ref >& model_stack,
			  const object_ptr<const alphabet>& a,
			  const shared_ptr<const valarray<double> >& frequencies)
{
  string arg;

  formula_expression_ref R;

  //------ Get the base markov model (Reversible Markov) ------//
  /*
  if (match(string_stack,"EQU",arg))
    model_stack.push_back(EQU(*a));
  */
  if (match(string_stack,"F81",arg))
  {
    if (frequencies)
      model_stack.push_back(F81_Model(*a,*frequencies));
    else
      model_stack.push_back(F81_Model(*a));
  }

  else if (match(string_stack,"HKY",arg)) 
  {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
    if (not N)
      throw myexception()<<"HKY: '"<<a->name<<"' is not a nucleotide alphabet.";

    R = HKY_Model(*a);
  }
  else if (match(string_stack,"TN",arg)) 
  {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
    if (not N)
      throw myexception()<<"TN: '"<<a->name<<"' is not a nucleotide alphabet.";

    R = TN_Model(*a);
  }
  else if (match(string_stack,"GTR",arg)) 
  {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
    if (not N)
      throw myexception()<<"GTR: '"<<a->name<<"' is not a nucleotide alphabet.";

    // FIXME - allow/make a general GTR model!

    R = GTR_Model(*a);
  }
  /*
  else if (match(string_stack,"EQUx3",arg)) {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (T) 
      model_stack.push_back(Singlet_to_Triplet_Exchange(*T,EQU(T->getNucleotides())));
    else
      throw myexception()<<"EQUx3:: '"<<a->name<<"' is not a triplet alphabet.";
  }
  */
  else if (match(string_stack,"HKYx3",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    formula_expression_ref nuc_S = prefix_formula("nuc",HKY_Model(T->getNucleotides()));
    if (T) 
      model_stack.push_back(Singlet_to_Triplet_Exchange(*T)(nuc_S));
    else
      throw myexception()<<"HKYx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  else if (match(string_stack,"TNx3",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    formula_expression_ref nuc_S = prefix_formula("nuc",TN_Model(T->getNucleotides()));
    if (T) 
      model_stack.push_back(Singlet_to_Triplet_Exchange(*T)(nuc_S));
    else
      throw myexception()<<"TNx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  else if (match(string_stack,"GTRx3",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    formula_expression_ref nuc_S = prefix_formula("nuc",GTR_Model(T->getNucleotides()));
    if (T) 
      model_stack.push_back(Singlet_to_Triplet_Exchange(*T)(nuc_S));
    else
      throw myexception()<<"GTRx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  else if (match(string_stack,"PAM",arg)) {
    if (*a != AminoAcids())
      throw myexception()<<"PAM: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back((PAM,a));
  }
  else if (match(string_stack,"JTT",arg)) {
    if (*a != AminoAcids())
      throw myexception()<<"JTT: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back((JTT,a));
  }
  else if (match(string_stack,"WAG",arg)) {
    if (*a != AminoAcids())
      throw myexception()<<"WAG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back((WAG,a));
  }
  else if (match(string_stack,"LG",arg)) {
    if (*a != AminoAcids())
      throw myexception()<<"LG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back((LG,a));
  }
  else if (match(string_stack,"Empirical",arg)) {
    model_stack.push_back((Empirical,a,arg));
  }
  else if (match(string_stack,"C10",arg))
  {
    if (*a != AminoAcids())
      throw myexception()<<"C20: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back(C10_CAT_FixedFrequencyModel());
  }
  else if (match(string_stack,"C20",arg))
  {
    if (*a != AminoAcids())
      throw myexception()<<"C20: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back(C20_CAT_FixedFrequencyModel());
  }
  else if (match(string_stack,"CAT-Fix",arg)) {
    if (*a != AminoAcids())
      throw myexception()<<"CAT-Fix: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    CAT_FixedFrequencyModel M(*a);
    M.load_file(arg);
    model_stack.push_back(M);
  }

  else if (match(string_stack,"M0",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<"M0: '"<<a->name<<"' is not a 'Codons' alphabet";

    formula_expression_ref N_submodel = HKY_Model(C->getNucleotides());

    if (not arg.empty()) 
    {
      formula_expression_ref submodel = get_smodel_(arg, const_ptr( C->getNucleotides() ) );

      if (not submodel.result_as<AlphabetExchangeModelObject>() or 
	  not dynamic_pointer_cast<const Nucleotides>(submodel.result_as<AlphabetExchangeModelObject>()->get_alphabet()))
	throw myexception()<<"Submodel '"<<arg<<"' for M0 is not a nucleotide replacement model.";

      N_submodel = submodel;
    }

    formula_expression_ref S1 = TN_Model(C->getNucleotides());
    formula_expression_ref w = def_parameter("M0::omega", Double(1), lower_bound(0), log_laplace_dist, Tuple(0.0,0.1));
    formula_expression_ref M0 = M0E(a)(S1)(w);

    model_stack.push_back( M0 );
  }
  else
    return false;

  if (R.exp())
    model_stack.push_back(R);
  return true;
}

/// \brief Construct an AlphabetExchangeModel from model \a M
formula_expression_ref get_EM(const formula_expression_ref& R, const string& name)
{
  if (R.result_as<AlphabetExchangeModelObject>())
    return R;

  if (R.result_as<SymmetricMatrixObject>())
    return R;

  throw myexception()<<name<<": '"<<R.exp()<<"' is not an exchange model.";
}


/// \brief Construct an AlphabetExchangeModel from the top of the model stack
formula_expression_ref get_EM(vector<formula_expression_ref>& model_stack, const string& name)
{
  if (model_stack.empty())
    throw myexception()<<name<<": Needed an exchange model, but no model was given.";

  return get_EM(model_stack.back(), name);
}

/// \brief Construct an AlphabetExchangeModel from the top of the model stack
formula_expression_ref get_EM_default(vector<formula_expression_ref>& model_stack, 
				      const string& name,
				      const object_ptr<const alphabet>& a, 
				      const shared_ptr< const valarray<double> >& frequencies)
{
  if (model_stack.empty())
    model_stack.push_back( get_smodel_(default_markov_model(*a), a, frequencies));

  return get_EM(model_stack.back(), name);
}


/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param model_stack The list of models that may be used as components of the current model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
bool process_stack_Frequencies(vector<string>& string_stack,
			       vector<formula_expression_ref >& model_stack,
			       const object_ptr<const alphabet>& a,
			       const shared_ptr<const valarray<double> >& frequencies)
{
  string arg;
  
  if (match(string_stack,"F=constant",arg)) 
  {
    formula_expression_ref EM = get_EM_default(model_stack,"+F=constant", a, frequencies);

    formula_expression_ref F = Plus_gwF(*a)(1.0)( get_tuple(*frequencies) );

    model_stack.back() = Reversible_Markov_Model(EM,F);
  }
  else if (match(string_stack,"F",arg)) {
    formula_expression_ref EM = get_EM_default(model_stack,"+F", a, frequencies);

    formula_expression_ref F;
    if (frequencies)
      F = Plus_F_Model(*a,*frequencies);
    else
      F = Plus_F_Model(*a);

    model_stack.back() = Reversible_Markov_Model(EM,F);
  }
  else if (match(string_stack,"F=uniform",arg)) 
  {
    formula_expression_ref EM = get_EM_default(model_stack,"+F=uniform", a, frequencies);

    vector<double> pi(a->size(),1.0/a->size() );

    formula_expression_ref F = Plus_gwF(*a)(1.0)( get_tuple( pi ) );

    model_stack.back() = Reversible_Markov_Model(EM,F);
  }
  else if (match(string_stack,"F=nucleotides",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (not T)
      throw myexception()<<"+F=nucleotides:: '"<<a->name<<"' is not a triplet alphabet.";

    formula_expression_ref EM = get_EM_default(model_stack,"+F=nucleotides", a, frequencies);

    model_stack.back() = Reversible_Markov_Model(EM, IndependentNucleotideFrequencyModel(*T));
  }
  else if (match(string_stack,"F=amino-acids",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<"+F=amino-acids:: '"<<a->name<<"' is not a codon alphabet.";

    formula_expression_ref EM = get_EM_default(model_stack,"+F=amino-acids", a, frequencies);

    model_stack.back() = Reversible_Markov_Model(EM, AACodonFrequencyModel(*C));
  }
  else if (match(string_stack,"F=triplets",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (not T)
      throw myexception()<<"+F=triplets:: '"<<a->name<<"' is not a triplet alphabet.";

    formula_expression_ref EM = get_EM_default(model_stack,"+F=triplets", a, frequencies);

    model_stack.back() = Reversible_Markov_Model(EM,TripletsFrequencyModel(*T));
  }
  else if (match(string_stack,"F=codons",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<"+F=codons:: '"<<a->name<<"' is not a codon alphabet.";

    formula_expression_ref EM = get_EM_default(model_stack,"+F=codons", a, frequencies);

    model_stack.back() = Reversible_Markov_Model(EM,CodonsFrequencyModel(*C));
  }
  else if (match(string_stack,"F=codons2",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<"+F=codons2:: '"<<a->name<<"' is not a codon alphabet.";

    formula_expression_ref EM = get_EM_default(model_stack,"+F=codons2", a, frequencies);

    model_stack.back() = Reversible_Markov_Model(EM,CodonsFrequencyModel2(*C));
  }
  else
    return false;
  return true;
}

/// \brief Construct a ReversibleMarkovModel from model \a M
formula_expression_ref get_RA(const formula_expression_ref& M, const string& name,
			      const object_ptr<const alphabet>& a, const shared_ptr< const valarray<double> >& frequencies)
{
  if (is_exactly(M.result(SModel_Functions()), "F81"))
    return M;

  if (is_exactly(M.result(SModel_Functions()), "ReversibleMarkov"))
    return M;

  try {
    formula_expression_ref top = get_EM(M,name);
    // If the frequencies.size() != alphabet.size(), this call will throw a meaningful exception.
    if (frequencies)
      return Simple_gwF_Model(top, *a, *frequencies); 
    else
      return Simple_gwF_Model(top, *a); 
  }
  catch (std::exception& e) { 
    throw myexception()<<name<<": Can't construct a SimpleReversibleMarkovModel from '"<<M.exp()<<"':\n "<<e.what();
  }
}

/// \brief Construct a ReversibleMarkovModel from the top of the model stack
formula_expression_ref get_RA(vector<formula_expression_ref >& model_stack, 
			      const string& name,
			      const object_ptr<const alphabet>& a, const shared_ptr< const valarray<double> >& frequencies)
{
  if (model_stack.empty())
    throw myexception()<<name<<": couldn't find any model to use.";
  
  return get_RA(model_stack.back(), name, a, frequencies);
}


/// \brief Construct a ReversibleMarkovModel from the top of the model stack
formula_expression_ref get_RA_default(vector<formula_expression_ref >& model_stack, 
				      const string& name,
				      const object_ptr<const alphabet>& a,
				      const shared_ptr< const valarray<double> >& frequencies)
{
  if (model_stack.empty())
    model_stack.push_back( get_smodel_(default_markov_model(*a), a, frequencies));
  
  return get_RA(model_stack.back(), name, a, frequencies);
}


/// \brief Construct a MultiModel from model \a M
formula_expression_ref
get_MM(const formula_expression_ref& M, const string& name, 
       const object_ptr<const alphabet>& a, const shared_ptr< const valarray<double> >& frequencies)
{
  if (is_exactly(M.result(SModel_Functions()), "MixtureModel"))
    return M;

  try { 
    return Unit_Model( get_RA(M,name,a, frequencies) ) ; 
  }
  catch (std::exception& e) { 
    throw myexception()<<name<<": Can't construct a UnitModel from '"<<M.exp()<<"':\n"<<e.what();
  }
}

/// \brief Construct a MultiModel from the top of the model stack.
formula_expression_ref
get_MM(vector<formula_expression_ref >& model_stack, const string& name, 
       const object_ptr<const alphabet>& a, const shared_ptr< const valarray<double> >& frequencies)
{
  if (model_stack.empty())
    throw myexception()<<name<<": Trying to construct a MultiModel, but no model was given.";

  return get_MM(model_stack.back(), name, a, frequencies);
}

/// \brief Construct a MultiModel from the top of the model stack.
formula_expression_ref
get_MM_default(vector<formula_expression_ref >& model_stack, const string& name, 
	       const object_ptr<const alphabet>& a, 
	       const shared_ptr< const valarray<double> >& frequencies)
{
  if (model_stack.empty())
    model_stack.push_back( get_smodel_(default_markov_model(*a), a, frequencies));

  return get_MM(model_stack.back(), name, a, frequencies);
}

#include "operations.H"

bool process_stack_Multi(vector<string>& string_stack,  
			 vector<formula_expression_ref >& model_stack,
			  const object_ptr<const alphabet>& a,
			 const shared_ptr< const valarray<double> >& frequencies)
{
  string arg;
  vector<string> args;

  if (match(string_stack,"single",arg)) 
    model_stack.back() = get_MM_default(model_stack,"single",a,frequencies);

  // else if (match(string_stack,"gamma_plus_uniform",arg)) {
  else if (match(string_stack,"gamma",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    formula_expression_ref base = get_RA_default(model_stack,"gamma",a,frequencies);
    formula_expression_ref dist = (Discretize, model_formula(Gamma()), n);
    model_stack.back() = MultiRate(base,  dist);
  }
  else if (match(string_stack,"gamma_inv",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    formula_expression_ref base = get_RA_default(model_stack,"gamma",a,frequencies);
    formula_expression_ref dist = (Discretize, model_formula(Gamma()), n);
    formula_expression_ref p = def_parameter("INV::p", 0.01, between(0,1), beta_dist, Tuple(1.0, 2.0) );
    dist = (ExtendDiscreteDistribution, dist, p, 0.0);
    model_stack.back() = MultiRate(base,  dist);
  }
  else if (match(string_stack,"log-normal",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    formula_expression_ref base = get_RA_default(model_stack,"log-normal",a,frequencies);
    formula_expression_ref dist = (Discretize, model_formula(LogNormal()), n);
    model_stack.back() = MultiRate(base,  dist);
  }
  else if (match(string_stack,"log-normal_inv",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    formula_expression_ref base = get_RA_default(model_stack,"log-normal_inv",a,frequencies);
    formula_expression_ref dist = (Discretize, model_formula(LogNormal()), n);
    formula_expression_ref p = def_parameter("INV::p", 0.01, between(0,1), beta_dist, Tuple(2)(1.0, 2.0) );
    dist = ExtendDiscreteDistribution(dist)(0.0)(p);
    model_stack.back() = MultiRate(base,  dist);
  }
  else if (match(string_stack,"multi_freq",arg)) {
    // Pr(l|m) = Pr(m|l)*Pr(l)/Pr(m)
    // Idea: store Pr(m|l) = probability a letter is in each model
    //       store Pr(l)   = overall frequencies for each letter
    //       store Pr(m)   = probability of each model
    std::abort();
  }
  //  else if (match(string_stack,"INV",arg))
  //  (a) either specify the FREQUENCIES of the model, or
  //  (b) split every model and make a zero-scaled version of it.

  else if (match(string_stack,"DP",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    vector<expression_ref> fs;
    vector<expression_ref> rates;

    formula_expression_ref dist = ListEnd;
    for(int i=0;i<n;i++)
    {
      formula_expression_ref f = def_parameter("DP::f"+convertToString(i+1), Double(1.0/n), between(0,1));
      formula_expression_ref rate = def_parameter("DP::rate"+convertToString(i+1), Double(1.0), between(0,n));
      
      fs.push_back(f.exp());
      rates.push_back(rate.exp());

      // dist = (f,rate):dist
      dist = Tuple(f, rate) & dist;
    }
    dist = DiscreteDistribution(dist);
    dist.add_expression( distributed( get_tuple(fs), Tuple(dirichlet_dist, get_tuple(vector<Double>(n,1.0+n/2.0))) ) );
    dist.add_expression( distributed( get_tuple(rates), Tuple(dirichlet_dist, get_tuple(vector<Double>(n,2.0))) ) );

    formula_expression_ref base = get_RA_default(model_stack,"DP",a,frequencies);
    model_stack.back() = MultiRate(base,  dist);
  }
  else if (match(string_stack,"Modulated",arg))
  {
    formula_expression_ref MM = get_MM_default(model_stack,"Modulated",a,frequencies);

    //    int n = ... n_base_models();
    //    model_stack.back() = Modulated_Markov_E(MM, SimpleExchangeModel(n));
  }
  else if (match(string_stack,"Mixture",args)) 
  {
    vector <formula_expression_ref> models;
    for(int i=0;i<args.size();i++)
      models.push_back( get_MM (get_smodel_(args[i], a, frequencies),"Mixture",a,frequencies ) );

    model_stack.push_back(Mixture_Model(models));
    std::cout<<"Mixture formula = "<<model_stack.back()<<std::endl;
  }
  else if (match(string_stack,"M2",arg)) 
  {
    formula_expression_ref p1 = def_parameter("M2::f[AA INV]", Double(1.0/3), between(0,1));
    formula_expression_ref p2 = def_parameter("M2::f[Neutral]", Double(1.0/3), between(0,1));
    formula_expression_ref p3 = def_parameter("M2::f[Selected]", Double(1.0/3), between(0,1));
    formula_expression_ref m2_omega = def_parameter("M2::omega", Double(1.0), lower_bound(0));
    formula_expression_ref D = DiscreteDistribution(Tuple(p1,0.0)&
						    Tuple(p2,1.0)&
						    Tuple(p3,m2_omega)&
						    ListEnd
						    );
    vector<Double> n(3);
    n[0] = 1;
    n[1] = 98;
    n[2] = 1;
    expression_ref N = get_tuple(n);
    D.add_expression( distributed( Tuple(p1,p2,p3),   Tuple(dirichlet_dist, N) ) );
    D.add_expression( distributed( m2_omega, Tuple(log_exponential_dist, 0.05) ) );

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    assert(C);
    formula_expression_ref S1 = TN_Model(C->getNucleotides());
    formula_expression_ref S2 = M0E(a)(S1)(dummy(0));
    formula_expression_ref R = Plus_gwF_Model(*a);
    formula_expression_ref M0 = lambda_quantify(dummy(0), Reversible_Markov_Model(S2,R) );

    model_stack.push_back( MultiParameter(M0,D) );
  }
  else if (match(string_stack,"M3",arg)) 
  {
    int n=3;
    if (not arg.empty())
      n = convertTo<int>(arg);

    formula_expression_ref D = ListEnd;
    vector<expression_ref> fraction;
    for(int i=n-1;i>=0;i--)
    {
      string pname_f = "M3::f" + convertToString(i+1);
      string pname_w = "M3::omega" + convertToString(i+1);
      formula_expression_ref f = def_parameter("M3::f"     + convertToString(i+1), Double(1.0/n), between(0,1));
      fraction.insert(fraction.begin(), f.exp());
      formula_expression_ref w = def_parameter("M3::omega" + convertToString(i+1), Double(1.0), lower_bound(0));
      // P *= ((1-f)*exponential_pdf(-log(w),0.05)/w + f*exponential_pdf(log(w),0.05)/w);
      w.add_expression( distributed( w, Tuple(log_exponential_dist, 0.05) ) );

      D = Tuple(f,w)&D;
    }
    D = DiscreteDistribution(D);
    D.add_expression(distributed( get_tuple(fraction), Tuple(dirichlet_dist, get_tuple(vector<Double>(n,4.0))) ) );

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    assert(C);
    formula_expression_ref S1 = TN_Model(C->getNucleotides());
    formula_expression_ref S2 = M0E(a)(S1)(dummy(0));
    formula_expression_ref R = Plus_gwF_Model(*a);
    formula_expression_ref M0 = lambda_quantify(dummy(0), Reversible_Markov_Model(S2,R) );

    model_stack.push_back( MultiParameter(M0,D) );
  }
  else if (match(string_stack,"M2a",arg)) 
  {
    formula_expression_ref p1 = def_parameter("M2a::f[AA INV]", Double(1.0/3), between(0,1));
    formula_expression_ref p2 = def_parameter("M2a::f[Neutral]", Double(1.0/3), between(0,1));
    formula_expression_ref p3 = def_parameter("M2a::f[Selected]", Double(1.0/3), between(0,1));
    formula_expression_ref w1 = def_parameter("M2a::omega1", Double(1.0), between(0,1));
    formula_expression_ref w3 = def_parameter("M2a::omega3", Double(1.0), lower_bound(1));
    formula_expression_ref D = DiscreteDistribution(Tuple(p1,w1)&
						    Tuple(p2,1.0)&
						    Tuple(p3,w3)&
						    ListEnd
						    );
    vector<Double> n(3);
    n[0] = 1;
    n[1] = 98;
    n[2] = 1;
    expression_ref N = get_tuple(n);
    D.add_expression( distributed( Tuple(p1,p2,p3),   Tuple(dirichlet_dist, N) ) );
    expression_ref divide = lambda_expression( Divide<Double>() );
    D.add_expression( distributed( divide(1.0)(w1), Tuple(log_exponential_dist, 0.05) ) );
    D.add_expression( distributed( w3, Tuple(log_exponential_dist, 0.05) ) );

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    assert(C);
    formula_expression_ref S1 = TN_Model(C->getNucleotides());
    formula_expression_ref S2 = M0E(a)(S1)(dummy(0));
    formula_expression_ref R = Plus_gwF_Model(*a);
    formula_expression_ref M0 = lambda_quantify(dummy(0), Reversible_Markov_Model(S2,R) );

    model_stack.push_back( MultiParameter(M0,D) );
  }
  else if (match(string_stack,"M8b",arg))
  {
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

    int n=3;
    if (not arg.empty())
      n = convertTo<int>(arg);


    expression_ref Scale;
    {
      // FIXME - There's GOT to be a quicker way to create a function expression than this!
      // How about making a "program"?
      // - It would be a managed list of equality statements.
      expression_ref scale = dummy("scale");

      expression_ref p = dummy(0);
      expression_ref q = dummy(1);
      expression_ref x = dummy(2);
      expression_ref t = dummy(3);

      vector<expression_ref> patterns;
      vector<expression_ref> bodies;
      // scale p [] = []
      patterns.push_back( Tuple(p, ListEnd) );
      bodies.push_back( ListEnd );

      // scale p (q,x):t = (p*q,x):(scale p t)
      patterns.push_back( Tuple(p, Tuple(q,x)&t ) );
      bodies.push_back(  Tuple(lambda_expression( Multiply<Double>() )(p,q),x) & scale(p,t) );

      expression_ref def_scale;// = def_function(patterns, bodies);

      Scale = let_expression(scale, def_scale, scale);
    }
							   
    expression_ref Unwrap;
    {
      expression_ref unwrap = dummy("unwrap");

      expression_ref p = dummy(0);
      expression_ref q = dummy(1);
      expression_ref x = dummy(2);
      expression_ref t = dummy(3);

      vector<expression_ref> patterns;
      vector<expression_ref> bodies;

      // unwrap DiscreteDistribution x = x
      patterns.push_back( Tuple(1)( DiscreteDistribution( dummy(0) ) ) );
      bodies.push_back( dummy(0) );

      expression_ref def_unwrap;// = def_function(patterns, bodies);

      Unwrap = let_expression(unwrap, def_unwrap, unwrap);
    }
							   
    // UnitMixture p x = DiscreteDistribution [(p,x)]
    // [] ++ l2 = l2
    // h:t ++ l2 = h:(t ++ l2)
    // (DiscreteDistribution l1) ++ (DiscreteDistribution l2) = DiscreteDistribution (l1++l2)
    // fmap1 f [] = []
    // fmap1 f (p,x):t = (f p,x):(fmap1 f t)
    // Scale p (DiscreteDistribution l) = DiscreteDistribution (fmap1 \q->q*p l)
    // ExtendDiscreteDistribution p x d = (UnitMixure p x) ++ (Scale (1-p) d)
    // if T y z = y
    // if F y z = z
    // w3 = if I w 1
    // D = (Scale p1 (Discretize Beta n)) ++ (UnitMixture p2 1.0) ++ (UnitMixture p3 w3)
    //
    // *Question*: How much does D simplify with "completely lazy" evaluation?
    // - can we simplify case x of (DiscreteDistribution l) -> case x of (DiscreteDistribution l) -> l
    // - can the rho-calculus help here?

    // How do I add definitions to the available functions, instead of simply constructing them here?
    // Can I load these into a "program", that finally reduces to a single expression?
    // - Can I ALSO remove the unreferenced variables in the program?
    // - Can I ALSO transform the program to make it faster?

    formula_expression_ref p1 = def_parameter("M8b::f[Purifying]", Double(0.6), between(0,1));
    formula_expression_ref p2 = def_parameter("M8b::f[Neutral]", Double(0.3), between(0,1));
    formula_expression_ref p3 = def_parameter("M8b::f[Positive]", Double(0.1), between(0,1));
    formula_expression_ref w = def_parameter("M8b::omega3", Double(1.0), lower_bound(1), log_exponential_dist, 0.05);
    formula_expression_ref I  = def_parameter("M8b::omega3_non_zero", Bool(true));
    formula_expression_ref w3 = (If, I, w, 1.0);
    //    formula_expression_ref w3b = case_expression(

    formula_expression_ref D = Tuple(p3,w3) &
                               Tuple(p2,1.0) &
                               (Scale, p1, (Unwrap,(Discretize,model_formula(Beta()),n)));

    D = DiscreteDistribution(D);

    vector<Double> N(3);
    N[0] = 10;
    N[1] = 10;
    N[2] = 1;

    D.add_expression( distributed( Tuple(p1,p2,p3),   Tuple(dirichlet_dist, get_tuple(N)) ) );
    D.add_expression( distributed( w, Tuple(log_exponential_dist, 0.05) ) );

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    assert(C);
    formula_expression_ref S1 = TN_Model(C->getNucleotides());
    formula_expression_ref S2 = M0E(a)(S1)(dummy(0));
    formula_expression_ref R = Plus_gwF_Model(*a);
    formula_expression_ref M0 = lambda_quantify(dummy(0), Reversible_Markov_Model(S2,R) );

    model_stack.push_back( MultiParameter(M0,D) );
  }
  else if (match(string_stack,"M7",arg))
  {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    formula_expression_ref D = (Discretize, model_formula(Beta()), n);

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    assert(C);
    formula_expression_ref S1 = TN_Model(C->getNucleotides());
    formula_expression_ref S2 = M0E(a)(S1)(dummy(0));
    formula_expression_ref R = Plus_gwF_Model(*a);
    formula_expression_ref M0 = lambda_quantify(dummy(0), Reversible_Markov_Model(S2,R) );

    model_stack.push_back( MultiParameter(M0,D) );
  }
  else
    return false;
  return true;
}

formula_expression_ref 
get_smodel_(const string& smodel,const object_ptr<const alphabet>& a,const shared_ptr<const valarray<double> >& frequencies) 
{
  // Initialize the string stack from the model name
  vector<string> string_stack;
  if (smodel != "") 
  {
    string_stack = split(smodel,'+');
    std::reverse(string_stack.begin(),string_stack.end());
  }
  else
  {
    string model_name = default_markov_model(*a);
    if (not model_name.size())
      throw myexception()<<"You must specify a substitution model - there is no default substitution model for alphabet '"<<a->name<<"'";
    string_stack.push_back(model_name);
    //    if (not process_stack_Markov(string_stack,model_stack,a,frequencies))
    //      throw myexception()<<"Can't guess the base CTMC model for alphabet '"<<a->name<<"'";
  }


  // Initialize the model stack 
  vector<formula_expression_ref > model_stack;

  //-------- Run the model specification -----------//
  while(string_stack.size()) {
    int length = string_stack.size();

    process_stack_Markov(string_stack, model_stack, a, frequencies);

    process_stack_Frequencies(string_stack, model_stack, a, frequencies);

    process_stack_Multi(string_stack, model_stack, a, frequencies);

    if (string_stack.size() == length)
      throw myexception()<<"Couldn't process substitution model level \""<<string_stack.back()<<"\"";
  }

  //---------------------- Stack should be empty now ----------------------//
  if (model_stack.size()>1) {
    throw myexception()<<"Substitution model "<<model_stack.back()<<" was specified but not used!\n";
  }

  return model_stack.back();
}

formula_expression_ref
get_smodel_(const string& smodel,const object_ptr<const alphabet>& a)
{
  return get_smodel_(smodel, a, shared_ptr<const valarray<double> >());
}

formula_expression_ref
get_smodel_(const string& smodel)
{
  return get_smodel_(smodel, object_ptr<const alphabet>(),shared_ptr<const valarray<double> >());
}


/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param smodel_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
formula_expression_ref
get_smodel(const string& smodel_name, const object_ptr<const alphabet>& a, const shared_ptr<const valarray<double> >& frequencies) 
{
  assert(frequencies->size() == a->size());

  //------------------ Get smodel ----------------------//
  formula_expression_ref smodel = get_smodel_(smodel_name,a,frequencies);

  // check if the model actually fits alphabet a...

  // --------- Convert smodel to MultiModel ------------//
  formula_expression_ref full_smodel = get_MM(smodel,"Final",a,frequencies);

  return full_smodel;
}

/// \brief Construct a substitution::MultiModel model for a collection of alignments
///
/// \param smodel_name The name of the substitution model.
/// \param A The alignments.
///
/// This routine constructs the initial frequencies based on all of the alignments.
///
formula_expression_ref get_smodel(const variables_map& args, const string& smodel_name,const vector<alignment>& A) 
{
  for(int i=1;i<A.size();i++)
    if (A[i].get_alphabet() != A[0].get_alphabet())
      throw myexception()<<"alignments in partition don't all have the same alphabet!";

  shared_ptr< const valarray<double> > frequencies (new valarray<double>(empirical_frequencies(args,A)));

  return get_smodel(smodel_name, const_ptr( A[0].get_alphabet() ), frequencies);
}

formula_expression_ref get_smodel(const variables_map& args, const string& smodel_name,const alignment& A) 
{
  shared_ptr< const valarray<double> > frequencies (new valarray<double>(empirical_frequencies(args,A)));

  return get_smodel(smodel_name, const_ptr( A.get_alphabet() ), frequencies);
}

formula_expression_ref get_smodel(const variables_map& args, const alignment& A) 
{
  string smodel_name = args["smodel"].as<string>();

  shared_ptr< const valarray<double> > frequencies (new valarray<double>(empirical_frequencies(args,A)));

  return get_smodel(smodel_name, const_ptr( A.get_alphabet() ), frequencies);
}
