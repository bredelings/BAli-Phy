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
/// \brief Create substitution models from strings of the form model[arg,arg]+...+model[arg,arg].
///

#include <vector>
#include <boost/program_options.hpp>
#include "setup.H"
#include "util.H"
#include "myexception.H"
#include "smodel/operations.H"
#include "computation/operations.H"
#include "probability/distribution-operations.H"
#include "computation/prelude.H"
#include "computation/program.H"
#include "smodel/functions.H"

using std::string;
using std::vector;
using std::valarray;
using namespace substitution;
using boost::program_options::variables_map;
using boost::shared_ptr;
using boost::dynamic_pointer_cast;


// Turn an expression of the form head[arg1, arg2, ..., argn] -> {head, arg1, arg2, ..., argn}.
vector<string> split_args(string s)
{
  vector<string> args;

  // 1. Get the head
  int pos = s.find('[');
  if (pos == -1)
  {
    args = {s};
    return args;
  }
  else
    args  = { s.substr(0,pos) };

  s = s.substr(pos);

  //2. Get the arguments from '[arg1, arg2, ... , argn]'.
  int depth = 0;
  int start = 1;
  for(int i=0;i<s.size();i++)
  {
    // Record finished arg.
    if ((s[i] == ']' or s[i] == ',') and depth == 1)
    {
      assert(i >= start);
      args.push_back(s.substr(start,i-start));
      start = i+1;
    }

    if (s[i] == '[')
      depth++;
    else if (s[i] == ']')
    {
      depth--;
      if (depth < 0) throw myexception()<<"Malformed expression '"<<s<<"': ']' has no matching '['";
    }
  }
  if (depth > 0) throw myexception()<<"Malformed expression '"<<s<<"': missing ']'";

  return args;
}

/// \brief Turn an expression of the form h1[a]+h2[b] -> h2[h1["",a],b].
///
/// \param sstack A stack of strings that represent a substitution model.
/// \param s The model name to match.
/// \param args The possible argument.
///
vector<string> split_top_level(const string& s)
{
  // 1. Find last '+' on the top level
  int split = -1;
  int depth = 0;
  for(int i=0;i<s.size();i++)
    if (s[i] == '[')
      depth++;
    else if (s[i] == '[')
      depth--;
    else if (depth == 0 and s[i] == '+')
      split = i;
  
  // Here if explicitly insert an "" argument if there are no '+' args before us.
  vector<string> args;
  if (split == -1)
  {
    args = split_args(s);
    args.insert(args.begin()+1,"");
  }
  else
  {
    args = split_args(s.substr(split+1,s.size() - split));
    args.insert(args.begin()+1,s.substr(0,split));
  }
  return args;
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
get_smodel_(string smodel,const object_ptr<const alphabet>& a, const shared_ptr<const valarray<double> >&);

formula_expression_ref
get_smodel_(const string& smodel,const object_ptr<const alphabet>& a);

formula_expression_ref
get_smodel_(const string& smodel);

void check_n_args(vector<string>& model_args, int m, int n = -1)
{
  if (n == -1) n = m;

  int n_args = model_args.size() - 1;
  if (model_args.size() < n+1)
    model_args.resize(n+1);

  if (n_args == 1 and model_args[1] == "")
    n_args = 0;

  if (n_args > n)
  {
    if (n == 0)
      throw myexception()<<model_args[0]<<" does not take any arguments.";
    else
      throw myexception()<<model_args[0]<<" only takes "<<n<<" arguments, but you supplied "<<n_args<<".";
  }

  if (n_args < m)
  {
    throw myexception()<<model_args[0]<<" requires "<<m<<" arguments, but you only supplied "<<n_args<<".";
  }
}

/// \brief Construct an AlphabetExchangeModel from string \a smodel.
formula_expression_ref coerce_to_EM(string smodel,
				    const object_ptr<const alphabet>& a, 
				    const shared_ptr< const valarray<double> >& frequencies)

{
  if (smodel == "")
  {
    smodel = default_markov_model(*a);
    if (smodel == "")
      throw myexception()<<"You must specify a substitution model - there is no default substitution model for alphabet '"<<a->name<<"'";
  }

  formula_expression_ref S = get_smodel_(smodel, a, frequencies);

  if (S.exp() and S.result_as<SymmetricMatrixObject>())
    return S;

  throw myexception()<<": '"<<smodel<<"' is not an exchange model.";
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
formula_expression_ref process_stack_Markov(vector<string>& model_args,
					    const object_ptr<const alphabet>& a,
					    const shared_ptr<const valarray<double> >& /* frequencies */)
{
  //------ Get the base markov model (Reversible Markov) ------//
  if (model_args[0] == "F81")
  {
    check_n_args(model_args, 0);

    return EQU_Model(*a);
  }

  else if (model_args[0] == "F81")
  {
    check_n_args(model_args, 0);
    /*
    if (frequencies)
      return F81_Model(*a,*frequencies);
    else
      return F81_Model(*a);
    */
    return formula_expression_ref();
  }

  else if (model_args[0] == "HKY")
  {
    check_n_args(model_args, 0);

    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
    if (not N)
      throw myexception()<<"HKY: '"<<a->name<<"' is not a nucleotide alphabet.";

    return HKY_Model(*a);
  }
  else if (model_args[0] == "TN")
  {
    check_n_args(model_args, 0);

    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
    if (not N)
      throw myexception()<<"TN: '"<<a->name<<"' is not a nucleotide alphabet.";

    return TN_Model(*a);
  }
  else if (model_args[0] == "GTR")
  {
    check_n_args(model_args, 0);

    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
    if (not N)
      throw myexception()<<"GTR: '"<<a->name<<"' is not a nucleotide alphabet.";

    // FIXME - allow/make a general GTR model!

    return GTR_Model(*a);
  }
  /*
  else if (model_args[0] == "EQUx3")) {
    check_n_args(model_args, 0);

    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (T) 
      return Singlet_to_Triplet_Exchange(*T,EQU(T->getNucleotides()));
    else
      throw myexception()<<"EQUx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  */
  else if (model_args[0] == "HKYx3")
  {
    check_n_args(model_args, 0);

    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    formula_expression_ref nuc_S = prefix_formula("nuc",HKY_Model(T->getNucleotides()));
    if (T) 
      return (Singlet_to_Triplet_Exchange, *T,nuc_S);
    else
      throw myexception()<<"HKYx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  else if (model_args[0] == "TNx3")
  {
    check_n_args(model_args, 0);

    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    formula_expression_ref nuc_S = prefix_formula("nuc",TN_Model(T->getNucleotides()));
    if (T) 
      return (Singlet_to_Triplet_Exchange, *T, nuc_S);
    else
      throw myexception()<<"TNx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  else if (model_args[0] == "GTRx3") 
  {
    check_n_args(model_args, 0);

    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    formula_expression_ref nuc_S = prefix_formula("nuc",GTR_Model(T->getNucleotides()));
    if (T) 
      return (Singlet_to_Triplet_Exchange, *T, nuc_S);
    else
      throw myexception()<<"GTRx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  else if (model_args[0] == "PAM") {
    check_n_args(model_args, 0);

    if (*a != AminoAcids())
      throw myexception()<<"PAM: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return (PAM,a);
  }
  else if (model_args[0] == "JTT") {
    check_n_args(model_args, 0);

    if (*a != AminoAcids())
      throw myexception()<<"JTT: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return (JTT,a);
  }
  else if (model_args[0] == "WAG") {
    check_n_args(model_args, 0);

    if (*a != AminoAcids())
      throw myexception()<<"WAG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return (WAG,a);
  }
  else if (model_args[0] == "LG") {
    check_n_args(model_args, 0);

    if (*a != AminoAcids())
      throw myexception()<<"LG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return (LG,a);
  }
  else if (model_args[0] == "Empirical") 
  {
    check_n_args(model_args,0,2);
    return (Empirical,a,model_args[2]);
  }
  /*
  else if (model_args[0] == "C10")
  {
    if (*a != AminoAcids())
      throw myexception()<<"C20: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return C10_CAT_FixedFrequencyModel();
  }
  else if (model_args[0] == "C20")
  {
    if (*a != AminoAcids())
      throw myexception()<<"C20: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return C20_CAT_FixedFrequencyModel();
  }
  else if (model_args[0] == "CAT-Fix") {
    if (*a != AminoAcids())
      throw myexception()<<"CAT-Fix: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    CAT_FixedFrequencyModel M(*a);
    M.load_file(arg);
    return M;
  }
  */
  else if (model_args[0] == "M0") //M0[0,S]
  {
    check_n_args(model_args,0,2);

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<"M0: '"<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    formula_expression_ref S1 = HKY_Model( N );
    if (model_args[2] != "")
    {
      S1 = coerce_to_EM(model_args[2], const_ptr(N), {});
      if (not S1.result_as<SymmetricMatrixObject>())
	throw myexception()<<"Submodel '"<<model_args[2]<<"' for M0 is not a (nucleotide) exchange model.";
    }
    formula_expression_ref w = def_parameter("M0.omega", Double(1), lower_bound(0), log_laplace_dist, Tuple(0.0,0.1));

    return (M0E, a, S1, w);
  }

  return formula_expression_ref();
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
formula_expression_ref process_stack_Frequencies(vector<string>& model_args,
						 const object_ptr<const alphabet>& a,
						 const shared_ptr<const valarray<double> >& frequencies)
{
  formula_expression_ref R;

  if (model_args[0] == "F=constant") 
  {
    if (frequencies)
      R = (Plus_gwF, *a, 1.0, get_tuple(*frequencies) );
    else
      R = (Plus_gwF, *a, 1.0, get_tuple(*frequencies) );
  }

  else if (model_args[0] == "F") 
  {
    if (frequencies)
      R = Plus_F_Model(*a,*frequencies);
    else
      R = Plus_F_Model(*a);
  }
  else if (model_args[0] == "F=uniform") 
  {
    vector<double> piv(a->size(),1.0/a->size() );
    expression_ref pi = get_list(piv);

    R = let_expression(v1,(var("listToVectorDouble"),pi),
		       (ReversibleFrequency, *a, (Iota<unsigned>(), a->size()), v1, (Plus_gwF, *a, 1.0, v1))
		       );
  }
  else if (model_args[0] == "F1x4")
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (not T)
      throw myexception()<<"+F1x4: '"<<a->name<<"' is not a triplet alphabet.";

    R = F1x4_Model(*T);
  }
  else if (model_args[0] == "F3x4") 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (not T)
      throw myexception()<<"+F3x4: '"<<a->name<<"' is not a triplet alphabet.";

    R = F3x4_Model(*T);
  }
  else if (model_args[0] == "MG94") 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (not T)
      throw myexception()<<"+MG94: '"<<a->name<<"' is not a triplet alphabet.";

    R = MG94_Model(*T);
  }
  else if (model_args[0] == "MG94w9") 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (not T)
      throw myexception()<<"+MG94w9: '"<<a->name<<"' is not a triplet alphabet.";

    R = MG94w9_Model(*T);
  }
  /*
  else if (model_args[0] == "F=amino-acids") 
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<"+F=amino-acids: '"<<a->name<<"' is not a codon alphabet.";

    return AACodonFrequencyModel(*C);
  }
  else if (model_args[0] == "F=triplets") 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (not T)
      throw myexception()<<"+F=triplets: '"<<a->name<<"' is not a triplet alphabet.";

    return TripletsFrequencyModel(*T);
  }
  else if (model_args[0] == "F=codons") 
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<"+F=codons: '"<<a->name<<"' is not a codon alphabet.";

    return CodonsFrequencyModel(*C);
  }
  else if (model_args[0] == "F=codons2") 
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<"+F=codons2: '"<<a->name<<"' is not a codon alphabet.";

    return CodonsFrequencyModel2(*C);
  }
  */

  if (R.exp() and model_args[1] != "")
  {
    formula_expression_ref EM = coerce_to_EM(model_args[1], a, frequencies);

    R = Reversible_Markov_Model(EM,R);
  }

  return R;
}

formula_expression_ref coerce_to_frequency_model(const formula_expression_ref& M,
						 const object_ptr<const alphabet>& /* a */,
						 const shared_ptr< const valarray<double> >& /* frequencies */)
{
  if (is_exactly(M.result(SModel_Functions(),Distribution_Functions()), "ReversibleFrequency"))
    return M;

  throw myexception()<<": '"<<M.exp()<<"' is not an exchange model.";
}

formula_expression_ref coerce_to_frequency_model(string smodel,
						 const object_ptr<const alphabet>& a,
						 const shared_ptr< const valarray<double> >& frequencies)
{
  formula_expression_ref M = get_smodel_(smodel, a, frequencies);

  return coerce_to_frequency_model(M, a,  frequencies);
}


/// \brief Construct a ReversibleMarkovModel from model \a M
formula_expression_ref coerce_to_RA(const formula_expression_ref& M,
				    const object_ptr<const alphabet>& a,
				    const shared_ptr< const valarray<double> >& frequencies)
{
  object_ref result = M.result(SModel_Functions(), Distribution_Functions());

  if (is_exactly(result, "F81"))
    return M;

  if (is_exactly(result, "ReversibleMarkov"))
    return M;

  try 
  {
    if (is_exactly(result, "ReversibleFrequency"))
    {
      formula_expression_ref S = coerce_to_EM("",a,frequencies);
      
      return Reversible_Markov_Model(S,M);
    }

    if (boost::dynamic_pointer_cast<const SymmetricMatrixObject>(result))
    {
      // If the frequencies.size() != alphabet.size(), this call will throw a meaningful exception.
      if (frequencies)
	return Reversible_Markov_Model(M, Plus_gwF_Model(*a, *frequencies));
      else
	return Reversible_Markov_Model(M, Plus_gwF_Model(*a));
    }
    throw myexception()<<": Can't construct a SimpleReversibleMarkovModel from '"<<M.exp()<<"\n";
  }
  catch (std::exception& e) { 
    throw myexception()<<": Can't construct a SimpleReversibleMarkovModel from '"<<M.exp()<<"':\n "<<e.what();
  }
}

/// \brief Construct a ReversibleMarkovModel from model \a M
formula_expression_ref coerce_to_RA(string smodel,
				    const object_ptr<const alphabet>& a,
				    const shared_ptr< const valarray<double> >& frequencies)
{
  formula_expression_ref M;
  if (smodel == "")
    M = coerce_to_EM("", a, frequencies);
  else
    M = get_smodel_(smodel, a, frequencies);

  return coerce_to_RA(M, a, frequencies);
}

/// \brief Construct a MultiModel from model \a M
formula_expression_ref coerce_to_MM(const formula_expression_ref& M,
				    const object_ptr<const alphabet>& a, 
				    const shared_ptr< const valarray<double> >& frequencies)
{
  if (M.exp() and is_exactly(M.result(SModel_Functions(),Distribution_Functions()), "MixtureModel"))
    return M;

  try { 
    return Unit_Model( coerce_to_RA(M,a, frequencies) ) ; 
  }
  catch (std::exception& e) { 
    throw myexception()<<": Can't construct a MixtureModel from '"<<M.exp()<<"':\n"<<e.what();
  }
}

/// \brief Construct a MultiModel from model \a M
formula_expression_ref coerce_to_MM(string smodel,
				    const object_ptr<const alphabet>& a, 
				    const shared_ptr< const valarray<double> >& frequencies)
{
  formula_expression_ref M;
  if (smodel == "")
    M = coerce_to_RA("", a, frequencies);
  else
    M = get_smodel_(smodel, a, frequencies);

  return coerce_to_MM(M,a,frequencies);
}

/// \brief Construct a MultiModel from model \a M
formula_expression_ref coerce_to_MMM(const formula_expression_ref& M,
				     const object_ptr<const alphabet>& a,
				     const shared_ptr< const valarray<double> >& frequencies)
{
  if (is_exactly(M.result(SModel_Functions(),Distribution_Functions()), "MixtureModels"))
    return M;

  try { 
    return (MixtureModels, (coerce_to_MM(M,a,frequencies)&ListEnd));
  }
  catch (std::exception& e) { 
    throw myexception()<<": Can't construct a MixtureModels from '"<<M.exp()<<"':\n"<<e.what();
  }
}

/// \brief Construct a MultiModel from model \a M
formula_expression_ref coerce_to_MMM(string smodel,
				     const object_ptr<const alphabet>& a,
				     const shared_ptr< const valarray<double> >& frequencies)
{
  formula_expression_ref M;

  if (smodel == "")
    M = coerce_to_MM("", a, frequencies);
  else
    M = get_smodel_(smodel, a, frequencies);
  
  return coerce_to_MMM(M, a, frequencies);
}

formula_expression_ref get_M0_omega_function(const object_ptr<const alphabet>& a,
					     const shared_ptr< const valarray<double> >& frequencies,
					     vector<string> model_args,
					     int where)
{
  const Codons* C = dynamic_cast<const Codons*>(&*a);
  if (not C)
    throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
  const Nucleotides& N = C->getNucleotides();

  if (model_args.size() < where+2)
    model_args.resize(where+2);

  formula_expression_ref S1 = HKY_Model( N );
  if (model_args[where] != "")
  {
    S1 = coerce_to_EM(model_args[where], const_ptr(N), {});
    if (not S1.result_as<SymmetricMatrixObject>())
      throw myexception()<<"Submodel '"<<model_args[where]<<"' for M0 is not a (nucleotide) exchange model.";
  }
  formula_expression_ref S2 = (M0E, a, S1, dummy(0));

  formula_expression_ref R = Plus_F_Model(*a);
  if (model_args[where+1] != "")
    R = coerce_to_frequency_model(model_args[where+1], a, frequencies);
  
  formula_expression_ref M0 = lambda_quantify(dummy(0), Reversible_Markov_Model(S2,R) );

  return M0;
}


#include "operations.H"

formula_expression_ref process_stack_Multi(vector<string>& model_args,
					   const object_ptr<const alphabet>& a,
					   const shared_ptr< const valarray<double> >& frequencies)
{
  expression_ref plus = lambda_expression(Add());
  expression_ref minus = lambda_expression(Minus());
  expression_ref times = lambda_expression(Multiply());
  expression_ref divide = lambda_expression(Divide());

  if (model_args[0] == "single") 
    return coerce_to_MM(coerce_to_RA(model_args[1],a,frequencies),a,frequencies);

  // else if (model_args[0] == "gamma_plus_uniform") {
  else if (model_args[0] == "gamma") 
  {
    check_n_args(model_args, 1, 2);

    int n=4;
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);

    formula_expression_ref base = coerce_to_RA(model_args[1],a,frequencies);

    formula_expression_ref W = def_parameter("Gamma.sigmaOverMu", 0.1, lower_bound(0), log_laplace_dist, Tuple(-3.0, 1.0) );
    formula_expression_ref b = (times, W, W);
    formula_expression_ref a = (divide, 1.0, b);
    formula_expression_ref dist = (UniformDiscretize, (var("gammaQuantile"), Tuple(a,b)) , n);

    return (MultiRate, base,  dist);
  }
  else if (model_args[0] == "gamma_inv") 
  {
    check_n_args(model_args, 1, 2);

    int n=4;
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);

    formula_expression_ref base = coerce_to_RA(model_args[1],a,frequencies);

    formula_expression_ref W = def_parameter("Gamma.sigmaOverMu", 0.1, lower_bound(0), log_laplace_dist, Tuple(-3.0, 1.0) );
    formula_expression_ref b = (times, W, W);
    formula_expression_ref a = (divide, 1.0, b);
    formula_expression_ref dist = (UniformDiscretize, (var("gammaQuantile"), Tuple(a,b)) , n);

    formula_expression_ref p = def_parameter("INV.p", 0.01, between(0,1), beta_dist, Tuple(1.0, 2.0) );
    dist = (ExtendDiscreteDistribution, dist, p, 0.0);

    return (MultiRate, base,  dist);
  }
  else if (model_args[0] == "log-normal") {
    int n=4;
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);

    formula_expression_ref base = coerce_to_RA(model_args[1],a,frequencies);

    formula_expression_ref W = def_parameter("LogNormal.sigmaOverMu", 0.1, lower_bound(0), log_laplace_dist, Tuple(-3.0, 1.0) );
    formula_expression_ref Var = (times, W, W);
    formula_expression_ref lVar = (Log, (plus, 1.0, Var ) );
    formula_expression_ref lmu = (times, -0.5, lVar);
    formula_expression_ref lsigma = (Sqrt, lVar);
    formula_expression_ref dist = (UniformDiscretize, (var("logNormalQuantile"), Tuple(lmu,lsigma)) , n);

    return (MultiRate, base,  dist);
  }
  else if (model_args[0] == "log-normal_inv") {
    int n=4;
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);

    formula_expression_ref base = coerce_to_RA(model_args[1],a,frequencies);

    formula_expression_ref W = def_parameter("LogNormal.sigmaOverMu", 0.1, lower_bound(0), log_laplace_dist, Tuple(-3.0, 1.0) );
    formula_expression_ref Var = (times, W, W);
    formula_expression_ref lVar = (Log, (plus, 1.0, Var ) );
    formula_expression_ref lmu = (times, -0.5, lVar);
    formula_expression_ref lsigma = (Sqrt, lVar);
    formula_expression_ref dist = (UniformDiscretize, (var("logNormalQuantile"), Tuple(lmu,lsigma)) , n);

    formula_expression_ref p = def_parameter("INV.p", 0.01, between(0,1), beta_dist, Tuple(1.0, 2.0) );
    dist = (ExtendDiscreteDistribution, dist, p, 0.0);

    return (MultiRate, base,  dist);
  }
  else if (model_args[0] == "multi_freq") {
    // Pr(l|m) = Pr(m|l)*Pr(l)/Pr(m)
    // Idea: store Pr(m|l) = probability a letter is in each model
    //       store Pr(l)   = overall frequencies for each letter
    //       store Pr(m)   = probability of each model
    std::abort();
  }
  //  else if (model_args[0] == "INV")
  //  (a) either specify the FREQUENCIES of the model, or
  //  (b) split every model and make a zero-scaled version of it.

  else if (model_args[0] == "DP") {
    int n=4;
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);

    vector<expression_ref> fs;
    vector<expression_ref> rates;

    formula_expression_ref dist = ListEnd;
    for(int i=0;i<n;i++)
    {
      formula_expression_ref f = def_parameter("DP.f"+convertToString(i+1), Double(1.0/n), between(0,1));
      formula_expression_ref rate = def_parameter("DP.rate"+convertToString(i+1), Double(1.0), between(0,n));
      
      fs.push_back(f.exp());
      rates.push_back(rate.exp());

      // dist = (f,rate):dist
      dist = Tuple(f, rate) & dist;
    }
    dist = (DiscreteDistribution, dist);
    dist.add_expression( (distributed, get_list(fs), Tuple(dirichlet_dist, get_list(vector<Double>(n,1.0+n/2.0))) ) );
    dist.add_expression( (distributed, get_list(rates), Tuple(dirichlet_dist, get_list(vector<Double>(n,2.0))) ) );

    formula_expression_ref base = coerce_to_RA(model_args[1],a,frequencies);
    return (MultiRate, base,  dist);
  }
  else if (model_args[0] == "Modulated")
  {
    formula_expression_ref MM = coerce_to_MM(model_args[1],a,frequencies);

    //    int n = ... n_base_models();
    //    return Modulated_Markov_E(MM, SimpleExchangeModel(n));
  }
  else if (model_args[0] == "Mixture")
  {
    if (model_args.size() < 4)
      throw myexception()<<"Mixture[..] requires at least two sub-models!";
    if (model_args[1] != "")
      throw myexception()<<"Mixture[..] does not take a submodel (i.e. with '+')";

    vector <formula_expression_ref> models;
    for(int i=0;i<model_args.size()-2;i++)
      models.push_back( coerce_to_MM(model_args[i+2], a, frequencies) );

    return Mixture_Model(models);
  }
  else if (model_args[0] == "M2") 
  {
    formula_expression_ref p1 = def_parameter("M2.fAaINV", Double(1.0/3), between(0,1));
    formula_expression_ref p2 = def_parameter("M2.fNeutral", Double(1.0/3), between(0,1));
    formula_expression_ref p3 = def_parameter("M2.fSelected", Double(1.0/3), between(0,1));
    formula_expression_ref m2_omega = def_parameter("M2.omega", Double(1.0), lower_bound(0));
    formula_expression_ref D = (DiscreteDistribution, Tuple(p1,0.0)&
						    Tuple(p2,1.0)&
						    Tuple(p3,m2_omega)&
						    ListEnd
						    );

    D.add_expression( (distributed, p1&(p2&(p3&ListEnd)),   Tuple(dirichlet_dist, List(1.0, 98.0, 1.0)) ) );
    D.add_expression( (distributed, m2_omega, Tuple(log_exponential_dist, 0.05) ) );

    formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_args,2);

    return (MultiParameter,M0,D);
  }
  else if (model_args[0] == "M3u") // M3u[0,n,S,F]
  {
    int n=3;
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);

    formula_expression_ref D = ListEnd;
    formula_expression_ref F = ListEnd;
    for(int i=n-1;i>=0;i--)
    {
      string pname_f = "M3.f" + convertToString(i+1);
      string pname_w = "M3.omega" + convertToString(i+1);
      formula_expression_ref f = def_parameter("M3.f"     + convertToString(i+1), Double(1.0/n), between(0,1));
      formula_expression_ref w = def_parameter("M3.omega" + convertToString(i+1), Double(1.0), lower_bound(0), uniform_dist, Tuple(0.0, 1.0));

      D = Tuple(f,w)&D;
      F = f&F;
    }
    D = (DiscreteDistribution, D);
    D.add_expression((distributed, F, Tuple(dirichlet_dist, get_list(vector<Double>(n,4.0))) ) );

    formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_args,3);

    return (MultiParameter, M0, D);
  }
  else if (model_args[0] == "M3") // M3[0,n,S,F]
  {
    throw myexception()<<"The M3 model is not working right now.  Try M3u, with only stabilizing selection.";

    int n=3;
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);

    formula_expression_ref D = ListEnd;
    vector<expression_ref> fraction;
    for(int i=n-1;i>=0;i--)
    {
      string pname_f = "M3.f" + convertToString(i+1);
      string pname_w = "M3.omega" + convertToString(i+1);
      formula_expression_ref f = def_parameter("M3.f"     + convertToString(i+1), Double(1.0/n), between(0,1));
      fraction.insert(fraction.begin(), f.exp());
      formula_expression_ref w = def_parameter("M3.omega" + convertToString(i+1), Double(1.0), lower_bound(0));
      // P *= ((1-f)*exponential_pdf(-log(w),0.05)/w + f*exponential_pdf(log(w),0.05)/w);
      w.add_expression( (distributed, w, Tuple(log_exponential_dist, 0.05) ) );

      D = Tuple(f,w)&D;
    }
    D = (DiscreteDistribution, D);
    D.add_expression((distributed, get_list(fraction), Tuple(dirichlet_dist, get_list(vector<Double>(n,4.0))) ) );

    formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_args,3);

    return (MultiParameter, M0, D);
  }
  else if (model_args[0] == "M2a") // M2a[0,S,F]
  {
    formula_expression_ref p1 = def_parameter("M2a.fAaINV", Double(1.0/3), between(0,1));
    formula_expression_ref p2 = def_parameter("M2a.fNeutral", Double(1.0/3), between(0,1));
    formula_expression_ref p3 = def_parameter("M2a.fSelected", Double(1.0/3), between(0,1));
    formula_expression_ref w1 = def_parameter("M2a.omega1", Double(1.0), between(0,1));
    formula_expression_ref w3 = def_parameter("M2a.omega3", Double(1.0), lower_bound(1));
    formula_expression_ref D = (DiscreteDistribution,Tuple(p1,w1)&Tuple(p2,1.0)&Tuple(p3,w3)&ListEnd);

    D.add_expression( (distributed, p1&(p2&(p3&ListEnd)),   Tuple(dirichlet_dist, List(1.0, 98.0, 1.0)) ) );
    D.add_expression( (distributed, (divide, 1.0, w1), Tuple(log_exponential_dist, 0.05) ) );
    D.add_expression( (distributed, w3, Tuple(log_exponential_dist, 0.05) ) );

    formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_args,2);

    return (MultiParameter,M0,D);
  }
  else if (model_args[0] == "M8b") // M8b[0,n,S,+F]
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
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);

    // Determine the a and b parameters of the beta distribution
    formula_expression_ref mu = def_parameter("Beta.mu", Double(0.5), between(0,1), beta_dist, Tuple(10.0, 1.0));
    formula_expression_ref gamma = def_parameter("Beta.varOverMu", Double(0.1), between(0,1), exponential_dist, 0.1);
    formula_expression_ref N = (minus, (divide, 1.0, gamma), 1.0); // N = 1.0/gamma - 1.0;
    formula_expression_ref alpha = (times, N, mu); // a = N * mu;
    formula_expression_ref beta = (times, N, (minus, 1.0, mu)); // b = N * (1.0 - mu)
    // Create the discrete distribution for omega
    formula_expression_ref D = (UniformDiscretize, (var("betaQuantile"), Tuple(alpha,beta)), n);

    // *Question*: How much does D simplify with "completely lazy" evaluation?
    formula_expression_ref p1 = def_parameter("M8b.fPurifying", Double(0.6), between(0,1));
    formula_expression_ref p2 = def_parameter("M8b.fNeutral", Double(0.3), between(0,1));
    formula_expression_ref p3 = def_parameter("M8b.fPositive", Double(0.1), between(0,1));
    // [positive selection, if it exists] w ~ log_exponential(0.05)
    formula_expression_ref w = def_parameter("M8b.omega3", Double(1.0), lower_bound(1), log_exponential_dist, 0.05);
    formula_expression_ref I  = def_parameter("M8b.omega3NonZero", Bool(true), nullptr, bernoulli_dist, 0.5);
    formula_expression_ref w3 = (If, I, w, 1.0);

    // Add the neutral and (possibility) positive selection categories
    // mu    = E(X)
    // gamma = Var(X)/[ mu * (1-mu)] = 1/(1 + a + b)  \in (0,1]
    //
    // N = a + b = 1/gamma - 1

    D = (DiscreteDistribution,Tuple(p3,w3) & (Tuple(p2,1.0) & (fmap1, (v4^(times,v4,p1)), (UnwrapDD, D))));
    // (p1,p2,p3) ~ Dirichlet(10, 10, 1)
    D.add_expression( (distributed, p1&(p2&(p3&ListEnd)),   Tuple(dirichlet_dist, List(10.0, 10.0, 1.0)) ) );

    formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_args,3);

    return (MultiParameter, M0, D);
  }
  else if (model_args[0] == "M7")
  {
    int n=4;
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);

    // Determine the a and b parameters of the beta distribution
    formula_expression_ref mu = def_parameter("Beta.mu", Double(0.5), between(0,1), beta_dist, Tuple(10.0, 1.0));
    formula_expression_ref gamma = def_parameter("Beta.varOverMu", Double(0.1), between(0,1), exponential_dist, 0.1);
    formula_expression_ref N = (minus, (divide, 1.0, gamma), 1.0); // N = 1.0/gamma - 1.0;
    formula_expression_ref alpha = (times, N, mu); // a = N * mu;
    formula_expression_ref beta = (times, N, (minus, 1.0, mu)); // b = N * (1.0 - mu)
    // Create the discrete distribution for omega
    formula_expression_ref D = (UniformDiscretize, (var("betaQuantile"), Tuple(alpha,beta)), n);

    formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_args,3);

    return (MultiParameter, M0, D);
  }
  else if (model_args[0] == "branch-site-test1") // branch-site-test1[0,S,F]
  {
    check_n_args(model_args, 0, 3);

    formula_expression_ref f0 = def_parameter("BranchSite.f0",Double(0.5));
    formula_expression_ref f1 = def_parameter("BranchSite.f1",Double(0.5));
    formula_expression_ref f2 = def_parameter("BranchSite.posP",Double(0.1),between(0,1),beta_dist,Tuple(1.0,10.0));
    formula_expression_ref I  = def_parameter("BranchSite.posSelection", Bool(true), nullptr, bernoulli_dist, 0.5);

    formula_expression_ref p2 = (If, I, f2, 0.0);
    formula_expression_ref p0 = (times, f0, (minus,1.0,p2));
    formula_expression_ref p1 = (times, f1, (minus,1.0,p2));
    formula_expression_ref p2a = (times, f0, p2);
    formula_expression_ref p2b = (times, f1, p2);

    formula_expression_ref w0 = def_parameter("BranchSite.w0", Double(0.5), between(0,1), uniform_dist, Tuple(0.0, 1.0));
    // Mean of log(w2) should be 1.0, sigma/mu for log(w2) should be 0.7071
    formula_expression_ref w2 = def_parameter("branch-site.pos-w", Double(1.5), lower_bound(1), log_gamma_dist, Tuple(4.0, 0.25));
    // FIXME - look at the effect on power of using various different priors for w2 here!
    // FIXME - allow specifying the prior on the command line?

    formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_args,2);

    formula_expression_ref mixture1 = (DiscreteDistribution,Tuple(p0,w0)&(Tuple(p1,1.0)&(Tuple(p2a,w0)&(Tuple(p2b,1.0)&ListEnd))));
    formula_expression_ref mixture2 = (DiscreteDistribution,Tuple(p0,w0)&(Tuple(p1,1.0)&(Tuple(p2a,w2)&(Tuple(p2b,w2)&ListEnd))));
    mixture1 = (MultiParameter, M0, mixture1);
    mixture2 = (MultiParameter, M0, mixture2);
    formula_expression_ref branch_site = (MixtureModels,mixture1&(mixture2&ListEnd));

    branch_site.add_expression( (distributed, f0&(f1&ListEnd), Tuple(dirichlet_dist, List(1.0, 1.0)) ) );

    return branch_site;
  }
  else if (model_args[0] == "branch-site")  // branch-site-test[0,n,S,F]
  {
    check_n_args(model_args, 0, 4);

    // Determine how many categories there are, minus the positive category.
    int n=2;
    if (model_args.size() > 2 and model_args[2] != "")
      n = convertTo<int>(model_args[2]);
    if (n < 2) throw myexception()<<"The branch-site model needs at least two categories.";

    // Create the parameter for degree of positive selection, if it exists.
    formula_expression_ref w_pos = def_parameter("BranchSite.posW", Double(1.5), lower_bound(1), log_gamma_dist, Tuple(4.0, 0.25));
    formula_expression_ref I  = def_parameter("BranchSite.posSelection", Bool(true), nullptr, bernoulli_dist, 0.5);
    formula_expression_ref w_pos_effective = (If, I, w_pos, 1.0);

    // Create the partial distributions, and their parameters.
    // FIXME - should I try to make the prior mean for the neutral w always near 0.5?
    formula_expression_ref D1 = ListEnd;
    formula_expression_ref D2 = ListEnd;
    formula_expression_ref F = ListEnd;
    for(int i=n-1;i>=0;i--)
    {
      string n_ = convertToString(i);
      formula_expression_ref f = def_parameter("BranchSite.f"+n_, Double(1.0/n));
      formula_expression_ref w = def_parameter("BranchSite.w"+n_, Double(0.5), between(0,1), uniform_dist, Tuple(0.0, 1.0));
      if (i == n-1) w = expression_ref(1.0);

      D1 = Tuple(f,w)&D1;
      D2 = Tuple(f,w_pos_effective)&D2;
      F = f&F;
    }
    D1 = (DiscreteDistribution, D1);
    D2 = (DiscreteDistribution, D2);

    formula_expression_ref p_pos = def_parameter("BranchSite.posP",Double(0.1),between(0,1),beta_dist,Tuple(1.0,10.0));

    // Mean of log(w2) should be 1.0, sigma/mu for log(w2) should be 0.7071
    // FIXME - look at the effect on power of using various different priors for w2 here!
    // FIXME - allow specifying the prior on the command line?

    formula_expression_ref M0 = get_M0_omega_function(a, frequencies, model_args, 3);
    formula_expression_ref mixture1 = (MultiParameter, M0, (MixDiscreteDistributions, List(p_pos, (minus, 1.0, p_pos)), List(D1,D1) ) );
    formula_expression_ref mixture2 = (MultiParameter, M0, (MixDiscreteDistributions, List(p_pos, (minus, 1.0, p_pos)), List(D2,D1) ) );

    formula_expression_ref branch_site = (MixtureModels, mixture1&(mixture2&ListEnd) );
    branch_site.add_expression( (distributed, F, Tuple(dirichlet_dist,get_list(vector<Double>(n,1.0) ) ) ) );

    return branch_site;
  }

  return formula_expression_ref();
}

formula_expression_ref 
get_smodel_(string smodel,const object_ptr<const alphabet>& a,const shared_ptr<const valarray<double> >& frequencies) 
{
  if (smodel == "")
    throw myexception()<<"You must specify a substitution model!";

  vector<string> model_args = split_top_level(smodel);

  formula_expression_ref m;

  m = process_stack_Markov(model_args, a, frequencies);
  if (m.exp()) return m;

  m = process_stack_Frequencies(model_args, a, frequencies);
  if (m.exp()) return m;

  m = process_stack_Multi(model_args, a, frequencies);
  if (m.exp()) return m;

  throw myexception()<<"Couldn't process substitution model description \""<<smodel<<"\"";
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


//FIXME change to return a (model, standardized name) pair.


/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param smodel_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
formula_expression_ref
get_smodel(const string& smodel, const object_ptr<const alphabet>& a, const shared_ptr<const valarray<double> >& frequencies) 
{
  assert(frequencies->size() == a->size());

  // --------- Convert smodel to MultiMixtureModel ------------//
  formula_expression_ref full_smodel = coerce_to_MMM(smodel,a,frequencies);

  std::cerr<<"smodel = "<<full_smodel.exp()<<"\n";

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
