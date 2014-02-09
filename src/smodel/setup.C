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
#include "smodel/setup.H"
#include "util.H"
#include "myexception.H"
#include "smodel/objects.H"
#include "computation/module.H"
#include "computation/loader.H"

using std::string;
using std::vector;
using std::valarray;
using namespace substitution;
using boost::program_options::variables_map;
using boost::shared_ptr;
using boost::dynamic_pointer_cast;

const vector<vector<string>> default_arguments = 
  {
    {"F81"},
    {"HKY"},
    {"TN"},
    {"GTR"},
    {"HKYx3"},
    {"TNx3"},
    {"GTRx3"},
    {"PAM"},
    {"JTT"},
    {"WAG"},
    {"LG"},
    {"Empirical",""},
    {"M0","HKY"},
    {"gamma","","4"},
    {"gamma_inv","","4"},
    {"log-normal","","4"},
    {"log-normal_inv","","4"},
    {"M1a","HKY","F61"},
    {"M2a","HKY","F61"},
    {"M2a_Test","HKY","F61"},
    {"M3u","3","HKY","F61"},
    {"M3","3","HKY","F61"},
    {"M7","4","HKY","F61"},
    {"M8b_Test","4","HKY","F61"},
    {"branch-site","2","HKY","F61"}
  };

string show(vector<string> args)
{
  string output = args[0];
  args.erase(args.begin());
  if (args.empty()) return output;

  output += '[';
  output += join(args,',');
  output += ']';
  return output;
}

void handle_default_args(vector<string>& args, const vector<vector<string>>& D)
{
  for(const auto& d: D)
  {
    if (args[0] != d[0]) continue;

    if (args.size() > d.size())
      throw myexception()<<"Too many arguments: "<<show(args);

    args.resize(d.size());

    for(int i=1;i<d.size();i++)
    {
      if (not args[i].empty()) continue;

      if (not d[i].empty())
	args[i] = d[i];
      else
      {
	args[i] = "*";
	throw myexception()<<"Missing argument at *: "<<show(args);
      }
    }
  }
}

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

  args = { s.substr(0,pos) };
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

/// \brief Turn an expression of the form h1[a]+h2[b] -> h2[h1[a],b].
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
    else if (s[i] == ']')
    {
      depth--;
      if (depth < 0) throw myexception()<<"Too many ']' in string '"<<s<<"'";
    }
    else if (depth == 0 and s[i] == '+')
      split = i;
  if (depth != 0)
    throw myexception()<<"Too many '[' in string '"<<s<<"'";
  
  // If there are no plus expressions then we can take the arguments as is.
  string head = s.substr(split+1,s.size() - split);

  vector<string> args = split_args(head);

  if (split != -1)
  {
    string arg0 = s.substr(0,split);
    args.insert(args.begin()+1, arg0);
  }

  handle_default_args(args,default_arguments);

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

expression_ref
get_smodel_(const module_loader& L,string smodel,const object_ptr<const alphabet>& a, const shared_ptr<const valarray<double> >&);

expression_ref
get_smodel_(const module_loader& L,const string& smodel,const object_ptr<const alphabet>& a);

expression_ref
get_smodel_(const module_loader& L,const string& smodel);

/// \brief Construct an AlphabetExchangeModel from string \a smodel.
expression_ref coerce_to_EM(const module_loader& L,
				    string smodel,
				    const object_ptr<const alphabet>& a, 
				    const shared_ptr< const valarray<double> >& frequencies)

{
  if (smodel == "")
    throw myexception()<<"Can't construct substitution model from empty description!";

  expression_ref S = get_smodel_(L,smodel, a, frequencies);

  if (S and dynamic_pointer_cast<const SymmetricMatrixObject>(result(S,L,vector<string>{"SModel","Distributions","Range"})))
    return S;

  throw myexception()<<": '"<<smodel<<"' is not an exchange model.";
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
expression_ref process_stack_Markov(const module_loader& L,
					    vector<string>& model_args,
				    const object_ptr<const alphabet>& a)
{
  //------ Get the base markov model (Reversible Markov) ------//
  /*
  if (model_args[0] == "EQU")
  {
    return EQU_Model(*a);
  }
  else if (model_args[0] == "F81")
  {
    if (frequencies)
      return F81_Model(*a,*frequencies);
    else
      return F81_Model(*a);
  }
  */

  if (model_args[0] == "HKY")
  {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
    if (not N)
      throw myexception()<<"HKY: '"<<a->name<<"' is not a nucleotide alphabet.";

    return model_expression({identifier("hky_model"),*a});
  }
  else if (model_args[0] == "TN")
  {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
    if (not N)
      throw myexception()<<"TN: '"<<a->name<<"' is not a nucleotide alphabet.";

    return model_expression({identifier("tn_model"),*a});
  }
  else if (model_args[0] == "GTR")
  {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
    if (not N)
      throw myexception()<<"GTR: '"<<a->name<<"' is not a nucleotide alphabet.";

    // FIXME - allow/make a general GTR model!

    return model_expression({identifier("gtr_model"),*a});
  }
  /*
  else if (model_args[0] == "EQUx3")) {

    const Triplets* T = dynamic_cast<const Triplets*>(&*a);
    if (T) 
      return Singlet_to_Triplet_Exchange(*T,EQU(T->getNucleotides()));
    else
      throw myexception()<<"EQUx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  */
  else if (model_args[0] == "HKYx3")
  {
    if (dynamic_cast<const Triplets*>(&*a))
      return model_expression({identifier("hkyx3_model"),*a});
    else
      throw myexception()<<"HKYx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  else if (model_args[0] == "TNx3")
  {
    if (dynamic_cast<const Triplets*>(&*a))
      return model_expression({identifier("tnx3_model"),*a});
    else
      throw myexception()<<"TNx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  else if (model_args[0] == "GTRx3")
  {
    if (dynamic_cast<const Triplets*>(&*a))
      return model_expression({identifier("gtrx3_model"),*a});
    else
      throw myexception()<<"GTRx3: '"<<a->name<<"' is not a triplet alphabet.";
  }
  else if (model_args[0] == "PAM")
  {
    if (*a != AminoAcids())
      throw myexception()<<"PAM: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return (identifier("SModel.pam"),a);
  }
  else if (model_args[0] == "JTT") {
    if (*a != AminoAcids())
      throw myexception()<<"JTT: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return (identifier("SModel.jtt"),a);
  }
  else if (model_args[0] == "WAG") {
    if (*a != AminoAcids())
      throw myexception()<<"WAG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return (identifier("SModel.wag"),a);
  }
  else if (model_args[0] == "LG") {
    if (*a != AminoAcids())
      throw myexception()<<"LG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
    return (identifier("SModel.lg"),a);
  }
  else if (model_args[0] == "Empirical") 
  {
    return (identifier("SModel.empirical"),a,model_args[2]);
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
  else if (model_args[0] == "M0") //M0[S]
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<"M0: '"<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L,model_args[1], const_ptr(N), {});

    return model_expression({identifier("m0_model"), a , S});
  }

  return {};
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
expression_ref process_stack_Frequencies(const module_loader& L,
					 vector<string>& model_args,
					 const object_ptr<const alphabet>& a,
					 const shared_ptr<const valarray<double> >& frequencies)
{
  expression_ref R;

  if (model_args[0] == "F=constant") 
  {
    if (not frequencies)
      throw myexception()<<"F=constant: frequency estimates not available here.";

    Vector<double> v;
    v.resize(a->size());
    for(int i=0;i<a->size();i++)
      v[i] = (*frequencies)[i];

    R = (identifier("ReversibleFrequency"), *a, (identifier("iotaUnsigned"), a->size()), v, (identifier("SModel.plus_gwF"), a, 1.0, v));
  }

  else if (model_args[0] == "F61")
  {
    if (a->size() != 61)
      throw myexception()<<"Cannot use 'F61' frequency model since alphabet contains "<<a->size()<<" letters.";
    model_args[0] = "F";
    return process_stack_Frequencies(L,model_args,a,frequencies);
  }
  else if (model_args[0] == "F") 
    return model_expression({identifier("plus_f_model"),a});
  else if (model_args[0] == "gwF") 
    return model_expression({identifier("plus_gwf_model"),a});
  else if (model_args[0] == "F=uniform") 
    return (identifier("uniform_f_model"),a);
  else if (model_args[0] == "F1x4")
  {
    if (not dynamic_cast<const Triplets*>(&*a))
      throw myexception()<<"+F1x4: '"<<a->name<<"' is not a triplet alphabet.";
    return model_expression({identifier("f1x4_model"),a});
  }
  else if (model_args[0] == "F3x4") 
  {
    if (not dynamic_cast<const Triplets*>(&*a))
      throw myexception()<<"+F1x4: '"<<a->name<<"' is not a triplet alphabet.";
    return model_expression({identifier("f3x4_model"),a});
  }
  else if (model_args[0] == "MG94") 
  {
    if (not dynamic_cast<const Triplets*>(&*a))
      throw myexception()<<"+MG94w9: '"<<a->name<<"' is not a triplet alphabet.";

    return model_expression({identifier("mg94_model"),a});
  }
  else if (model_args[0] == "MG94w9") 
  {
    if (not dynamic_cast<const Triplets*>(&*a))
      throw myexception()<<"+MG94w9: '"<<a->name<<"' is not a triplet alphabet.";

    return model_expression({identifier("mg94w9_model"),a});
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

  if (R and model_args[1] != "")
  {
    // If the frequencies.size() != alphabet.size(), this call will throw a meaningful exception.
    expression_ref s = coerce_to_EM(L,model_args[1], a, frequencies);
    expression_ref mm = model_expression({identifier("reversible_markov_model"), s, R});
    return mm;
  }

  return R;
}

expression_ref coerce_to_frequency_model(const module_loader& L,
					 const expression_ref& M,
					 const object_ptr<const alphabet>& /* a */,
					 const shared_ptr< const valarray<double> >& /* frequencies */)
{
  if (is_exactly(result(M,L,{"SModel","Distributions","Range"}), "SModel.ReversibleFrequency"))
    return M;

  throw myexception()<<": '"<<M<<"' is not an exchange model.";
}

expression_ref coerce_to_frequency_model(const module_loader& L,
					 string smodel,
					 const object_ptr<const alphabet>& a,
					 const shared_ptr< const valarray<double> >& frequencies)
{
  expression_ref M = get_smodel_(L, smodel, a, frequencies);

  return coerce_to_frequency_model(L, M, a,  frequencies);
}


/// \brief Construct a ReversibleMarkovModel from model \a M
expression_ref coerce_to_RA(const module_loader& L,
			    const expression_ref& M,
			    const object_ptr<const alphabet>& a)
{
  object_ref result = ::result(M, L, {"SModel", "Distributions","Range"});

  if (is_exactly(result, "SModel.F81"))
    return M;

  if (is_exactly(result, "SModel.ReversibleMarkov"))
    return M;

  try 
  {
    if (is_exactly(result, "SModel.ReversibleFrequency"))
      throw myexception()<<"Cannot construct CTMC model from frequency model alone!";

    if (boost::dynamic_pointer_cast<const SymmetricMatrixObject>(result))
    {
      // If the frequencies.size() != alphabet.size(), this call will throw a meaningful exception.
      expression_ref r = model_expression({identifier("plus_f_model"),*a});
      expression_ref s = M;
      expression_ref mm = model_expression({identifier("reversible_markov_model"), s, r});
      return mm;
    }
    throw myexception()<<": Can't construct a SimpleReversibleMarkovModel from '"<<M<<"\n";
  }
  catch (std::exception& e) { 
    throw myexception()<<": Can't construct a SimpleReversibleMarkovModel from '"<<M<<"':\n "<<e.what();
  }
}

/// \brief Construct a ReversibleMarkovModel from model \a M
expression_ref coerce_to_RA(const module_loader& L,
			    string smodel,
			    const object_ptr<const alphabet>& a,
			    const shared_ptr< const valarray<double> >& frequencies)
{
  expression_ref M = get_smodel_(L, smodel, a, frequencies);

  return coerce_to_RA(L, M, a);
}

/// \brief Construct a MultiModel from model \a M
expression_ref coerce_to_MM(const module_loader& L,
				    const expression_ref& M,
				    const object_ptr<const alphabet>& a)
{
  if (M and is_exactly(result(M, L,{"SModel","Distributions","Range"}), "SModel.MixtureModel"))
    return M;

  try { 
    expression_ref ra = coerce_to_RA(L,M,a);
    return model_expression({identifier("unit_model"), ra});
  }
  catch (std::exception& e) { 
    throw myexception()<<": Can't construct a MixtureModel from '"<<M<<"':\n"<<e.what();
  }
}

/// \brief Construct a MultiModel from model \a M
expression_ref coerce_to_MM(const module_loader& L,
				    string smodel,
				    const object_ptr<const alphabet>& a, 
				    const shared_ptr< const valarray<double> >& frequencies)
{
  expression_ref M = get_smodel_(L, smodel, a, frequencies);

  return coerce_to_MM(L, M, a);
}

/// \brief Construct a MultiModel from model \a M
expression_ref coerce_to_MMM(const module_loader& L,
				     const expression_ref& M,
				     const object_ptr<const alphabet>& a)
{
  if (is_exactly(result(M,L,{"SModel","Distributions","Range"}), "SModel.MixtureModels"))
    return M;

  try { 
    expression_ref mm = coerce_to_MM(L, M, a);
    return model_expression({identifier("mmm"), mm});
  }
  catch (std::exception& e) { 
    throw myexception()<<": Can't construct a MixtureModels from '"<<M<<"':\n"<<e.what();
  }
}

/// \brief Construct a MultiModel from model \a M
expression_ref coerce_to_MMM(const module_loader& L,
			     string smodel,
			     const object_ptr<const alphabet>& a,
			     const shared_ptr< const valarray<double> >& frequencies)
{
  expression_ref M = get_smodel_(L, smodel, a, frequencies);
  
  return coerce_to_MMM(L, M, a);
}


expression_ref process_stack_Multi(const module_loader& L,
				   vector<string>& model_args,
				   const object_ptr<const alphabet>& a,
				   const shared_ptr< const valarray<double> >& frequencies)
{
  expression_ref plus = identifier("+");
  expression_ref minus = identifier("-");
  expression_ref times = identifier("*");
  expression_ref divide = identifier("/");

  if (model_args[0] == "single") 
    return coerce_to_MM(L,coerce_to_RA(L,model_args[1],a,frequencies),a);

  // else if (model_args[0] == "gamma_plus_uniform") {
  else if (model_args[0] == "gamma") 
  {
    expression_ref base = coerce_to_RA(L, model_args[1],a,frequencies);

    int n = convertTo<int>(model_args[2]);

    return model_expression({identifier("gamma_model"), base, n});
  }
  else if (model_args[0] == "gamma_inv") 
  {
    expression_ref base = coerce_to_RA(L, model_args[1],a,frequencies);

    int n = convertTo<int>(model_args[2]);

    return model_expression({identifier("gamma_inv_model"), base, n});
  }
  else if (model_args[0] == "log-normal") 
  {
    expression_ref base = coerce_to_RA(L, model_args[1],a,frequencies);

    int n = convertTo<int>(model_args[2]);

    return model_expression({identifier("log_normal_model"), base, n});
  }
  else if (model_args[0] == "log-normal_inv") 
  {
    expression_ref base = coerce_to_RA(L, model_args[1],a,frequencies);

    int n = convertTo<int>(model_args[2]);

    return model_expression({identifier("log_normal_inv_model"), base, n});
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

  else if (model_args[0] == "DP") 
  {
    expression_ref base = coerce_to_RA(L, model_args[1],a,frequencies);

    int n = convertTo<int>(model_args[2]);

    return model_expression({identifier("dp_model"), base, n});
  }
  /*
  else if (model_args[0] == "Modulated")
  {
    formula_expression_ref MM = coerce_to_MM(L, model_args[1],a,frequencies);

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
      models.push_back( coerce_to_MM(L, model_args[i+2], a, frequencies) );

    return Mixture_Model(models);
  }
  else if (model_args[0] == "M2") 
  {
    formula_expression_ref p1 = def_parameter("M2.fAaINV", Double(1.0/3), between(0,1));
    formula_expression_ref p2 = def_parameter("M2.fNeutral", Double(1.0/3), between(0,1));
    formula_expression_ref p3 = def_parameter("M2.fSelected", Double(1.0/3), between(0,1));
    formula_expression_ref m2_omega = def_parameter("M2.omega", Double(1.0), lower_bound(0));
    formula_expression_ref D = (identifier("DiscreteDistribution"), Tuple(p1,0.0)&
						    Tuple(p2,1.0)&
						    Tuple(p3,m2_omega)&
						    ListEnd
						    );

    D.add_expression( constructor(":~",2) + (p1&(p2&(p3&ListEnd))).exp() + (identifier("dirichlet"), List(1.0, 98.0, 1.0)) );
    D.add_expression( constructor(":~",2) + m2_omega.exp() + (identifier("logExponential"), 0.05) );

    formula_expression_ref M0 = get_M0_omega_function(L, a,frequencies,model_args,2);

    return (identifier("multiParameter"),M0,D);
  }
  else if (model_args[0] == "M3u") // M3u[n,S,F]
  {
    int n = convertTo<int>(model_args[1]);

    formula_expression_ref D = ListEnd;
    formula_expression_ref F = ListEnd;
    for(int i=n-1;i>=0;i--)
    {
      string pname_f = "M3.f" + convertToString(i+1);
      string pname_w = "M3.omega" + convertToString(i+1);
      formula_expression_ref f = def_parameter("M3.f"     + convertToString(i+1), Double(1.0/n), between(0,1));
      formula_expression_ref w = def_parameter("M3.omega" + convertToString(i+1), Double(1.0), lower_bound(0), (identifier("uniform"), 0.0, 1.0));

      D = Tuple(f,w)&D;
      F = f&F;
    }
    D = (identifier("DiscreteDistribution"), D);
    D.add_expression( constructor(":~",2) + F.exp() + (identifier("dirichlet'"), n, 4.0));

    formula_expression_ref M0 = get_M0_omega_function(L,a,frequencies,model_args,2);

    return (identifier("multiParameter"), M0, D);
  }
  else if (model_args[0] == "M3") // M3[n,S,F]
  {
    throw myexception()<<"The M3 model is not working right now.  Try M3u, with only stabilizing selection.";

    int n = convertTo<int>(model_args[1]);

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
      w.add_expression( constructor(":~",2) + w.exp() + (identifier("logExponential"), 0.05) );

      D = Tuple(f,w)&D;
    }
    D = (identifier("DiscreteDistribution"), D);
    D.add_expression(constructor(":~",2) +  get_list(fraction) + (identifier("dirichlet'"), n, 4.0));

    formula_expression_ref M0 = get_M0_omega_function(L,a,frequencies,model_args,2);

    return (identifier("multiParameter"), M0, D);
  }
    */
  else if (model_args[0] == "M1a") // M2a[S,F]
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_args[1], const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_args[2], a, frequencies);

    return model_expression({identifier("m1a_model"),a,S,R});
  }
  else if (model_args[0] == "M2a") // M2a[S,F]
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_args[1], const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_args[2], a, frequencies);

    return model_expression({identifier("m2a_model"),a,S,R});
  }
  else if (model_args[0] == "M2a_Test") // M2a[S,F]
  {
    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_args[1], const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_args[2], a, frequencies);

    return model_expression({identifier("m2a_test_model"),a,S,R});
  }
  else if (model_args[0] == "M8b_Test") // M8b[n,S,F]
  {
    int n = convertTo<int>(model_args[1]);

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_args[2], const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_args[3], a, frequencies);

    return model_expression({identifier("m8b_test_model"),a,n,S,R});
  }
  else if (model_args[0] == "M7")
  {
    int n = convertTo<int>(model_args[1]);

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_args[2], const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_args[3], a, frequencies);

    return model_expression({identifier("m7_model"),a,n,S,R});
  }
  else if (model_args[0] == "branch-site")  // branch-site-test[n,S,F]
  {
    int n = convertTo<int>(model_args[1]);

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = coerce_to_EM(L, model_args[2], const_ptr(N), {});

    expression_ref R = coerce_to_frequency_model(L, model_args[3], a, frequencies);

    return model_expression({identifier("branch_site_test_model"),a,n,S,R});
  }

  return {};
}

expression_ref 
get_smodel_(const module_loader& L, string smodel,const object_ptr<const alphabet>& a,const shared_ptr<const valarray<double> >& frequencies) 
{
  if (smodel == "")
    throw myexception()<<"You must specify a substitution model!";

  vector<string> model_args = split_top_level(smodel);

  expression_ref m;

  m = process_stack_Markov(L, model_args, a);
  if (m) return m;

  m = process_stack_Frequencies(L, model_args, a, frequencies);
  if (m) return m;

  m = process_stack_Multi(L, model_args, a, frequencies);
  if (m) return m;

  throw myexception()<<"Couldn't process substitution model description \""<<smodel<<"\"";
}

expression_ref
get_smodel_(const module_loader& L,const string& smodel,const object_ptr<const alphabet>& a)
{
  return get_smodel_(L, smodel, a, shared_ptr<const valarray<double> >());
}

expression_ref
get_smodel_(const module_loader& L,const string& smodel)
{
  return get_smodel_(L, smodel, object_ptr<const alphabet>(),shared_ptr<const valarray<double> >());
}


//FIXME change to return a (model, standardized name) pair.


/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param smodel_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
expression_ref
get_smodel(const module_loader& L, const string& smodel, const object_ptr<const alphabet>& a, const shared_ptr<const valarray<double> >& frequencies) 
{
  assert(frequencies->size() == a->size());

  // --------- Convert smodel to MultiMixtureModel ------------//
  expression_ref full_smodel = coerce_to_MMM(L, smodel,a,frequencies);

  std::cerr<<"smodel = "<<full_smodel<<"\n";

  return full_smodel;
}

/// \brief Construct a substitution::MultiModel model for a collection of alignments
///
/// \param smodel_name The name of the substitution model.
/// \param A The alignments.
///
/// This routine constructs the initial frequencies based on all of the alignments.
///
expression_ref get_smodel(const module_loader& L, const variables_map& args, const string& smodel_name,const vector<alignment>& A) 
{
  for(int i=1;i<A.size();i++)
    if (A[i].get_alphabet() != A[0].get_alphabet())
      throw myexception()<<"alignments in partition don't all have the same alphabet!";

  shared_ptr< const valarray<double> > frequencies (new valarray<double>(empirical_frequencies(args,A)));

  return get_smodel(L, smodel_name, const_ptr( A[0].get_alphabet() ), frequencies);
}

expression_ref get_smodel(const module_loader& L, const variables_map& args, const string& smodel_name,const alignment& A) 
{
  shared_ptr< const valarray<double> > frequencies (new valarray<double>(empirical_frequencies(args,A)));

  return get_smodel(L, smodel_name, const_ptr( A.get_alphabet() ), frequencies);
}

expression_ref get_smodel(const module_loader& L,const variables_map& args, const alignment& A) 
{
  string smodel_name = args["smodel"].as<string>();

  shared_ptr< const valarray<double> > frequencies (new valarray<double>(empirical_frequencies(args,A)));

  return get_smodel(L, smodel_name, const_ptr( A.get_alphabet() ), frequencies);
}
