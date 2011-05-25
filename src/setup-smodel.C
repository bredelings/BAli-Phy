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
#include "smodel.H"
#include "util.H"
#include "rates.H"
#include "myexception.H"

using std::vector;
using std::valarray;
using namespace substitution;
using boost::program_options::variables_map;

/// \brief Take a string of the form \a s[\a arg] off the top of \a sstack, if its present
///
/// \param sstack A stack of strings that represent a substitution model.
/// \param s The model name to match.
/// \param arg A posible argument.
///
bool match(vector<string>& sstack,const string& s,string& arg) {
  if (not sstack.size())
    return false;
  
  string name = sstack.back();

  vector<string> arguments = get_arguments(name,'[',']');

  if (name != s) return false;

  if (arguments.size() == 0)
    arg = "";
  else if (arguments.size() > 0)
    arg = arguments[0];

  sstack.pop_back();

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

owned_ptr< ::Model> 
get_smodel_(const string& smodel,const alphabet& a,const valarray<double>&);

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param model_stack The list of models that may be used as components of the current model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
bool process_stack_Markov(vector<string>& string_stack,
			  vector<owned_ptr< ::Model> >& model_stack,
			  const alphabet& a,
			  const valarray<double>& frequencies)
{
  string arg;

  //------ Get the base markov model (Reversible Markov) ------//
  if (match(string_stack,"EQU",arg))
    model_stack.push_back(EQU(a));

  else if (match(string_stack,"F81",arg))
    model_stack.push_back(F81_Model(a,frequencies));

  else if (match(string_stack,"HKY",arg)) 
  {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(HKY(*N));
    else
      throw myexception()<<"HKY: '"<<a.name<<"' is not a nucleotide alphabet.";
  }
  else if (match(string_stack,"TN",arg)) 
  {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(TN(*N));
    else
      throw myexception()<<"TN: '"<<a.name<<"' is not a nucleotide alphabet.";
  }
  else if (match(string_stack,"GTR",arg)) 
  {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(GTR(*N));
    else
      throw myexception()<<"GTR: '"<<a.name<<"' is not a nucleotide alphabet.";
  }
  /* THINKO - Must tripletmodels be constructed from nucleotide-specific models?
  else if (match(string_stack,"EQUx3",arg)) {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (T) 
      model_stack.push_back(SingletToTripletExchangeModel(*T,EQU(T->getNucleotides())));
    else
      throw myexception()<<"EQUx3:: '"<<a.name<<"' is not a triplet alphabet.";
  }
  */      
  else if (match(string_stack,"HKYx3",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (T) 
      model_stack.push_back(SingletToTripletExchangeModel(*T,HKY(T->getNucleotides())));
    else
      throw myexception()<<"HKYx3: '"<<a.name<<"' is not a triplet alphabet.";
  }
  else if (match(string_stack,"TNx3",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (T) 
      model_stack.push_back(SingletToTripletExchangeModel(*T,TN(T->getNucleotides())));
    else
      throw myexception()<<"TNx3: '"<<a.name<<"' is not a triplet alphabet.";
  }
  else if (match(string_stack,"GTRx3",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (T) 
      model_stack.push_back(SingletToTripletExchangeModel(*T,GTR(T->getNucleotides())));
    else
      throw myexception()<<"GTRx3: '"<<a.name<<"' is not a triplet alphabet.";
  }
  else if (match(string_stack,"PAM",arg)) {
    if (a != AminoAcids())
      throw myexception()<<"PAM: '"<<a.name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back(PAM());
  }
  else if (match(string_stack,"JTT",arg)) {
    if (a != AminoAcids())
      throw myexception()<<"JTT: '"<<a.name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back(JTT());
  }
  else if (match(string_stack,"WAG",arg)) {
    if (a != AminoAcids())
      throw myexception()<<"WAG: '"<<a.name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back(WAG());
  }
  else if (match(string_stack,"LG",arg)) {
    if (a != AminoAcids())
      throw myexception()<<"LG: '"<<a.name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back(LG());
  }
  else if (match(string_stack,"Empirical",arg)) {
    Empirical M(a);
    M.load_file(arg);
    model_stack.push_back(M);
  }
  else if (match(string_stack,"C10",arg))
  {
    if (a != AminoAcids())
      throw myexception()<<"C20: '"<<a.name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back(C10_CAT_FixedFrequencyModel());
  }
  else if (match(string_stack,"C20",arg))
  {
    if (a != AminoAcids())
      throw myexception()<<"C20: '"<<a.name<<"' is not an 'Amino-Acids' alphabet.";
    model_stack.push_back(C20_CAT_FixedFrequencyModel());
  }
  else if (match(string_stack,"CAT-Fix",arg)) {
    if (a != AminoAcids())
      throw myexception()<<"CAT-Fix: '"<<a.name<<"' is not an 'Amino-Acids' alphabet.";
    CAT_FixedFrequencyModel M(a);
    M.load_file(arg);
    model_stack.push_back(M);
  }

  else if (match(string_stack,"M0",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&a);
    if (not C)
      throw myexception()<<"M0: '"<<a.name<<"' is not a 'Codons' alphabet";

    owned_ptr<NucleotideExchangeModel> N_submodel = HKY(C->getNucleotides());

    if (not arg.empty()) 
    {
      owned_ptr< ::Model> submodel = get_smodel_(arg,C->getNucleotides(),valarray<double>());

      if (not submodel.as<NucleotideExchangeModel>())
	throw myexception()<<"Submodel '"<<arg<<"' for M0 is not a nucleotide replacement model.";

      N_submodel = submodel.as<NucleotideExchangeModel>();
    }

    model_stack.push_back( M0(*C, *N_submodel) );
  }
  else
    return false;

  return true;
}

/// \brief Construct an AlphabetExchangeModel from model \a M
owned_ptr<AlphabetExchangeModel> get_EM(::Model* M, const string& name)
{
  assert(M);

  AlphabetExchangeModel* AEM = dynamic_cast<AlphabetExchangeModel*>(M);

  if (not AEM)
    throw myexception()<<name<<": '"<<M->name()<<"' is not an exchange model.";

  return *AEM;
}


/// \brief Construct an AlphabetExchangeModel from the top of the model stack
owned_ptr<AlphabetExchangeModel> get_EM(vector<owned_ptr< ::Model> >& model_stack, const string& name)
{
  if (model_stack.empty())
    throw myexception()<<name<<": Needed an exchange model, but no model was given.";

  return get_EM(model_stack.back().get(),name);
}


/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param model_stack The list of models that may be used as components of the current model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
bool process_stack_Frequencies(vector<string>& string_stack,
			       vector<owned_ptr< ::Model> >& model_stack,
			       const alphabet& a,
			       const valarray<double>& frequencies)
{
  string arg;
  
  if (match(string_stack,"F=constant",arg)) {
    owned_ptr<AlphabetExchangeModel> EM = get_EM(model_stack,"F=constant");

    SimpleFrequencyModel F(a,frequencies);

    for(int i=0;i<F.n_parameters();i++)
      F.set_fixed(i,true);

    model_stack.back() = ReversibleMarkovSuperModel(*EM,F);
  }
  else if (match(string_stack,"F",arg)) {
    owned_ptr<AlphabetExchangeModel> EM = get_EM(model_stack,"F");

    SimpleFrequencyModel F(a,frequencies);

    model_stack.back() = ReversibleMarkovSuperModel(*EM,F);
  }
  else if (match(string_stack,"F=uniform",arg)) {
    owned_ptr<AlphabetExchangeModel> EM = get_EM(model_stack,"F=uniform");

    UniformFrequencyModel UF(a,frequencies);

    model_stack.back() = ReversibleMarkovSuperModel(*EM,UF);
  }
  else if (match(string_stack,"F=nucleotides",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (not T)
      throw myexception()<<"F=nucleotides:: '"<<a.name<<"' is not a triplet alphabet.";

    owned_ptr<AlphabetExchangeModel> EM = get_EM(model_stack,"F=nucleotides");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,IndependentNucleotideFrequencyModel(*T));
  }
  else if (match(string_stack,"F=amino-acids",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&a);
    if (not C)
      throw myexception()<<"F=amino-acids:: '"<<a.name<<"' is not a codon alphabet.";

    owned_ptr<AlphabetExchangeModel> EM = get_EM(model_stack,"F=nucleotides");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,AACodonFrequencyModel(*C));
  }
  else if (match(string_stack,"F=triplets",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (not T)
      throw myexception()<<"F=triplets:: '"<<a.name<<"' is not a triplet alphabet.";

    owned_ptr<AlphabetExchangeModel> EM = get_EM(model_stack,"F=triplets");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,TripletsFrequencyModel(*T));
  }
  else if (match(string_stack,"F=codons",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&a);
    if (not C)
      throw myexception()<<"F=codons:: '"<<a.name<<"' is not a codon alphabet.";

    owned_ptr<AlphabetExchangeModel> EM = get_EM(model_stack,"F=codons");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,CodonsFrequencyModel(*C));
  }
  else if (match(string_stack,"F=codons2",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&a);
    if (not C)
      throw myexception()<<"F=codons2:: '"<<a.name<<"' is not a codon alphabet.";

    owned_ptr<AlphabetExchangeModel> EM = get_EM(model_stack,"F=codons2");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,CodonsFrequencyModel2(*C));
  }
  else
    return false;
  return true;
}

/// \brief Construct a ReversibleAdditiveModel from model \a M
owned_ptr<ReversibleAdditiveModel> get_RA(::Model* M, const string& name,
					     const valarray<double>& frequencies)
{
  if (ReversibleAdditiveModel* RA  = dynamic_cast<ReversibleAdditiveModel*>(M))
    return *RA;

  try {
    // If the frequencies.size() != alphabet.size(), this call with throw a meaningful exception.
    return SimpleReversibleMarkovModel(*get_EM(M,name),frequencies); 
  }
  catch (std::exception& e) { 
    throw myexception()<<name<<": Can't construct a SimpleReversibleMarkovModel from '"<<M->name()<<"':\n "<<e.what();
  }
}

/// \brief Construct a ReversibleAdditiveModel from the top of the model stack
owned_ptr<ReversibleAdditiveModel> get_RA(vector<owned_ptr< ::Model> >& model_stack, 
					     const string& name,
					     const valarray<double>& frequencies)
{
  if (model_stack.empty())
    throw myexception()<<name<<": couldn't find any model to use.";

  return get_RA(model_stack.back().get(), name, frequencies);
}


/// \brief Construct a MultiModel from model \a M
owned_ptr<MultiModel> 
get_MM(::Model *M, const string& name, const valarray<double>& frequencies)
{
  if (MultiModel* MM = dynamic_cast<MultiModel*>(M))
    return *MM;

  try { 
    return UnitModel(*get_RA(M,name,frequencies)) ; 
  }
  catch (std::exception& e) { 
    throw myexception()<<name<<": Can't construct a UnitModel from '"<<M->name()<<"':\n"<<e.what();
  }
}

/// \brief Construct a MultiModel from the top of the model stack.
owned_ptr<MultiModel>
get_MM(vector<owned_ptr< ::Model> >& model_stack, const string& name,const valarray<double>& frequencies)
{
  if (model_stack.empty())
    throw myexception()<<name<<": Trying to construct a MultiModel, but no model was given.";

  return get_MM(model_stack.back().get(), name, frequencies);
}

bool process_stack_Multi(vector<string>& string_stack,  
			 vector<owned_ptr< ::Model> >& model_stack,
			 const valarray<double>& frequencies)
{
  string arg;
  if (match(string_stack,"single",arg)) 
      model_stack.back() = UnitModel(*get_RA(model_stack,"single",frequencies));

  else if (match(string_stack,"gamma_plus_uniform",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    model_stack.back() = DistributionParameterModel(UnitModel(*get_RA(model_stack,"gamma_plus_uniform",frequencies)),
						    Uniform() + Gamma(),
						    -1,//rate!
						    n);
  }
  else if (match(string_stack,"gamma",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    model_stack.back() = GammaParameterModel(*get_MM(model_stack,"gamma",frequencies),n);
  }
  else if (match(string_stack,"double_gamma",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    model_stack.back() = DistributionParameterModel(UnitModel(*get_RA(model_stack,"double_gamma",frequencies)),
						    Gamma() + Gamma(),
						    -1,//rate!
						    n);
  }
  else if (match(string_stack,"log-normal",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    model_stack.back() = LogNormalParameterModel(*get_MM(model_stack,"log-normal",frequencies),n);
  }
  else if (match(string_stack,"multi_freq",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    model_stack.back() = MultiFrequencyModel(*get_EM(model_stack,"multi_freq"),n);
  }
  else if (match(string_stack,"INV",arg))
    model_stack.back() = WithINV(*get_MM(model_stack,"INV",frequencies));

  else if (match(string_stack,"DP",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);
    model_stack.back() = DirichletParameterModel(*get_MM(model_stack,"DP",frequencies),-1,n);
  }
  else if (match(string_stack,"Modulated",arg))
  {
    owned_ptr<MultiModel> MM = get_MM(model_stack,"Modulated",frequencies);

    int n = MM->n_base_models();
    model_stack.back() = ModulatedMarkovModel(*MM,
					      SimpleExchangeModel(n));
  }
  else if (match(string_stack,"Mixture",arg)) 
  {
    int n=1;
    if (arg.empty())
      throw myexception()<<"Mixture model must specify number of submodels as Mixture[n]";
    else
      n = convertTo<int>(arg);

    if (model_stack.size() < n)
      throw myexception()<<"Dual: can't find "<<n<<" models to combine";
    
    vector <owned_ptr<MultiModel> > models;
    for(int m=0;m<n;m++) {
      owned_ptr<MultiModel> M = get_MM(model_stack,"Mixture",frequencies);
      model_stack.pop_back();
      models.push_back(M);
    }

    model_stack.push_back(MixtureModel(models));
  }
  else if (match(string_stack,"M2",arg)) {

    M0* YM = dynamic_cast<M0*>(model_stack.back().get());

    if (not YM)
      throw myexception()<<"Trying to construct an M2 model from a '"<<model_stack.back().get()->name()
			 <<"' model, which is not a M0 model.";

    model_stack.back() = M2(*YM, SimpleFrequencyModel(YM->Alphabet()));
  }
  else if (match(string_stack,"M2a",arg)) {

    M0* YM = dynamic_cast<M0*>(model_stack.back().get());

    if (not YM)
      throw myexception()<<"Trying to construct an M2a model from a '"<<model_stack.back().get()->name()
			 <<"' model, which is not a M0 model.";

    model_stack.back() = M2a(*YM, SimpleFrequencyModel(YM->Alphabet()));
  }
  else if (match(string_stack,"M8b",arg)) {
    int n=3;
    if (not arg.empty())
      n = convertTo<int>(arg);

    M0* YM = dynamic_cast<M0*>(model_stack.back().get());

    if (not YM)
      throw myexception()<<"Trying to construct an M8b model from a '"<<model_stack.back().get()->name()
			 <<"' model, which is not a M0 model.";

    model_stack.back() = M8b(*YM, SimpleFrequencyModel(YM->Alphabet()), n);
  }
  else if (match(string_stack,"M3",arg)) {
    int n=3;
    if (not arg.empty())
      n = convertTo<int>(arg);

    M0* YM = dynamic_cast<M0*>(model_stack.back().get());

    if (not YM)
      throw myexception()<<"Trying to construct an M3 model from a '"<<model_stack.back().get()->name()
			 <<"' model, which is not a M0 model.";

    model_stack.back() = M3(*YM, SimpleFrequencyModel(YM->Alphabet()), n);
  }
  else if (match(string_stack,"M7",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    M0* YM = dynamic_cast<M0*>(model_stack.back().get());

    if (not YM)
      throw myexception()<<"Trying to construct an M7 model from a '"<<model_stack.back().get()->name()
			 <<"' model, which is not a M0 model.";

    model_stack.back() = M7(*YM, SimpleFrequencyModel(YM->Alphabet()), n);
  }
  else
    return false;
  return true;
}

owned_ptr< ::Model> 
get_smodel_(const string& smodel,const alphabet& a,const valarray<double>& frequencies) 
{
  // Initialize the string stack from the model name
  vector<string> string_stack;
  if (smodel != "") 
    string_stack = split(smodel,'+');
  std::reverse(string_stack.begin(),string_stack.end());

  // Initialize the model stack 
  vector<owned_ptr< ::Model> > model_stack;
  if (not process_stack_Markov(string_stack,model_stack,a,frequencies)) 
  {
    string model_name = default_markov_model(a);
    if (not model_name.size())
      throw myexception()<<"You must specify a substitution model - there is no default substitution model for alphabet '"<<a.name<<"'";
    string_stack.push_back(model_name);
    if (not process_stack_Markov(string_stack,model_stack,a,frequencies))
      throw myexception()<<"Can't guess the base CTMC model for alphabet '"<<a.name<<"'";
  }


  //-------- Run the model specification -----------//
  while(string_stack.size()) {
    int length = string_stack.size();

    process_stack_Markov(string_stack,model_stack,a,frequencies);

    process_stack_Frequencies(string_stack,model_stack,a,frequencies);

    process_stack_Multi(string_stack,model_stack,frequencies);

    if (string_stack.size() == length)
      throw myexception()<<"Couldn't process substitution model level \""<<string_stack.back()<<"\"";
  }

  //---------------------- Stack should be empty now ----------------------//
  if (model_stack.size()>1) {
    throw myexception()<<"Substitution model "<<model_stack.back()->name()<<" was specified but not used!\n";
  }

  return model_stack.back();
}


/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param smodel_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
owned_ptr<MultiModel>
get_smodel(const string& smodel_name, const alphabet& a, const valarray<double>& frequencies) 
{
  assert(frequencies.size() == a.size());

  //------------------ Get smodel ----------------------//
  owned_ptr< ::Model> smodel = get_smodel_(smodel_name,a,frequencies);

  // check if the model actually fits alphabet a...

  // --------- Convert smodel to MultiModel ------------//
  owned_ptr<MultiModel> full_smodel = get_MM(smodel.get(),"Final",frequencies);

  return full_smodel;
}

/// \brief Construct a substitution::MultiModel model for a collection of alignments
///
/// \param smodel_name The name of the substitution model.
/// \param A The alignments.
///
/// This routine constructs the initial frequencies based on all of the alignments.
///
owned_ptr<MultiModel> get_smodel(const variables_map& args, const string& smodel_name,const vector<alignment>& A) 
{
  for(int i=1;i<A.size();i++)
    if (A[i].get_alphabet() != A[0].get_alphabet())
      throw myexception()<<"alignments in partition don't all have the same alphabet!";
  return get_smodel(smodel_name,A[0].get_alphabet(),empirical_frequencies(args,A));
}

owned_ptr<MultiModel> get_smodel(const variables_map& args, const string& smodel_name,const alignment& A) 
{
  return get_smodel(smodel_name,A.get_alphabet(),empirical_frequencies(args,A));
}

owned_ptr<MultiModel> get_smodel(const variables_map& args, const alignment& A) 
{
  string smodel_name = args["smodel"].as<string>();
  return get_smodel(smodel_name,A.get_alphabet(),empirical_frequencies(args,A));
}
