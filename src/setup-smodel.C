#include <vector>
#include <boost/program_options.hpp>
#include "setup.H"
#include "util.H"
#include "rates.H"
#include "myexception.H"

using std::vector;
using std::valarray;
using namespace substitution;
using boost::program_options::variables_map;

/// Take something off the string stack, if its present
bool match(vector<string>& sstack,const string& s,string& arg) {
  if (not sstack.size())
    return false;
  
  bool success=false;
  string top = sstack.back();

  if (top == s) {
    arg = "";
    success=true;
  }
  else if (top.size() > 1 and top[top.size()-1] == ']') {
    int loc = top.find('[');
    if (loc == -1) return false;
    string name = top.substr(0,loc);
    arg = top.substr(loc+1,top.size()-loc-2);
    success = (name == s);
  }

  if (success)
    sstack.pop_back();
  return success;
}

/// If no markov model is specified, try to add one.
void guess_markov_model(vector<string>& string_stack,const alphabet& a) 
{
  if (dynamic_cast<const Nucleotides*>(&a))
    string_stack.push_back("HKY");
  else if (dynamic_cast<const AminoAcids*>(&a))
    string_stack.push_back("Empirical");
  else if (dynamic_cast<const Codons*>(&a))
    string_stack.push_back("M0");
  else if (dynamic_cast<const Triplets*>(&a))
    string_stack.push_back("HKYx3");
}

OwnedPointer< ::Model> 
get_smodel(const variables_map& args,const string& smodel,const alphabet& a,const valarray<double>&);

bool process_stack_Markov(vector<string>& string_stack,
			  vector<OwnedPointer< ::Model> >& model_stack,
			  const alphabet& a,
			  const variables_map& args) 
{
  string arg;

  //------ Get the base markov model (Reversible Markov) ------//
  if (match(string_stack,"EQU",arg))
    model_stack.push_back(EQU(a));
  else if (match(string_stack,"HKY",arg)) {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(HKY(*N));
    else
      throw myexception()<<"HKY:: '"<<a.name<<"' is not a nucleotide alphabet.";
  }
  else if (match(string_stack,"TN",arg)) {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(TN(*N));
    else
      throw myexception()<<"TN::  '"<<a.name<<"' is not a nucleotide alphabet.";
  }
  else if (match(string_stack,"GTR",arg)) {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(GTR(*N));
    else
      throw myexception()<<"GTR:: '"<<a.name<<"' is not a nucleotide alphabet.";
  }
  /* THINKO
  else if (match(string_stack,"EQUx3",arg)) {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (T) 
      model_stack.push_back(SingletToTripletExchangeModel(*T,EQU(T->getNucleotides())));
    else
      throw myexception()<<"EQUx3:: '"<<a.name<<"' is not a triplet alphabet.";
  }
  */      
  else if (match(string_stack,"HKYx3",arg)) {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (T) 
      model_stack.push_back(SingletToTripletExchangeModel(*T,HKY(T->getNucleotides())));
    else
      throw myexception()<<"HKYx3:: '"<<a.name<<"' is not a triplet alphabet.";
  }
  else if (match(string_stack,"TNx3",arg)) {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (T) 
      model_stack.push_back(SingletToTripletExchangeModel(*T,TN(T->getNucleotides())));
    else
      throw myexception()<<"TNx3:: '"<<a.name<<"' is not a triplet alphabet.";
  }
  else if (match(string_stack,"GTRx3",arg)) {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (T) 
      model_stack.push_back(SingletToTripletExchangeModel(*T,GTR(T->getNucleotides())));
    else
      throw myexception()<<"GTRx3:: '"<<a.name<<"' is not a triplet alphabet.";
  }
  else if (match(string_stack,"Empirical",arg)) {
    string filename = arg;
    if (filename.empty()) filename = "wag";
    filename = args["data-dir"].as<string>() + "/" + filename + ".dat";

    model_stack.push_back(Empirical(a,filename));
  }
  else if (match(string_stack,"M0",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&a);
    if (not C)
      throw myexception()<<"Can't figure out how to make a codon model from non-codon alphabet '"<<a.name<<"'";

    OwnedPointer<NucleotideExchangeModel> N_submodel = HKY(C->getNucleotides());

    if (not arg.empty()) {
      OwnedPointer< ::Model> submodel = get_smodel(args,arg,C->getNucleotides(),valarray<double>());
      NucleotideExchangeModel* temp = dynamic_cast<NucleotideExchangeModel*>( submodel.get());
      if (not temp)
	throw myexception()<<"Submodel '"<<arg<<"' for M0 is not a nucleotide replacement model.";
      N_submodel = temp->clone();
    }

    model_stack.push_back( M0(*C, *N_submodel) );
  }
  else
    return false;

  return true;
}

OwnedPointer<ExchangeModel> get_EM(::Model* M, const string& name)
{
  ExchangeModel* EM = dynamic_cast<ExchangeModel*>(M);

  if (not EM) 
    throw myexception()<<name<<":couldn't find an exchange-model to use.";

  return *EM;
}


OwnedPointer<ExchangeModel> get_EM(vector<OwnedPointer< ::Model> >& model_stack, const string& name)
{
  if (model_stack.empty())
    throw myexception()<<name<<": couldn't find any model to use.";

  return get_EM(model_stack.back().get(),name);
}


bool process_stack_Frequencies(vector<string>& string_stack,
			       vector<OwnedPointer< ::Model> >& model_stack,
			       const alphabet& a,
			       const valarray<double>& frequencies)
{
  string arg;
  
  if (match(string_stack,"pi=constant",arg)) {
    OwnedPointer<ExchangeModel> EM = get_EM(model_stack,"pi=constant");

    SimpleFrequencyModel F(a,frequencies);

    for(int i=0;i<F.parameters().size();i++)
      F.fixed(i,true);

    model_stack.back() = ReversibleMarkovSuperModel(*EM,F);
  }
  else if (match(string_stack,"pi",arg)) {
    OwnedPointer<ExchangeModel> EM = get_EM(model_stack,"pi");

    SimpleFrequencyModel F(a,frequencies);

    model_stack.back() = ReversibleMarkovSuperModel(*EM,F);
  }
  else if (match(string_stack,"pi=nucleotides",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (not T)
      throw myexception()<<"pi=nucleotides:: '"<<a.name<<"' is not a triplet alphabet.";

    OwnedPointer<ExchangeModel> EM = get_EM(model_stack,"pi=nucleotides");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,IndependentNucleotideFrequencyModel(*T));
  }
  else if (match(string_stack,"pi=amino-acids",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&a);
    if (not C)
      throw myexception()<<"pi=amino-acids:: '"<<a.name<<"' is not a codon alphabet.";

    OwnedPointer<ExchangeModel> EM = get_EM(model_stack,"pi=nucleotides");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,AACodonFrequencyModel(*C));
  }
  else if (match(string_stack,"pi=triplets",arg)) 
  {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);
    if (not T)
      throw myexception()<<"pi=triplets:: '"<<a.name<<"' is not a triplet alphabet.";

    OwnedPointer<ExchangeModel> EM = get_EM(model_stack,"pi=triplets");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,TripletsFrequencyModel(*T));
  }
  else if (match(string_stack,"pi=codons",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&a);
    if (not C)
      throw myexception()<<"pi=codons:: '"<<a.name<<"' is not a codon alphabet.";

    OwnedPointer<ExchangeModel> EM = get_EM(model_stack,"pi=codons");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,CodonsFrequencyModel(*C));
  }
  else if (match(string_stack,"pi=codons2",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&a);
    if (not C)
      throw myexception()<<"pi=codons2:: '"<<a.name<<"' is not a codon alphabet.";

    OwnedPointer<ExchangeModel> EM = get_EM(model_stack,"pi=codons2");

    model_stack.back() = ReversibleMarkovSuperModel(*EM,CodonsFrequencyModel2(*C));
  }
  else
    return false;
  return true;
}


OwnedPointer<ReversibleAdditiveModel> get_RA(::Model* M, const string& name,
					     const valarray<double>& frequencies)
{
  if (ReversibleAdditiveModel* RA  = dynamic_cast<ReversibleAdditiveModel*>(M))
    return *RA;

  try {
    return SimpleReversibleMarkovModel(*get_EM(M,name),frequencies); 
  }
  catch (std::exception& e) { 
    throw myexception()<<name<<":couldn't find a reversible+additive model to use.";
  }
}

OwnedPointer<ReversibleAdditiveModel> get_RA(vector<OwnedPointer< ::Model> >& model_stack, 
					     const string& name,
					     const valarray<double>& frequencies)
{
  if (model_stack.empty())
    throw myexception()<<name<<": couldn't find any model to use.";

  return get_RA(model_stack.back().get(), name, frequencies);
}


OwnedPointer<MultiModel> get_MM(::Model *M, const string& name, const valarray<double>& frequencies)
{
  if (MultiModel* MM = dynamic_cast<MultiModel*>(M))
    return *MM;

  try { 
    return UnitModel(*get_RA(M,name,frequencies)) ; 
  }
  catch (std::exception& e) { 
    throw myexception()<<name<<":couldn't find a multimodel model to use.";
  }
}

OwnedPointer<MultiModel> get_MM(vector<OwnedPointer< ::Model> >& model_stack, const string& name,
				const valarray<double>& frequencies)
{
  if (model_stack.empty())
    throw myexception()<<name<<": couldn't find any model to use.";

  return get_MM(model_stack.back().get(), name, frequencies);
}

bool process_stack_Multi(vector<string>& string_stack,  
			 vector<OwnedPointer< ::Model> >& model_stack,
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

  else if (match(string_stack,"INV2",arg))
    model_stack.back() = WithINV2(*get_MM(model_stack,"INV2",frequencies));

  else if (match(string_stack,"Mixture",arg)) {
    int n=1;
    if (arg.empty())
      throw myexception()<<"Mixture model must specify number of submodels as Mixture[n]";
    else
      n = convertTo<int>(arg);

    if (model_stack.size() < n)
      throw myexception()<<"Dual: can't find "<<n<<" models to combine";
    
    vector <OwnedPointer<MultiModel> > models;
    for(int m=0;m<n;m++) {
      OwnedPointer<MultiModel> M = get_MM(model_stack,"Mixture",frequencies);
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

OwnedPointer< ::Model> 
get_smodel(const variables_map& args,const string& smodel,const alphabet& a,
	   const valarray<double>& frequencies) 
{
  // Initialize the string stack from the model name
  vector<string> string_stack;
  if (smodel != "") 
    string_stack = split(smodel,'+');
  std::reverse(string_stack.begin(),string_stack.end());

  // Initialize the model stack 
  vector<OwnedPointer< ::Model> > model_stack;
  if (not process_stack_Markov(string_stack,model_stack,a,args)) {
    guess_markov_model(string_stack,a);
    if (not process_stack_Markov(string_stack,model_stack,a,args))
      throw myexception()<<"Can't guess the base CTMC model for alphabet '"<<a.name<<"'";
  }


  //-------- Run the model specification -----------//
  while(string_stack.size()) {
    int length = string_stack.size();

    process_stack_Markov(string_stack,model_stack,a,args);

    process_stack_Frequencies(string_stack,model_stack,a,frequencies);

    process_stack_Multi(string_stack,model_stack,frequencies);

    if (string_stack.size() == length)
      throw myexception()<<"Error: Couldn't process substitution model level \""<<string_stack.back()<<"\"";
  }

  //---------------------- Stack should be empty now ----------------------//
  if (model_stack.size()>1) {
    throw myexception()<<"Substitution model "<<model_stack.back()->name()<<" was specified but not used!\n";
  }

  return model_stack.back();
}


OwnedPointer<MultiModel>
get_smodel(const variables_map& args, const alphabet& a,const valarray<double>& frequencies) 
{
  //------------------ Get smodel ----------------------//
  string smodel_name = "";
  if (args.count("smodel")) smodel_name = args["smodel"].as<string>();

  OwnedPointer< ::Model> smodel = get_smodel(args,smodel_name,a,frequencies);

  // --------- Convert smodel to MultiModel ------------//
  OwnedPointer<MultiModel> full_smodel = get_MM(smodel.get(),"Final",frequencies);

  //------------ How does the tree fit in? -------------//
  if (args["letters"].as<string>() == "star") 
    full_smodel->full_tree = false;
  else
    full_smodel->full_tree = true;
      
  return full_smodel;
}

OwnedPointer<MultiModel> get_smodel(const variables_map& args, const alignment& A) {
  return get_smodel(args,A.get_alphabet(),empirical_frequencies(args,A));
}

