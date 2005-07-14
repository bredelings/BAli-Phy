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
    string_stack.push_back("YangM0");
}

OwnedPointer<substitution::Model> 
get_smodel(const variables_map& args,const string& smodel,const alphabet& a);

bool process_stack_Markov(vector<string>& string_stack,
			  vector<OwnedPointer<substitution::Model> >& model_stack,
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
      throw myexception()<<"HKY:: Unrecognized alphabet '"<<a.name<<"'";
  }
  else if (match(string_stack,"TNY",arg)) {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(TNY(*N));
    else
      throw myexception()<<"TNY:: Unrecognized alphabet '"<<a.name<<"'";
  }
  else if (match(string_stack,"Empirical",arg)) {
    string filename = arg;
    if (filename.empty()) filename = "wag";
    filename = args["data-dir"].as<string>() + "/" + filename + ".dat";

    model_stack.push_back(Empirical(a,filename));
  }
  else if (match(string_stack,"YangM0",arg)) 
  {
    const Codons* C = dynamic_cast<const Codons*>(&a);
    if (not C)
      throw myexception()<<"Can't figure out how to make a codon model from non-codon alphabet '"<<a.name<<"'";

    OwnedPointer<ReversibleMarkovNucleotideModel> N_submodel = HKY(C->getNucleotides());

    if (not arg.empty()) {
      OwnedPointer<substitution::Model> submodel = get_smodel(args,arg,C->getNucleotides());
      ReversibleMarkovNucleotideModel* temp = dynamic_cast<ReversibleMarkovNucleotideModel*>( submodel.get());
      if (not temp)
	throw myexception()<<"Submodel '"<<arg<<"' for YangM0 is not a reversible Markov nucleotide model.";
      N_submodel = temp->clone();
    }

    model_stack.push_back( YangM0(*C, *N_submodel) );
  }
  else
    return false;

  return true;
}


bool process_stack_IA(vector<string>& string_stack,  
		      vector<OwnedPointer<substitution::Model> >& model_stack,
		      const alphabet& a,
		      const variables_map& args) 
{
  ReversibleMarkovModel* markov = dynamic_cast<ReversibleMarkovModel*>(model_stack.back().get());
  string arg;
  if (match(string_stack,"gamma_branch",arg)) {
    if (markov)
      model_stack.back() = Gamma_Branch_Model(*markov);
    else if (model_stack.empty())
      throw myexception()<<"gamma_branch: no model to use.";
    else 
      throw myexception()<<"gamma_branch: couldn't find a Markov model to use.";
    
  }
  else if (match(string_stack,"gamma_stretched_branch",arg)) {
    if (markov)
      model_stack.back() = Gamma_Stretched_Branch_Model(*markov);
    else if (model_stack.empty())
      throw myexception()<<"gamma_branch: no model to use.";
    else
      throw myexception()<<"gamma_stretched_branch: couldn't find a Markov model to use.";
  }
  else if (match(string_stack,"no_branch_lengths",arg))
    ;
  else
    return false;
  return true;
}


bool process_stack_Multi(vector<string>& string_stack,  
			 vector<OwnedPointer<substitution::Model> >& model_stack,
			 const alphabet& a,
			 const variables_map& args) 
{

  ReversibleAdditiveModel* RA = dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get());
  MultiModel* MM = dynamic_cast<MultiModel*>(model_stack.back().get());
  string arg;
  if (match(string_stack,"single",arg)) {
    if (RA)
      model_stack.back() = UnitModel(*RA);
    else if (model_stack.empty())
      throw myexception()<<"single: couldn't find any model to use.";
    else
      throw myexception()<<"single: couldn't find a reversible+additive model to use.";
  }
  else if (match(string_stack,"gamma_plus_uniform",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);
    if (RA)
      model_stack.back() = DistributionParameterModel(UnitModel(*RA),
						      Uniform() + Gamma(),
						      -1,//rate!
						      n);
    else if (model_stack.empty())
      throw myexception()<<"gamma_plus_uniform: couldn't find any model to use.";
    else
      throw myexception()<<"gamma_plus_uniform: couldn't find a reversible+additive model to use.";

  }
  else if (match(string_stack,"gamma",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    if (MM)
      model_stack.back() = GammaParameterModel(*MM,n);
    else if (RA)
      model_stack.back() = GammaParameterModel(UnitModel(*RA),n);
    else if (model_stack.empty())
      throw myexception()<<"gamma_plus_uniform: couldn't find any model to use.";
    else
      throw myexception()<<"gamma: couldn't find a reversible+additive model to use.";
  }
  else if (match(string_stack,"double_gamma",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    if (RA)
      model_stack.back() = DistributionParameterModel(UnitModel(*RA),
						      Gamma() + Gamma(),
						      -1,//rate!
						      n);
    else if (model_stack.empty())
      throw myexception()<<"gamma_plus_uniform: couldn't find any model to use.";
    else
      throw myexception()<<"gamma: couldn't find a reversible+additive model to use.";
  }
  else if (match(string_stack,"multi_freq",arg)) {
    int n=4;
    if (not arg.empty())
      n = convertTo<int>(arg);

    if (MM)
      model_stack.back() = MultiFrequencyModel(*MM,4);
    else if (model_stack.empty())
      throw myexception()<<"multi_freq: couldn't find any model to use.";
    else if (RA)
      model_stack.back() = MultiFrequencyModel(UnitModel(*RA),n);
    else
      throw myexception()<<"We can only create multi_freq models on top of MultiRateModels or Markov models";
  }
  else if (match(string_stack,"INV",arg)) {
    if (MM)
      model_stack.back() = WithINV(*MM);
    else if (model_stack.empty())
      throw myexception()<<"gamma_plus_uniform: couldn't find any model to use.";
    else if (RA)
      model_stack.back() = WithINV(UnitModel(*RA));
    else
      throw myexception()<<"We can only create INV models on top of MultiRateModels or Markov models";
  }
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
      OwnedPointer<MultiModel> M;
      if (dynamic_cast<MultiModel*>(model_stack.back().get()) )
	M = *dynamic_cast<MultiModel*>(model_stack.back().get());
      else if (dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get()) )
	M = UnitModel(*dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get()) );
      else
	throw myexception()<<"Mixture: model "<<m<<"isn't reversible+additive or a multi-model";
      model_stack.pop_back();
      models.push_back(M);
    }

    model_stack.push_back(MixtureModel(models));
  }
  else if (match(string_stack,"YangM2",arg)) {
    if (not dynamic_cast<YangM0*>(model_stack.back().get()))
      throw myexception()<<"Trying to construct a Yang M2 model from a '"<<model_stack.back().get()->name()
			 <<"' model, which is not a YangM0 model.";

    OwnedPointer<YangM0> YM = *dynamic_cast<YangM0*>(model_stack.back().get());
    model_stack.pop_back();
    model_stack.push_back(YangM2(*YM));
  }
  else
    return false;
  return true;
}

OwnedPointer<substitution::Model> 
get_smodel(const variables_map& args,const string& smodel,const alphabet& a) 
{
  // Initialize the string stack from the model name
  vector<string> string_stack;
  if (smodel != "") 
    string_stack = split(smodel,'+');
  std::reverse(string_stack.begin(),string_stack.end());

  // Initialize the model stack 
  vector<OwnedPointer<substitution::Model> > model_stack;
  if (not process_stack_Markov(string_stack,model_stack,a,args)) {
    guess_markov_model(string_stack,a);
    if (not process_stack_Markov(string_stack,model_stack,a,args))
      throw myexception()<<"Can't guess the base CTMC model for alphabet '"<<a.name<<"'";
  }


  //-------- Run the model specification -----------//
  while(string_stack.size()) {
    int length = string_stack.size();

    process_stack_Markov(string_stack,model_stack,a,args);

    process_stack_IA(string_stack,model_stack,a,args);

    process_stack_Multi(string_stack,model_stack,a,args);

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

  OwnedPointer<substitution::Model> smodel = get_smodel(args,smodel_name,a);

  // --------- Convert smodel to MultiModel ------------//
  ReversibleAdditiveModel* RA = dynamic_cast<ReversibleAdditiveModel*>(smodel.get());
  MultiModel* MM = dynamic_cast<MultiModel*>(smodel.get());
  OwnedPointer<MultiModel> full_smodel;

  if (MM)
    full_smodel = *MM;
  else if (RA)
    full_smodel = UnitModel(*RA);
  else 
    throw myexception()<<"Model cannot be converted to a MultiModel";

  //------------ How does the tree fit in? -------------//
  if (args["letters"].as<string>() == "star") 
    full_smodel->full_tree = false;
  else
    full_smodel->full_tree = true;
      

  //------ Set frequencies for base markov model -------//
  full_smodel->frequencies(frequencies);
  
  return full_smodel;
}

OwnedPointer<MultiModel> get_smodel(const variables_map& args, const alignment& A) {
  return get_smodel(args,A.get_alphabet(),empirical_frequencies(args,A));
}

