#include <vector>
#include "setup.H"
#include "util.H"
#include "rates.H"
#include "myexception.H"

using std::vector;
using std::valarray;
using namespace substitution;

/// Take something off the string stack, if its present
bool match(vector<string>& sstack,const string& s) {
  bool m = false;
  if (sstack.size() and sstack.back() == s) {
    m = true;
    sstack.pop_back();
  }
  return m;
}

/// If no markov model is specified, try to add one.
void guess_markov_model(vector<string>& string_stack,const alphabet& a) {
  if (dynamic_cast<const Nucleotides*>(&a)) 
    string_stack.push_back("HKY");
  else if (dynamic_cast<const AminoAcids*>(&a))
    string_stack.push_back("Empirical");
  else if (dynamic_cast<const Codons*>(&a))
    string_stack.push_back("YangCodonModel");
}

bool process_stack_Markov(vector<string>& string_stack,
			  vector<OwnedPointer<substitution::Model> >& model_stack,
			  const alphabet& a,
			  Arguments& args) 
{
  //------ Get the base markov model (Reversible Markov) ------//
  OwnedPointer<AminoAcids> AA = AminoAcids();
  if (args.set("Use Stop"))
    *AA = AminoAcidsWithStop();

  if (match(string_stack,"EQU"))
    model_stack.push_back(EQU(a));
  else if (match(string_stack,"HKY")) {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(HKY(*N));
    else
      throw myexception()<<"HKY:: Unrecognized alphabet '"<<a.name<<"'";
  }
  else if (match(string_stack,"TNY")) {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(TNY(*N));
    else
      throw myexception()<<"TNY:: Unrecognized alphabet '"<<a.name<<"'";
  }
  else if (match(string_stack,"Empirical")) {
    if (not args.set("Empirical"))
      args["Empirical"] = "wag";
    string filename = args["Empirical"];
    filename = args["datadir"] + "/" + filename + ".dat";

    model_stack.push_back(Empirical(a,filename));
  }
  else if (match(string_stack,"YangCodonModel")) {
    string dna_filename = args["datadir"] + "/" + "genetic_code_dna.dat";
    string rna_filename = args["datadir"] + "/" + "genetic_code_rna.dat";

    Codons DNA_codons(DNA(),*AA,dna_filename);
    Codons RNA_codons(RNA(),*AA,rna_filename);

    if (a == DNA_codons)
      model_stack.push_back(YangCodonModel(DNA_codons,HKY(DNA())));
    else if (a == RNA_codons)
      model_stack.push_back(YangCodonModel(RNA_codons,HKY(RNA())));
    else
      throw myexception()<<"Can't figure out how to make a codon model from alphabet '"<<a.name<<";";
  }
  else
    return false;

  return true;
}


bool process_stack_IA(vector<string>& string_stack,  
		      vector<OwnedPointer<substitution::Model> >& model_stack,
		      const alphabet& a,
		      Arguments& args) 
{
  ReversibleMarkovModel* markov = dynamic_cast<ReversibleMarkovModel*>(model_stack.back().get());

  if (match(string_stack,"gamma_branch")) {
    if (markov)
      model_stack.back() = Gamma_Branch_Model(*markov);
    else if (model_stack.empty())
      throw myexception()<<"gamma_branch: no model to use.";
    else 
      throw myexception()<<"gamma_branch: couldn't find a Markov model to use.";
    
  }
  else if (match(string_stack,"gamma_stretched_branch")) {
    if (markov)
      model_stack.back() = Gamma_Stretched_Branch_Model(*markov);
    else if (model_stack.empty())
      throw myexception()<<"gamma_branch: no model to use.";
    else
      throw myexception()<<"gamma_stretched_branch: couldn't find a Markov model to use.";
  }
  else if (match(string_stack,"no_branch_lengths"))
    ;
  else
    return false;
  return true;
}


bool process_stack_Multi(vector<string>& string_stack,  
			 vector<OwnedPointer<substitution::Model> >& model_stack,
			 const alphabet& a,
			 Arguments& args) 
{

  ReversibleAdditiveModel* RA = dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get());
  MultiModel* MM = dynamic_cast<MultiModel*>(model_stack.back().get());
  if (match(string_stack,"single")) {
    if (RA)
      model_stack.back() = UnitModel(*RA);
    else if (model_stack.empty())
      throw myexception()<<"single: couldn't find any model to use.";
    else
      throw myexception()<<"single: couldn't find a reversible+additive model to use.";
  }
  else if (match(string_stack,"gamma_plus_uniform")) {
    int n=4;
    if (args.set("gamma_plus_uniform") and args["gamma_plus_uniform"] != "gamma_plus_uniform")
      n = convertTo<int>(args["gamma_plus_uniform"]);
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
  else if (match(string_stack,"gamma")) {
    int n=args.loadvalue("gamma_bins",4);

    if (MM)
      model_stack.back() = GammaParameterModel(*MM,n);
    else if (RA)
      model_stack.back() = GammaParameterModel(UnitModel(*RA),n);
    else if (model_stack.empty())
      throw myexception()<<"gamma_plus_uniform: couldn't find any model to use.";
    else
      throw myexception()<<"gamma: couldn't find a reversible+additive model to use.";
  }
  else if (match(string_stack,"double_gamma")) {
    int n=args.loadvalue("double_gamma_bins",4);

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
  else if (match(string_stack,"multi_freq")) {
    int n = args.loadvalue("multi_freq_bins",4);
    if (MM)
      model_stack.back() = MultiFrequencyModel(*MM,4);
    else if (model_stack.empty())
      throw myexception()<<"multi_freq: couldn't find any model to use.";
    else if (RA)
      model_stack.back() = MultiFrequencyModel(UnitModel(*RA),n);
    else
      throw myexception()<<"We can only create multi_freq models on top of MultiRateModels or Markov models";
  }
  else if (match(string_stack,"INV")) {
    if (MM)
      model_stack.back() = WithINV(*MM);
    else if (model_stack.empty())
      throw myexception()<<"gamma_plus_uniform: couldn't find any model to use.";
    else if (RA)
      model_stack.back() = WithINV(UnitModel(*RA));
    else
      throw myexception()<<"We can only create INV models on top of MultiRateModels or Markov models";
  }
  else if (match(string_stack,"Dual")) {
    if (model_stack.size() < 2)
      throw myexception()<<"Dual: can't find 2 models to combine";

    OwnedPointer<MultiModel> M2;
    if (dynamic_cast<MultiModel*>(model_stack.back().get()) )
      M2 = *dynamic_cast<MultiModel*>(model_stack.back().get());
    else if (dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get()) )
      M2 = UnitModel(*dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get()) );
    else
      throw myexception()<<"Dual: second model isn't reversible+additive or a multi-model";
    model_stack.pop_back();

    OwnedPointer<MultiModel> M1;
    if (dynamic_cast<MultiModel*>(model_stack.back().get()) )
      M1 = *dynamic_cast<MultiModel*>(model_stack.back().get());
    else if (dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get()) )
      M1 = UnitModel(*dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get()) );
    else
      throw myexception()<<"Dual: first model isn't reversible+additive or a multi-model";
    model_stack.pop_back();

    vector <OwnedPointer<MultiModel> > models;
    models.push_back(M1);
    models.push_back(M2);

    model_stack.push_back(DualModel(models));
  }
  else
    return false;
  return true;
}


//FIXME - use dynamic_cast to see if we are in certain subclasses...
OwnedPointer<MultiModel>
get_smodel(Arguments& args, const alphabet& a,const valarray<double>& default_frequencies) {

  vector<string> string_stack;
  if (args["smodel"] != "")
    string_stack = split(args["smodel"],'+');
  std::reverse(string_stack.begin(),string_stack.end());

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

  ReversibleAdditiveModel* RA = dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get());
  MultiModel* MM = dynamic_cast<MultiModel*>(model_stack.back().get());
  OwnedPointer<MultiModel> full_smodel;

  if (MM)
    full_smodel = *MM;
  else if (RA)
    full_smodel = UnitModel(*RA);
  else 
    throw myexception()<<"Model cannot be converted to a MultiModel";

  /*------ Set the parameters for all levels of the model ------*/
  if (args.set("s_parameters")) {
    vector<double> p = split<double>(args["s_parameters"],',');
    if (p.size() != full_smodel->parameters().size())
      throw myexception()<<"Substitution model "<<full_smodel->name()<<
	" takes "<<full_smodel->parameters().size()<<" parameters, but you have supplied "
			 <<p.size();
    full_smodel->parameters(p);
  }

  /* ---------- How does the tree fit in? ------------*/
  if (args["letters"]== "star") 
    full_smodel->full_tree = false;
  else
    full_smodel->full_tree = true;
      

  //------ Set frequencies for base markov model ------//
  if (args.set("frequencies")) {
    vector<double> f = split<double>(args["frequencies"],',');
    if (f.size() != a.size())
      throw myexception()<<"You specified "<<f.size()<<" frequencies, but there are "<<a.size()<<" letters of the alphabet!";

    valarray<double> f2(f.size());
    for(int i=0;i<f.size();i++)
      f2[i] = f[i];
    full_smodel->frequencies(f2);
  }
  else 
    full_smodel->frequencies(default_frequencies);

  return full_smodel;
}

OwnedPointer<MultiModel> get_smodel(Arguments& args, const alignment& A) {
  return get_smodel(args,A.get_alphabet(),empirical_frequencies(args,A));
}

