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
			  vector<OwnedPointer<Model> >& model_stack,
			  const alphabet& a,
			  Arguments& args) 
{
  //------ Get the base markov model (Reversible Markov) ------//
  OwnedPointer<AminoAcids> aa = AminoAcids();
  if (args.set("Use Stop"))
    *aa = AminoAcidsWithStop();

  if (match(string_stack,"EQU"))
    model_stack.push_back(EQU(a));
  else if (match(string_stack,"HKY")) {
    const Nucleotides* N = dynamic_cast<const Nucleotides*>(&a);
    if (N)
      model_stack.push_back(HKY(*N));
    else
      throw myexception()<<"HKY:: Unrecognized alphabet '"<<a.name<<"'";
  }
  else if (match(string_stack,"Empirical")) {
    if (not args.set("Empirical"))
      args["Empirical"] = "wag";
    string filename = args["Empirical"];
    filename = args["datadir"] + "/" + filename + ".dat";

    model_stack.push_back(Empirical(*aa,filename));
  }
  else if (match(string_stack,"YangCodonModel")) {
    string dna_filename = args["datadir"] + "/" + "genetic_code_dna.dat";
    string rna_filename = args["datadir"] + "/" + "genetic_code_rna.dat";

    Translation_Table DNA_table(Codons(DNA()),*aa,dna_filename);
    Translation_Table RNA_table(Codons(RNA()),*aa,rna_filename);

    if (a == DNA_table.getCodons())
      model_stack.push_back(YangCodonModel(DNA_table));
    else if (a == RNA_table.getCodons())
      model_stack.push_back(YangCodonModel(RNA_table));
    else
    throw myexception()<<"Can't figure out how to make a codon model from alphabet '"<<a.name<<";";
  }

  return true;
}


bool process_stack_IA(vector<string>& string_stack,  
		      vector<OwnedPointer<Model> >& model_stack,
		      const alphabet& a,
		      Arguments& args) 
{
  ReversibleMarkovModel* markov = dynamic_cast<ReversibleMarkovModel*>(model_stack.back().get());
  if (match(string_stack,"gamma_branch")) {
    if (markov)
      model_stack.back() = Gamma_Branch_Model(*markov);
    else
      throw myexception()<<"gamma_branch: couldn't find a Markov model to use.";
  }
  else if (match(string_stack,"gamma_stretched_branch")) {
    if (markov)
      model_stack.back() = Gamma_Stretched_Branch_Model(*markov);
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
			 vector<OwnedPointer<Model> >& model_stack,
			 const alphabet& a,
			 Arguments& args) 
{
  ReversibleAdditiveModel* RA = dynamic_cast<ReversibleAdditiveModel*>(model_stack.back().get());
  MultiRateModel* MRM = dynamic_cast<MultiRateModel*>(model_stack.back().get());

  if (match(string_stack,"gamma_plus_uniform")) {
    int n=4;
    if (args.set("gamma_plus_uniform") and args["gamma_plus_uniform"] != "gamma_plus_uniform")
      n = convertTo<int>(args["gamma_plus_uniform"]);
    if (RA)
      model_stack.back() = DistributionRateModel(*RA,
						 Uniform() + Gamma(),
						 n);
    else
      throw myexception()<<"gamma_plus_uniform: couldn't find a reversible+additive model to use.";

  }
  else if (match(string_stack,"gamma")) {
    int n=4;
    if (args.set("gamma") and args["gamma"] != "gamma")
      n = convertTo<int>(args["gamma"]);

    if (RA)
      model_stack.back() = GammaRateModel(*RA,n);
    else
      throw myexception()<<"gamma: couldn't find a reversible+additive model to use.";
  }
  else if (match(string_stack,"double_gamma")) {
    int n=4;
    if (args.set("double_gamma") and args["double_gamma"] != "double_gamma")
      n = convertTo<int>(args["double_gamma"]);

    if (RA)
      model_stack.back() = DistributionRateModel(*RA,
						 Gamma() + Gamma(),
						 n);
    else
      throw myexception()<<"gamma: couldn't find a reversible+additive model to use.";
  }
  else if (match(string_stack,"INV")) {
    if (MRM)
      model_stack.back() = INV_Model(*MRM);
    else if (RA)
      model_stack.back() = INV_Model(SingleRateModel(*RA));
    else
      throw myexception()<<"We can only create INV models on top of MultiRateModels or Markov models";
  }
  else if (match(string_stack,"Dual")) {
    if (model_stack.size() < 2)
      throw myexception()<<"Dual: can't find 2 models to combine";

    OwnedPointer<MultiModel> M2 ( dynamic_cast<MultiModel*>(model_stack.back().get()) );
    model_stack.pop_back();
    OwnedPointer<MultiModel> M1 ( dynamic_cast<MultiModel*>(model_stack.back().get()) );
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

  vector<OwnedPointer<Model> > model_stack;

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
    full_smodel = SingleRateModel(*RA);
  else 
    throw myexception()<<"Model cannot be convert to a MultiModel";

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
    assert(f.size() == a.size());

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

