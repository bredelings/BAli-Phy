#include <vector>
#include "setup.H"
#include "util.H"
#include "rates.H"
#include "myexception.H"

using std::vector;
using std::valarray;

bool match(vector<string>& sstack,const string& s) {
  bool m = false;
  if (sstack.size() and sstack.back() == s) {
    m = true;
    sstack.pop_back();
  }
  return m;
}

substitution::MultiRateModel* get_smodel(Arguments& args, const alphabet& a,const valarray<double>& default_frequencies) {
  vector<string> smodel;
  if (args["smodel"] != "")
    smodel = split(args["smodel"],'+');
  std::reverse(smodel.begin(),smodel.end());

  /*------ Get the base markov model (Reversible Markov) ------*/
  substitution::ReversibleMarkovModel* base_markov_smodel = 0;

  OwnedPointer<AminoAcids> aa = AminoAcids();
  if (args.set("Use Stop"))
    *aa = AminoAcidsWithStop();


  if (match(smodel,"EQU"))
    base_markov_smodel = new substitution::EQU(a);
  else if (a == DNA())
    base_markov_smodel = new substitution::HKY(DNA());
  else if (a == RNA())
    base_markov_smodel = new substitution::HKY(RNA());
  else if (a == AminoAcids()) {
    string filename = "wag";
    if (args.set("Empirical"))
      filename = args["Empirical"];
    filename = string("Data/") + filename + ".dat";

    base_markov_smodel = new substitution::Empirical(*aa,filename);
  }
  else {
    Translation_Table DNA_table(Codons(DNA()),*aa,"Data/genetic_code_dna.dat");
    Translation_Table RNA_table(Codons(RNA()),*aa,"Data/genetic_code_rna.dat");

    if (a == DNA_table.getCodons())
      base_markov_smodel = new substitution::YangCodonModel(DNA_table);
    else if (a == RNA_table.getCodons())
      base_markov_smodel = new substitution::YangCodonModel(RNA_table);
    else
      assert(0);
  }

  /*------ Set frequencies for base markov model ------*/
  if (args.set("frequencies")) {
    vector<double> f = split<double>(args["frequencies"],',');
    assert(f.size() == a.size());

    valarray<double> f2(f.size());
    for(int i=0;i<f.size();i++)
      f2[i] = f[i];
    base_markov_smodel->frequencies(f2);
  }
  else 
    base_markov_smodel->frequencies(default_frequencies);

  /*-------- Get the base IA model -----------*/
  substitution::ReversibleModel* base_smodel=0;

  if (match(smodel,"gamma_branch"))
    base_smodel = new substitution::Gamma_Branch_Model(*base_markov_smodel);
  else if (match(smodel,"gamma_stretched_branch"))
    base_smodel = new substitution::Gamma_Stretched_Branch_Model(*base_markov_smodel);
  else if (match(smodel,"no_branch_lengths"))
    ;
  else
    base_smodel = base_markov_smodel;

  /*------ Get the multi-rate model over the base model ------*/
  substitution::MultiRateModel *full_smodel = 0;
  if (match(smodel,"gamma_plus_uniform")) {
    int n=4;
    if (args.set("gamma_plus_uniform") and args["gamma_plus_uniform"] != "gamma_plus_uniform")
      n = convertTo<int>(args["gamma_plus_uniform"]);
    full_smodel = new substitution::DistributionRateModel(*base_smodel,
							  substitution::Uniform() + substitution::Gamma(),
							  n);
  }
  else if (match(smodel,"gamma")) {
    int n=4;
    if (args.set("gamma") and args["gamma"] != "gamma")
      n = convertTo<int>(args["gamma"]);
    full_smodel = new substitution::GammaRateModel(*base_smodel,n);
  }
  else if (match(smodel,"double_gamma")) {
    int n=4;
    if (args.set("double_gamma") and args["double_gamma"] != "double_gamma")
      n = convertTo<int>(args["double_gamma"]);
    full_smodel = new substitution::DistributionRateModel(*base_smodel,
							  substitution::Gamma() + substitution::Gamma(),
							  n);
  }
  else 
    full_smodel = new substitution::SingleRateModel(*base_smodel);
  delete base_smodel;

  if (match(smodel,"INV")) {
    substitution::MultiRateModel *temp = full_smodel;
    full_smodel = new substitution::INV_Model(*full_smodel);
    delete temp;
  }

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
      

  //---------------------- Stack should be empty now ----------------------//
  if (smodel.size() != 0) {
    throw myexception()<<"Error: Couldn't process substitution model level \""<<smodel.back()<<"\"";
  }
  return full_smodel;
}

substitution::MultiRateModel* get_smodel(Arguments& args, const alignment& A) {
  return get_smodel(args,A.get_alphabet(),empirical_frequencies(args,A));
}

