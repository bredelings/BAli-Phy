#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "imodel.H"
#include "smodel.H"
#include "substitution.H"
#include "parameters.H"
#include "rng.H"
#include "logsum.H"
#include "util.H"
#include "optimize.H"
#include "setup.H"

#include <boost/numeric/ublas/io.hpp>

using std::cout;
using std::cerr;
using std::endl;

using namespace optimize;

class substitution_score: public function {
  alignment A;
  SequenceTree T;
  substitution::MultiRateModel *smodel;
  IndelModel* imodel;

public:
  double operator()(const optimize::Vector&) const;

  substitution_score(const alignment& A1,
		     const substitution::MultiRateModel& SM,
		     const SequenceTree& T1)
    : A(A1),T(T1),smodel(SM.clone()),imodel(new SingleIndelModel(-6))
  { }

  ~substitution_score() {delete smodel;delete imodel;}
};

double substitution_score::operator()(const optimize::Vector& v) const {
  assert(v.size() == T.branches());

  SequenceTree T2 = T;
  for(int i=0;i<v.size();i++) {
    if (v[i] <= 0)
      return log_0;
    T2.branch(i).length() = v[i];
  }

  Parameters P(*smodel,*imodel,T2);

  return substitution::Pr(A,P);
}

double getSimilarity(const alignment& A,int s1,int s2) {
  int match=0;
  int total=0;
  for(int column=0;column<A.length();column++) {
    if (A.gap(column,s1) or A.gap(column,s2)) continue;
    total++;

    if (A(column,s1) == A(column,s2))
      match++;
  }
  return double(match)/total;
}

Matrix getSimilarity(const alignment& A) {
  const int n = A.size2()/2+1;
  Matrix S(n,n);

  for(int i=0;i<n;i++)
    for(int j=0;j<i;j++)
      S(i,j) = getSimilarity(A,i,j);

  return S;
}



int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    unsigned long seed = 0;
    if (args.set("seed")) {
      seed = convertTo<unsigned long>(args["seed"]);
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    cerr<<"random seed = "<<seed<<endl<<endl;
    
    cerr.precision(10);
    cout.precision(10);
    
    SequenceTree T;
    if (not args.set("tree"))
      throw myexception("Tree file not specified! (tree=<filename>)");
    T.read(args["tree"]);

    /* ----- Try to load alignment ------ */

    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");

    /* ----- Alphabets to try ------ */
    alphabet dna("DNA nucleotides","AGTC","N");
    alphabet rna("RNA nucleotides","AGUC","N");
    alphabet amino_acids("Amino Acids","ARNDCQEGHILKMFPSTWYV","X");
    vector<alphabet> alphabets;
    alphabets.push_back(dna);
    alphabets.push_back(rna);
    alphabets.push_back(amino_acids);


    std::ifstream afile(args["align"].c_str());    
    alignment A;
    A.load_phylip(alphabets,afile);
    afile.close();

    link(A,T);

    std::cout<<"Using alphabet: "<<A.get_alphabet().name<<endl<<endl;

    /*------------- Show Similarity/Distances between sequences ---------*/
    Matrix S = getSimilarity(A);

    std::cout<<"similarity = \n";
    for(int i=0;i<S.size1();i++) {
      std::cout<<A.seq(i).name<<"  ";
      for(int j=0;j<i;j++)
	std::cout<<S(i,j)<<"  ";
      std::cout<<"\n";
    }

    std::cout<<"\n";
    std::cout<<"distance = \n";
    for(int i=0;i<S.size1();i++) {
      std::cout<<A.seq(i).name<<"  ";
      for(int j=0;j<i;j++)
	std::cout<<1.0-S(i,j)<<"  ";
      std::cout<<"\n";
    }


    substitution::MultiRateModel* full_smodel = get_smodel(args,A);
    std::cout<<"Using substitution model: "<<full_smodel->name()<<endl;

    /* ------- Set up function to maximize -------- */

    substitution_score score(A,*full_smodel,T);
    delete full_smodel;

    optimize::Vector start(0.1,T.branches());
    optimize::Vector end = search_basis(start,score);

    std::cout<<"\n";
    std::cout<<"tree distances (input) = \n";
    for(int i=0;i<T.leaves();i++) {
      std::cout<<T.seq(i)<<"  ";
      for(int j=0;j<i;j++)
	std::cout<<T.distance(i,j)<<"  ";
      std::cout<<"\n";
    }

    for(int b=0;b<T.branches();b++)
      T.branch(b).length() = end[b];

    std::cout<<T<<std::endl;

    std::cout<<"\n";
    std::cout<<"tree distances (estimated) = \n";
    for(int i=0;i<T.leaves();i++) {
      std::cout<<T.seq(i)<<"  ";
      for(int j=0;j<i;j++)
	std::cout<<T.distance(i,j)<<"  ";
      std::cout<<"\n";
    }

  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;

}
