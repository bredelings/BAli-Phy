#include "index-matrix.H"
#include <fstream>
#include <string>
#include <cmath>
#include <list>
#include <numeric>
#include "myexception.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "alignment-util.H"
#include "distance-methods.H"
#include "rng.H"
#include "alignment-util.H"
#include "statistics.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

void do_setup(const variables_map& args,vector<alignment>& alignments) 
{
  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();
  unsigned skip = args["skip"].as<unsigned>();

  // --------------------- try ---------------------- //
  std::cerr<<"Loading alignments...";
  list<alignment> As = load_alignments(std::cin,load_alphabets(args),skip,maxalignments);
  alignments.insert(alignments.begin(),As.begin(),As.end());
  std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;
  if (not alignments.size())
    throw myexception()<<"Alignment sample is empty.";
}

#undef NDEBUG

unsigned n_with_identity(const alignment& A,int s1,int s2,double I)
{
  // Get matches
  vector<int> F(A.length()+1);

  unsigned L=0;
  unsigned T = 0;
  F[0]=0;
  for(int i=0;i<A.length();i++) 
  {
    if (not A.character(i,s1) and not A.character(i,s2)) continue;

    L++;
    
    if (A(i,s1) == A(i,s2))
      T++;

    F[L] = T;
  }
  F.resize(L+1);

  // Get positions
  vector<int> FI(T+1);
  FI[0]=0;
  for(int i=0;i<L;i++)
    if (F[i+1] > F[i])
      FI[F[i+1]] = i+1;

  // tag positions that 
  vector<int> tagged(L,0);

  const unsigned w = 4;
  for(int i=1;i<=T;i++) {
    for(int j=20;j>=w;j--) {
      int i2 = i+j;
      if (i2 > T) continue;
      assert(FI[i]  > 0 and FI[i]  <=L);
      assert(FI[i2] > 0 and FI[i2] <=L);
      assert(FI[i2] > FI[i]);

      if (double(i2-i+1)/(FI[i2]-FI[i]+1) > I) {
	for(int k=FI[i];k<=FI[i2];k++)
	  tagged[k-1]=1;
	break;
      }
    }
  }

  return sum(tagged);
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ("with-indels", "Calculate percent-identity w/ indels")
    ("seed", value<unsigned long>(),"random seed")
    ("skip",value<unsigned>()->default_value(0),"number of tree samples to skip")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("cutoff",value<double>()->default_value(0.75),"ignore events below this probability")
    ("identity",value<double>()->default_value(0.4),"Find fraction of sequences that have this level of identity.")
    ("strict","require all implied pairs pass the cutoff")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-identity [OPTIONS] < sequence-file\n";
    cout<<"Report statistics on pairwise identity.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);
    bool strict = args.count("strict")>0;

    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    cerr<<"random seed = "<<seed<<endl<<endl;
    
    //------------ Load alignment and tree ----------//
    vector<alignment> alignments;
    vector<ublas::matrix<int> > Ms;

    do_setup(args,alignments);
    for(int i=0;i<alignments.size();i++)
      alignments[i] = chop_internal(alignments[i]);

    if (not alignments.size()) 
      throw myexception()<<"Didn't read any alignments!";      

    int N = alignments[0].n_sequences();
    if (alignments.size() > 1) {
      assert(alignments[1].n_sequences() == N);
      assert(alignments[1].seqlength(N-1) == alignments[0].seqlength(N-1));
    }
    const alignment& A = alignments[0];

    vector<int> L(N);
    for(int i=0;i<L.size();i++)
      L[i] = A.seqlength(i);

    
    //--------- Construct alignment indexes ---------//
    for(int i=0;i<alignments.size();i++)
      Ms.push_back(M(alignments[i]));


    //--------- Get list of supported pairs ---------//
    Edges E(L);

    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<s1;s2++)
	add_edges(E,Ms,s1,s2,L[s1],L[s2],0.5);


    E.build_index();
    //--------- Build alignment from list ---------//
    double cutoff = args["cutoff"].as<double>();
    if (cutoff < 0.5) cutoff = 0.5;

    //-------- Build a beginning alignment --------//
    index_matrix M = unaligned_matrix(L);

    M.merge(E,cutoff,strict);

    ublas::matrix<int> M2 = get_ordered_matrix(M);

    alignment consensus = get_alignment(M2,alignments[0]);

    //---------- Get %identity ------------//
    bool gaps_count = args.count("with-indels");
    vector<vector<valarray<double> > > identity(N,vector<valarray<double> >(N));
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<N;s2++) 
	  identity[s1][s2].resize(alignments.size());

    for(int i=0;i<alignments.size();i++)
    {
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<N;s2++)
	  identity[s1][s2][i] = fraction_identical(alignments[i],s1,s2,gaps_count);
    }

    Matrix identity_median(N,N);
    Matrix identity_Q1(N,N);
    Matrix identity_Q2(N,N);
    const double p = 0.05;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++) {
	identity_median(s1,s2) = statistics::median(identity[s1][s2]);
	identity_Q1(s1,s2) = statistics::quantile(identity[s1][s2],p/2);
	identity_Q2(s1,s2) = statistics::quantile(identity[s1][s2],1.0-p/2);
      }

    int s1_min=0; int s2_min=0;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	if (identity_median(s1,s2) < identity_median(s1_min,s2_min)) {
	  s1_min = s1;
	  s2_min = s2;
	}

    cout<<"Min identity = "<<identity_median(s1_min,s2_min)<<" ("<<identity_Q1(s1_min,s2_min)<<","<<identity_Q2(s1_min,s2_min)<<")  ["<<A.seq(s1_min).name<<","<<A.seq(s2_min).name<<"]"<<endl;
    
    //---------- Get % WITH identity I ------------//
    const double I = args["identity"].as<double>();
    vector<vector<valarray<double> > > ifraction(N,vector<valarray<double> >(N));
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<N;s2++) 
	  ifraction[s1][s2].resize(alignments.size());

    for(int i=0;i<alignments.size();i++)
    {
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<N;s2++)
	  ifraction[s1][s2][i] = n_with_identity(alignments[i],s1,s2,I);
    }

    Matrix ifraction_median(N,N);
    Matrix ifraction_Q1(N,N);
    Matrix ifraction_Q2(N,N);
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++) {
	ifraction_median(s1,s2) = statistics::median(ifraction[s1][s2]);
	ifraction_Q1(s1,s2) = statistics::quantile(ifraction[s1][s2],p/2);
	ifraction_Q2(s1,s2) = statistics::quantile(ifraction[s1][s2],1.0-p/2);
      }

    s1_min=0; s2_min=0;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	if (ifraction_median(s1,s2) < ifraction_median(s1_min,s2_min)) {
	  s1_min = s1;
	  s2_min = s2;
	}

    cout<<"Min ifraction = "<<ifraction_median(s1_min,s2_min)<<" ("<<ifraction_Q1(s1_min,s2_min)<<","<<ifraction_Q2(s1_min,s2_min)<<")  ["<<A.seq(s1_min).name<<","<<A.seq(s2_min).name<<"]"<<endl;
    
    //---------- Get %-homology > cutoff --------------//
    Matrix percent_aligned(N,N);
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	percent_aligned(s1,s2) = fraction_homologous(consensus,s1,s2);

    s1_min=0;s2_min=0;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	if (percent_aligned(s1,s2) < percent_aligned(s1_min,s2_min)) {
	  s1_min = s1;
	  s2_min = s2;
	}

    cout<<"Min %aligned = "<<percent_aligned(s1_min,s2_min)<<"  ["<<A.seq(s1_min).name<<","<<A.seq(s2_min).name<<"]"<<endl;
    
    //---------- Get #-homology > cutoff --------------//
    Matrix n_aligned(N,N);
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	n_aligned(s1,s2) = n_homologous(consensus,s1,s2);

    s1_min=0;s2_min=0;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	if (n_aligned(s1,s2) < n_aligned(s1_min,s2_min)) {
	  s1_min = s1;
	  s2_min = s2;
	}

    cout<<"Min #aligned = "<<n_aligned(s1_min,s2_min)<<"  ["<<A.seq(s1_min).name<<","<<A.seq(s2_min).name<<"]"<<endl;
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
