#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <list>
#include <numeric>
#include <set>
#include "myexception.H"
#include "alignment.H"
#include "mytypes.H"
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

  // --------------- Alphabets to try --------------- //
  vector<OwnedPointer<alphabet> > alphabets;
  alphabets.push_back(DNA());
  alphabets.push_back(RNA());
  alphabets.push_back(AminoAcids());

  // --------------------- try ---------------------- //
  std::cerr<<"Loading alignments...";
  list<alignment> As = load_alignments(std::cin,alphabets,maxalignments);
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
    ("with-indels", "Calculate percent-identity w/ indels")
    ("seed", value<unsigned long>(),"random seed")
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



struct Edge 
{
  int s1;
  int x1;

  int s2;
  int x2;

  double p;
};

struct edge_comp {
bool operator()(const Edge& E1, const Edge& E2) {
  return E1.p  > E2.p;
}
};

class Edges: public std::multiset<Edge,edge_comp>
{
  vector<vector<vector<std::multiset<Edge,edge_comp>::iterator> > > lookup;
public:
  void build_index();

  double PP(int s1,int x1,int s2,int x2) const;
  int index_in_sequence(int s1,int x1,int s2) const;

  Edges(const vector<int>& L);
};


Edges::Edges(const vector<int>& L)
  :lookup(L.size())
{
  for(int i=0;i<L.size();i++)
    lookup[i].resize(L.size());

  for(int i=0;i<L.size();i++)
    for(int j=0;j<L.size();j++) {
      lookup[i][j].clear();
      lookup[i][j].resize(L[i],end());
    }
}

void Edges::build_index() {
  foreach(e,*this) {
    if (e->x1 >= 0)
      lookup[e->s1][e->s2][e->x1] = e;

    if (e->x2 >= 0)
      lookup[e->s2][e->s1][e->x2] = e;
  }
}

double Edges::PP(int s1, int x1, int s2, int x2) const 
{
  assert(x1 >= 0);

  if (lookup[s1][s2][x1] == end())
    return 0;

  const Edge& e = *lookup[s1][s2][x1];
  if (e.s1 != s1) {
    std::swap(s1,s2);
    std::swap(x1,x2);
    assert(e.s1 == s1);
  }

  if (e.x2 == x2)
    return e.p;
  else
    return 0;
}

int Edges::index_in_sequence(int s1,int x1,int s2) const 
{
  assert(x1 >= 0);

  if (lookup[s1][s2][x1] == end())
    return -3;

  const Edge& e = *lookup[s1][s2][x1];
  if (e.s1 == s1) {
    return e.x2;
  }
  else {
    assert(e.s1 == s2);
    return e.x1;
  }
}


void add_edges(Edges& E, const vector< ublas::matrix<int> >& Ms,
		   int s1,int s2,int L1, int L2) 
{ 
  ublas::matrix<int> count(L1+1,L2+1);
  for(int i=0;i<count.size1();i++)
    for(int j=0;j<count.size2();j++)
      count(i,j) = 0;

  // get counts of each against each
  for(int i=0;i<Ms.size();i++) {
    const ublas::matrix<int>& M = Ms[i];

    for(int c=0;c<M.size1();c++) {
      int index1 = M(c,s1);
      int index2 = M(c,s2);
      count(index1 + 1, index2 + 1)++;
    }
  }
  count(0,0) = 0;


  // determine Ml pairs
  for(int i=0;i<count.size1();i++) 
    for(int j=0;j<count.size2();j++) 
    {
      if (2*count(i,j) > Ms.size()) {
	Edge e;
	e.s1 = s1;
	e.x1 = i-1;

	e.s2 = s2;
	e.x2 = j-1;

	e.p  = double(count(i,j))/Ms.size();

	E.insert(e);
      }
    }
}

class matrix: public ublas::matrix<int> 
{
  vector<vector<int> > column_index;

public:

  int  index(int i,int j) const {return (*this)(i,j);}
  int& index(int i,int j)       {return (*this)(i,j);}

  int  column(int i,int j) const {assert(j>=0); return column_index[i][j];}
  int& column(int i,int j)       {assert(j>=0); return column_index[i][j];}

  int length(int i) const {return column_index[i].size();}

  bool columns_conflict(int c1, int c2) const;

  bool consistent(int c1, int s,int x, const Edges& E,double cutoff) const;
  bool consistent(int c1, int c2,const Edges& E, double cutoff) const;

  void merge_columns(int c1,int c2);
  void merge_simple(const Edges& E,double p);
  void merge_strict(const Edges& E,double p);

  matrix(int C,const vector<int>& L)
    :ublas::matrix<int>(C,L.size())
  {
    for(int i=0;i<L.size();i++)
      column_index.push_back(vector<int>(L[i]));
  }
};

matrix unaligned_matrix(const vector<int>& L) 
{
  int C = 0;
  for(int i=0;i<L.size();i++)
    C += L[i];

  matrix M(C,L);
  
  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      M(i,j) = -3;
  
  int c=0;
  for(int i=0;i<M.size2();i++) {
    for(int j=0;j<M.length(i);j++,c++) {
      M.column(i,j) = c;
      M(c,i) = j;
    }
  }
  assert(c == C);

  return M;
}

bool matrix::columns_conflict(int c1, int c2) const
{
  for(int i=0;i<size2();i++) {

    // if either value is 'unknown', then we can't conflict
    if (index(c1,i) == -3 or index(c2,i) == -3)
      continue;

    // two gaps can be merged
    if (index(c1,i) == -1 and index(c2,i) == -1)
      continue;

    // two letters, or a letter and a gap
    return true;
  }

  return false;
}

bool matrix::consistent(int c, int s2,int x2, const Edges& E,double cutoff) const 
{
  bool ok = true;
  for(int s1=0;s1<size2() and true;s1++) {
    int x1 = index(c,s1);
    if (x1 < 0) continue;

    if (E.PP(s1,x1,s2,x2) < cutoff)
      ok = false;
  }
  return ok;
}

bool matrix::consistent(int c1, int c2,const Edges& E, double cutoff) const 
{
  for(int s2=0;s2<size2() and true;s2++) {
    int x2 = index(c2,s2);
    if (x2 == -3) continue;

    if (not consistent(c1,s2,x2,E,cutoff)) return false;
  }
  return true;
}

void matrix::merge_columns(int c1, int c2) 
{
  if (c1 > c2) std::swap(c1,c2);

  for(int i=0;i<size2();i++) 
  {
    // don't need to move an 'unknown'
    if (index(c2,i) == -3)
      continue;

    // need to move a 'gap', and can merge with another 'gap'
    if (index(c2,i) == -1) {
      assert(index(c1,i) == -3 or index(c1,i) == -1);
      index(c2,i) = -3;
      index(c1,i) = -1;
    }

    // need to move a letter, and cannot merge w/ anything.
    else {
      assert(index(c2,i) >= 0);

      assert(index(c1,i) == -3);

      index(c1,i) = index(c2,i);

      column(i,index(c1,i)) = c1;
    }
  }  
}


bool skips(const ublas::matrix<int>& M,int c,const vector<int>& index) {
  for(int i=0;i<M.size2();i++) {
    if (M(c,i) < 0) continue;

    assert(M(c,i) > index[i]);

    if (M(c,i) > index[i]+1)
      return true;
  }

  return false;
}

void matrix::merge_simple(const Edges& E,double cutoff)
{
    //-------- Merge some columns --------//
    foreach(e,E) 
    {
      if (e->p < cutoff) break;

      if (e->x2 == -1) {
	int c1 = column(e->s1,e->x1);

	if (index(c1,e->s2) == -3)
	  index(c1,e->s2) = -1;
      }
      else if (e->x1 == -1) {
	int c1 = column(e->s2,e->x2);

	if (index(c1,e->s1) == -3)
	  index(c1,e->s1) = -1;

      }
      else {
	assert(e->x1 >= 0 and e-> x2>=0);

	int c1 = column(e->s1,e->x1);
	int c2 = column(e->s2,e->x2);

	if (c1 == c2) continue;

	if (not columns_conflict(c1,c2))
	  merge_columns(c1,c2);
      }
    }
}

void matrix::merge_strict(const Edges& E,double cutoff)
{
    //-------- Merge some columns --------//
    foreach(e,E) 
    {
      if (e->p < cutoff) break;

      if (e->x2 == -1) {
	int c1 = column(e->s1,e->x1);

	if (not consistent(c1,e->s2,-1,E,cutoff))
	  continue;

	if (index(c1,e->s2) == -3)
	  index(c1,e->s2) = -1;
      }
      else if (e->x1 == -1) {
	int c1 = column(e->s2,e->x2);

	if (not consistent(c1,e->s1,-1,E,cutoff))
	  continue;

	if (index(c1,e->s1) == -3)
	  index(c1,e->s1) = -1;

      }
      else {
	assert(e->x1 >= 0 and e-> x2>=0);

	int c1 = column(e->s1,e->x1);
	int c2 = column(e->s2,e->x2);

	if (c1 == c2) continue;

	if (not columns_conflict(c1,c2) and 
	    consistent(c1,c2,E,cutoff))
	  merge_columns(c1,c2);
      }
    }
}
    

ublas::matrix<int> get_ordered_matrix(const matrix& M)
{
  //-------- sort columns of M ----------//
  vector<int> index(M.size2(),-1);
  vector<int> columns;
  while(true) {
    
    int c1=-1;
    for(int i=0;i<index.size();i++) {
      // skip this sequence if its already done
      if (index[i]+1 >= M.length(i)) continue;
      
      // what is the column where the next index in this sequence appears.
      c1 = M.column(i,index[i]+1);
      
      if (not skips(M,c1,index))
	break;
    }
    
    // if all the sequences are done, then bail.
    if (c1 == -1)
      break;
    
    columns.push_back(c1);
    
    // record these letters as processed.
    for(int i=0;i<M.size2();i++)
      if (M(c1,i) >= 0) {
	index[i]++;
	assert(M(c1,i) == index[i]);
      }
    
  }

  ublas::matrix<int> M2(columns.size(),M.size2());

  for(int i=0;i<M2.size1();i++) {
    for(int j=0;j<M2.size2();j++)
      M2(i,j) = M(columns[i],j);
  }

  return M2;
}

alignment get_alignment(const ublas::matrix<int>& M, alignment& A1) 
{
  alignment A2 = A1;
  A2.changelength(M.size1());

  // get letters information
  vector<vector<int> > sequences;
  for(int i=0;i<A1.n_sequences();i++) {
    vector<int> sequence;
    for(int c=0;c<A1.length();c++) {
      if (not A1.gap(c,i))
	sequence.push_back(A1(c,i));
    }
    sequences.push_back(sequence);
  }

  for(int i=0;i<A2.n_sequences();i++) {
    for(int c=0;c<A2.length();c++) {
      int index = M(c,i);

      if (index >= 0)
	index = sequences[i][index];

      A2(c,i) = index;
    }
  }

  return A2;
}



int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

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
	add_edges(E,Ms,s1,s2,L[s1],L[s2]);


    E.build_index();
    //--------- Build alignment from list ---------//
    double cutoff = args["cutoff"].as<double>();
    if (cutoff < 0.5) cutoff = 0.5;

    //-------- Build a beginning alignment --------//
    matrix M = unaligned_matrix(L);

    if (args.count("strict"))
      M.merge_strict(E,cutoff);
    else
      M.merge_simple(E,cutoff);

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
