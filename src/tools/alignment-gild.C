#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <list>
#include "myexception.H"
#include "alignment.H"
#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "setup.H"
#include "alignmentutil.H"
#include "alignment-util.H"
#include "distance-methods.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

// FIXME - also show which COLUMNS are more that 99% conserved?

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using std::list;
using namespace optimize;

inline double log_fact(int n) {
  if (n<2)
    return 0;

  double total=0;
  for(int i=2;i<=n;i++)
    total += log(i);

  return total;
}

// Compute the probability that residues (i,j) are aligned
//   - v[i][j] represents the column of the feature j in alignment i.
//   - so if v[i][j] == v[i][k] then j and k are paired in alignment i.
Matrix counts_to_probability(const Tree& T,const vector< vector<int> >& v) {
  assert(v.size() > 0);
  assert(v[0].size() > 0);

  int N = v[0].size();


  const double pseudocount = 1;
  const double edge_prior = 0.5/(T.n_branches()/2);
  const double prior = 0.5;

  // initialize the matrix, and add a pseudocount as a conjugate prior
  Matrix Pr_align_pair(N,N);
  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      Pr_align_pair(i,j)=pseudocount*(prior + edge_prior*T.edges_distance(i,j));

  // For each label, count all present pairs
  for(int i=0;i<v.size();i++) {
    const vector<int>& label = v[i];
    for(int l1=0;l1<label.size();l1++) 
      for(int l2=0;l2<l1;l2++) 
	if (label[l1] == label[l2]) {
	  Pr_align_pair(l1,l2)++;
	  Pr_align_pair(l2,l1)++;
	}
  }

  // Divide by count to yield an average
  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      Pr_align_pair(i,j) /= (v.size() + pseudocount*(prior + edge_prior*T.edges_distance(i,j)));

  // Check symmetry
  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      assert(std::abs(Pr_align_pair(i,j) -Pr_align_pair(j,i)) < 1.0e-9);

  return Pr_align_pair;
}

class SSE_match_pairs: public function {
  Tree T;
  Matrix Pr_align_pair;
public:
  const Tree& t() const {return T;}
  double operator()(const optimize::Vector& v) const;

  SSE_match_pairs(const vector< vector<int> >& v1,const Tree& T1)
    :T(T1),Pr_align_pair(T.n_leaves(),T.n_leaves())
  { 
    Pr_align_pair = counts_to_probability(T,v1);
  }
};

double SSE_match_pairs::operator()(const optimize::Vector& v) const {
  // We get one length for each branch, and they should all be positive
  assert(v.size() == T.n_branches());

  Tree temp = T;
  for(int b=0;b<T.n_branches();b++) {
    if (v[b] <= 0) return log_0;
    temp.branch(b).set_length(v[b]);
  }

  double SSE=0;
  for(int l1=0;l1<T.n_leaves();l1++) 
    for(int l2=0;l2<l1;l2++) {
      double length = temp.distance(l1,l2);
      double P2 = 1.0 - exp(-length);
      double P1 = 1.0 - Pr_align_pair(l1,l2);
      double E = log(P2 + 0.0001) - log(P1 + 0.0001);
      SSE += E*E;
    }
  return -SSE;
}


class LeastSquares: public function {
  Tree T;
  Matrix Pr_align_pair;
  Matrix D;
public:
  const Tree& t() const {return T;}
  double operator()(const optimize::Vector& v) const;

  LeastSquares(const vector< vector<int> >& v1,const Tree& T1)
    :T(T1),
     Pr_align_pair(T.n_leaves(),T.n_leaves()),
     D(T.n_leaves(),T.n_leaves())
    
  { 
    Pr_align_pair = counts_to_probability(T,v1);
    D = probability_to_distance(Pr_align_pair);
  }
};

double LeastSquares::operator()(const optimize::Vector& v) const {
  // We get one length for each branch, and they should all be positive
  assert(v.size() == T.n_branches());

  Tree temp = T;
  for(int b=0;b<T.n_branches();b++) {
    if (v[b] < 0) return log_0;
    temp.branch(b).set_length(v[b]);
  }

  double SSE=0;
  for(int l1=0;l1<T.n_leaves();l1++) 
    for(int l2=0;l2<l1;l2++) {
      double D1 = D(l1,l2);
      double D2 = temp.distance(l1,l2);
      double E2 = (D1-D2)*(D1-D2);
      SSE += E2;
    }
  return -SSE;
}


class poisson_match_pairs: public function {
  int n;
  Tree T;
  Matrix Pr_align_pair;
public:
  const Tree& t() const {return T;}
  double operator()(const optimize::Vector& v) const;

  poisson_match_pairs(const vector< vector<int> >& v1,const Tree& T1)
    :T(T1),Pr_align_pair(T.n_leaves(),T.n_leaves())
  { 
    Pr_align_pair = counts_to_probability(T,v1);
  }
};

double poisson_match_pairs::operator()(const optimize::Vector& v) const {
  // We get one length for each branch, and they should all be positive
  assert(v.size() == T.n_branches());

  Tree temp = T;
  for(int b=0;b<T.n_branches();b++) {
    if (v[b] < 0) return log_0;
    temp.branch(b).set_length(v[b]);
  }

  double Pr=0;
  for(int l1=0;l1<T.n_leaves();l1++) 
    for(int l2=0;l2<l1;l2++) {
      double length = temp.distance(l1,l2);
      //probability of remaining aligned
      double p = exp(-length);
      // number of times we remained aligned
      int count = (int)(Pr_align_pair(l1,l2) + 0.5);

      // binomial probability of being aligned count times out of n
      Pr += log_fact(n) - log_fact(count) - log_fact(n-count);
      Pr += count*log(p) + (n-count)*log(1-p);
    }


  // include some kind of exponential branch length - branch_mean = 1
  for(int b=0;b<v.size();b++)
    Pr += -v[b];

  return Pr;
}


void do_setup(const variables_map& args,list<alignment>& alignments,alignment& A,SequenceTree& T) {
  //----------- Load and link template A and T -----------------//
  load_A_and_T(args,A,T,false);

  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();

  string tag = "align[";
  tag += args["tag"].as<string>();

  vector< OwnedPointer<alphabet> > alphabets;
  alphabets.push_back(A.get_alphabet());
  std::cerr<<"Loading alignments...";
  alignments = load_alignments(std::cin,tag,alphabets,maxalignments);
  std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;

  //-------- Check compatability of estimate & samples-------//
  assert(A.size2() == T.n_leaves());
  
  if (alignments.front().size2() != T.n_nodes())
    throw myexception()<<"Number of sequences in alignment estimate is NOT equal to number of tree nodes!";
  
  for(int i=0;i<A.size2();i++) {
    if (A.seq(i).name != alignments.front().seq(i).name)
      throw myexception()<<"Alignment estimate has different sequences or sequence order than alignment samples";
    
    if (A.seq(i).name != alignments.front().seq(i).name)
      throw myexception()<<"Sequence "<<i<<" has different length in alignment estimate and alignment samples!";
    
  }

}


//using namespace boost::numeric::ublas;

vector<int> get_column(const ublas::matrix<int>& MA,int c,int nleaves) {
  vector<int> column(nleaves);
  for(int i=0;i<nleaves;i++)
    column[i] = MA(c,i);
  return column;
}




double get_column_probability(const vector<int>& column, 
			      const list<alignment>& alignments,
			      const vector< vector< vector<int> > >& column_indexes) {
  unsigned int count=0;
  int i=0;
  foreach(A,alignments) {
    bool found=true;

    // Can we find a common column for all features?
    int c=-1;
    for(int j=0;j<column.size() and found;j++) {

      // if there is a gap in this column, ignore it
      if (column[j] == -1) continue;

      // find the column that for the column[j]-th feature of species j
      int cj = column_indexes[i][j][column[j]];

      if (c == -1)
	c = cj;
      else if (c != cj)
	found = false;

    }
    
    assert(c != -1);

    // Does this column have gaps in the right place?
    for(int j=0;j<column.size() and found;j++) {

      // if there is a NOT gap in this column, ignore it
      if (column[j] != -1 ) continue;

      // if the template doesn't have a gap, then this doesn't match
      if (not A->gap(c,j))
	found = false;
    }

    if (found) count++;
    i++;
  }

  return double(0.5+count)/(1.0+alignments.size());
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("tree",value<string>(),"file with initial tree")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("tag", value<string>()->default_value("sample"),"only read alignments preceded by 'align[<tag>'")
    ("refine", value<string>(),"procedure for refining Least-Squares positivized branch lengths: SSE, Poisson, LeastSquares")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-gild <file1> <file2> ... [OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) { 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //----------- Load alignment and tree ---------//
    alignment A;
    SequenceTree T;
    list<alignment> alignments;
    vector<ublas::matrix<int> > Ms;
    do_setup(args,alignments,A,T);
    foreach(i,alignments)
      Ms.push_back(M(*i));

    //----------- Find root branch ---------//
    int rootb=-1;
    double rootd = -1;
    find_root(T,rootb,rootd);
    std::cerr<<"root branch = "<<rootb<<std::endl;
    std::cerr<<"x = "<<rootd<<std::endl;
    std::cerr<<"distances from root:"<<std::endl;
    for(int i=0;i<T.n_leaves();i++)
      std::cerr<<" "<<T.seq(i)<<":  "<<rootdistance(T,i,rootb,rootd)<<std::endl;

    //----------- Construct alignment indexes ----------//
    vector< vector< vector<int> > >  column_indexes;
    foreach(i,alignments)
      column_indexes.push_back( column_lookup(*i,T.n_leaves()) );

    //------- Convert template to index form-------//
    ublas::matrix<int> MA = M(A);

    //--------- Compute full entire column probabilities -------- */
    vector<double> column_probabilities(A.length());
    for(int c=0;c<A.length();c++)
      column_probabilities[c] = get_column_probability(get_column(MA,c,T.n_leaves()),
						       alignments,
						       column_indexes
						       );

    //------- Print column names -------//
    for(int i=0;i<T.n_leaves();i++) {
      std::cout<<T.seq(i);
      if (i != T.n_leaves()-1)
	std::cout<<" ";
      else
	std::cout<<endl;
    }

    //------- Analyze the columns -------//
    Matrix P_total(T.n_leaves(),T.n_leaves());
    for(int i=0;i<P_total.size1();i++)
      for(int j=0;j<P_total.size2();j++)
	P_total(i,j) = 0;

    for(int c=0;c<A.length();c++) {
      vector<int> column = get_column(MA,c,T.n_leaves());

      // Get labels - should I instead get *counts*?
      vector< vector<int> > labels(alignments.size());
      for(int i=0;i<alignments.size();i++)
	labels[i] = get_splitgroup_columns(MA,c,Ms[i],column_indexes[i]);

      //Get initial estimate using fast least squares
      vector<double> branch_lengths(T.n_branches());

      Matrix Pc = probability_to_distance(counts_to_probability(T,labels));
      branch_lengths = FastLeastSquares(T,Pc);
      for(int b=0;b<branch_lengths.size();b++)
	if (branch_lengths[b] < 0) branch_lengths[b] = 0;
      P_total += Pc;

      // Define an objective function for refining the initial estimates
      function * f = NULL;
      if (args["refine"].as<string>() == "SSE")
	f = new SSE_match_pairs(labels,T);
      else if (args["refine"].as<string>() == "Poisson")
	f = new poisson_match_pairs(labels,T);
      else if (args["refine"].as<string>() == "LeastSquares")
	f = new LeastSquares(labels,T);

      // refine initial estimate if requested
      if (f) {
	optimize::Vector x(T.n_branches());
	for(int i=0;i<branch_lengths.size();i++)
	  x[i] = branch_lengths[i];

	x = search_gradient(x,*f,1.0e-5,3);
	delete f;

	for(int i=0;i<branch_lengths.size();i++)
	  branch_lengths[i] = x[i];
      }
      
      // Print uncertainty values for the letters
      Tree T2 = T;
      for(int b=0;b<T2.n_branches();b++)
	T2.branch(b).set_length(branch_lengths[b]);

      for(int i=0;i<T2.n_leaves();i++) {
	double length = rootdistance(T2,i,rootb,rootd);
	assert(length >= 0.0);
	double P = exp(-length);  // P(no events between leaf and root)
	std::cout<<P<<" ";
      }
      std::cout<<column_probabilities[c]<<endl;
    }

    // average the COUNTS instead of the LENGTHS?
    // should count unalignment events differently? - i.e. AA-- gives counts out of (1,1,0,0)?

    P_total /= A.length();
    vector<double> branch_lengths = FastLeastSquares(T,P_total);
    for(int i=0;i<branch_lengths.size();i++)
      if (branch_lengths[0] <0) branch_lengths[i] = 0;
    
    // Print uncertainty values for the letters
    SequenceTree T2 = T;
    for(int b=0;b<T2.n_branches();b++)
      T2.branch(b).set_length(branch_lengths[b]);

    // refine for a few iterations...
    // FIXME - fix the refiners to take the alignment probability matrix...

    std::cerr<<T2<<std::endl;
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
