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
Matrix counts_to_probability(const Tree& T,const vector< vector<int> >& v) 
{
  assert(v.size() > 0);
  assert(v[0].size() > 0);

  int N = v[0].size();

  // initialize the pseudocount matrix
  const double edge_prior = 0.5/(T.n_branches()/2);
  const double prior = 0.5;

  Matrix pseudocount = EdgesDistanceMatrix(T);
  for(int i=0;i<pseudocount.size1();i++)
    for(int j=0;j<pseudocount.size2();j++)
      pseudocount(i,j) = prior + pseudocount(i,j)*edge_prior;

  for(int i=0;i<pseudocount.size1();i++)
    for(int j=0;j<pseudocount.size2();j++)
      assert(pseudocount(i,j) > 0);

  // initialize the matrix - add a pseudocount to avoid P=0 or P=1
  Matrix Pr_align_pair = 0.1*0.5*pseudocount;

  // For each label, count all present pairs
  for(int l=0;l<v.size();l++) {
    const vector<int>& label = v[l];
    for(int i=0;i<label.size();i++) 
      for(int j=0;j<i;j++) 
	if (label[i] == label[j]) {
	  Pr_align_pair(i,j)++;
	  Pr_align_pair(j,i)++;
	}
  }

  // Divide by count to yield an average
  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      Pr_align_pair(i,j) /= (v.size() + 0.1*pseudocount(i,j));

  // we didn't handle the diagonal entries at all...
  for(int i=0;i<N;i++)
    Pr_align_pair(i,i) = 1.0;

  // Check symmetry
  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      assert(std::abs(Pr_align_pair(i,j) -Pr_align_pair(j,i)) < 1.0e-9);

  for(int i=0;i<pseudocount.size1();i++)
    for(int j=0;j<pseudocount.size2();j++)
      if (i==j)
	assert(Pr_align_pair(i,j) == 1.0);
      else
	assert(0.0 < Pr_align_pair(i,j) and Pr_align_pair(i,j) < 1.0);

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
  for(int b=0;b<T.n_branches();b++)
    temp.branch(b).set_length(v[b]);

  Matrix TD = DistanceMatrix(temp);

  double SSE=0;
  for(int i=0;i<T.n_leaves();i++) 
    for(int j=0;j<i;j++) {
      double E = D(i,j) - TD(i,j);
      SSE += E*E;
    }

  return -SSE;
}


class WeightedLeastSquares: public function 
{
  Tree T;
  Matrix Pr_align_pair;
  Matrix D;
  Matrix S2;

public:
  const Tree& t() const {return T;}
  double operator()(const optimize::Vector& v) const;

  WeightedLeastSquares(const vector< vector<int> >& v1,const Tree& T1)
    :T(T1),
     Pr_align_pair(T.n_leaves(),T.n_leaves()),
     D(T.n_leaves(),T.n_leaves()),
     S2(T.n_leaves(),T.n_leaves())
  { 
    Pr_align_pair = counts_to_probability(T,v1);
    D = probability_to_distance(Pr_align_pair);
    S2 = probability_to_distance_variance(Pr_align_pair);
  }
};

double WeightedLeastSquares::operator()(const optimize::Vector& v) const 
{
  // We get one length for each branch, and they should all be positive
  assert(v.size() == T.n_branches());

  Tree temp = T;
  for(int b=0;b<T.n_branches();b++)
    temp.branch(b).set_length(v[b]);

  Matrix TD = DistanceMatrix(temp);

  double SSE=0;
  for(int i=0;i<T.n_leaves();i++) 
    for(int j=0;j<i;j++) {
      double E = D(i,j) - TD(i,j);
      SSE += E*E/S2(i,j);  // /D(i,j)/D(i,j);
    }

  //  for(int b=0;b<T.n_branches();b++)
  //    if (v[b] < 0) SSE += (-v[b]+v[b]*v[b])*1000000;


  return -SSE;
}


class poisson_match_pairs: public function 
{
  unsigned n;
  Tree T;
  Matrix Pr_align_pair;

public:
  const Tree& t() const {return T;}
  double operator()(const optimize::Vector& v) const;

  poisson_match_pairs(const vector< vector<int> >& v1,const Tree& T1)
    :n(1),T(T1),Pr_align_pair(T.n_leaves(),T.n_leaves())
  { 
    Pr_align_pair = counts_to_probability(T,v1);
  }
};

double poisson_match_pairs::operator()(const optimize::Vector& v) const 
{
  // We get one length for each branch, and they should all be positive
  assert(v.size() == T.n_branches());

  Tree temp = T;
  for(int b=0;b<T.n_branches();b++) {
    if (v[b] < 0) return log_0;
    temp.branch(b).set_length(v[b]);
  }

  Matrix D = DistanceMatrix(T);

  double Pr=0;
  for(int i=0;i<T.n_leaves();i++) 
    for(int j=0;j<i;j++) 
      {
	double mu = D(i,j);

	//probability of remaining aligned
	double p = exp(-mu);

	// number of times we remained aligned
	double count = n*Pr_align_pair(i,j);

	// binomial probability of being aligned count times out of n
	// Pr += log_fact(n) - log_fact(count) - log_fact(n-count);
	Pr += count*log(p) + (n-count)*log(1-p);
    }


  // include some kind of exponential branch length - branch_mean = 1
  for(int b=0;b<v.size();b++)
    Pr += -v[b];

  return Pr;
}


void do_setup(const variables_map& args,list<alignment>& alignments,alignment& A,RootedSequenceTree& T) {
  //----------- Load and link template A and T -----------------//
  load_A_and_T(args,A,T,false);

  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();

  vector< OwnedPointer<alphabet> > alphabets;
  alphabets.push_back(A.get_alphabet());
  std::cerr<<"Loading alignments...";
  alignments = load_alignments(std::cin,alphabets,maxalignments);
  std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;

  //-------- Check compatability of estimate & samples-------//
  assert(A.n_sequences() == T.n_leaves());
  
  //  if (alignments.front().n_sequences() != T.n_nodes())
  //    throw myexception()<<"Number of sequences in alignment estimate is NOT equal to number of tree nodes!";
  
  for(int i=0;i<A.n_sequences();i++) {
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
    ("find-root","estimate the root position from branch lengths")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("refine", value<string>(),"procedure for refining Least-Squares positivized branch lengths: SSE, Poisson, LeastSquares")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  p.add("tree", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-gild <alignment-file> <tree-file> ... [OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}

int which_directed_branch_equals(const Tree& T, const valarray<bool>& p)
{
  assert(p.size() == T.n_leaves());

  for(int b=0;b<2*T.n_branches();b++)
    if (equal(branch_partition(T,b),p))
      return b;

  return -1;
}

struct root_position
{
  int b;
  double x;
  root_position() {}
  root_position(int i,double d):b(i),x(d) {}
};


root_position find_root_branch_and_position(const SequenceTree& T,const RootedSequenceTree& RT)
{
  root_position rootp;

  vector<const_branchview> branches;
  append(RT.root().branches_out(),branches);

  if (branches.size() < 2)
    throw myexception()<<"Can't use a leaf node as root, sorry.";

  rootp.b = which_directed_branch_equals(T, branch_partition(RT,branches[0]));
  rootp.x = 0;

  assert(rootp.b != -1);

  if (branches.size() == 2)
    rootp.x = branches[1].length()/(branches[0].length() + branches[1].length());
  return rootp;
}


int main(int argc,char* argv[]) { 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);
    
    //----------- Load alignment and tree ---------//
    alignment A;
    RootedSequenceTree RT;
    list<alignment> alignments;
    vector<ublas::matrix<int> > Ms;
    do_setup(args,alignments,A,RT);
    foreach(i,alignments)
      Ms.push_back(M(*i));

    SequenceTree T = RT;
    remove_sub_branches(T);
    if (not is_Cayley(T))
      throw myexception()<<"Multifurcating trees are not handled yet.";

    //----------- Find root branch ---------//
    if (args.count("find-root"))
      RT = find_rooted_tree(T);

    root_position rootp = find_root_branch_and_position(T,RT);

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

    vector<vector<int> > leaf_sets = partition_sets(T);

    for(int c=0;c<A.length();c++) 
    {
      cerr<<"column = "<<c<<endl;

      vector<int> column = get_column(MA,c,T.n_leaves());

      // Get labels - should I instead get *counts*?
      vector< vector<int> > labels(alignments.size());
      for(int i=0;i<alignments.size();i++)
	labels[i] = get_splitgroup_columns(MA,c,Ms[i],column_indexes[i]);

      //Get initial estimate using fast least squares
      vector<double> LS_branch_lengths(T.n_branches());

      Matrix Q = counts_to_probability(T,labels);
      Matrix Pc = probability_to_distance(Q);
      LS_branch_lengths = FastLeastSquares(T,Pc,leaf_sets);
      P_total += Pc;

      // Print uncertainty values for the letters
      SequenceTree T2 = T;
      for(int b=0;b<T2.n_branches();b++)
	T2.branch(b).set_length(std::max(0.0, LS_branch_lengths[b]));
      cerr<<"unrefined tree = "<<T2<<endl;

      // Define an objective function for refining the initial estimates
      function * f = NULL;
      if (not args.count("refine"))
	;
      else if (args["refine"].as<string>() == "SSE")
	f = new SSE_match_pairs(labels,T);
      else if (args["refine"].as<string>() == "Poisson")
	f = new poisson_match_pairs(labels,T);
      else if (args["refine"].as<string>() == "LeastSquares")
	f = new LeastSquares(labels,T);
      else if (args["refine"].as<string>() == "WeightedLeastSquares")
	f = new WeightedLeastSquares(labels,T);
      else
	throw myexception()<<"Unknown refiner '"<<args["refine"].as<string>()<<"'";

      // refine initial estimate if requested
      vector<double> r_branch_lengths = LS_branch_lengths;

      if (f) 
      {
	optimize::Vector x(T.n_branches());
	for(int i=0;i<LS_branch_lengths.size();i++)
	  x[i] = std::max(0.00001, LS_branch_lengths[i]);

	x = search_gradient(x, *f, 1.0e-6, 500);
	delete f;

	for(int i=0;i<r_branch_lengths.size();i++)
	  r_branch_lengths[i] = x[i];
      }
      
      // Print uncertainty values for the letters
      for(int b=0;b<T2.n_branches();b++)
	T2.branch(b).set_length(std::max(0.0, r_branch_lengths[b]));

      /*
      cerr<<"refined tree = "<<T2<<endl;
      cerr<<endl;
      for(int i=0;i<T.n_branches();i++)
	cerr<<" b = "<<i<<"       u = "<<LS_branch_lengths[i]<<"   r = "<<r_branch_lengths[i]<<endl;
      cerr<<endl;
      */

      Matrix mu_fit = DistanceMatrix(T2);

      /*
      for(int i=0;i<T.n_leaves();i++)
	for(int j=0;j<i;j++) {
	  double odds = log(Q(i,j)/(1-Q(i,j)));

	  double q_fit = exp(-mu_fit(i,j));
	  double odds_fit = log(q_fit/(1-q_fit));

	  if (std::abs(odds - odds_fit) > 0.01)
	    cerr<<T.seq(i)<<" - "<<T.seq(j)<<": q = "<<Q(i,j)<<"   q_fit = "<<q_fit<<"       delta odds = "<<odds-odds_fit<<endl;
	}
      */	  

      for(int i=0;i<T2.n_leaves();i++) {
	double length = rootdistance(T2,i,rootp.b,rootp.x);
	assert(length >= 0.0);
	double P = exp(-length);  // P(no events between leaf and root)
	std::cout<<P<<" ";
      }
      cerr<<endl<<endl;
      std::cout<<column_probabilities[c]<<endl;
    }

    // average the COUNTS instead of the LENGTHS?
    // should count unalignment events differently? - i.e. AA-- gives counts out of (1,1,0,0)?

    P_total /= A.length();
    vector<double> branch_lengths = FastLeastSquares(T,P_total,leaf_sets);
    for(int i=0;i<branch_lengths.size();i++)
      if (branch_lengths[0] <0) branch_lengths[i] = 0;
    
    // Print uncertainty values for the letters
    SequenceTree T2 = T;
    for(int b=0;b<T2.n_branches();b++)
      T2.branch(b).set_length(std::max(0.0,branch_lengths[b]));

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
