#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include "myexception.H"
#include "alignment.H"
#include "arguments.H"
#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "setup.H"
#include "alignmentutil.H"

// FIXME - also show which COLUMNS are more that 99% conserverd?

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using namespace optimize;

inline double log_fact(int n) {
  if (n<2)
    return 0;

  double total=0;
  for(int i=2;i<=n;i++)
    total += log(i);

  return total;
}

/// Return a mapping of letters to columns for each leaf sequence
vector< vector<int> > column_lookup(const alignment& A,int nleaves) {
  vector< vector<int> > result;

  for(int i=0;i<nleaves;i++) {
    vector<int> columns;
    for(int column=0;column<A.length();column++) {
      if (not A.gap(column,i))
	columns.push_back(column);
    }
    result.push_back(columns);
  }

  return result;
}

/// Function to optimize for the star tree algorithm
class alignment_probability: public function {
  int leaves;
  int leafbranches;
  vector< vector<int> > labels;

  vector< vector<int> > branch_to_group;

  vector< vector< vector<int> > > groups;
  double alignment_probability::Pr(const optimize::Vector&,int,int) const;

  double alignment_probability::Pr(const optimize::Vector&,int) const;

public:
  double operator()(const optimize::Vector& v) const;

  alignment_probability(const vector< vector<int> >& v1):labels(v1)
  { 
    leaves = labels[0].size();
    leafbranches = leaves;
    if (leaves==2)
      leafbranches=1;

    for(int L=0;L<labels.size();L++) {
      const vector<int>& label = labels[L];

      // map each branch to a group
      branch_to_group.push_back(vector<int>(leaves));

      // map each group to a set of branches
      groups.push_back(vector< vector<int> >());

      // compute the mappings
      for(int i=0;i<label.size();i++) {

	// find which group we're in
	int group = -1;
	for(int j=0;j<i;j++) {
	  if (label[j] == label[i]) {
	    group = branch_to_group[L][j];
	    break;
	  }
	}

	// if we are the first letter w/ this column, start a new group
	if (group == -1) {
	  group = groups[L].size();
	  groups[L].push_back(vector<int>());
	}      
	
	branch_to_group[L][i] = group;
	groups[L][group].push_back(i);

      }
    }
  }

};

double alignment_probability::Pr(const optimize::Vector& v,int L,int mask) const
{
  const vector<int>& label = labels[L];
  double total=0;

  // calculate the probability of branches being connected to the center
  int nconnected=0;
  for(int i=0;i<label.size();i++) {
    if (mask & (1<<i)) {
      total += -v[i];
      nconnected++;
    }
    else {
      total += log(1.0-exp(-v[i]));
      if (groups[L][branch_to_group[L][i]].size() > 1)
	return log_0;
    }
  }
  
  double sun = v[label.size()];
  int o = groups[L].size();
  int m = nconnected;
  if (o==0)
    return total;

  // calculate the probability of there being groups.size() groups
  // given than nconnected leaves are connected to the center;

  // poisson probability of splitting (o-1) times into o groups
  total += (o-1)*log(sun)-sun-log_fact(o-1);
  total += log_fact(o) - log_fact(m) + log_fact(m-o);
  for(int i=0;i<groups[L].size();i++)
    total -= log_fact(groups[L][i].size()-1);

  return total;
}


double alignment_probability::Pr(const optimize::Vector& v,int L) const {
  const int max = 1<<labels[L].size();
  assert(max != 0);

  // sum of probability of each possibility for each branch
  double total = log_0;
  for(int i=0;i<max;i++)
    total = logsum(total,Pr(v,L,i));

  return total;
}


double alignment_probability::operator()(const optimize::Vector& v) const {
  assert(v.size() == leafbranches+1);

  // probability need to be positive :P
  for(int i=0;i<v.size();i++)
    if (v[i]<0)
      return log_0;

  // product of probability of each label
  double total = 0;
  for(int L=0;L<labels.size();L++) 
    total += Pr(v,L);

  return total;
}


class SSE_match_pairs: public function {
  tree T;
  Matrix Pr_align_pair;
public:
  const tree& t() const {return T;}
  double operator()(const optimize::Vector& v) const;

  SSE_match_pairs(const vector< vector<int> >& v1,const tree& T1)
    :T(T1),Pr_align_pair(T.leaves(),T.leaves())
  { 
    assert(v1.size() > 0);
    assert(T.leaves() == v1[0].size());

    const double pseudocount = 1;
    // initialize the matrix, and add a pseudocount for a conjugate prior
    for(int i=0;i<T.leaves();i++)
      for(int j=0;j<T.leaves();j++)
	Pr_align_pair(i,j)=pseudocount*0.5;

    // For each label, count all present pairs
    for(int i=0;i<v1.size();i++) {
      const vector<int>& label = v1[i];
      for(int l1=0;l1<label.size();l1++) 
	for(int l2=0;l2<l1;l2++) 
	  if (label[l1] == label[l2])
	    Pr_align_pair(l1,l2)++;
    }
    
    // Divide by count to yield an average
    for(int i=0;i<T.leaves();i++)
      for(int j=0;j<T.leaves();j++)
	Pr_align_pair(i,j) /= (v1.size() + pseudocount);
  }
};

double SSE_match_pairs::operator()(const optimize::Vector& v) const {
  // We get one length for each branch, and they should all be positive
  assert(v.size() == T.branches());

  tree temp = T;
  for(int b=0;b<T.branches();b++) {
    if (v[b] <= 0) return log_0;
    temp.branch(b).length() = v[b];
  }

  double SSE=0;
  for(int l1=0;l1<T.leaves();l1++) 
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
  tree T;
  Matrix Pr_align_pair;
public:
  const tree& t() const {return T;}
  double operator()(const optimize::Vector& v) const;

  LeastSquares(const vector< vector<int> >& v1,const tree& T1)
    :T(T1),Pr_align_pair(T.leaves(),T.leaves())
  { 
    assert(v1.size() > 0);
    assert(T.leaves() == v1[0].size());

    const double pseudocount = 1;
    // initialize the matrix, and add a pseudocount for a conjugate prior
    for(int i=0;i<T.leaves();i++)
      for(int j=0;j<T.leaves();j++)
	Pr_align_pair(i,j)=pseudocount*0.5;

    // For each label, count all present pairs
    for(int i=0;i<v1.size();i++) {
      const vector<int>& label = v1[i];
      for(int l1=0;l1<label.size();l1++) 
	for(int l2=0;l2<l1;l2++) 
	  if (label[l1] == label[l2])
	    Pr_align_pair(l1,l2)++;
    }
    
    // Divide by count to yield an average
    for(int i=0;i<T.leaves();i++)
      for(int j=0;j<T.leaves();j++)
	Pr_align_pair(i,j) /= (v1.size() + pseudocount);
  }
};

double LeastSquares::operator()(const optimize::Vector& v) const {
  // We get one length for each branch, and they should all be positive
  assert(v.size() == T.branches());

  tree temp = T;
  for(int b=0;b<T.branches();b++) {
    if (v[b] <= 0) return log_0;
    temp.branch(b).length() = v[b];
  }

  double SSE=0;
  for(int l1=0;l1<T.leaves();l1++) 
    for(int l2=0;l2<l1;l2++) {
      double D1 = -log(Pr_align_pair(l1,l2));
      double D2 = temp.distance(l1,l2);
      double E2 = (D1-D2)*(D1-D2);
      SSE += E2;
    }
  return -SSE;
}


class poisson_match_pairs: public function {
  int n;
  tree T;
  Matrix Pr_align_pair;
public:
  const tree& t() const {return T;}
  double operator()(const optimize::Vector& v) const;

  poisson_match_pairs(const vector< vector<int> >& v1,const tree& T1)
    :T(T1),Pr_align_pair(T.leaves(),T.leaves())
  { 
    assert(v1.size() > 0);
    assert(T.leaves() == v1[0].size());

    const int pseudocount = 1;
    // initialize the matrix, and add a pseudocount for a conjugate prior
    for(int i=0;i<T.leaves();i++)
      for(int j=0;j<T.leaves();j++)
	Pr_align_pair(i,j)=pseudocount*0.5;

    // For each label, count all present pairs
    for(int i=0;i<v1.size();i++) {
      const vector<int>& label = v1[i];
      for(int l1=0;l1<label.size();l1++) 
	for(int l2=0;l2<l1;l2++) 
	  if (label[l1] == label[l2])
	    Pr_align_pair(l1,l2)++;
    }
    
    n = v1.size() + pseudocount;
  }
};

double poisson_match_pairs::operator()(const optimize::Vector& v) const {
  // We get one length for each branch, and they should all be positive
  assert(v.size() == T.branches());

  tree temp = T;
  for(int b=0;b<T.branches();b++) {
    if (v[b] < 0) return log_0;
    temp.branch(b).length() = v[b];
  }

  double Pr=0;
  for(int l1=0;l1<T.leaves();l1++) 
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

  /*
  // include some kind of pressure to make lengths reflect evolutionary
  // distances
  double sum1 = 0;
  double sum2 = 0;
  for(int b=0;b<T.branches();b++) {
    sum1 += T.branch(b).length();
    sum2 += v[b];
  }

  for(int b=0;b<T.branches();b++) {
    double diff = 
      log(v[b]/sum2 + 0.005/sum2) -
      log(T.branch(b).length()/sum1 + 0.005/sum1);

    Pr -= std::abs(diff)*25;
  }
  */

  return Pr;
}


void do_setup(Arguments& args,vector<alignment>& alignments,alignment& A,SequenceTree& T) {
  //----------- Load and link template A and T -----------------//
  load_A_and_T(args,A,T);

  //------------ Try to load alignments -----------//
  int maxalignments = args.loadvalue("maxalignments",1000);

  string tag = "align[sample";
  if (args.set("tag"))
    tag = args["tag"];

  vector< OwnedPointer<alphabet> > alphabets;
  alphabets.push_back(A.get_alphabet());
  alignments = load_alignments(std::cin,tag,alphabets,maxalignments);
}

vector<int> getlabels(const alignment& A,
		      const vector<int>& column,
		      const vector< vector<int> >& columns) {
  vector<int> label(column.size());
  for(int i=0;i<label.size();i++) {
    if (column[i] == -1)
      label[i] = -1;
    else
      label[i] = columns[i][column[i]];
  }

  // If letter from the original column is in a column with a gap here
  // then put this gap in the same column as the letter
  for(int i=0;i<label.size();i++) {
    if (label[i] != -1) continue;
    for(int j=0;j<label.size();j++) {
      if (label[j] == -1) continue;
      if (A.gap(label[j],i))
	label[i] = label[j];
    }
  }
  

  return label;
}

alignment M(const alignment& A1) {
  alignment A2 = A1;
  for(int i=0;i<A2.size2();i++) {
    int pos=0;
    for(int column=0;column<A2.length();column++) {
      if (not A2.gap(column,i)) {
	A2(column,i) = pos;
	pos++;
      }
    }

    assert(pos == A2.seqlength(i));

  }
  return A2;
}

//using namespace boost::numeric::ublas;

vector<int> get_column(const alignment& A,int c,int nleaves) {
  vector<int> column(nleaves);
  for(int i=0;i<nleaves;i++)
    column[i] = A(c,i);
  return column;
}




double get_column_probability(const vector<int>& column, 
			      const vector<alignment>& alignments,
			      const vector< vector< vector<int> > >& column_indexes) {
  unsigned int count=0;
  for(int i=0;i<alignments.size();i++) {
    bool found=true;

    // Can we find a common column for all features?
    int c=-1;
    for(int j=0;j<column.size() and found;j++) {
      if (column[j] == -1) continue;

      int cj = column_indexes[i][j][column[j]];

      if (c == -1)
	c = cj;
      else if (c != cj)
	found = false;

    }
    
    assert(c != -1);

    // Does this column have gaps in the right place?
    for(int j=0;j<column.size() and found;j++) {
      if (column[j] != -1 ) continue;

      if (not alignments[i].gap(c,j))
	found = false;
    }

    if (found) count++;
  }

  return double(0.5+count)/(1.0+alignments.size());
}


int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cerr);

    /*----------- Load alignment and tree ---------*/
    alignment A;
    SequenceTree T;
    vector<alignment> alignments;
    do_setup(args,alignments,A,T);
    cerr<<"Read "<<alignments.size()<<" alignments\n";
    const int n = T.leaves();

    /*----------- Find root branch ---------*/
    int rootb=-1;
    double rootd = -1;
    find_root(T,rootb,rootd);
    std::cerr<<"root branch = "<<rootb<<std::endl;
    std::cerr<<"x = "<<rootd<<std::endl;
    for(int i=0;i<T.leaves();i++)
      std::cerr<<T.seq(i)<<"  "<<rootdistance(T,i,rootb,rootd)<<std::endl;

    /*----------- Construct alignment indexes ----------*/
    vector< vector< vector<int> > >  column_indexes;
    for(int i = 0;i<alignments.size();i++)
      column_indexes.push_back( column_lookup(alignments[i],n) );

    /*------- Convert template to index form-------*/
    A = M(A);

    /*--------- Compute full entire column probabilities -------- */

    vector<double> column_probabilities(A.length());
    for(int c=0;c<A.length();c++)
      column_probabilities[c] = get_column_probability(get_column(A,c,n),
						       alignments,
						       column_indexes
						       );

    /*-------- Check compatability of estimate & samples-------*/
    if (A.size2() != alignments[0].size2())
      throw myexception()<<"Alignment estimate has different not of sequences than alignment samples!";

    for(int i=0;i<A.size2();i++) {
      if (A.seq(i).name != alignments[0].seq(i).name)
	throw myexception()<<"Alignment estimate has different sequences or sequence order than alignment samples";

      if (A.seq(i).name != alignments[0].seq(i).name)
	throw myexception()<<"Sequence "<<i<<" has different length in alignment estimate and alignment samples!";
      
    }

    /*------- Print column names -------*/
    for(int i=0;i<T.leaves();i++) {
      std::cout<<T.seq(i);
      if (i != T.leaves()-1)
	std::cout<<" ";
      else
	std::cout<<endl;
    }
    

    /*------- Analyze the columns -------*/
    for(int c=0;c<A.length();c++) {
      vector<int> column = get_column(A,c,n);

      vector< vector<int> > labels;
      for(int i=0;i<alignments.size();i++)
	labels.push_back(getlabels(alignments[i],column,column_indexes[i]));

      // alignment_probability f(labels);
      function * f;
      if (args["type"] == "SSE")
	f = new SSE_match_pairs(labels,T);
      else if (args["type"] == "LeastSquares")
	f = new LeastSquares(labels,T);
      else
	f = new poisson_match_pairs(labels,T);
      optimize::Vector start(0.01,T.branches());
      assert((*f)(start) > log_0);

      optimize::Vector end = search_basis(start,*f,1.0e-5,500);
      // optimize::Vector end = search_gradient(start,*f,1.0e-5,500);
      
      // Print uncertainty values for the letters
      tree T2 = T;
      for(int b=0;b<T2.branches();b++)
	T2.branch(b).length() = end[b];

      for(int i=0;i<T2.leaves();i++) {
	double length = rootdistance(T2,i,rootb,rootd);
	assert(length >= 0.0);
	double P = exp(-length);  // P(no events between leaf and root)
	std::cout<<P<<" ";
      }
      std::cout<<column_probabilities[c]<<endl;

      delete f;
    }
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
