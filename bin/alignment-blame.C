#include <fstream>
#include <string>
#include "myexception.H"
#include "alignment.H"
#include "arguments.H"
#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"

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


class alignment_probability: public function {
  int leaves;
  int leafbranches;
  vector< vector<int> > labels;

  vector< vector<int> > branch_to_group;

  vector< vector< vector<int> > > groups;
  double alignment_probability::Pr(const vector<double>&,int,int) const;

  double alignment_probability::Pr(const vector<double>&,int) const;

public:
  double operator()(const vector<double>& v) const;

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

double alignment_probability::Pr(const vector<double>& v,int L,int mask) const
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


double alignment_probability::Pr(const vector<double>& v,int L) const {
  const int max = 1<<labels[L].size();
  assert(max != 0);

  // sum of probability of each possibility for each branch
  double total = log_0;
  for(int i=0;i<max;i++)
    total = logsum(total,Pr(v,L,i));

  return total;
}


double alignment_probability::operator()(const vector<double>& v) const {
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



bool match_tag(const string& line,const string& tag) {
  if (line.size() < tag.size())
    return false;

  return (line.substr(0,tag.size()) == tag);
}

void do_setup(Arguments& args,vector<alignment>& alignments,alignment& A) {
  //  /*------ Try to load tree -------------*/
  //  if (not args.set("tree"))
  //    throw myexception("Tree file not specified! (tree=<filename>)");
  //  else 
  //    T.read(args["tree"]);

  /* ----- Alphabets to try ------ */
  vector<alphabet> alphabets;
  alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
  alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
  alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));

  /* ----- Try to load template alignment -----*/
  if (not args.set("align")) 
    throw myexception("Alignment file not specified! (align=<filename>)");

  ifstream ifile(args["align"].c_str());
  A.load_phylip(alphabets,ifile);
  ifile.close();
  

  /* ----- Try to load alignments ------ */
  string tag = "align[sample";
  if (args.set("tag"))
    tag = args["tag"];

  string line;
  while(getline(cin,line)) {
    if (match_tag(line,tag)) {
      alignment A;
      A.load_phylip(alphabets,cin);

      remove_empty_columns(A);
      if (A.num_sequences() == 0) 
	throw myexception(string("Alignment didn't contain any sequences!"));
      alignments.push_back(A);
    }
  }
}

vector<int> getlabels(const alignment& A,const vector<int>& column) {
  vector< vector<int> > columns = column_lookup(A,column.size());
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
  }
  return A2;
}

int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cerr);

    /*----------- Load alignment and tree ---------*/
    alignment A;
    vector<alignment> alignments;
    do_setup(args,alignments,A);
    const int n = A.size2()/2 + 1;

    cerr<<"Read "<<alignments.size()<<" alignments\n";

    /*------- Convert template to index form-------*/
    A = M(A);
    for(int c=0;c<A.length();c++) {
      vector<int> column(n);
      for(int i=0;i<n;i++)
	column[i] = A(c,i);

      vector< vector<int> > labels;
      for(int i=0;i<alignments.size();i++)
	labels.push_back(getlabels(alignments[i],column));

      alignment_probability f(labels);
      vector<double> start(n+1,0.1);
      vector<double> end = search_basis(start,f);
      for(int i=0;i<end.size()-1;i++)
	std::cout<<end[i]+end[n]/n<<" ";
      std::cout<<endl;
    }
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}
