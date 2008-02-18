#include <string>
#include <valarray>
#include "myexception.H"
#include "alignment.H"
#include "sequence-format.H"
#include "sequencetree.H"
#include "setup.H"
#include "alignment-util.H"
#include "util.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;
using std::string;
using std::cout;
using std::cerr;
using std::valarray;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>()->default_value("-"),"file with alignment to convert (default STDIN)")
    ("tree",value<string>(),"file with initial tree")
    ("alphabet",value<string>(),"specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons")
    ("data-dir", value<string>()->default_value("Data"),"data directory")
    ("groups", value<string>(),"file with taxon groups")
    ("ignore-rate-change","ignore rate changes")
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
    std::cout<<"Usage: alignment-find conserved <alignment-file> [OPTIONS]\n";
    std::cout<<"Convert the alignment to FASTA or PHYLIP.\n\n";
    std::cout<<all<<"\n";
    exit(0);
  }

  return args;
}


vector<vector<int> > load_groups(const alignment& A,const string& filename)
{
  vector<string> seq_names = sequence_names(A);
  vector<vector<int> > groups;

  ifstream file(filename.c_str());
  string line;
  for(int g=1;getline(file,line);g++)
  {
    if (not groups.size() or groups.back().size())
      groups.push_back(vector<int>());

    vector<string> names = split(line,' ');

    for(int i=0;i<names.size();i++)
      if (names[i].size())
      {
	int j = find_index(seq_names, names[i]);
	if (j == -1)
	  throw myexception()<<"Group "<<g<<": I cant find taxon '"<<names[i]<<"' in the alignment!";
	groups.back().push_back(j);
      }
  }

  return groups;
}


bool all_same_or(const vector<int>& v,int c)
{
  int value = c;
  for(int i=0;i<v.size() and value == c;i++)
    value = v[i];

  bool same = true;
  for(int i=0;i<v.size();i++)
    if (v[i] != value and v[i] != c)
      same = false;
  return same;
}

bool all_same(const vector<int>& v)
{
  bool same = true;
  for(int i=1;i<v.size();i++)
    if (v[i] != v[0])
      same = false;
  return same;
}

int most_common(const vector<int>& v)
{
  int half = v.size()/2;

  vector<int> counted(v.size(),0);

  vector<int> values;
  vector<int> counts;

  for(int i=0;i<v.size();i++) 
  {
    if (not counted[i]) 
    {
      int count = 0;
      for(int j=i;j<v.size();j++) {
	if (v[j] == v[i]) {
	  counted[j] = 1;
	  count++;
	}
      }

      if (count > half)
	return v[i];

      values.push_back(v[i]);
      counts.push_back(count);
    }
  }

  int m = argmax(counts);
  return values[m];
}

int number_of(const vector<int>& v,int n)
{
  int count = 0;
  for(int i=0;i<v.size();i++)
    if (v[i] == n)
      count++;
  return count;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //----------- Load alignment and tree ---------//
    alignment A;
    SequenceTree T;
    if (args.count("tree"))
      load_A_and_T(args,A,T,false);
    else
      A = load_A(args,false);

    const alphabet& a = A.get_alphabet();
    
    //------- Load groups and find branches -------//
    vector<vector<int> > groups;
    if (args.count("groups")) 
      groups = load_groups(A,args["groups"].as<string>());

    for(int i=0;i<groups.size();i++) {
      for(int j=0;j<groups[i].size();j++)
	cerr<<A.seq(groups[i][j]).name<<" ";
      cerr<<endl;
    }

    vector<int> group_branches;
    if (args.count("tree")) 
    {
      for(int i=0;i<groups.size();i++) 
      {
	valarray<bool> p(false,T.n_leaves());
	for(int j=0;j<groups[i].size();j++)
	  p[groups[i][j]] = true;
	

	int found = -1;
	for(int b=0;b<2*T.n_branches() and found == -1;b++)
	  if (equal(p,branch_partition(T,b)))
	    found = b;
	if (found == -1)
	  throw myexception()<<"I can't find group "<<i+1<<" on the tree!";
	
	group_branches.push_back(found);
      }
    }

    //-------------------------------------------//

    Matrix C(A.length(),A.n_sequences()+1);
    for(int i=0;i<C.size1();i++)
      for(int j=0;j<C.size2();j++)
	C(i,j) = 0;

    // yes but, how much more conservation THAN EXPECTED do we see?

    for(int c=0;c<C.size1();c++) 
    {
      vector<int> value(groups.size(),alphabet::gap);

      for(int g=0;g<groups.size();g++) 
      {
	vector<int> temp;
	for(int i=0;i<groups[g].size();i++)
	  temp.push_back(A(c,groups[g][i]));

	int best = most_common(temp);
	int count = number_of(temp,best);
	
	if (count >= groups[g].size()-1 and count >=3 and count> groups[g].size()/2)
	  value[g] = best;
      }


      cerr<<c+1<<"   ";
      for(int i=0;i<value.size();i++) 
	cerr<<a.lookup(value[i])<<" ";

      bool different = not all_same(value);

      if (args.count("ignore-rate-change"))
	different = not all_same_or(value,alphabet::gap);

      cerr<<"    "<<different<<endl;

      // When something becomes less conserved, that is interesting, but not
      // as interesting as an AMINO ACID CHANGE that happens on the branch that
      // stems from the duplication!

      // So, EEEEE vs EEKEE isn't so interesting, since the K didn't change on the duplication
      // branch...
      for(int g=0;g<groups.size();g++)
	if (value[g] != alphabet::gap)
	  for(int i=0;i<groups[g].size();i++)
	    C(c,groups[g][i]) = different?1.0:0.5;
    }


    cout<<join(sequence_names(A),' ')<<endl;
    for(int i=0;i<C.size1();i++) {
      vector<double> temp;
      for(int j=0;j<C.size2();j++)
	temp.push_back(C(i,j));
      cout<<join(temp,' ')<<endl;
    }
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}
