#include <iostream>
#include <fstream>
#include <string>
#include "tree.H"
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"
#include "setup.H"
#include "findroot.H"

#include <boost/program_options.hpp>

using std::valarray;

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;

using std::string;

template<typename T>
void add(vector<T>& v1,const vector<T>& v2) 
{
  for(int i=0;i<v1.size();i++)
    v1[i] += v2[i];
}


vector<int> find_triplet(const sequence& s,const string& triplet) 
{
  sequence s2 = s;
  s2.strip_gaps();

  vector<int> found(3,0);

  int pos=-1;
  while(pos=s.find(triplet,pos+1),pos != -1)
    found[pos%3]++;

  return found;
}

vector<int> find_triplet(const vector<sequence>& sequences,const string& triplet) 
{
  vector<int> found(3,0);
  for(int i=0;i<sequences.size();i++)
    add(found, find_triplet(sequences[i],triplet) );
  return found;
}


int min(const vector<int>& v) {
  int m = v[0];
  for(int i=1;i<v.size();i++)
    if (v[i]<m)
      m = v[i];
  return m;
}

int n_mutations(const alphabet& a, const vector<int>& letters, const Tree& T) 
{
  const int A = a.size();

  assert(letters.size() == T.n_leaves());

  vector< vector<int> > n_muts(T.n_nodes(),vector<int>(A,0));

  int root = T.directed_branch(0).target();
  vector<const_branchview> branches = branches_toward_node(T,root);

  vector<int> temp(A);

  // set the leaf lengths
  for(int s=0;s<T.n_leaves();s++)
    for(int l=0;l<A;l++)
      n_muts[s][l] = not a.matches(l,letters[s]);

  for(int i=0;i<branches.size();i++) 
  {
    int s = branches[i].source();
    int t = branches[i].target();

    for(int l=0;l<A;l++)
      temp[l] = n_muts[s][l]+1;

    for(int l=0;l<A;l++) {
      temp[l]--;
      n_muts[t][l] += min(temp);
      temp[l]++;
    }
  }

  return min(n_muts[root]);
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
    ("alphabet",value<string>(),"specify the alphabet: DNA, RNA, Amino Acids, Triplets, or Codons")
    ("data-dir", value<string>()->default_value("Data"),"data directory")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  p.add("tree", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-info <alignment-file> [<tree-file>] [OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


int n_letters(const valarray<int>& count,int level) {
  int n = 0;
  for(int l=0;l<count.size();l++)
    if (count[l] > level) n++;
  return n;

}

bool is_informative(const valarray<int>& count,int level) 
{
  int n = n_letters(count,level);
  if (level == 0) assert(n > 0);
  return n>1;
}


double fraction_identical(const alignment& A,int s1,int s2,bool gaps_count) 
{
  unsigned total=0;
  unsigned same =0;
  for(int i=0;i<A.length();i++) {
    if (A.gap(i,s1) and A.gap(i,s2)) 
      continue;

    if (not gaps_count and (A.gap(i,s1) or A.gap(i,s2)))
      continue;

    total++;

    if (A(i,s1) == A(i,s2))
      same++;
  }

  double f = 1;
  if (total > 0)
    f = double(same)/total;

  return f;
}


double min_identity(const alignment& A,bool gaps_count)
{
  double identity = 1.0;
  for(int i=0;i<A.n_sequences();i++)
    for(int j=0;j<i;j++)
      identity = std::min(identity,fraction_identical(A,i,j,gaps_count));

  return identity;
}

unsigned letter_classes(const alignment& A) 
{
  const alphabet& a = A.get_alphabet();

  // Count the occurrence of the different letters
  unsigned count=0;
  for(int i=0;i<A.length();i++) {
    for(int j=0;j<A.n_sequences();j++) {
      if (alphabet::is_letter_class(A(i,j)) and not a.is_letter(A(i,j)))
	count++;
    }
  }

  return count;
}

int main(int argc,char* argv[]) 
{ 
  try {
    cerr.precision(10);
    cout.precision(10);
    
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
    
    //----- Count informative/non-constant sites ----//
    valarray<bool> informative(A.length());
    valarray<bool> different(A.length());

    valarray<int> count(a.size());
    for(int c=0;c<A.length();c++) {
      count = 0;
      for(int i=0;i<A.n_sequences();i++) {
	int l = A(c,i);
	if (a.is_letter(l))
	  count[l]++;
      }

      different[c] =  is_informative(count,0);
      informative[c] = is_informative(count,1);
    }
    
    int n_different = n_elements(different);
    int n_informative = n_elements(informative);


    cout.precision(3);

    vector<int> lengths;
    for(int i=0;i<A.n_sequences();i++)
      lengths.push_back(A.seqlength(i));

    cout<<"Alphabet: "<<a.name<<"\n\n";
    cout<<"Alignment: "<<A.length()<<" columns of "<<A.n_sequences()<<" sequences\n";
    cout<<"  "<<max(lengths)<<"/"<<min(lengths)<<" max/min sequence lengths.\n";
    cout<<"  "<<n_different<<" ("<<double(n_different)/A.length()*100<<"%) sites are not constant.\n";
    cout<<"  "<<n_informative<<" ("<<double(n_informative)/A.length()*100<<"%) are informative.\n";

    cout<<"  "<<min_identity(A,true)*100<<"%/"<<min_identity(A,false)*100<<"% minimum sequence identity with/without indels.\n";

    //------------ Get Tree Lengths ------------//
    int tree_length = 0;
    if (args.count("tree")) {
      vector<int> letters(A.n_sequences());
      for(int c=0;c<A.length();c++) {
	for(int i=0;i<A.n_sequences();i++)
	  letters[i] = A(c,i);
	int length = n_mutations(a,letters,T);
	tree_length += length;
	if (informative[c]) {

	  count = 0;
	  for(int i=0;i<A.n_sequences();i++) {
	    int l = A(c,i);
	    if (a.is_letter(l))
	      count[l]++;
	  }

	  //	  cout<<"#"<<c<<"    "<<length<<"   -    "<<n_letters(count,0)-1<<"     =     "<<
	  //   length-(n_letters(count,0)-1)<<"\n";
	}
      }
      cout<<"  tree length = "<<tree_length<<"\n";
    }

    if (dynamic_cast<const Nucleotides*>(&a)) 
    {
      vector<sequence> sequences = A.get_sequences();

      vector<int> found(3,0);
      add(found, find_triplet( sequences , "TAA" ) );
      add(found, find_triplet( sequences , "TGA" ) );
      add(found, find_triplet( sequences , "TAG" ) );

      cout<<"\nStop Codons:\n";
      for(int i=0;i<3;i++) 
	cout<<"   Frame "<<i<<": "<<found[i]<<"\n";
    }

    valarray<double> counts = letter_counts(A);
    valarray<double> frequencies = A.get_alphabet().get_frequencies_from_counts(counts,A.n_sequences()/2);

    cout<<"\nFreqencies:\n  ";
    for(int i=0;i<a.size();i++)
      cout<<a.lookup(i)<<"="<<frequencies[i]*100<<"%  ";
    cout<<"\n";

    int classes = letter_classes(A);
    int wildcards = letter_count(A,alphabet::not_gap);
    int total = classes + wildcards + (int)counts.sum();
    cout<<"  Classes:  "<<classes<<" ["<<double(classes)/total*100<<"%]\n";
    cout<<"  Wildcards: "<<wildcards<<" ["<<double(wildcards)/total*100<<"%]\n";
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}

