#include <iostream>
#include <fstream>
#include <string>
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"
#include "sequence-format.H"
#include <boost/program_options.hpp>

using namespace sequence_format;
namespace po = boost::program_options;
using po::variables_map;

using std::ifstream;
using std::istream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

bool all_same_length(const vector<sequence>& s)
{
  for(int i=1;i<s.size();i++)
    if (s[i].size() != s[0].size())
      return false;
  return true;
}


//FIXME - make this handle un-aligned gaps...
// diagnose sequences which are not a multiple of 3
// look for reading frames?  start codons?
// translate just the sequences before translating
// the ALIGNMENT of the sequences to print out

vector<int> get_mapping(const vector<sequence>& S1, const vector<sequence>& S2)
{

  vector<string> names1(S1.size());
  for(int i=0;i<S1.size();i++)
    names1[i] = S1[i].name;

  vector<string> names2(S2.size());
  for(int i=0;i<S2.size();i++)
    names2[i] = S2[i].name;

  vector<int> mapping;

  try {
    mapping = compute_mapping(names1,names2);
  }
  catch (const bad_mapping<string>& b) {
    bad_mapping<string> b2(b.missing,b.from);
    if (b.from == 0)
      b2<<"Couldn't find sequence '"<<b2.missing<<"'.";
    else
      b2<<"Extra sequence '"<<b2.missing<<"' not contained in earlier alignments.";
    throw b2;    
  }
  return mapping;
}


vector<sequence> concatenate(const vector<sequence>& S1, const vector<sequence>& S2)
{
  if (not S1.size())
    return S2;

  assert(S1.size() == S2.size());

  vector<sequence> S = S1;
  for(int i=0;i<S1.size();i++) {
    
    vector<int> mapping = get_mapping(S1,S2);

    (string&)S[i] = S1[i] + S2[mapping[i]];
  }

  return S;
}


void get_range(const string& range,int L,int& begin,int& end)
{
  vector<string> R = split(range,'-');

  if (R.size() == 1) {
    begin = end = convertTo<int>(range)-1;
  }
  else if (R.size() != 2)
    throw myexception()<<"Malformed range '"<<range<<"'";
  else {
    begin = 0;
    if (R[0].size())
      begin = convertTo<int>(R[0])-1;

    end = L-1;
    if (R[1].size())
      end = convertTo<int>(R[1])-1;
  }
    
  if (begin < 0)
    throw myexception()<<"Bad range '"<<range<<"': begins before 1.";
    
  if (begin > L-1)
    throw myexception()<<"Bad range '"<<range<<"': begins after end of sequence (L="<<L<<").";
    
  if (end < 0)
    throw myexception()<<"Bad range '"<<range<<"': ends before 1!";
    
  if (end > L-1)
    throw myexception()<<"Bad range '"<<range<<"': ends after end of sequence (L="<<L<<").";
    
  if (end < begin)
    throw myexception()<<"Bad range '"<<range<<"': begins after end!";
}


vector<sequence> select(const vector<sequence>& s,const vector<int>& columns)
{
  assert(all_same_length(s));

  //------- Start with empty sequences --------//
  vector<sequence> S = s;
  for(int i=0;i<s.size();i++)
    S[i].string::operator=("");

  //------- Append columns to sequences -------//
  for(int i=0;i<columns.size();i++)
    for(int j=0;j<s.size();j++)
      S[j] += s[j][columns[i]];

  return S;
}

vector<sequence> select(const vector<sequence>& s,const string& range)
{
  assert(all_same_length(s));

  vector<string> ranges = split(range,',');

  vector<int> columns;
  for(int i=0;i<ranges.size();i++) 
  {
    int begin=-1,end=-1;
    
    get_range(ranges[i],s[0].size(),begin,end);
    
    for(int c=begin;c<=end;c++)
      columns.push_back(c);
  }

  return select(s,columns);
}

vector<sequence> remove_empty_columns(const vector<sequence>& s,const vector<char>& missing)
{
  assert(all_same_length(s));

  // cache length of longest sequences
  int L = s[0].size();

  // find non-empty columns
  vector<int> columns;
  for(int c=0;c<L;c++)
  {
    bool empty = true;
    for(int j=0;j<s.size() and empty;j++)
      if ((c < s[j].size()) and (not includes(missing,s[j][c])))
	empty=false;

    if (not empty)
      columns.push_back(c);
  }

  // select the non-empty columns
  return select(s,columns);
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("file", value<vector<string> >(),"Alignment files")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "produce help message")
    ("output", value<string>()->default_value("fasta"),"which output format: fasta or phylip?")
    ("columns,c", value<string>(),"Ranges of columns to keep, like: 1-10,30-")
    ("taxa,t", value<string>(),"Taxa to keep, comma-separated")
    ("pad", "Add gaps to make sequence lengths identical")
    ("remove-empty-columns,r","Remove columns with no characters (e.g. all-gap columns)")
    ("missing",value<string>()->default_value("-?"),"Letters which don't count as characters (e.g. gaps)")
    ;

  options_description all("All options");
  all.add(visible).add(invisible);

  // positional options
  positional_options_description p;
  p.add("file", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  bool error = false;

  if (args.count("help") or error) {
    cout<<"Usage: alignment-cat [file1] {[file2] ...} \n";
    cout<<"Concatenate several alignments (with the same sequence names) end-to-end.\n\n";
    cout<<visible<<"\n";
    exit(0);
  }

  return args;
}

void pad_to_same_length(vector<sequence>& s)
{
  // find total alignment length
  vector<unsigned> L;
  for(int i=0;i<s.size();i++)
    L.push_back(s[i].size());
  unsigned AL = max(L);

  // pad sequences if they are less than this length
  for(int i=0;i<s.size();i++)
    if (L[i] < AL)
      (string&)s[i] = (string&)s[i] + string(AL-L[i],'-');
}

vector<sequence> load_file(istream& file,bool pad)
{
  vector<sequence> s = sequence_format::read_guess(file);
  if (s.size() == 0)
    throw myexception()<<"Alignment file didn't contain any sequences!";

  if (pad)
    pad_to_same_length(s);

  for(int i=1;i<s.size();i++)
    if (s[i].size() != s[0].size())
      throw myexception()<<"Alignment file: sequence #"<<i+1<<" '"<<s[i].name<<"' has length "
                         <<s[i].size()<<" != "<<s[0].size();
  return s;
}

vector<sequence> load_file(const string& filename,bool pad)
{
  ifstream file(filename.c_str());
  vector<sequence> s = sequence_format::read_guess(file);
  if (s.size() == 0)
    throw myexception()<<"Alignment file '"<<filename<<"' didn't contain any sequences!";

  if (pad)
    pad_to_same_length(s);

  for(int i=1;i<s.size();i++)
    if (s[i].size() != s[0].size())
      throw myexception()<<"Alignment file '"<<filename<<"': sequence #"<<i+1<<" '"<<s[i].name<<"' has length "
			 <<s[i].size()<<" != "<<s[0].size();
  file.close();
  return s;
}

vector<sequence> select_taxa(const vector<sequence>& S,const vector<string>& names)
{
  vector<sequence> S2;

  vector<int> mapping(names.size(),-1);
  for(int i=0;i<names.size();i++) {
    for(int j=0;j<S.size() and mapping[i] == -1;j++)
      if (names[i] == S[j].name)
	mapping[i] = j;
  }

  bool ok=true;
  myexception error;
  for(int i=0;i<mapping.size();i++)
    if (mapping[i] == -1) {
      if (not ok)
	error<<"\n";
      error<<"Alignment contains no sequence named '"<<names[i]<<"'";
      ok = false;
    }

  if (not ok) throw error;

  for(int i=0;i<mapping.size();i++)
    S2.push_back(S[mapping[i]]);

  return S2;
}


int main(int argc,char* argv[]) 
{ 

  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    assert(args.count("file"));

    bool pad = (args.count("pad")>0);

    vector<string> names;
    if (args.count("taxa"))
      names = split(args["taxa"].as<string>(),',');

    //------- Try to load sequences --------//
    vector <sequence> S;
    if (not args.count("file")) {
      S = load_file(cin,pad);
      if (args.count("taxa"))
	S = select_taxa(S,names);
    }
    else 
    {
      vector<string> filenames = args["file"].as<vector<string> >();

      for(int i=0;i<filenames.size();i++) 
	{
	vector<sequence> s = load_file(filenames[i],pad);
	try {
	  if (args.count("taxa"))
	    s = select_taxa(s,names);
	  S = concatenate(S,s);
	}
	catch (std::exception& e) {
	  throw myexception()<<"File '"<<filenames[i]<<"': "<<e.what();
	}
      }
    }
      
    if (args.count("columns"))
      S = select(S,args["columns"].as<string>());
    
    if (args.count("remove-empty-columns")) {
      string missing = args["missing"].as<string>();
      vector<char> missing2;
      for(int i=0;i<missing.size();i++)
	missing2.push_back(missing[i]);
      S = remove_empty_columns(S,missing2);
    }

    if (args["output"].as<string>() == "phylip")
      write_phylip(cout,S);
    else if (args["output"].as<string>() == "fasta")
      write_fasta(cout,S);
    else
      throw myexception()<<"I don't recognize requested format '"<<args["output"].as<string>()<<"'";
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
