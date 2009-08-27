#include <iostream>
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::string;
using std::vector;
using std::endl;

using std::cout;
using std::cerr;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("data-dir", value<string>()->default_value("Data"),"data directory")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ("invariant",value<int>(),"print only sites where this site and <arg> neighbors are invariant.")
    ("differences",value<int>()->default_value(0),"how many sequences may differ from the majority?")
    ("avoid-gaps",value<int>()->default_value(3),"How far from a gap must a column be to be invariant?")
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
    cout<<"Usage: alignment-indices <alignment-file> [OPTIONS]\n";
    cout<<"Show the alignment in terms of the index of each character in its sequence.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


vector<int> column_count(const alignment& A, int c)
{
  const alphabet& a = A.get_alphabet();
  vector<int> count(a.size()+1,0);

  for(int i=0;i<A.n_sequences();i++) {
    int l = A(c,i);
    if (A.get_alphabet().is_letter(l))
      count[l]++;

    if (l == alphabet::gap or l == alphabet::unknown)
      count.back()++;
  }
  return count;
}

std::pair<vector<int>,vector<int> > find_major_character(const alignment& A,int allowed_differences)
{
  const alphabet& a = A.get_alphabet();

  vector<int> majority(A.length(), alphabet::unknown);

  vector<int> safe(A.length(), 0);

  for(int c=0;c<majority.size();c++) 
  {
    vector<int> count = column_count(A,c);
    
    int max_letter = argmax(count);
    majority[c] = max_letter;
    
    // NOTE! Major character is gap if there is more than 1 gap!
    if (count[a.size()] > 1)
      majority[c] = alphabet::gap;
    else if (A.n_sequences() - count[max_letter] <= allowed_differences)
      safe[c] = 1;
    
    /*
      if (safe[c] == 1) {
      std::cerr<<"Column "<<c+1<<" is safe: "<<a.lookup(max_letter)<<"\n";
      }
    */
  }
  
  return std::pair<vector<int>,vector<int> >(majority,safe);
}

int main(int argc,char* argv[]) 
{ 

  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //------- Try to load alignment --------//
    alignment A = load_A(args);

    ublas::matrix<int> MA = M(A);

    //------- Write the sequence names ------//
    for(int i=0;i<A.n_sequences();i++) {
      cout<<A.seq(i).name;
      if (i==A.n_sequences()-1)
	cout<<std::endl;
      else
	cout<<" ";
    }
      
    const alphabet& a = A.get_alphabet();


    //------- Determine invariant sites -----//
    int allowed_differences = args["differences"].as<int>();
    int avoid_gaps = args["avoid-gaps"].as<int>();

    std::pair<vector<int>,vector<int> > result = find_major_character(A,allowed_differences);
    vector<int> majority = result.first;
    vector<int> safe     = result.second;

    int invariant = -1;
    vector<int> safe2 = safe;

    if (args.count("invariant")) 
    {
      invariant = args["invariant"].as<int>();

      for(int i=0;i<safe2.size();i++) 
      {
	bool ok = true;
	if (not safe2[i]) continue;

	// Unsafe if we are in 3 residues of a gap
	for(int k=i-avoid_gaps;k<=i+avoid_gaps and ok;k++) 
        {
	  if (k < 0 or k >= A.length())
	    ;
	  else if (majority[k] == alphabet::gap)
	    ok = false;
	}

	// Unsafe if we are in @invariant characters of a gap or an unsafe column
	for(int k=i-invariant;k<=i+invariant and ok;k++) 
        {
	  if (k < 0 or k >= A.length())
	    ok = false;
	  else if (majority[k] == alphabet::gap)
	    ok = false;
	  else if (not safe[k])
	    ok = false;
	}

	if (not ok)
	  safe2[i] = 0;
      }

    }

    //------- Write the columns ------//
    for(int c=0;c<MA.size1();c++) 
    {
      if (invariant != -1 and not safe2[c]) continue;

      // write the indices
      for(int i=0;i<MA.size2();i++) {
	if (MA(c,i) == alphabet::gap or MA(c,i) == alphabet::unknown
	    or (invariant != -1 and A(c,i) != majority[c]))
	  cout<<"-";
	else
	  cout<<MA(c,i);
	cout<<" ";
      } 

      // start a comment
      cout<<"    #  ";

      // write the column number
      cout<<c+1<<"   ";

      // write the letters
      for(int i=0;i<MA.size2();i++)
	cout<<a.lookup(A(c,i))<<" ";
      cout<<std::endl;
    }
  }
  catch (std::exception& e) {
    cerr<<"alignment-indices: Error! "<<e.what()<<endl;
  }
  return 0;
}
