#include <iostream>
#include <fstream>
#include <string>
#include "tree.H"
#include "alignment.H"
#include "alignment-util.H"
#include "tree-util.H"
#include "util.H"
#include "setup.H"
#include "distance-methods.H"
#include <utility>
#include "index-matrix.H"

using std::pair;

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;

using std::string;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("cutoff",value<unsigned>(),"only leave taxa with more mismatches than this value")
    ("longer-than",value<unsigned>(),"only leave taxa w/ sequences longer than this")
    ("shorter-than",value<unsigned>(),"only leave taxa w/ sequences shorter than this")
    ("down-to",value<int>(),"number of taxa to keep")
    ("keep",value<int>(),"comma-separated list of taxon names to keep - remove others")
    ("remove",value<string>(),"comma-separated list of taxon names to remove")
    ("min-letters",value<int>(),"Remove columns with fewer letters.")
    ("remove-unique",value<int>(),"Remove insertions in a single sequence if longer than this many letters")
    ("verbose,v","Output more log messages on stderr.")
    ("show-lengths","just print out sequence lengths")
    ("sort","Sort partially ordered columns to minimize the number of visible indels.")
    ("remove-crazy",value<int>(),"Remove sequence that have deleted conserved sites")
    ("conserved-fraction",value<double>()->default_value(0.75),"Fraction of sequences that must contain a letter for it to be considered conserved.")
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
    cout<<"Usage: alignment-thin <alignment-file>\n";
    cout<<"Remove the most closely related sequences from an alignment.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

std::pair<int,int> argmin(ublas::matrix<int>& M)
{
  double mvalue = M(0,0);
  int m1 = 0;
  int m2 = 0;

  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      if (M(i,j) < mvalue) {
	mvalue = M(i,j);
	m1 = i;
	m2 = j;
      }
  return std::pair<int,int>(m1,m2);
}

unsigned pairwise_distance(const alignment& A,int i,int j)
{
  const int L = A.length();
  unsigned D=0;

  if (i==j) return 0;

  for(int c=0;c<L;c++)
    if (A(c,i) >= 0 and A(c,i) != A(c,j))
      D++;

  return D;
}

ublas::matrix<int> pairwise_distance_matrix(const alignment& A)
{
  const int N = A.n_sequences();

  ublas::matrix<int> D(N,N);

  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      D(i,j) = pairwise_distance(A,i,j);

  return D;
}

int remove_almost_empty_columns(alignment& A,int n)
{
  int length = 0;

  for(int column=0;column<A.length();column++)
    if (n_characters(A,column) >= n) 
    {
      if (column != length)
	for(int i=0;i<A.n_sequences();i++)
	  A(length,i) = A(column,i);
      length++;
    }

  int n_empty = A.length() - length;

  A.changelength(length);

  return n_empty;
}

int unique_letter(const alignment& A,int c)
{
  if (n_characters(A,c) != 1) return -1;

  for(int i=0;i<A.n_sequences();i++)
    if (A.character(c,i))
      return i;
}


int main(int argc,char* argv[])
{
  try {
    cerr.precision(10);
    cout.precision(10);
    
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //----------- Load alignment and tree ---------//
    alignment A = load_A(args,false);
    const int N = A.n_sequences();
    const int L = A.length();

    if (log_verbose) 
      cerr<<"Read "<<N<<" sequences at length "<<L<<endl;

    vector<string> names = sequence_names(A);

    vector<int> AL(N);
    for(int i=0;i<N;i++)
      AL[i] = A.seqlength(i);

    if (args.count("show-lengths")) {
      for(int i=0;i<A.n_sequences();i++) {
	cout<<AL[i]<<endl;
      }
      exit(0);
    }

    vector<int> keep(A.n_sequences(),1);

    //----------------- remove by length ------------------//

    if (args.count("longer-than"))
    {
      unsigned cutoff = args["longer-than"].as<unsigned>();
      
      for(int i=0;i<A.n_sequences();i++)
	if (AL[i] <= cutoff)
	  keep[i] = 0;
    }

    if (args.count("shorter-than"))
    {
      unsigned cutoff = args["shorter-than"].as<unsigned>();
      
      for(int i=0;i<A.n_sequences();i++)
	if (AL[i] >= cutoff)
	  keep[i] = 0;
    }

    //--------------------- keep -------------------------//

    vector<string> protect;
    if (args.count("keep"))
      protect = split(args["keep"].as<string>(),',');

    for(int i=0;i<protect.size();i++) {
      int p = find_index(names,protect[i]);
      if (p == -1)
	throw myexception()<<"keep: can't find sequence '"<<protect[i]<<"' to keep.";
      keep[p] = 2;
    }

    //-------------------- remove ------------------------//

    vector<string> remove;
    if (args.count("remove"))
      remove = split(args["remove"].as<string>(),',');

    for(int i=0;i<remove.size();i++) {
      int r = find_index(names,remove[i]);
      if (r == -1)
	throw myexception()<<"remove: can't find sequence '"<<remove[i]<<"' to remove.";
      if (keep[r] == 2)
	throw myexception()<<"Can't both keep AND remove '"<<remove[i]<<"'.";
      keep[r] = 0;
    }
      

    if (log_verbose and keep.size() != sum(keep)) cerr<<"Removed "<<keep.size()-sum(keep)<<" sequences because of length constraints."<<endl;

    //-------------------- remove ------------------------//

    if (args.count("remove-crazy")) 
    {
      int n_remove = args["remove-crazy"].as<int>();
      double conserved_fraction = args["conserved-fraction"].as<double>();

      vector<int> conserved(A.n_sequences());
      int n_conserved_columns=0;
      for(int i=0;i<A.length();i++)
      {
	double fraction = double(n_characters(A,i))/A.n_sequences();
	if (fraction < conserved_fraction) continue;

	n_conserved_columns++;
	
	for(int j=0;j<A.n_sequences();j++)
	  if (A.character(i,j))
	    conserved[j]++;
      }

      vector<int> order = iota<int>(conserved.size());
      sort(order.begin(), order.end(), sequence_order<int>(conserved));

      if (log_verbose) {
	cerr<<"total # conserved columns = "<<n_conserved_columns<<endl;
	cerr<<"  conserved: ";
	cerr<<"  min = "<<conserved[order[0]];
	cerr<<"  median = "<<conserved[order[order.size()/2]];
	cerr<<"  max = "<<conserved[order.back()]<<endl;
      }
      for(int i=0;i<n_remove;i++) {
	if ("remove_crazy")
	  cerr<<"Remove crazy: "<<names[order[i]]<<"    "<<conserved[order[i]]<<endl;
	keep[order[i]] = 0;
      }

    }

    //------- Find the most redundant --------//
    ublas::matrix<int> D;

    if (args.count("down-to") or args.count("cutoff"))
    {
      D = pairwise_distance_matrix(A);

      // make sure that removed sequences are never part of an argmin pair.
      for(int i=0;i<N;i++)
	for(int j=0;j<N;j++)
	  if (not keep[i] or not keep[j])
	    D(i,j) = L+1;
	  else if (i==j)
	    D(i,j) = L;

      while(true)
      {
	std::pair<int,int> p = argmin(D);
      
	int p1 = p.first;
	int p2 = p.second;
	
	int MD = D(p1,p2);
	
	bool done = true;
	if (args.count("down-to") and sum(keep) > args["down-to"].as<int>()) done = false;
	if (args.count("cutoff")  and MD < args["cutoff"].as<unsigned>()) done = false;
	if (done) break;
	
	
	//remove the second sequence, if they are the same
	if (D(p1,p2) == D(p2,p1) and A.seq(p1).comment.size() > A.seq(p2).comment.size())
	  std::swap(p1,p2);
	
	keep[p1] = 0;
	for(int i=0;i<N;i++)
	  D(p1,i) = D(i,p1) = L+1;
      }
    }

    //------- Select the sequences -------//
    alignment A2 = select_rows(A,keep);

    //------- Remove consecutive unique sites -----//

    // Question: should I remove ALL of the sites, or just the sites beyond L aa/nucs?
    //    And, how would I remove the MIDDLE sites?
    // Question: should I remove only sites that have no ?  How should I do so?
    if (args.count("remove-unique"))
    {
      int L = args["remove-unique"].as<int>();

      vector<int> keep_sites(A2.length(),1);

      int last=-1;
      int count=0;
      for(int i=0;i<keep_sites.size();i++)
      {
	int u = unique_letter(A,i);
	if (u == -1)
	  count = 0;
	else {
	  if (u == last)
	    count++;
	  else
	    count = 1;
	}
	if (count > L)
	  keep_sites[i] = 0;
	if (count == L+1)
	  for(int j=0;j<L-1;j++)
	    keep_sites[i-j-1] = 0;
	last = u;
      }

      // actually remove the unwanted sites
      vector<int> sites;
      for(int i=0;i<keep_sites.size();i++)
	if (keep_sites[i])
	  sites.push_back(i);
      A2 = select_columns(A2,sites);
    }


    //------- Remove columns with too few letters ------//
    if (args.count("min-letters")) {
      int m = args["min-letters"].as<int>();
      remove_almost_empty_columns(A2,m);
    }

    if (args.count("sort"))
      A2 = get_ordered_alignment(A2);

    //------- Print out the alignment -------//
    std::cout<<A2;

    if (log_verbose) cerr<<"Went from "<<A.n_sequences()<<" -> "<<A2.n_sequences()<<" sequences."<<endl;
    if (log_verbose) cerr<<"Went from "<<A.length()<<" -> "<<A2.length()<<" columns."<<endl;

  }
  catch (std::exception& e) {
    cerr<<"alignment-thin: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
