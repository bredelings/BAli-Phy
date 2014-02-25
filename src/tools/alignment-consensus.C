/*
   Copyright (C) 2005-2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#include "index-matrix.H"
#include <utility>
#include <fstream>
#include <string>
#include <cmath>
#include <list>
#include <numeric>
#include "myexception.H"
#include "math/logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "alignment/alignment-util.H"
#include "distance-methods.H"
#include "rng.H"
#include "statistics.H"



#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

void do_setup(const variables_map& args,vector<alignment>& alignments) 
{
  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();
  unsigned skip = args["skip"].as<unsigned>();

  // --------------------- try ---------------------- //
  if (log_verbose)
    std::cerr<<"alignment-consensus: Loading alignments...";
  list<alignment> As = load_alignments(std::cin,load_alphabets(args),skip,maxalignments);
  alignments.insert(alignments.begin(),As.begin(),As.end());
  if (log_verbose)
    std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;

  if (not alignments.size())
    throw myexception()<<"Alignment sample is empty.";
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ("seed", value<unsigned long>(),"random seed")
    ("skip",value<unsigned>()->default_value(0),"number of tree samples to skip")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("strict",value<double>(),"ignore events below this probability")
    ("cutoff",value<double>(),"ignore events below this probability")
    ("uncertainty",value<string>(),"file-name for AU uncertainty vs level")
    ("verbose","Output more log messages on stderr.")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-consensus [OPTIONS] < alignments-file\n";
    cout<<"Construct a consensus alignment to summarize an alignment sample.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
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
    if (log_verbose)
      cerr<<"alignment-consensus: random seed = "<<seed<<endl<<endl;
    
    //------------ Load alignment and tree ----------//
    vector<alignment> alignments;
    vector<matrix<int> > Ms;

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
    vector<int> L(N);
    for(int i=0;i<L.size();i++)
      L[i] = alignments[0].seqlength(i);

    
    //--------- Construct alignment indexes ---------//
    for(int i=0;i<alignments.size();i++)
      Ms.push_back(M(alignments[i]));


    //--------- Build alignment from list ---------//
    double cutoff_strict = -1;
    double cutoff = -1;

    if (args.count("strict"))
      cutoff_strict = args["strict"].as<double>();

    if (args.count("cutoff"))
      cutoff = args["cutoff"].as<double>();

    if (cutoff_strict < 0 and cutoff < 0)
      cutoff = 0.75;

    //--------- Get list of supported pairs ---------//
    Edges E(L);

    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<s1;s2++)
	add_edges(E,Ms,s1,s2,L[s1],L[s2],
		  min(abs(cutoff),abs(cutoff_strict))
		  );


    E.build_index();

    //-------- Build a beginning alignment --------//
    index_matrix M = unaligned_matrix(L);

    map<unsigned,pair<unsigned,unsigned> > graph;

    if (cutoff_strict > 0) 
      graph = M.merge2(E,cutoff_strict,true);

    if (cutoff > 0) 
      graph = M.merge2(E,cutoff,false);

    //-------- Construct Build a beginning alignment --------//

    matrix<int> M2 = get_ordered_matrix(M);

    alignment consensus = get_alignment(M2,alignments[0]);

    std::cout<<consensus<<std::endl;


    if (args.count("uncertainty")) {

      string filename = args["uncertainty"].as<string>();
      ofstream graph_file(filename.c_str());

      int total_seq_length=0;
      for(int i=0;i<L.size();i++)
	total_seq_length += L[i];

      double scale1 = double(N)/total_seq_length;
      double scale2 = 1.0/total_seq_length;

      foreach(i,graph) {
	double LOD = log10(statistics::odds((*i).first,Ms.size(),1));
	unsigned columns = (*i).second.first;
	unsigned unknowns = (*i).second.second;
	graph_file<<LOD<<" "<<unknowns*scale2<<"  "<<columns*scale1<<endl;
      }
      graph_file.close();
    }

  }
  catch (std::exception& e) {
    std::cerr<<"alignment-consensus: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
