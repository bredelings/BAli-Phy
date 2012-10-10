/*
   Copyright (C) 2005-2008 Benjamin Redelings

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

#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <list>
#include <numeric>
#include "myexception.H"
#include "alignment/alignment.H"
#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "alignment/alignment-util.H"
#include "distance-methods.H"
#include "distance-report.H"
#include "io.H"

#include <boost/program_options.hpp>
#include <boost/numeric/ublas/matrix.hpp>

namespace ublas = boost::numeric::ublas;
namespace po = boost::program_options;
using po::variables_map;

// FIXME - also show which COLUMNS are more that 99% conserved?

// Questions: 1. where does these fit on the length distribution:
//                 a) pair-median  b) splits-median c) MAP
//            2. where do the above alignments fit on the L/prior/L+prior distribution?
//            3. graph average distance between alignments in the (0,q)-th quantile.
//            4. graph autocorrelation to see how quickly it decays...
//            5. distance between the two medians, and the MAP...
//            6. E (average distance) and Var (average distance)

using namespace std;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("files",value<vector<string> >()->composing(),"tree samples to examine")
    ;

  // named options
  options_description input("Input options");
  input.add_options()
    ("help,h", "Produce help message")
    ("skip,s",value<unsigned>()->default_value(0),"number of alignment samples to skip")
    ("max,m",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("verbose,v","Output more log messages on stderr.")
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ;

  options_description analysis("Analysis options");
  analysis.add_options()
    ("metric", value<string>()->default_value("splits"),"type of distance: pairs, splits, splits2")
    ("analysis", value<string>()->default_value("matrix"), "Analysis: matrix, median, diameter")
    ("CI",value<double>()->default_value(0.95),"Confidence interval size.")
    ("mean", "Show mean and standard deviation")
    ("median", "Show median and confidence interval")
    ("minmax", "Show minumum and maximum distances")
    ;

  options_description visible("All options");
  visible.add(input).add(analysis);

  options_description all("All options");
  all.add(invisible).add(input).add(analysis);

  // positional options
  positional_options_description p;
  p.add("analysis", 1);
  p.add("files", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-median [OPTIONS] < in-file\n";
    cout<<"Don't use this program.  It doesn't work.\n";
    cout<<"Find the 'median' alignment in a list of alignments.\n\n";
    cout<<visible<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

typedef long int (*distance_fn)(const ublas::matrix<int>& ,const vector< vector<int> >&,const ublas::matrix<int>& ,const vector< vector<int> >&);

ublas::matrix<double> distances(const vector<ublas::matrix<int> >& Ms,
				const vector< vector< vector<int> > >& column_indices,
				distance_fn distance)
{
  assert(Ms.size() == column_indices.size());
  ublas::matrix<double> D(Ms.size(),Ms.size());

  for(int i=0;i<D.size1();i++) 
    for(int j=0;j<D.size2();j++)
      D(i,j) = distance(Ms[i],column_indices[i],
			Ms[j],column_indices[j]);
  return D;
}

double diameter(const ublas::matrix<double>& D)
{
  double total = 0;
  for(int i=0;i<D.size1();i++)
    for(int j=0;j<i;j++)
      total += D(i,j);
  
  int N = D.size1() * (D.size1() - 1) /2;

  return total/N;
}

/// The number of letters in sequence i that are aligned against different letters in sequence j
int pairwise_alignment_distance_asymmetric(int i, int j, const ublas::matrix<int>& M1 ,const vector< vector<int> >& CI1,const ublas::matrix<int>& M2, const vector< vector<int> >& CI2)
{
  int Li = CI1[i].size();
  assert(Li == CI2[i].size());

  int diff = 0;
  for(int k=0; k<Li; k++)
  {
    int col1 = CI1[i][k];
    int col2 = CI2[i][k];
    if (M1(col1,j) != M2(col2,j))
      diff++;
  }
  return diff;
}

double pairwise_alignment_distance(int i, int j, const ublas::matrix<int>& M1 ,const vector< vector<int> >& CI1,const ublas::matrix<int>& M2, const vector< vector<int> >& CI2)
{
  int total_diff = pairwise_alignment_distance_asymmetric(i,j,M1,CI1,M2,CI2) + pairwise_alignment_distance_asymmetric(j,i,M1,CI1,M2,CI2);

  int Li = CI1[i].size();
  int Lj = CI1[j].size();

  return double(total_diff)/(Li+Lj);
}

Matrix pairwise_alignment_distances(const ublas::matrix<int>& M1 ,const vector< vector<int> >& CI1,const ublas::matrix<int>& M2, const vector< vector<int> >& CI2)
{
  int N = CI1.size();
  Matrix D(N,N);
  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      D(i,j) = pairwise_alignment_distance(i,j,M1,CI1,M2,CI2);

  return D;
}

struct alignment_sample
{
  vector<alignment> alignments;
  vector<ublas::matrix<int> > Ms;
  vector< vector< vector<int> > >  column_indices;

  void load(list<alignment>& As);

  void load(const variables_map& args, const string& filename);

  void load(const variables_map& args, const vector<string>& seq_names, const alphabet& a, const string& filename);

  unsigned size() const {return alignments.size();}

  const alignment& operator[](int i) const {return alignments[i];}

  vector<string> sequence_names() const {return ::sequence_names(alignments[0]);}

  const alphabet& get_alphabet() const {return alignments[0].get_alphabet();}

  alignment_sample(const variables_map& args, const string& filename)
  {
    load(args,filename);

    if (not alignments.size())
      throw myexception()<<"Alignment sample is empty.";
  }

  alignment_sample(const variables_map& args, const vector<string>& seq_names, const alphabet& a, const string& filename)
  {
    load(args,seq_names,a,filename);

    if (not alignments.size())
      throw myexception()<<"Alignment sample is empty.";
  }
};

void alignment_sample::load(list<alignment>& As)
{
  for(auto& a: As)
  {
    // Chop off internal node sequences, if any
    a = chop_internal(a);
    Ms.push_back(M(a));
    column_indices.push_back( column_lookup(a) );
  }
  alignments.insert(alignments.end(),As.begin(),As.end());
}

void alignment_sample::load(const variables_map& args, const string& filename)
{
  //------------ Try to load alignments -----------//
  int maxalignments = -1;
  if (args.count("max"))
    maxalignments = args["max"].as<int>();
 
  unsigned skip = 0;
  if (args.count("skip"))
    skip = args["skip"].as<unsigned>();

  if (log_verbose) cerr<<"alignment-median: Loading alignments...";

  istream_or_ifstream input(cin,"-",filename,"alignment file");

  list<alignment> As;
  if (not alignments.size())
    As = load_alignments(input,load_alphabets(args),skip,maxalignments);
  else
    As = load_alignments(input, sequence_names(), get_alphabet(), skip,maxalignments);

  if (log_verbose) cerr<<"done. ("<<alignments.size()<<" alignments)"<<endl;
  load(As);
}

void alignment_sample::load(const variables_map& args, const vector<string>& seq_names, const alphabet& a, const string& filename)
{
  //------------ Try to load alignments -----------//
  int maxalignments = -1;
  if (args.count("max"))
    maxalignments = args["max"].as<int>();
 
  unsigned skip = 0;
  if (args.count("skip"))
    skip = args["skip"].as<unsigned>();

  if (log_verbose) cerr<<"alignment-median: Loading alignments...";

  istream_or_ifstream input(cin,"-",filename,"alignment file");

  assert(not alignments.size());

  list<alignment> As;
  As = load_alignments(input, seq_names, a, skip, maxalignments);

  if (log_verbose) cerr<<"done. ("<<alignments.size()<<" alignments)"<<endl;

  load(As);
}

ublas::matrix<double> distances(const alignment_sample& A, distance_fn distance)
{
  return distances(A.Ms, A.column_indices, distance);
}


int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line ---------//
    variables_map args = parse_cmd_line(argc,argv);

    string analysis = args["analysis"].as<string>();

    string metric = args["metric"].as<string>();

    //--------------- filenames ---------------//
    vector<string> files;
    if (args.count("files"))
      files = args["files"].as<vector<string> >();

    //--------- Determine distance function -------- //
    distance_fn metric_fn;

    metric_fn = splits_distance;
    if (metric == "splits2")
      metric_fn = splits_distance2;
    else if (metric == "pairwise")
      metric_fn = pairs_distance;
      
    //---------- write out distance matrix --------- //
    if (analysis == "matrix") 
    {
      check_supplied_filenames(1,files,false);

      alignment_sample As(args, files[0]);
      ublas::matrix<double> D = distances(As.Ms, As.column_indices, metric_fn);

      for(int i=0;i<D.size1();i++) {
	vector<double> v(D.size2());
	for(int j=0;j<v.size();j++)
	  v[j] = D(i,j);
	cout<<join(v,'\t')<<endl;
      }

      exit(0);
    }
    else if (analysis == "accuracy-matrix") 
    {
      check_supplied_filenames(2,files,false);

      alignment_sample As(args, files[0]);

      alignment_sample A({}, As.sequence_names(), As.get_alphabet(), files[1]);

      if (A.size() != 1) throw myexception()<<"The second file should only contain one alignment!";

      std::cerr<<"Averaging over "<<As.size()<<" sampled alignments.";

      int N = A.sequence_names().size();
      Matrix D(N,N);

      for(int i=0;i<As.size();i++) 
      {
	if (i == 0)
	  D = pairwise_alignment_distances(As.Ms[i], As.column_indices[i], A.Ms[0], A.column_indices[0]);
	else
	  D += pairwise_alignment_distances(As.Ms[i], As.column_indices[i], A.Ms[0], A.column_indices[0]);
      }
      
      D /= As.size();

      for(int i=0;i<D.size1();i++) {
	vector<double> v(D.size2());
	for(int j=0;j<v.size();j++)
	  v[j] = D(i,j);
	cout<<join(v,'\t')<<endl;
      }
      
      exit(0);
    }
    else if (analysis == "compare")
    {
      check_supplied_filenames(2,files);

      alignment_sample both(args,files[0]);
      int N1 = both.size();
      both.load(args, files[1]);
      int N2 = both.size() - N1;

      ublas::matrix<double> D  = distances(both,metric_fn);

      report_compare(args, D, N1, N2);
    }
    else if (analysis == "median") 
    {
      check_supplied_filenames(1,files,false);

      alignment_sample As(args, files[0]);

      ublas::matrix<double> D = distances(As, metric_fn);

      //----------- accumulate distances ------------- //
      vector<double> ave_distances( As.size() , 0);
      for(int i=0;i<ave_distances.size();i++)
	for(int j=0;j<i;j++) {
	  ave_distances[i] += D(i,j);
	  ave_distances[j] += D(i,j);
	}
      for(int i=0;i<ave_distances.size();i++)
	ave_distances[i] /= (D.size1()-1);

      int argmin = ::argmin(ave_distances);

      cout<<As[argmin]<<endl;

      // Get a list of alignments in decreasing order of E D(i,A)
      vector<int> items = iota<int>(As.size());
      sort(items.begin(),items.end(),sequence_order<double>(ave_distances));

      cerr<<endl;
      for(int i=0;i<As.size() and i < 5;i++) 
      {
	int j = items[i];
	cerr<<"alignment = "<<i<<"   length = "<<As.Ms[j].size1();
	cerr<<"   E D = "<<ave_distances[j]<<endl;
      }

      cerr<<endl;
      double total=0;
      for(int i=1;i<items.size() and i < 5;i++) {
	for(int j=0;j<i;j++)
	  total += D(items[i], items[j]);
	
	cerr<<"fraction = "<<double(i)/(items.size()-1)<<"     AveD = "<<double(total)/(i*i+i)*2<<endl;
      }
      cerr<<endl;
      cerr<<"diameter = "<<diameter(D)<<endl;
      exit(0);  
    }
    else if (analysis == "diameter")
    {
      check_supplied_filenames(1,files,false);

      alignment_sample As(args, files[0]);

      ublas::matrix<double> D = distances(As, metric_fn);

      diameter(D,"1",args);
    }
    else if (analysis == "compression")
    {
      alignment_sample As(args, files[0]);

      ublas::matrix<double> D = distances(As, metric_fn);

      //----------- accumulate distances ------------- //
      vector<double> ave_distances( As.size() , 0);
      for(int i=0;i<ave_distances.size();i++)
	for(int j=0;j<i;j++) {
	  ave_distances[i] += D(i,j);
	  ave_distances[j] += D(i,j);
	}
      for(int i=0;i<ave_distances.size();i++)
	ave_distances[i] /= (D.size1()-1);

      int argmin = ::argmin(ave_distances);

      // Get a list of alignments in decreasing order of E D(i,A)
      vector<int> items = iota<int>(As.size());
      sort(items.begin(),items.end(),sequence_order<double>(ave_distances));

      for(int i=0;i<As.size();i++) 
	{
	  int j = items[i];
	  cerr<<"alignment = "<<i<<"   length = "<<As.Ms[j].size1();
	  cerr<<"   E D = "<<ave_distances[j]
	      <<"   E D1 = "<<ave_distances[argmin];
	  cerr<<endl;
	}


      double total=0;
      for(int i=1;i<items.size();i++) {
	for(int j=0;j<i;j++)
	  total += D(items[i], items[j]);
	
	cerr<<"fraction = "<<double(i)/(items.size()-1)<<"     AveD = "<<double(total)/(i*i+i)*2<<endl;
      }
    }
    else
      throw myexception()<<"Analysis '"<<analysis<<"' not recognized.";
  }
  catch (exception& e) {
    cerr<<"alignment-median: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
