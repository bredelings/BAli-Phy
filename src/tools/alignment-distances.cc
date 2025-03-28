/*
  Copyright (C) 2005-2009,2011-2012,2014,2017-2018 Benjamin Redelings

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
#include "util/myexception.H"
#include "alignment/alignment.H"
#include "optimize.H"
#include "findroot.H"
#include "alignment/alignment-util.H"
#include "alignment/load.H"
#include "distance-methods.H"
#include "distance-report.H"

#include "util/io.H"
#include "util/string/split.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include "util/cmdline.H"
#include "util/range.H"

#include <boost/program_options.hpp>

extern int log_verbose;

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
	("skip,s",value<unsigned>()->default_value(0),"Number of alignment samples to skip.")
	("max,m",value<int>()->default_value(1000),"Maximum number of alignments to analyze.")
	("verbose,V","Output more log messages on stderr.")
	("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
	;

    options_description analysis("Analysis options");
    analysis.add_options()
	("distances", value<string>()->default_value("splits:splits2:nonrecall:inaccuracy"),"Colon-separated list of distances.")
	("analysis", value<string>(), "Analysis: score, AxA, NxN, compare, median, distances")
	("CI",value<double>()->default_value(0.95),"Confidence interval size.")
	("mean", "Show mean and standard deviation")
	("median", "Show median and confidence interval")
	("minmax", "Show minimum and maximum distances")
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

    if (args.count("help") or not args.count("analysis"))
    {
	cout<<"Compute distances between alignments.\n\n";
	cout<<"Usage: alignment-distances <analysis> alignments-file1 [alignments-file2 ...]\n\n";
	cout<<visible<<"\n";

	cout<<"Distances:\n";
	cout<<"  splits, splits2, pairwise, recall, accuracy, nonrecall, inaccuracy\n\n";

	cout<<"Examples:\n\n";

	cout<<" Compute distances from true.fasta to each in As.fasta:\n";
	cout<<"   % alignment-distances score true.fasta As.fasta\n\n";

	cout<<" Compute distance matrix between all pairs of alignments in all files:\n";
	cout<<"   % alignment-distances AxA file1.fasta ... fileN.fasta\n\n";

	cout<<" Compute all NxN pairwise alignment accuracies, averaged over As:\n";
	cout<<"   % alignment-distances NxN true.fasta As.fasta\n\n";

	cout<<" Find alignment with smallest average distance to other alignments:\n";
	cout<<"   % alignment-distances median As.fasta A.fasta\n\n";

	cout<<" Compare the distances with-in and between the two groups:\n";
	cout<<"   % alignment-distances compare A-dist1.fasta A-dist2.fasta\n\n";

	cout<<" Report distribution of average distance to other alignments:\n";
	cout<<"   % alignment-distances distances As.fasta A.fasta\n\n";

	exit(0);
    }

    if (args.count("verbose")) log_verbose = 1;

    return args;
}

typedef double (*distance_fn)(const matrix<int>& ,const vector< vector<int> >&,const matrix<int>& ,const vector< vector<int> >&);

typedef double (*pairwise_distance_fn)(int, int, const matrix<int>& ,const vector< vector<int> >&,const matrix<int>& ,const vector< vector<int> >&);

matrix<double> distances(const vector<matrix<int> >& Ms,
			 const vector< vector< vector<int> > >& column_indices,
			 distance_fn distance)
{
    assert(Ms.size() == column_indices.size());
    matrix<double> D(Ms.size(),Ms.size());

    for(int i=0;i<D.size1();i++) 
	for(int j=0;j<D.size2();j++)
	    D(i,j) = distance(Ms[i],column_indices[i],
			      Ms[j],column_indices[j]);
    return D;
}

double diameter(const matrix<double>& D)
{
    double total = 0;
    for(int i=0;i<D.size1();i++)
	for(int j=0;j<i;j++)
	    total += D(i,j);
  
    int N = D.size1() * (D.size1() - 1) /2;

    return total/N;
}

long int pairwise_shared_homologies(int i, int j, const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    int Li = CI1[i].size();
    assert(Li == CI2[i].size());

    long int num = 0;
    for(int k=0; k<Li; k++)
    {
	int col1 = CI1[i][k];

	// Not a homology in A1
	if (not alphabet::is_feature(M1(col1,j))) continue;

	// Not a shared homology with A2
	int col2 = CI2[i][k];
	if (M1(col1,j) != M2(col2,j)) continue;

	num++;
    }
    return num;
}


long int total_homologies(const matrix<int>& M1)
{
    long int total = 0;
    for(int c=0;c<M1.size1();c++)
    {
	int n = 0;
	for(int j=0;j<M1.size2();j++)
	    if (alphabet::is_feature(M1(c,j)))
		n++;
	total += n*(n-1)/2;
    }
    return total;
}

/// The number of letters in sequence i that are aligned against different letters in sequence j
long int pairwise_alignment_distance_asymmetric(int i, int j, const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    // broken
    int Li = CI1[i].size();
    assert(Li == CI2[i].size());

    int diff = 0;
    for(int k=0; k<Li; k++)
    {
	int col1 = CI1[i][k];
	int col2 = CI2[i][k];

	assert(M1(col1,i) == M2(col2,i));

	if (M1(col1,j) != M2(col2,j))
	    diff++;
    }
    return diff;
}

double pairwise_alignment_distance(int i, int j, const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    int total_diff = pairwise_alignment_distance_asymmetric(i,j,M1,CI1,M2,CI2) + pairwise_alignment_distance_asymmetric(j,i,M1,CI1,M2,CI2);

    int Li = CI1[i].size();
    int Lj = CI1[j].size();

    return double(total_diff)/(Li+Lj);
}

Matrix pairwise_alignment_distances(const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    int N = CI1.size();
    Matrix D(N,N);
    for(int i=0;i<N;i++)
	for(int j=0;j<N;j++)
	    D(i,j) = pairwise_alignment_distance(i,j,M1,CI1,M2,CI2);

    return D;
}

double fraction_shared_homologies(const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    int N = M1.size2();
    long int shared = 0;
    for(int i=0;i<N;i++)
	for(int j=0;j<i;j++)
	    shared += pairwise_shared_homologies(i,j,M1,CI1,M2,CI2);
    long int total = total_homologies(M1);
    return double(shared)/total;
}

double homology_recall(const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    return fraction_shared_homologies(M1, CI1, M2, CI2);
}

double homology_unrecalled(const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    return 1.0 - homology_recall(M1, CI1, M2, CI2);
}

double homology_accuracy(const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    return fraction_shared_homologies(M2, CI2, M1, CI1);
}

double homology_inaccuracy(const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    return 1.0 - homology_accuracy(M1, CI1, M2, CI2);
}



struct alignment_sample
{
    vector<alignment> alignments;
    vector<matrix<int> > Ms;
    vector< vector< vector<int> > >  column_indices;

    int load(list<alignment>& As);

    int load(const variables_map& args, const string& filename);

    int load(const variables_map& args, const vector<string>& seq_names, const alphabet& a, const string& filename);

    unsigned size() const {return alignments.size();}

    const alignment& operator[](int i) const {return alignments[i];}

    vector<string> sequence_names() const {return ::sequence_names(alignments[0]);}

    const alphabet& get_alphabet() const {return alignments[0].get_alphabet();}

    alignment_sample() = default;

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

int alignment_sample::load(list<alignment>& As)
{
    for(auto& a: As)
    {
	// Chop off internal node sequences, if any
	a = chop_internal(a);
	Ms.push_back(M(a));
	column_indices.push_back( column_lookup(a) );
    }
    alignments.insert(alignments.end(),As.begin(),As.end());

    return As.size();
}

int alignment_sample::load(const variables_map& args, const string& filename)
{
    //------------ Try to load alignments -----------//
    int maxalignments = -1;
    if (args.count("max"))
	maxalignments = args["max"].as<int>();
 
    unsigned skip = 0;
    if (args.count("skip"))
	skip = args["skip"].as<unsigned>();

    if (log_verbose) cerr<<"alignment-distances: Loading alignments...";

    istream_or_ifstream input(cin,"-",filename,"alignment file");

    list<alignment> As;
    if (not alignments.size())
	As = load_alignments(input,get_alphabet_name(args),skip,maxalignments);
    else
	As = load_alignments(input, sequence_names(), get_alphabet(), skip,maxalignments);

    if (log_verbose) cerr<<"done. ("<<As.size()<<" alignments)"<<endl;

    return load(As);
}

int alignment_sample::load(const variables_map& args, const vector<string>& seq_names, const alphabet& a, const string& filename)
{
    //------------ Try to load alignments -----------//
    int maxalignments = -1;
    if (args.count("max"))
	maxalignments = args["max"].as<int>();
 
    unsigned skip = 0;
    if (args.count("skip"))
	skip = args["skip"].as<unsigned>();

    if (log_verbose) cerr<<"alignment-distances: Loading alignments...";

    istream_or_ifstream input(cin,"-",filename,"alignment file");

    list<alignment> As;
    As = load_alignments(input, seq_names, a, skip, maxalignments);

    if (log_verbose) cerr<<"done. ("<<As.size()<<" alignments)"<<endl;

    return load(As);
}

matrix<double> distances(const alignment_sample& A, distance_fn distance)
{
    return distances(A.Ms, A.column_indices, distance);
}

distance_fn get_distance_function(const string& distance_name)
{
    if (distance_name == "splits")
	return splits_distance;
    else if (distance_name == "splits2")
	return splits_distance2;
    else if (distance_name == "pairwise")
	return pairs_distance;
    else if (distance_name == "recall")
	return homology_recall;
    else if (distance_name == "accuracy")
	return homology_accuracy;
    else if (distance_name == "nonrecall")
	return homology_unrecalled;
    else if (distance_name == "inaccuracy")
	return homology_inaccuracy;
    else
	throw myexception()<<"I don't recognize alignment distance '"<<distance_name<<"'";
}

int main(int argc,char* argv[]) 
{ 
    try {
	//----------- Parse command line ---------//
	variables_map args = parse_cmd_line(argc,argv);

        string analysis = args["analysis"].as<string>();

	string distance_names = args["distances"].as<string>();

	//--------------- filenames ---------------//
	vector<string> files;
	if (args.count("files"))
	    files = args["files"].as<vector<string> >();

	//--------- Determine distance functions -------- //
	vector<distance_fn> distance_fns;
	for(auto& distance_name: split(distance_names,':'))
	    distance_fns.push_back(get_distance_function(distance_name));

	if (distance_names.empty())
	    throw myexception()<<"No distance functions provided!";

        //---------- write out distance matrix --------- //
	if (analysis == "AxA") 
	{
	    check_supplied_filenames(1,files,false);

	    alignment_sample As;

	    for(auto& file: files)
	    {
		// FIXME: handline std::cin like trees-distances.
		As.load(args,file);
	    }

	    matrix<double> D = distances(As.Ms, As.column_indices, distance_fns[0]);

	    for(int i=0;i<D.size1();i++) {
		vector<double> v(D.size2());
		for(int j=0;j<v.size();j++)
		    v[j] = D(i,j);
		cout<<join(v,'\t')<<endl;
	    }

	    exit(0);
	}
	//---------- write out distance matrix --------- //
	else if (analysis == "score") 
	{
	    check_supplied_filenames(2,files,false);

	    // Load the true alignment to compare against
	    alignment_sample As1;
	    As1.load(args,files.front());
	    files.erase(files.begin());
	    if (As1.size() != 1) throw myexception()<<"The second file should only contain one alignment!";

            vector<string> names;

	    // Load the alignments to score
	    alignment_sample As2;
	    for(auto& file: files)
            {
		int delta = As2.load(args, As1.sequence_names(), As1.get_alphabet(), file);
                if (delta == 0) std::cerr<<"WARNING: file '"<<file<<"' contained 0 alignments.\n";
                for(int i=0;i<delta;i++)
                    names.push_back(file);
            }

	    // Print out [d(true,a)| a <- As2, d <- distances]
            auto field_names = split(distance_names,":");
            field_names.insert(field_names.begin(),("file"));
	    cout<<join(field_names,"\t")<<endl;
	    for(int i=0; i<As2.size(); i++)
	    {
		vector<string> v;
                v.push_back(names[i]);
		for(auto& D: distance_fns)
		    v.push_back( convertToString( D(As1.Ms[0], As1.column_indices[0], As2.Ms[i], As2.column_indices[i]) ));
		cout<<join(v,'\t')<<endl;
	    }
	    exit(0);
	}
	else if (analysis == "NxN") 
	{
	    check_supplied_filenames(2,files,false);

	    alignment_sample A(args, files[0]);

	    if (A.size() != 1) throw myexception()<<"The first file should only contain one alignment!";

	    alignment_sample As(args, A.sequence_names(), A.get_alphabet(), files[1]);

	    std::cerr<<"Averaging over "<<As.size()<<" sampled alignments.\n";

	    int N = A.sequence_names().size();
	    Matrix D(N,N);

	    for(int i=0;i<As.size();i++) 
	    {
		if (i == 0)
		    D = pairwise_alignment_distances(A.Ms[0], A.column_indices[0], As.Ms[i], As.column_indices[i]);
		else
		    D += pairwise_alignment_distances(A.Ms[0], A.column_indices[0], As.Ms[i], As.column_indices[i]);
	    }
      
	    D /= As.size();

	    cout<<join(As.sequence_names(), '\t')<<"\n";
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

	    matrix<double> D  = distances(both,distance_fns[0]);

	    report_compare(args, D, N1, N2);
	}
	else if (analysis == "median") 
	{
	    check_supplied_filenames(1,files,false);

	    alignment_sample As(args, files[0]);

	    matrix<double> D = distances(As, distance_fns[0]);

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
	else if (analysis == "distances")
	{
	    check_supplied_filenames(1,files,false);

	    alignment_sample As(args, files[0]);

	    matrix<double> D = distances(As, distance_fns[0]);

	    // from tools/distance-report.H
	    // computes distribution of average distance from A[i] to A[j], averaged over j
	    // computes distribution of distances from A[i] to A[j]

	    // We probably shouldn't call this a diameter
	    diameter(D,"1",args);
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
