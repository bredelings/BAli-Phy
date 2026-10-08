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
#include "util/myexception.hh"
#include "alignment/alignment.hh"
#include "optimize.hh"
#include "findroot.hh"
#include "alignment/alignment-util.hh"
#include "alignment/index-matrix.hh" // for M( )
#include "alignment/load.hh"
#include "distance-methods.hh"
#include "distance-report.hh"

#include "util/io.hh"
#include "util/string/split.hh"
#include "util/string/join.hh"
#include "util/string/convert.hh"
#include "util/range.hh"

#include <CLI/CLI.hpp>

extern int log_verbose;

// FIXME - also show which COLUMNS are more that 99% conserved?

// Questions: 1. where does these fit on the length distribution:
//                 a) pair-median  b) splits-median c) MAP
//            2. where do the above alignments fit on the L/prior/L+prior distribution?
//            3. graph average distance between alignments in the (0,q)-th quantile.
//            4. graph autocorrelation to see how quickly it decays...
//            5. distance between the two medians, and the MAP...
//            6. E (average distance) and Var (average distance)

using namespace std;

typedef double (*distance_fn)(const matrix<int>& ,const vector< vector<int> >&,const matrix<int>& ,const vector< vector<int> >&);

typedef double (*pairwise_distance_fn)(int, int, const matrix<int>& ,const vector< vector<int> >&,const matrix<int>& ,const vector< vector<int> >&);

// Evaluate ordered pairs; summaries require finite off-diagonal values.
matrix<double> distances(const vector<matrix<int> >& Ms,
			 const vector< vector< vector<int> > >& column_indices,
			 distance_fn distance, bool require_finite = false)
{
    assert(Ms.size() == column_indices.size());
    matrix<double> D(Ms.size(),Ms.size());

    for(int i=0;i<D.size1();i++) 
	for(int j=0;j<D.size2();j++)
        {
	    D(i,j) = distance(Ms[i],column_indices[i], Ms[j],column_indices[j]);
            if (require_finite and i != j and not std::isfinite(D(i,j)))
                throw myexception()<<"Cannot summarize undefined distance from alignment "<<i+1<<" to "<<j+1<<".";
        }
    return D;
}

// Average off-diagonal distances, including both directions for asymmetric measures.
double diameter(const matrix<double>& D, bool directed)
{
    double total = 0;
    for(int i=0;i<D.size1();i++)
	for(int j=0;j<i;j++)
            total += D(i,j) + (directed ? D(j,i) : 0.0);

    int N = D.size1() * (D.size1() - 1) / (directed ? 1 : 2);

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
	if (not alphabet::is_character(M1(col1,j))) continue;

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
	    if (alphabet::is_character(M1(c,j)))
		n++;
	total += n*(n-1)/2;
    }
    return total;
}

/// The number of letters in sequence i that are aligned against different letters in sequence j
long int pairwise_alignment_distance_asymmetric(int i, int j, const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
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
typedef double (*pairwise_alignment_distance_t)(int i, int j, const matrix<int>&, const vector< vector<int> >&,const matrix<int>&, const vector< vector<int> >&);

double pairwise_alignment_distance_symmetric(int i, int j, const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    int total_diff = pairwise_alignment_distance_asymmetric(i,j,M1,CI1,M2,CI2) + pairwise_alignment_distance_asymmetric(j,i,M1,CI1,M2,CI2);

    int Li = CI1[i].size();
    int Lj = CI1[j].size();

    return double(total_diff)/(Li+Lj);
}

double pairwise_alignment_distance_nonrecall(int i, int j, const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    int shared_homologies = pairwise_shared_homologies(i, j, M1, CI1, M2, CI2);
    int true_homologies  = pairwise_shared_homologies(i, j, M1, CI1, M1, CI1);

    assert(shared_homologies <= true_homologies);
    
    return double(true_homologies - shared_homologies) / true_homologies;
}

double pairwise_alignment_distance_inaccuracy(int i, int j, const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    int shared_homologies = pairwise_shared_homologies(i, j, M1, CI1, M2, CI2);
    int predicted_homologies  = pairwise_shared_homologies(i, j, M2, CI2, M2, CI2);

    assert(shared_homologies <= predicted_homologies);
    
    return double(predicted_homologies - shared_homologies) / predicted_homologies;
}

Matrix pairwise_alignment_distances(pairwise_alignment_distance_t distance_fn, 
                                    const matrix<int>& M1 ,const vector< vector<int> >& CI1,const matrix<int>& M2, const vector< vector<int> >& CI2)
{
    int N = CI1.size();
    Matrix D(N,N);
    for(int i=0;i<N;i++)
	for(int j=0;j<N;j++)
	    D(i,j) = distance_fn(i,j,M1,CI1,M2,CI2);

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

    int load(list<alignment>& As, const alignment* reference);

    int load(const string& filename, const string& alphabet_name, unsigned skip, int maxalignments,
             const alignment* reference = nullptr);

    unsigned size() const {return alignments.size();}

    const alignment& operator[](int i) const {return alignments[i];}

    vector<string> sequence_names() const {return ::sequence_names(alignments[0]);}

    const alphabet& get_alphabet() const {return alignments[0].get_alphabet();}

    alignment_sample() = default;

    // Load a sample with explicit thinning settings, or an unthinned reference.
    alignment_sample(const string& filename, const string& alphabet_name, unsigned skip, int maxalignments,
                     const alignment* reference = nullptr)
    {
        load(filename, alphabet_name, skip, maxalignments, reference);
        if (alignments.empty())
            throw myexception()<<"Alignment sample is empty.";
    }

};

// Normalize retained alignments before building residue indices or comparing their names.
int alignment_sample::load(list<alignment>& As, const alignment* reference)
{
    for(auto& a: As)
    {
	// Chop off internal node sequences, if any
	a = chop_internal(a);
        check_names_unique(a);
        if (not reference) reference = &a;
        if (a.n_sequences() != reference->n_sequences())
            throw myexception()<<"Expected "<<reference->n_sequences()<<" sequences, got "<<a.n_sequences()<<".";
        if (::sequence_names(a) != ::sequence_names(*reference))
            a = reorder_sequences(a, ::sequence_names(*reference));
        vector<int> lengths(reference->n_sequences());
        for(int i=0;i<lengths.size();i++) lengths[i] = reference->seqlength(i);
        check_same_sequence_lengths(lengths, a);
	Ms.push_back(M(a));
	column_indices.push_back( column_lookup(a) );
    }
    alignments.insert(alignments.end(),As.begin(),As.end());

    return As.size();
}

// Load each file independently so internal-node removal precedes matching to the reference.
int alignment_sample::load(const string& filename, const string& alphabet_name, unsigned skip, int maxalignments,
                           const alignment* reference)
{
    if (log_verbose) cerr<<"alignment-distances: Loading alignments...";
    istream_or_ifstream input(cin,"-",filename,"alignment file");

    if (not reference and not alignments.empty()) reference = &alignments[0];
    auto As = load_alignments(input, reference ? reference->get_alphabet().name : alphabet_name, skip, maxalignments);

    if (log_verbose) cerr<<"done. ("<<As.size()<<" alignments)"<<endl;
    return load(As, reference);
}


// Reuse the cached alignment indices when evaluating a sample.
matrix<double> distances(const alignment_sample& A, distance_fn distance, bool require_finite = false)
{
    return distances(A.Ms, A.column_indices, distance, require_finite);
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
        CLI::App app{"Compute distances between alignments.", "alignment-distances"};
        app.require_subcommand(1);
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string reference_file, sample_file, second_sample_file, alphabet_name, requested_distances;
        vector<string> sample_files;
        unsigned skip = 0;
        int maxalignments = 1000;
        double interval_probability = 0.95;
        bool verbose = false, show_mean = false, show_median = false, show_minmax = false;
        app.add_option("-s,--skip", skip, "Alignments to skip per sample file")
            ->type_name("N")->capture_default_str();
        app.add_option("-m,--max", maxalignments, "Maximum retained alignments per sample file (-1: unlimited)")
            ->type_name("N")->capture_default_str();
        app.add_option("--alphabet", alphabet_name,
                       "Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop")
            ->type_name("ALPHABET");
        app.add_flag("-V,--verbose", verbose, "Output more log messages on stderr");
        app.add_option("--distances", requested_distances,
                       "Measures (colon-separated for score; one for other commands)")->type_name("MEASURES");

        auto* score = app.add_subcommand("score", "Score sample alignments against a reference")->fallthrough();
        auto* axa = app.add_subcommand("AxA", "Compute a matrix of distances between alignments")->fallthrough();
        auto* nxn = app.add_subcommand("NxN", "Compute averaged sequence-pair disagreement scores")->fallthrough();
        auto* compare = app.add_subcommand("compare", "Compare within-group and between-group distances")->fallthrough();
        auto* median = app.add_subcommand("median", "Find the alignment with smallest mean outgoing distance")->fallthrough();
        auto* summary = app.add_subcommand("distances", "Summarize pairwise distances and outgoing means")->fallthrough();

        for (auto* command: {score, nxn})
            command->add_option("REFERENCE", reference_file, "File containing one reference alignment ('-' reads stdin)")
                ->required()->type_name("");
        for (auto* command: {score, axa})
            command->add_option("SAMPLE", sample_files, "Alignment sample files ('-' reads stdin)")
                ->required()->type_name("");
        for (auto* command: {nxn, median, summary})
            command->add_option("SAMPLE", sample_file, "Alignment sample file ('-' reads stdin)")
                ->required()->type_name("");
        compare->add_option("SAMPLE1", sample_file, "First alignment sample file ('-' reads stdin)")
            ->required()->type_name("");
        compare->add_option("SAMPLE2", second_sample_file, "Second alignment sample file ('-' reads stdin)")
            ->required()->type_name("");
        for (auto* command: {compare, summary})
        {
            command->add_option("--CI", interval_probability, "Central interval probability")
                ->type_name("P")->capture_default_str();
            command->add_flag("--mean", show_mean, "Show mean and standard deviation");
            command->add_flag("--median", show_median, "Show median and central interval (default report)");
            command->add_flag("--minmax", show_minmax, "Show minimum and maximum distances");
        }

        // Keep the measure descriptions and examples on separate lines in command help.
        app.get_formatter()->enable_footer_formatting(false);
        app.footer("Use alignment-distances COMMAND --help for arguments, measures, and examples.");
        score->footer("Measures: splits, splits2, pairwise, recall, accuracy, nonrecall, inaccuracy.\n"
                      "Default: splits:splits2:nonrecall:inaccuracy.\n"
                      "--skip and --max apply to samples, not the reference.\n\n"
                      "Example: alignment-distances score true.fasta As.fasta");
        axa->footer("Measures: splits, splits2, pairwise, recall, accuracy, nonrecall, inaccuracy.\n"
                    "Default: splits.\n\n"
                    "Example: alignment-distances AxA sample1.fastas sample2.fastas");
        nxn->footer("Measures: pairwise, nonrecall, inaccuracy. Default: pairwise.\n"
                    "--skip and --max apply to the sample, not the reference.\n\n"
                    "Example: alignment-distances NxN true.fasta As.fasta");
        compare->footer("Measures: splits, splits2, pairwise, recall, accuracy, nonrecall, inaccuracy.\n"
                        "Default: splits.\n\n"
                        "Example: alignment-distances compare --mean sample1.fastas sample2.fastas");
        median->footer("Measures: splits, splits2, pairwise, nonrecall, inaccuracy. Default: splits.\n"
                       "Minimizes distance: use nonrecall/inaccuracy rather than recall/accuracy.\n\n"
                       "Example: alignment-distances median As.fasta > A.fasta");
        summary->footer("Measures: splits, splits2, pairwise, recall, accuracy, nonrecall, inaccuracy.\n"
                        "Default: splits.\n\n"
                        "Example: alignment-distances distances As.fasta");
        // CLI11 2.6 omits inherited options from subcommand help; direct readers to the full list.
        for (auto* command: {score, axa, nxn, compare, median, summary})
            command->footer(command->get_footer() + "\n\nSee alignment-distances --help for shared options.");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            // Let CLI11 print help or diagnostics, retaining the tool's 0/1 exit statuses.
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        if (verbose) log_verbose = 1;
        const string analysis = app.get_subcommands().front()->get_name();

	if (analysis == "NxN") 
	{

            string distance_names = "pairwise";
            if (app.count("--distances"))
                distance_names = requested_distances;
            auto distances = split(distance_names,":");

            pairwise_alignment_distance_t distance_fn = nullptr;

            if (distances.size() != 1)
                throw myexception()<<"alignment-distances NxN: provided "<<distances.size()<<" distances, but only 1 is allowed for NxN!";
            if (distances[0] == "pairwise")
                distance_fn = pairwise_alignment_distance_symmetric;
            else if (distances[0] == "nonrecall")
                distance_fn = pairwise_alignment_distance_nonrecall;
            else if (distances[0] == "inaccuracy")
                distance_fn = pairwise_alignment_distance_inaccuracy;
            else
                throw myexception()<<"alignment-distances NxN: distance '"<<distances[0]<<"' not recognized!\n  Allowed values: pairwise, nonrecall, inaccuracy";

	    alignment_sample A(reference_file, alphabet_name, 0, -1);

	    if (A.size() != 1) throw myexception()<<"The first file should only contain one alignment!";

	    alignment_sample As(sample_file, A.get_alphabet().name, skip, maxalignments, &A[0]);

	    std::cerr<<"Averaging over "<<As.size()<<" sampled alignments.\n";

	    int N = A.sequence_names().size();
	    Matrix D(N, N, 0);

            for(int i=0;i<As.size();i++) 
                D += pairwise_alignment_distances(distance_fn, A.Ms[0], A.column_indices[0], As.Ms[i], As.column_indices[i]);

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

	string distance_names = analysis == "score" ? "splits:splits2:nonrecall:inaccuracy" : "splits";
        if (app.count("--distances"))
            distance_names = requested_distances;

	//--------- Determine distance functions -------- //
	vector<distance_fn> distance_fns;
	for(auto& distance_name: split(distance_names,':'))
	    distance_fns.push_back(get_distance_function(distance_name));

	if (distance_names.empty())
	    throw myexception()<<"No distance functions provided!";

        if (analysis != "score" and distance_fns.size() != 1)
            throw myexception()<<analysis<<" accepts only one distance measure.";
        const bool directed = distance_names == "recall" or distance_names == "accuracy"
                           or distance_names == "nonrecall" or distance_names == "inaccuracy";
        if (analysis == "median" and (distance_names == "recall" or distance_names == "accuracy"))
            throw myexception()<<"median minimizes distances; use nonrecall or inaccuracy instead of "<<distance_names<<".";

        //---------- write out distance matrix --------- //
	if (analysis == "AxA") 
	{

	    alignment_sample As;

	    for(auto& file: sample_files)
	    {
		// FIXME: handline std::cin like trees-distances.
		As.load(file, alphabet_name, skip, maxalignments);
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

	    // Load the true alignment to compare against
	    alignment_sample As1;
	    As1.load(reference_file, alphabet_name, 0, -1);
	    if (As1.size() != 1) throw myexception()<<"The first file should only contain one alignment!";

            vector<string> names;

	    // Load the alignments to score
	    alignment_sample As2;
	    for(auto& file: sample_files)
            {
		int delta = As2.load(file, As1.get_alphabet().name, skip, maxalignments, &As1[0]);
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
	else if (analysis == "compare")
	{

	    alignment_sample both(sample_file, alphabet_name, skip, maxalignments);
	    int N1 = both.size();
	    both.load(second_sample_file, alphabet_name, skip, maxalignments);
	    int N2 = both.size() - N1;

	    matrix<double> D  = distances(both,distance_fns[0],true);

	    report_compare(D, N1, N2, interval_probability, show_mean, show_median, show_minmax, directed);
	}
	else if (analysis == "median") 
	{

	    alignment_sample As(sample_file, alphabet_name, skip, maxalignments);

            if (As.size() == 1)
            {
                cout<<As[0]<<endl;
                cerr<<"Only one alignment; pairwise summaries are unavailable.\n";
                return 0;
            }

	    matrix<double> D = distances(As, distance_fns[0],true);

	    // Row means treat each candidate as the first argument and exclude self-comparisons.
	    vector<double> ave_distances( As.size() , 0);
	    for(int i=0;i<ave_distances.size();i++)
		for(int j=0;j<i;j++) {
		    ave_distances[i] += D(i,j);
		    ave_distances[j] += D(j,i);
		}
	    for(int i=0;i<ave_distances.size();i++)
		ave_distances[i] /= (D.size1()-1);

	    int argmin = ::argmin(ave_distances);

	    cout<<As[argmin]<<endl;

	    // Get a list of alignments in increasing order of E D(i,A)
	    vector<int> items = iota<int>(As.size());
	    sort(items.begin(),items.end(),sequence_order<double>(ave_distances));

	    cerr<<endl;
	    for(int i=0;i<As.size() and i < 5;i++) 
	    {
		int j = items[i];
		cerr<<"rank = "<<i<<"   length = "<<As.Ms[j].size1();
		cerr<<"   E D = "<<ave_distances[j]<<endl;
	    }

	    cerr<<endl;
	    double total=0;
	    for(int i=1;i<items.size() and i < 5;i++) {
		for(int j=0;j<i;j++)
		    total += D(items[i], items[j]) + (directed ? D(items[j], items[i]) : 0.0);
	
		cerr<<"fraction = "<<double(i)/(items.size()-1)<<"     AveD = "<<double(total)/(i*i+i)*(directed ? 1 : 2)<<endl;
	    }
	    cerr<<endl;
	    cerr<<"mean pairwise distance = "<<diameter(D, directed)<<endl;
	    exit(0);  
	}
	else if (analysis == "distances")
	{

	    alignment_sample As(sample_file, alphabet_name, skip, maxalignments);

	    matrix<double> D = distances(As, distance_fns[0],true);

	    // from tools/distance-report.hh
	    // computes distribution of average distance from A[i] to A[j], averaged over j
	    // computes distribution of distances from A[i] to A[j]

	    // We probably shouldn't call this a diameter
	    diameter(D,"1", interval_probability, show_mean, show_median, show_minmax, directed);
	}
    }
    catch (exception& e) {
	cerr<<"alignment-distances: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
