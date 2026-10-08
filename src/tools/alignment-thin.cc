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

#include <iostream>
#include <fstream>
#include <string>
#include "tree/tree.hh"
#include "alignment/alignment.hh"
#include "alignment/alignment-util.hh"
#include "alignment/load.hh"
#include "tree/tree-util.hh"
#include "util/mapping.hh"
#include "util/range.hh"
#include "util/cmdline.hh"
#include "distance-methods.hh"
#include <utility>
#include "alignment/index-matrix.hh"

#include <boost/dynamic_bitset.hpp>

extern int log_verbose;

using std::pair;
using std::vector;
using std::string;
using std::endl;

#include <CLI/CLI.hpp>


using std::cout;
using std::cerr;
using std::endl;

using std::string;
using boost::dynamic_bitset;

std::pair<int,int> argmin(matrix<int>& M)
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
    return {m1,m2};
}


/// Find a pair m1,m2 such that D(m1,m2) is smallest among the sequences not yet removed (keep[]==0) and such that 
/// m1 is not protected (keep[m1]==
std::pair<int,int> argmin(matrix<int>& M, const vector<int>& keep)
{
    double mvalue = -1;
    int m1 = -1;
    int m2 = -1;

    for(int i=0;i<M.size1();i++)
	if (keep[i] == 1) // present, but not protected
	    for(int j=0;j<M.size2();j++)
		if (i != j and keep[j])
		    if (M(i,j) < mvalue or m1 == -1) {
			mvalue = M(i,j);
			m1 = i;
			m2 = j;
		    }
    return {m1,m2};
}

int argmin_row(matrix<int>& M, int i, const vector<int>& keep)
{
    double mvalue = -1;
    int m2 = -1;

    for(int j=0;j<M.size2();j++)
	if (keep[j])
	    if (M(i,j) < mvalue or m2 == -1) {
		mvalue = M(i,j);
		m2 = j;
	    }

    return m2;
}

unsigned asymmetric_distance(const alignment& A,int i,int j)
{
    const int L = A.length();
    unsigned D=0;

    if (i==j) return 0;

    for(int c=0;c<L;c++)
	if (alphabet::is_character(A(c,i)) and A(c,i) != alphabet::not_gap and A(c,i) != A(c,j))
	    D++;

    return D;
}

unsigned symmetric_distance(const alignment& A,int i,int j)
{
    const int L = A.length();
    unsigned D=0;

    if (i==j) return 0;

    for(int c=0;c<L;c++)
	if (alphabet::is_character(A(c,i)) and A(c,i) != alphabet::not_gap and
	    alphabet::is_character(A(c,j)) and A(c,j) != alphabet::not_gap and A(c,i) != A(c,j))
	    D++;

    return D;
}

double symmetric_overlap(const alignment& A,int i,int j)
{
    const int L = A.length();
    unsigned D=0;

    for(int c=0;c<L;c++)
	if (alphabet::is_character(A(c,i)) and A(c,i) != alphabet::not_gap and
	    alphabet::is_character(A(c,j)) and A(c,j) != alphabet::not_gap)
	    D++;

    return D;
}

int n_positive(const vector<int>& v) 
{
    int count=0;
    for(int x: v)
	if (x>0)
	    count++;
    return count;
}

matrix<int> asymmetric_distance_matrix(const alignment& A)
{
    const int N = A.n_sequences();

    matrix<int> D(N,N);

    for(int i=0;i<N;i++)
	for(int j=0;j<N;j++)
	    D(i,j) = asymmetric_distance(A,i,j);

    return D;
}

matrix<int> symmetric_distance_matrix(const alignment& A)
{
    const int N = A.n_sequences();

    matrix<int> D(N,N);

    for(int i=0;i<N;i++)
	for(int j=0;j<=i;j++)
	    D(i,j) = D(j,i) = symmetric_distance(A,i,j);

    return D;
}

matrix<int> symmetric_overlap_matrix(const alignment& A)
{
    const int N = A.n_sequences();

    matrix<int> D(N,N);

    for(int i=0;i<N;i++)
	for(int j=0;j<=i;j++)
	    D(i,j) = D(j,i) = symmetric_overlap(A,i,j);

    return D;
}

dynamic_bitset<> enough_letters(alignment& A, int n)
{
    dynamic_bitset<> keep(A.length());

    for(int column=0;column<A.length();column++)
	if (n_characters(A,column) >= n)
            keep[column] = true;
    return keep;
}

int unique_letter(const alignment& A,int c)
{
    if (n_characters(A,c) != 1) return -1;

    for(int i=0;i<A.n_sequences();i++)
	if (A.character(c,i))
	    return i;

    return -1;
}

vector<int> get_taxon_indices(const vector<string>& names, const vector<string>& lookup)
{
    vector<int> indices(lookup.size());
    for(int i=0;i<indices.size();i++) 
    {
	if (auto l = find_index(names,lookup[i]))
	    indices[i] = *l;
	else
	    throw myexception()<<"keep: can't find sequence '"<<lookup[i]<<"' to keep.";
    }
    return indices;
}

dynamic_bitset<> column_contains_sequence(const alignment& A, int index)
{
    dynamic_bitset<> present(A.length());
    for(int c=0; c < A.length(); c++)
        present[c] = A.character(c,index)?1:0;
    return present;
}


dynamic_bitset<> part_of_long_insertion(const alignment& A, int L)
{
    // Store the owning sequence index, or -1 for a column that is not unique to one sequence.
    // A bitset would collapse these indices and misclassify shared columns as unique.
    vector<int> unique(A.length());
    for(int i=0;i<A.length();i++)
        unique[i] = unique_letter(A,i);

    vector<vector<int> > columns = column_lookup(A);

    dynamic_bitset<> in_long_insertion(A.length());

    for(int i=0;i<A.n_sequences();i++)
    {
        int count = 0;

        for(int j=0;j<columns[i].size()+1;j++)
        {
            int u = -1;
            if (j< columns[i].size())
                u = unique[columns[i][j]];

            if (u == i)
                count++;
            else {
                // close out the current insertion if there is one
                if (count > L) 
                {
                    // remove count-L sites from the middle of the insertion
                    int start = j - count;
                    int end   = j - 1;

                    if (start == 0)
                        end -= L;
                    else if (end == columns[i].size()-1)
                        start += L;
                    else {
                        start += L/2;
                        end   -= (L-L/2);
                    }

                    for(int k=start;k<=end;k++)
                        in_long_insertion[columns[i][k]] = 1;
                }
                count = 0;
            }
        }
    }
    return in_long_insertion;
}

vector<int> conserved_columns(const alignment& A, const vector<int>& keep, double conserved_fraction)
{
    assert(keep.size() == A.n_sequences());

    vector<int> conserved;

    for(int i=0;i<A.length();i++)
    {
	int count = 0;
	int total = 0;
	for(int j=0; j<A.n_sequences();j++)
	{
	    if (keep[j] == 0) continue;

	    total++;

	    if (A.character(i,j))
		count++;
	}
	double fraction = double(count)/total;

	if (fraction >= conserved_fraction)
	    conserved.push_back(i);
    }

    return conserved;
}

int get_score(const alignment& A, const vector<int>& columns, int seq)
{
    int score = 0;

    for(int c: columns)
	if (A.character(c, seq))
	    score++;

    return score;

}

int main(int argc,char* argv[])
{
    try {
	cerr.precision(10);
	cout.precision(10);
    
	//---------- Parse command line  -------//
        CLI::App app{"Remove sequences or columns from an alignment.", "alignment-thin"};
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        app.get_help_ptr()->group("General options");
        string filename, protect_names, keep_names, remove_names, column_reference, duplicate_names;
        unsigned long_length = 0, short_length = 0, similarity_cutoff = 0;
        int down_to_count = 0, gappy_count = 0, min_letters = 0, unique_limit = 0;
        double conserved_fraction = 0.75;
        bool verbose = false, erase_empty = false, sort_alignment = false;
        app.add_option("ALIGNMENT-FILE", filename, "Alignment file ('-' reads stdin)")
            ->required()->type_name("");
        app.add_flag("-V,--verbose", verbose, "Output more log messages on stderr")
            ->group("General options");
        app.add_option("-p,--protect", protect_names, "Sequences that cannot be removed (list or @filename)")
            ->group("Sequence filtering options");
        app.add_option("-k,--keep", keep_names, "Keep listed sequences and protected sequences (list or @filename)")
            ->group("Sequence filtering options");
        app.add_option("-r,--remove", remove_names, "Remove listed unprotected sequences (list or @filename)")
            ->group("Sequence filtering options");
        app.add_option("-l,--longer-than", long_length, "Remove sequences not longer than this length")
            ->group("Sequence filtering options");
        app.add_option("-s,--shorter-than", short_length, "Remove sequences not shorter than this length")
            ->group("Sequence filtering options");
        app.add_option("-c,--cutoff", similarity_cutoff,
                       "Remove similar sequences with fewer than this many directional mismatches")
            ->group("Sequence filtering options");
        app.add_option("-d,--down-to", down_to_count, "Thin similar sequences to this count")
            ->group("Sequence filtering options");
        app.add_option("--remove-gappy", gappy_count, "Remove this many sequences missing conserved sites")
            ->group("Sequence filtering options");
        app.add_option("--conserved", conserved_fraction,
                       "Fraction of retained sequences needed to consider a column conserved")
            ->group("Sequence filtering options")->capture_default_str();
        app.add_option("-K,--keep-columns", column_reference, "Protect columns occupied by this sequence")
            ->group("Column filtering options");
        app.add_option("-m,--min-letters", min_letters, "Remove columns with fewer than this many present characters")
            ->group("Column filtering options");
        app.add_option("-u,--remove-unique", unique_limit, "Shorten private insertions to at most this many characters")
            ->group("Column filtering options");
        app.add_flag("-e,--erase-empty-columns", erase_empty, "Remove columns with no present characters")
            ->group("Column filtering options");
        app.add_flag("-S,--sort", sort_alignment, "Sort partially ordered columns to group similar gaps")
            ->group("Output options");
        app.add_option("-F,--find-dups", duplicate_names, "Report nearest targets from a comma-separated sequence list")
            ->group("Output options");
        app.get_option("--keep")->excludes("--remove");
        // Preserve the examples' indentation and line breaks in CLI11's footer.
        app.get_formatter()->enable_footer_formatting(false);
        app.footer(
                   "Examples:\n\n"
                   " Remove columns without a minimum number of letters:\n"
                   "   % alignment-thin --min-letters=5 file.fasta > file-thinned.fasta\n\n"
                   " Remove sequences by name:\n"
                   "   % alignment-thin --remove=seq1,seq2 file.fasta > file2.fasta\n\n"
                   "   % alignment-thin --keep=seq1,seq2   file.fasta > file2.fasta\n\n"
                   " Remove short sequences:\n"
                   "   % alignment-thin --longer-than=250 file.fasta > file-long.fasta\n\n"
                   " Remove similar sequences with < 5 differences from the closest other sequence:\n"
                   "   % alignment-thin --cutoff=5 file.fasta > more-than-5-differences.fasta\n\n"
                   " Remove similar sequences until we have the right number of sequences:\n"
                   "   % alignment-thin --down-to=30 file.fasta > file-30taxa.fasta\n\n"
                   " Remove dissimilar sequences that are missing conserved columns:\n"
                   "   % alignment-thin --remove-gappy=10 file.fasta > file2.fasta\n\n"
                   " Protect some sequences from being removed:\n"
                   "   % alignment-thin --down-to=30 file.fasta --protect=seq1,seq2 > file2.fasta\n\n"
                   "   % alignment-thin --down-to=30 file.fasta --protect=@filename > file2.fasta\n\n");
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


	//----------- Load alignment and tree ---------//
	alignment A = load_alignment(filename, "", false);
	const int N = A.n_sequences();
	const int L = A.length();

	if (log_verbose) 
	    cerr<<"Read "<<N<<" sequences at length "<<L<<endl;

	vector<string> names = sequence_names(A);

	vector<int> AL(N);
	for(int i=0;i<N;i++)
	    AL[i] = A.seqlength(i);

	//--------------------- keep -------------------------//

	// By default every sequence has status 1 which means, removeable, but not removed.
	vector<int> keep(A.n_sequences(),1);


        for(auto& name: get_string_list(protect_names))
	{
	    if (auto p = find_index(names,name))
		keep[*p] = 2;
	    else
		throw myexception()<<"keep: can't find sequence '"<<name<<"' to keep.";
	}

	//----------------- remove by length ------------------//

	if (app.count("--longer-than"))
	{
	    unsigned cutoff = long_length;
      
	    for(int i=0;i<A.n_sequences();i++)
		if (AL[i] <= cutoff and keep[i] < 2)
		    keep[i] = 0;
	}

	if (app.count("--shorter-than"))
	{
	    unsigned cutoff = short_length;
      
	    for(int i=0;i<A.n_sequences();i++)
		if (AL[i] >= cutoff and keep[i] < 2)
		    keep[i] = 0;
	}

	//-------------------- remove ------------------------//

	if (app.count("--keep"))
	{

	    // FIXME: Currently we protect these and remove everything else.
	    //        Should instead do remove-all-except these?
	    for(auto& r: get_string_list(keep_names))
	    {
		if (auto r_index = find_index(names,r))
		{
		    keep[*r_index] = 2;
		}
		else
		    throw myexception()<<"remove: can't find sequence '"<<r<<"' to remove.";
	    }
	    for(auto& k: keep)
		if (k != 2)
		    k=0;
	}

	for(auto& r: get_string_list(remove_names))
	{
	    if (auto r_index = find_index(names,r))
	    {
		if (keep[*r_index] == 2)
		    //	throw myexception()<<"Can't both keep AND remove '"<<remove[i]<<"'.";
		    ;
		else
		    keep[*r_index] = 0;
	    }
	    else
		throw myexception()<<"remove: can't find sequence '"<<r<<"' to remove.";
	}
      

	if (log_verbose and keep.size() != n_positive(keep)) cerr<<"Removed "<<keep.size()-sum(keep)<<" sequences because of length constraints."<<endl;

	//-------------------- remove ------------------------//

	if (app.count("--remove-gappy"))
	{
	    int n_remove = gappy_count;
	    n_remove = std::min(n_remove, A.n_sequences());

	    for(int i=0; i<n_remove; i++)
	    {
                // Protected survivors cannot be removed; stop before scoring an empty candidate set.
                if (std::find(keep.begin(), keep.end(), 1) == keep.end())
                {
                    cerr<<"No more taxa can be dropped.\n";
                    break;
                }
		auto columns = conserved_columns(A, keep, conserved_fraction);

		vector<int> sequences;

		for(int j=0;j<A.n_sequences();j++)
		    if (keep[j] > 0)
			sequences.push_back(j);

		std::map<int,int> score;
		for(int seq: sequences)
		    score.insert({seq, get_score(A, columns, seq)});

		vector<int> order = sequences;

		std::sort(order.begin(), order.end(), [&](int i, int j) {return score[i] < score[j];});

		if (log_verbose) {
		    cerr<<"total # conserved columns = "<<columns.size()<<endl;
		    cerr<<"  conserved: ";
		    cerr<<"  min = "<<score[order[0]];
		    cerr<<"  median = "<<score[order[order.size()/2]];
		    cerr<<"  max = "<<score[order.back()]<<endl;
		}

		for(int seq: order)
		{
		    if (keep[seq] == 1)
		    {
			cerr<<"Remove: "<<names[seq]<<"    "<<score[seq]<<" / " <<columns.size()<<"   median = "<<score[order[order.size()/2]]<<"   max = "<<score[order.back()]<<endl;
			keep[seq] = 0;
			break;
		    }
		}

	    }
	}

	//------- Find the most redundant --------//
	matrix<int> D;

	// report distances to specified taxa
	if (app.count("--find-dups"))
	{
            // Validate the target list before constructing matrices or searching for neighbors.
            vector<int> compare_to = get_taxon_indices(names, parse_string_list(duplicate_names));
            if (compare_to.empty())
                throw myexception()<<"--find-dups requires at least one target sequence";

	    D = asymmetric_distance_matrix(A);
	    matrix<int> DS = symmetric_distance_matrix(A);
	    matrix<int> DO = symmetric_overlap_matrix(A);

	    matrix<double> DEV(D.size1(),D.size2());
	    for(int i=0;i<DEV.size1();i++) 
		for(int j=0;j<DEV.size2();j++)
		    if (not DO(i,j))
			DEV(i,j) = 1.0;
		    else {
			DEV(i,j) = double(DS(i,j)+5.0)/double(DO(i,j)+5.0);
		    }
	
      
	    // convert the indices to a mask
	    vector<int> target(names.size(),0);
	    for(int i: compare_to)
		target[i] = 1;

	    // compute get the indices of the other taxa
	    vector<int> not_compare_to;
	    for(int i=0;i<target.size();i++)
		if (not target[i])
		    not_compare_to.push_back(i);

	    // find the closest neighbors
	    vector<int> closest(not_compare_to.size());
	    vector<double> distance(not_compare_to.size());
	    for(int i=0;i<not_compare_to.size();i++) {
		closest[i] = argmin_row(DS,not_compare_to[i],target);
		distance[i] = DEV(not_compare_to[i],closest[i]);
	    }
	
	    vector<int> order = iota<int>(not_compare_to.size());
	    sort(order.begin(), order.end(), sequence_order<double>(distance));

	    for(int i=0;i<not_compare_to.size();i++) 
	    {
		int p1 = not_compare_to[order[i]];

		int p2 = closest[order[i]];

		int percent = DEV(p1,p2)*100;
		cerr<<"  #"<<i<<": "<<names[p1]<<" -> "<<names[p2];
		cerr<<"     D=[ "<<D(p1,p2)<<" / "<<D(p2,p1)<<" ]";
		cerr<<"     D = "<<percent<<"% [ "<<DS(p1,p2)<<"/"<<DO(p1,p2)<<" ] "<<endl;
	    }
	}

	if (app.count("--cutoff") or app.count("--down-to"))
	{
	    int cutoff = -1;
	    if (app.count("--cutoff"))
		cutoff = similarity_cutoff;

            // Count survivors of earlier filters as well as removals and restorations below.
            int n_retained = n_positive(keep);
            int down_to = n_retained;
	    if (app.count("--down-to"))
		down_to = down_to_count;

	    D = asymmetric_distance_matrix(A);
	    matrix<int> DS = symmetric_distance_matrix(A);
      
	    vector<int> removed;

	    while(true)
	    {
		// find the smallest pair (p1,p2) where neither has been removed.
		std::pair<int,int> p = argmin(D,keep);
		int p1 = p.first;
		int p2 = p.second;
                if (p1 == -1)
                {
                    if (app.count("--down-to") and n_retained > down_to)
                        cerr<<"Cannot thin to "<<down_to<<" sequences: no removable pair remains ("
                            <<n_retained<<" retained).\n";
                    break;
                }
		int MD = D(p1,p2);
	
		// Stop when the distance reaches the cutoff and the retained count meets the target.
		if (MD >= cutoff and n_retained <= down_to) break;
	
		// remove the sequence with the shorter comment, if they are the same
		if (D(p1,p2) == D(p2,p1) and keep[p2]< 2 and A.seq(p1).comment.size() > A.seq(p2).comment.size())
		    std::swap(p1,p2);
	
		// mark as removed
		keep[p1] = 0;
		removed.push_back(p1);
                n_retained--;
	    }

	    // compute distances to those that remain
	    vector<int> closest(removed.size());
	    vector<int> distance(removed.size());

	    // Count how many we *actually* remove, after we decide to put some back!
	    int n_removed=0;

	    for(int i=closest.size()-1; i>=0; i--)
	    {
		closest[i]  = argmin_row(D,removed[i],keep);
		distance[i] = D(removed[i],closest[i]);

		// put item back if too far from remaining items
		if (cutoff != -1 and distance[i] >= cutoff and n_retained < down_to)
                {
		    keep[removed[i]] = 1;
                    n_retained++;
                }
		else
		    n_removed++;
	    }

	    if (log_verbose)
	    {
		vector<int> order = iota<int>(removed.size());
		sort(order.begin(), order.end(), sequence_order<int>(distance));

		cerr<<"\nRemoved "<<n_removed<<" similar sequences:"<<endl;
		n_removed=0;

		for(int i=0;i<removed.size();i++) 
		{
		    int p1 = removed[order[i]];

		    int p2 = closest[order[i]];

		    if (keep[p1] > 0) continue;

		    cerr<<"  #"<<++n_removed<<": "<<names[p1]<<" -> "<<names[p2]<<"  D=[ "<<D(p1,p2)<<" / "<<D(p2,p1)<<" / "<<DS(p2,p1)<<" ]"<<endl;
		}
	    }
	}

	//------- Select the sequences -------//
	alignment A2 = select_rows(A,keep);

        dynamic_bitset<> site_is_protected(A2.length());
        if (app.count("--keep-columns"))
        {
            string reference = column_reference;
            auto index = find_index(names,reference);
            if (not index)
                throw myexception()<<"--keep-columns: Can't find sequence '"<<reference<<"'";
            site_is_protected = column_contains_sequence(A, *index);
        }

	//------- Remove consecutive unique sites -----//
        dynamic_bitset<> keep_sites(A2.length());
        keep_sites.flip();

	if (app.count("--remove-unique"))
	{
            // Question: should I remove ALL of the sites, or just the sites beyond L aa/nucs?
            //    And, how would I remove the MIDDLE sites?
            // Question: should I remove only sites that have no ?  How should I do so?

	    int L = unique_limit;
            keep_sites &= ~part_of_long_insertion(A2, L);
	}

	// ------- Remove columns with too few letters ------ //
	if (app.count("--min-letters"))
            keep_sites &= enough_letters( A2, min_letters );

        // ------- Actually remove columns ----------------- //
        A2 = select_columns(A2, keep_sites | site_is_protected);

	if (erase_empty) 
	    remove_empty_columns(A2);

	if (sort_alignment)
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
