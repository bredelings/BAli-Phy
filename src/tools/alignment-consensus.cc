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

#include "alignment/index-matrix.hh"
#include <utility>
#include <fstream>
#include <string>
#include <cmath>
#include <list>
#include <numeric>
#include <optional>
#include "util/myexception.hh"
#include "util/log-level.hh"
#include "util/io.hh"
#include "optimize.hh"
#include "findroot.hh"
#include "alignment/alignment-util.hh"
#include "alignment/load.hh"
#include "distance-methods.hh"
#include "statistics.hh"



#include <CLI/CLI.hpp>

using namespace std;

sparse_index_matrix split_alignment(const sparse_index_matrix& M1, const sparse_index_matrix& M2)
{
    assert(M2.n_sequences() == M1.n_sequences());

    auto M3 = M1;
    for(auto& [col1,letters]: M1.letters_for_column())
    {
        // seq -> col2
        std::map<int,int> cmap;

        for(auto& [seq, index]: letters)
        {
            // which column in M2?
            int col2 = M2.column(seq, index);

            // look up the column in M3;
            auto iter = cmap.find(col2);

            // if we haven't seen c2 before, allocate a new column in c3 for it.
            if (iter == cmap.end())
            {
                int new_col1 = M3.new_column();
                cmap.insert({col2, new_col1});
                iter = cmap.find(col2);
                assert( iter != cmap.end() );
            }

            // Find the new column for find the column in M3
            int new_col1 = iter->second;

            // Put character k there.
            M3.erase(seq, index);
            M3.add(seq, index, new_col1);
        }
    }

    return M3;
}

// Summarize the retained alignment samples using strict intersection or supported merges.
int main(int argc,char* argv[]) 
{ 
    try {
        CLI::App app{"Construct a consensus alignment from an alignment sample.", "alignment-consensus"};
        app.usage("Usage: alignment-consensus [OPTIONS] [SAMPLE ...]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        vector<string> filenames;
        string alphabet_name;
        unsigned skip = 0;
        int maxalignments = 1000;
        optional<int> chop_to;
        optional<double> cutoff_strict, cutoff;
        optional<string> uncertainty_file;

        app.add_option("SAMPLE", filenames, "Alignment samples (default: stdin; '-' reads stdin)")
            ->type_size(1)->expected(-1);
        app.add_option("--alphabet", alphabet_name, "Sequence alphabet (default: infer from input)")->type_name("ALPHABET");
        app.add_option("-s,--skip", skip, "Samples to skip in each file")->capture_default_str();
        app.add_option("-m,--max", maxalignments, "Maximum samples retained per file by thinning")->capture_default_str();
        app.add_option("--strict", cutoff_strict, "Strict merging support threshold; 1 selects strict consensus")
            ->type_name("P");
        app.add_option("--cutoff", cutoff, "Merging support threshold (0.75 if neither threshold is supplied)")
            ->type_name("P");
        app.add_option("--uncertainty", uncertainty_file, "Write uncertainty versus support; unavailable at threshold 1")
            ->type_name("FILE");
        app.add_option("--chop-to", chop_to, "Keep only the first N sequences")->type_name("N");
        auto* verbosity = app.add_option("-V,--verbose", log_verbose, "Diagnostic verbosity (1 if no value is given)")
            ->type_name("LEVEL")->expected(0, 1);
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        // CLI11 represents a value-less optional argument as an empty result; -V means level 1.
        if (verbosity->count() and verbosity->results().front().empty())
            log_verbose = 1;

        if (not cutoff_strict and not cutoff)
            cutoff = 0.75;

        if ((cutoff_strict == 1.0 or cutoff == 1.0) and uncertainty_file)
            throw myexception()<<"--uncertainty is unavailable with the strict-consensus algorithm "
                               <<"(--strict 1 or --cutoff 1).";

        //------------ Load alignment and tree ----------//
        vector<alignment> alignments;

        // read from cin if nothing specified
        if (filenames.empty())
            filenames.push_back("-");

        // --------------------- try ---------------------- //
        for(auto& filename: filenames)
        {
            istream_or_ifstream input_stream(std::cin, "-", filename, "alignment collection");

            if (log_verbose)
                std::cerr<<"alignment-consensus: Loading alignments from "<<filename<<"...";

            list<alignment> As;
            if (alignments.empty())
            {
                As = load_alignments(input_stream, alphabet_name, skip, maxalignments);
            }
            else
            {
                As = load_alignments(input_stream, sequence_names(alignments[0]), alphabet_name, skip, maxalignments);
            }
            alignments.insert(alignments.end(),As.begin(),As.end());

            if (log_verbose)
                std::cerr<<"done. ("<<As.size()<<" alignments)"<<std::endl;
        }

        // Chop internal-node-sequences
        if (chop_to)
            for(auto& A: alignments)
                A = chop_internal(A, *chop_to);

        if (not alignments.size())
            throw myexception()<<"Alignment sample is empty.";
        for(auto& alignment: alignments)
            alignment = chop_internal(alignment);

        int N = alignments[0].n_sequences();
        vector<int> L(N);
        for(int i=0;i<L.size();i++)
            L[i] = alignments[0].seqlength(i);

        for(const auto& A: alignments)
            check_same_sequence_lengths(L, A);

        //--------- Build alignment from list ---------//
        alignment consensus;

        if (cutoff_strict == 1.0 or cutoff == 1.0)
        {
            // Compute the consensus
            auto consensus = SM(alignments[0]);

            int s = 0;
            for(auto& A: alignments)
            {
                if (s > 0)
                {
                    if (log_verbose) std::cerr<<"Splitting "<<s<<"/"<<alignments.size()-1<<"...";
                    consensus = split_alignment(consensus, SM(A));
                    if (log_verbose) std::cerr<<"done.\n";
                }
                s++;
            }

            auto& A = alignments[0];
            auto sequences = A.convert_to_letters();
            auto names = sequence_names(A);
            auto a = A.get_alphabet_ptr();
            sparse_alignment SA{names, sequences, a, A.get_ambiguities(), consensus};

            std::cout<<SA<<"\n";
        }
        else
        {
            vector<sparse_index_matrix> SMs;

            if (N > 250)
                std::cerr<<"\nWarning: this algorithm is quite slow and memory-intensive for > 250 sequences (N="<<N<<").  Consider computing a strict consensus by using --strict=1\n\n";

            // Can we make a --fast version that uses 5*N*log(N) time?
            // FSA --fast creates a minimum-spanning tree to ensure that we consider closely related pairs.
            // How?

            //--------- Construct alignment indexes ---------//
            for(auto& alignment: alignments)
                SMs.push_back(SM(alignment));

            //--------- Get list of supported pairs ---------//
            // Warning: This takes N*N*L memory for a lookup table!  And N*N time.
            Edges E(L);
            const double edge_cutoff = cutoff and cutoff_strict ? min(*cutoff, *cutoff_strict)
                                     : cutoff ? *cutoff : *cutoff_strict;

            // Warning: This should take N*N*L memory and time.
            for(int s1=0;s1<N;s1++)
            {
                if (log_verbose >= 1) std::cerr<<"s1 = "<<s1+1<<"/"<<N<<"\n";
                for(int s2=s1+1;s2<N;s2++)
                {
                    if (log_verbose >= 2) std::cerr<<"   s2 = "<<s2<<"/"<<N<<"\n";
                    add_edges(E, SMs, s1, s2, L[s1], L[s2],
                              edge_cutoff
                        );
                }
            }


            E.build_index();

            //-------- Build a beginning alignment --------//
            auto M = unaligned_matrix(L);

            map<unsigned,pair<unsigned,unsigned> > graph;

            if (cutoff_strict)
                graph = M.merge2(E,*cutoff_strict,true);

            if (cutoff)
                graph = M.merge2(E,*cutoff,false);

            //-------- Construct Build a beginning alignment --------//

            std::cerr<<"Sorting...";
            matrix<int> M2 = get_ordered_matrix(M);
            std::cerr<<"done.\n";

            consensus = get_alignment(M2,alignments[0]);

            std::cout<<consensus<<std::endl;


            if (uncertainty_file) {

                const string& filename = *uncertainty_file;
                ofstream graph_file(filename);
                if (not graph_file)
                    throw myexception()<<"Cannot open uncertainty file '"<<filename<<"'.";

                int total_seq_length=0;
                for(int length: L)
                    total_seq_length += length;

                double scale1 = double(N)/total_seq_length;
                double scale2 = 1.0/total_seq_length;

                for(const auto& [count,x]: graph)
                {
                    double LOD = log10(statistics::odds(count, SMs.size(), 1));
                    auto& [columns, unknowns] = x;
                    graph_file<<LOD<<" "<<unknowns*scale2<<"  "<<columns*scale1<<endl;
                }
                graph_file.close();
                if (not graph_file)
                    throw myexception()<<"Cannot finish writing uncertainty file '"<<filename<<"'.";
            }
        }
        cout.flush();
        if (not cout)
            throw myexception()<<"Cannot finish writing alignment to stdout.";
    }
    catch (std::exception& e) {
        std::cerr<<"alignment-consensus: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}
