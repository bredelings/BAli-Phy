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

#include "alignment/index-matrix.H"
#include <utility>
#include <fstream>
#include <string>
#include <cmath>
#include <list>
#include <numeric>
#include "util/myexception.H"
#include "util/log-level.H"
#include "util/io.H"
#include "optimize.H"
#include "findroot.H"
#include "alignment/alignment-util.H"
#include "alignment/load.H"
#include "distance-methods.H"
#include "statistics.H"



#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

void do_setup(const variables_map& args,vector<alignment>& alignments) 
{
    //------------ Try to load alignments -----------//
    vector<string> filenames;
    if (args.count("files"))
        filenames = args["files"].as<vector<string> >();

    // read from cin if nothing specified
    if (filenames.empty())
        filenames.push_back("-");

    int maxalignments = args["max"].as<int>();
    unsigned skip = args["skip"].as<unsigned>();

    std::optional<int> chop_to;
    if (args.count("chop-to"))
        chop_to = args["chop-to"].as<int>();

    // --------------------- try ---------------------- //
    for(auto& filename: filenames)
    {
        istream_or_ifstream input_stream(std::cin, "-", filename, "alignment collection");

        if (log_verbose)
            std::cerr<<"alignment-consensus: Loading alignments from "<<filename<<"...";

        list<alignment> As;
        if (alignments.empty())
        {
            As = load_alignments(input_stream, get_alphabet_name(args), skip, maxalignments);
        }
        else
        {
            As = load_alignments(input_stream, sequence_names(alignments[0]), get_alphabet_name(args), skip, maxalignments);
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
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // invisible options
    options_description invisible("Invisible options");
    invisible.add_options()
	("files",value<vector<string> >()->composing(),"tree samples to examine")
	;

    // visible options
    options_description visible("Allowed options");
    visible.add_options()
        ("help,h", "produce help message")
        ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
        ("skip,s",value<unsigned>()->default_value(0),"number of alignment samples to skip")
        ("max,m",value<int>()->default_value(1000),"maximum number of alignments to analyze")
        ("strict",value<double>(),"ignore events below this probability")
        ("cutoff",value<double>(),"ignore events below this probability")
        ("uncertainty",value<string>(),"file-name for AU uncertainty vs level")
        ("chop-to",value<int>(),"keep only the first arg taxa")
        ("verbose,V",value<int>()->implicit_value(1), "Output more log messages on stderr.")
        ;

    options_description all("All options");
    all.add(invisible).add(visible);

    positional_options_description p;
    p.add("files", -1);

    variables_map args;     
    store(command_line_parser(argc, argv).
	  options(all).positional(p).run(), args);
    notify(args);    

    if (args.count("help")) {
        cout<<"Construct a consensus alignment to summarize an alignment sample.\n\n";
        cout<<"Usage: alignment-consensus alignments-file [alignments-file ...] [OPTIONS] \n\n";
        cout<<visible<<"\n";
        exit(0);
    }

    if (args.count("verbose")) log_verbose = args.at("verbose").as<int>();

    return args;
}

matrix<int> split_alignment(const matrix<int>& M1, const matrix<int>& M2)
{
    assert(M1.size2() == M2.size2());

    const int N = M1.size2();

    // lookup and cache the column each feature is in for M2.
    vector< vector< int> > column_indices2 = column_lookup(M2);

    // allocate the output matrix
    // Note: |M1| + |M2| is NOT the maximum length for the consensus!
    matrix<int> M3(M1.size1() + M2.size1(), M1.size2());

    // Initialize it to contain only gaps
    for(int c1=0;c1<M3.size1();c1++)
        for(int i=0; i<N; i++)
            M3(c1, i)  = alphabet::gap;

    int c3_next = 0;

    for(int c1=0; c1<M1.size1(); c1++)
    {
        std::map<int,int> cmap;
        for(int i=0; i<N; i++)
        {
            // which character?
            int k = M1(c1,i);

            // not a character
            if (k < 0) continue;

            // which column in M2?
            int c2 = column_indices2[i][k];

            // look up the column in M3;
            auto iter = cmap.find(c2);

            // if we haven't seen c2 before, allocate a new column in c3 for it.
            if (iter == cmap.end())
            {
                cmap.insert({c2,c3_next++});
                iter = cmap.find(c2);
                assert (iter != cmap.end());
            }

            // find the column in M3
            int c3 = iter->second;

            if (c3 >= M3.size1())
            {
                int new_length = M3.size() * 2;
                matrix<int> M4(new_length, M3.size2());
                M4.fill(alphabet::gap);
                for(int i=0;i<M3.size1();i++)
                    for(int j=0;j<M3.size2();j++)
                        M4(i,j) = M3(i,j);
                std::swap(M3,M4);
            }

            // Put character k there.
            M3(c3,i) = k;
        }
    }

    matrix<int> M4(c3_next, M1.size2());
    for(int i=0;i<M4.size1();i++)
        for(int j=0;j<M4.size2();j++)
            M4(i,j) = M3(i,j);

    return M4;
}

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

int main(int argc,char* argv[]) 
{ 
    try {
        //---------- Parse command line  -------//
        variables_map args = parse_cmd_line(argc,argv);

        //------------ Load alignment and tree ----------//
        vector<alignment> alignments;

        do_setup(args,alignments);
        for(auto& alignment: alignments)
            alignment = chop_internal(alignment);

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

        //--------- Build alignment from list ---------//
        double cutoff_strict = -1;
        double cutoff = -1;

        if (args.count("strict"))
            cutoff_strict = args["strict"].as<double>();

        if (args.count("cutoff"))
            cutoff = args["cutoff"].as<double>();

        if (cutoff_strict < 0 and cutoff < 0)
            cutoff = 0.75;

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
            sparse_alignment SA{names, sequences, a, consensus};

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

            // Warning: This should take N*N*L memory and time.
            for(int s1=0;s1<N;s1++)
            {
                if (log_verbose >= 1) std::cerr<<"s1 = "<<s1+1<<"/"<<N<<"\n";
                for(int s2=s1+1;s2<N;s2++)
                {
                    if (log_verbose >= 2) std::cerr<<"   s2 = "<<s2<<"/"<<N<<"\n";
                    add_edges(E, SMs, s1, s2, L[s1], L[s2],
                              min(abs(cutoff), abs(cutoff_strict))
                        );
                }
            }


            E.build_index();

            //-------- Build a beginning alignment --------//
            auto M = unaligned_matrix(L);

            map<unsigned,pair<unsigned,unsigned> > graph;

            if (cutoff_strict > 0) 
                graph = M.merge2(E,cutoff_strict,true);

            if (cutoff > 0) 
                graph = M.merge2(E,cutoff,false);

            //-------- Construct Build a beginning alignment --------//

            std::cerr<<"Sorting...";
            matrix<int> M2 = get_ordered_matrix(M);
            std::cerr<<"done.\n";

            consensus = get_alignment(M2,alignments[0]);

            std::cout<<consensus<<std::endl;


            if (args.count("uncertainty")) {

                string filename = args["uncertainty"].as<string>();
                ofstream graph_file(filename);

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
            }
        }
    }
    catch (std::exception& e) {
        std::cerr<<"alignment-consensus: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}
