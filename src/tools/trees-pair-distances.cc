/*
   Copyright (C) 2007-2008 Benjamin Redelings

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
#include <list>
#include <utility>
#include "tree/tree.hh"
#include "tree/sequencetree.hh"
#include "tree/tree-util.hh"
#include "tree-dist.hh"
#include "util/myexception.hh"
#include "util/string/join.hh"
#include "util/matrix.hh"

#include <CLI/CLI.hpp>


using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::list;
using std::valarray;
using std::pair;


using bali_phy::matrix;

struct count_pair_distances:public accumulator<SequenceTree>
{
    bool RF;
    bool initialized = false;
    int n_samples = 0;
    int N = 0;
    matrix<double> m1; // first  moment;
    matrix<double> m2; // second moment;
    vector<string> names;
    void operator()(const SequenceTree&);

    void finalize() 
        {
            if (n_samples == 0)
                throw myexception()<<"No trees were read in!";
  
            m1 /= n_samples;
            m2 /= n_samples;

            // compute stddev
            for(int i=0;i<N; i++)
                for(int j=0;j<N; j++)
                    m2(i,j) = sqrt(m2(i,j) - m1(i,j) * m1(i,j));
        }

    count_pair_distances(bool b=false)
        :RF(b)
        {}
};

void count_pair_distances::operator()(const SequenceTree& T)
{
    if (not initialized)
    {
        N = T.n_leaves();
        names = T.get_leaf_labels();

        m1.resize(N, N, 0);
        m2.resize(N, N, 0);

        initialized = true;
    }

    n_samples++;

    // Theoretically, we could do this much faster, I think.
    //  vector<vector<int> > leaf_sets = partition_sets(T);

    for(int i=0;i<N;i++)
        for(int j=0;j<i;j++) 
        {
            double D = 0;
            if (RF)
                D = T.edges_distance(i,j);
            else
                D = T.distance(i,j);

            assert( D >= 0);

            m1(i,j) += D;
            m1(j,i) += D;

            m2(i,j) += D*D;
            m2(j,i) += D*D;
        }
}


int main(int argc,char* argv[]) 
{ 
    try {
        //----------- Parse command line  ----------//
        CLI::App app{"Summarize leaf-to-leaf distances across sampled trees.", "trees-pair-distances"};
        app.usage("Usage: trees-pair-distances [OPTIONS] < sampled.trees");
        int skip = 0, subsample = 1;
        std::optional<int> max;
        bool RF = false, var = false;
        app.add_option("--skip", skip, "Number of initial trees to skip")->capture_default_str();
        app.add_option("--max", max, "Maximum number of selected trees to process");
        app.add_option("--subsample", subsample, "Keep every Nth tree after skipping")->capture_default_str();
        app.add_flag("--RF", RF, "Count edges along leaf-to-leaf paths instead of summing lengths");
        app.add_flag("--var", var, "Report population standard deviations instead of means");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }


        //-------- Read in the tree samples --------//
        count_pair_distances D(RF);

        scan_trees(std::cin, skip, max, subsample, D);
        D.finalize();

        //------- Merge lengths and topology -------//
        const auto& distances = var ? D.m2 : D.m1;
        cout<<join(D.names,'\t')<<endl;
        for(int s1=0;s1<D.N;s1++)
        {
            for(int s2=0; s2<D.N; s2++)
            {
                cout<<distances(s1,s2);
                if (s2 + 1 < D.N)
                    cout<<'\t';
            }
            cout<<endl;
        }
    }
    catch (std::exception& e) {
        std::cerr<<"trees-pair-distances: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}
