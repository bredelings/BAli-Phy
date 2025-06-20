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
#include "tree/tree.H"
#include "tree/sequencetree.H"
#include "tree/tree-util.H"
#include "tree-dist.H"
#include "util/myexception.H"
#include "util/string/join.H"
#include "util/matrix.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::string;
using std::vector;
using std::list;
using std::valarray;
using std::pair;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description all("Allowed options");
    all.add_options()
        ("help,h", "produce help message")
        ("skip",value<int>()->default_value(0),"number of tree samples to skip")
        ("max",value<int>(),"maximum number of tree samples to read")
        ("subsample",value<int>()->default_value(1),"factor by which to sub-sample")
        ("RF","just count the number of branches")
        ("var","report standard deviation of branch lengths instead of mean")
        ;

    variables_map args;     
    store(parse_command_line(argc, argv, all), args);
    notify(args);    

    if (args.count("help")) {
        cout<<"Compute the mean (stddev) of all leaf-leaf distances.\n\n";
        cout<<"Usage: trees-pair-distances < in-file\n\n";
        cout<<all<<"\n";
        exit(0);
    }

    return args;
}

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
        variables_map args = parse_cmd_line(argc,argv);

        //-------- Read in the tree samples --------//
        int skip = args["skip"].as<int>();

        std::optional<int> max;
        if (args.count("max"))
            max = args["max"].as<int>();

        int subsample = args["subsample"].as<int>();

        bool RF = args.count("RF")>0;

        count_pair_distances D(RF);

        scan_trees(std::cin, skip, max, subsample, D);

        //------- Merge lengths and topology -------//
        vector<string> s_out;
        vector<double> v1_out;
        vector<double> v2_out;
        s_out.push_back("level");
        v1_out.push_back(-1);
        v2_out.push_back(-2);

        cout<<join(D.names,'\t')<<endl;
        for(int s1=0;s1<D.N;s1++)
        {
            for(int s2=0; s2<D.N; s2++)
            {
                cout<<D.m1(s1,s2);
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
