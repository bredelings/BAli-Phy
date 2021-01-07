/*
  Copyright (C) 2005,2008,2010 Benjamin Redelings

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

#include "joint-A-T.H"

#include <fstream>
#include <cmath>

#include "alignment/alignment.H"
#include "alignment/load.H"
#include "tree-align/link.H"
#include "tree/tree-util.H"

#include "util/myexception.H"
#include "util/log-level.H"
#include "util/io.H"

namespace po = boost::program_options;
using po::variables_map;

using std::vector;
using std::pair;
using std::shared_ptr;
using std::string;
using std::endl;
using std::optional;

// See tools/read-trees.{H,cc}
// See alignment/load.{H,cc}

optional<string> line_reader::next_one()
{
    std::string line;
    if (portable_getline(file,line))
        return line;
    else
        return {};
}

line_reader::line_reader(std::istream& f)
    :file_reader<string>(f)
{ }


optional<string> alignment_reader::next_one()
{
    if (not find_alignment(file)) return {};

    string alignment_string;
    string line;
    while(portable_getline(file,line) and line.size())
    {
        alignment_string += line;
        alignment_string += "\n";
    }

    assert(alignment_string.size() > 0 and alignment_string[0] == '>');
    return alignment_string;
}

alignment_reader::alignment_reader(std::istream& f)
    :file_reader<string>(f)
{ }


const std::vector<std::string>& joint_A_T::leaf_names() const
{
    return *leaf_names_;
}

joint_A_T& joint_A_T::load(const vector<alignment>& A,const vector<SequenceTree>& T,bool internal)
{
    unsigned s = std::min(A.size(),T.size());
    if (s != A.size())
        std::cerr<<"joint-A-T: Warning! only using "<<s<<"/"<<A.size()<<" alignments to match number of trees."<<endl;

    if (s != T.size())
        std::cerr<<"joint-A-T: Warning! only using "<<s<<"/"<<T.size()<<" trees to match number of alignments."<<endl;

    for(int i=0;i<s;i++)
        push_back({A[i],T[i]});

    if (s == 0) return *this;

    if (not leaf_names_)
        leaf_names_ = T[0].get_leaf_labels();
  
    for(int i=0;i<size();i++)
    {
        remap_T_leaf_indices((*this)[i].second, *leaf_names_);
        link((*this)[i].first, (*this)[i].second, true);
        link((*this)[i].first, (*this)[i].second, internal);
    }

    return *this;
}

joint_A_T::joint_A_T(const vector<alignment>& A,const vector<SequenceTree>& T,bool internal)
{
    load(A,T,internal);
}


joint_A_T get_joint_A_T(const variables_map& args,bool internal)
{
    checked_ifstream a_file(args["alignments"].as<string>(), "alignment samples file");

    checked_ifstream t_file(args["trees"].as<string>(), "tree samples file");

    unsigned subsample = args["subsample"].as<unsigned>();

    vector<alignment> A = load_alignments(a_file, get_alphabet_name(args));
    vector<SequenceTree> T = load_trees(t_file, 0, subsample);

    return joint_A_T(A,T,internal);
}

template<typename T>
void remove_unordered(vector<T>& v, int i)
{
    if (i < int(v.size())-1)
        std::swap(v[i], v.back());
    v.pop_back();
}

template <typename T>
void thin_by_half(vector<T>& v1)
{
    vector<T> v2;
    for(int i=0;i<v1.size()/2;i++)
        v2.push_back(std::move(v1[i*2]));
    std::swap(v1,v2);
}

int kill(int i, int total, int max)
{
    // We have this many extra Ts
    const int extra = total - max;
    return int( double(i+0.5)*total/extra);
}

template <typename T>
bool thin_down_to(vector<T>& v1,optional<int> M)
{
    if (not M) return false;

    int total = v1.size();
    int max = *M;
    if (total <= max) return false;

    assert(total <= max*2);

    int k = 0;
    int j = 0;
    vector<T> v2;
    for(int i=0;i<max;i++,j++)
    {
        while ( j == kill(k, total , max) )
        {
            j++;
            k++;
        }
        v2.push_back(std::move(v1[j]));
    }
    std::swap(v1, v2);
    assert(v1.size() == max);

    return true;
}

joint_A_T get_multiple_joint_A_T(const variables_map& args,bool internal)
{
    auto a_filenames = args["alignments"].as<vector<string>>();
    auto t_filenames = args["trees"].as<vector<string>>();

    // This is just for the trees, I think.
    unsigned alignment_thin_factor = args["subsample"].as<unsigned>();

    optional<int> max;
    if (args.count("max"))
        max = args["max"].as<unsigned>();

    if (a_filenames.size() != t_filenames.size())
        throw myexception()<<"The number of alignments files ("<<a_filenames.size()<<") and the number of trees files ("<<t_filenames.size()<<") don't match!";

    int N = a_filenames.size();

    vector<shared_ptr<checked_ifstream>> a_files;
    vector<shared_ptr<checked_ifstream>> t_files;
    for(int i=0;i<N;i++)
    {
        a_files.push_back( std::make_shared<checked_ifstream>(a_filenames[i], "alignment samples file") );
        t_files.push_back( std::make_shared<checked_ifstream>(t_filenames[i], "tree samples file") );
    }

    vector<shared_ptr<reader<pair<string,string>>>> readers;
    for(int i=0;i<N;i++)
    {
        auto r = new zip(alignment_reader(*a_files[i]),subsample(alignment_thin_factor,line_reader(*t_files[i])));
        readers.push_back(shared_ptr<reader<pair<string,string>>>(r));
    }

    int factor = 1;
    vector<pair<string,string>> A_T_strings(N);

    while(not readers.empty())
    {
        // Do one round of reading.
        for(int i=0;i<readers.size();i++)
        {
            auto at = readers[i]->next(factor);
            if (not at)
            {
                remove_unordered(readers,i);
                i--;
            }
            else
                A_T_strings.push_back(std::move(*at));
        }

        if (max and A_T_strings.size() > (*max)*2)
        {
            int old_size = A_T_strings.size();
            thin_by_half(A_T_strings);
            factor *= 2;
            if (log_verbose)
            {
                std::cerr<<"Halving: Shrinking from "<<old_size<<" to "<<A_T_strings.size()<<" samples.\n";
                std::cerr<<"  Increasing subsampling from "<<factor/2<<" to "<<factor<<".\n";
            }
        }
    }
    if (max)
        thin_down_to(A_T_strings, *max);

    vector<alignment> A;
    vector<SequenceTree> T;
    shared_ptr<const alphabet> alph;
    vector<string> names;
    auto f = [&](const pair<string,string>& at)
        {
            auto& [a,t] = at;
            auto tt = parse_sequence_tree(t);
            if (not tt) return;

            std::istringstream astringfile(a);
            alignment aa;
            if (not alph)
            {
                aa = load_next_alignment(astringfile, get_alphabet_name(args));
                alph = shared_ptr<const alphabet>(aa.get_alphabet().clone());
                names = sequence_names(aa);
            }
            else
                aa = load_next_alignment(astringfile, *alph, names);

            A.push_back(aa);
            T.push_back(*tt);
        };

    for(auto& at: A_T_strings)
        f(at);

    joint_A_T J;
    J.load(A,T,internal);

    return J;
}
