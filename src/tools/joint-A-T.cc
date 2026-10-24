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

#include "joint-A-T.hh"

#include <fstream>
#include <cmath>
#include <cstdint>
#include <numeric>

#include "alignment/alignment.hh"
#include "alignment/load.hh"
#include "tree-align/link.hh"
#include "tree/tree-util.hh"

#include "util/myexception.hh"
#include "util/log-level.hh"
#include "util/io.hh"

namespace po = boost::program_options;
using po::variables_map;

using std::vector;
using std::pair;
using std::shared_ptr;
using std::string;
using std::endl;
using std::optional;

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
        // The first link also labels and validates internal nodes. Preserve that validation
        // before dropping internal sequences, but do not repeat it when keeping them.
        if (not internal)
            link((*this)[i].first, (*this)[i].second, false);
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

namespace {

// Allocate the final cap proportionally using largest remainders (input order breaks ties).
// Floored quotas leave fewer than N slots; the remainder awards fill the cap without exceeding
// any chain's size. Bin midpoints are distinct because each quota is at most its chain size.
void thin_joint_samples(vector<vector<pair<string,string>>>& chains, std::size_t maximum)
{
    std::size_t total = 0;
    for (const auto& chain: chains) total += chain.size();
    if (total <= maximum) return;

    vector<std::size_t> counts(chains.size()), remainders(chains.size()), order(chains.size());
    std::size_t allotted = 0;
    for (std::size_t i = 0; i < chains.size(); i++)
    {
        auto product = std::uint64_t(maximum) * chains[i].size();
        counts[i] = product / total;
        remainders[i] = product % total;
        allotted += counts[i];
    }
    std::iota(order.begin(), order.end(), 0);
    std::stable_sort(order.begin(), order.end(), [&](auto i, auto j) {
        return remainders[i] > remainders[j];
    });
    for (std::size_t j = 0; j < maximum - allotted; j++) counts[order[j]]++;

    for (std::size_t i = 0; i < chains.size(); i++)
    {
        auto& chain = chains[i];
        auto count = counts[i];
        for (std::size_t j = 0; j < count; j++)
        {
            auto source = ((2 * std::uint64_t(j) + 1) * chain.size()) / (2 * count);
            // Source indices increase and are at least j, so moving in place cannot destroy a later sample.
            if (source != j) chain[j] = std::move(chain[source]);
        }
        chain.resize(count);
    }
}

}

// Select raw pairs independently by chain, then parse and link only the retained sample.
joint_A_T get_multiple_joint_A_T(const vector<string>& a_filenames, const vector<string>& t_filenames,
                               const string& alphabet_name, bool internal, unsigned alignment_thin_factor,
                               std::uint64_t first, optional<std::uint64_t> last, unsigned thin,
                               optional<unsigned> max)
{
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
    vector<std::uint64_t> tree_positions(N, 0);
    for(int i=0;i<N;i++)
    {
        auto r = new zip(alignment_reader(*a_files[i]),subsample(alignment_thin_factor,line_reader(*t_files[i])));
        readers.push_back(shared_ptr<reader<pair<string,string>>>(r));
        // Pair j contains tree j*d. Advance whole pairs to the inclusive lower bound;
        // this rounds burn-in up to the next saved alignment without shifting its tree.
        while(not r->done() and tree_positions[i] < first)
        {
            r->next();
            tree_positions[i] += alignment_thin_factor;
        }
    }

    std::uint64_t factor = 1;
    vector<std::uint64_t> positions(N, 0);
    vector<vector<pair<string,string>>> A_T_strings(N);
    std::size_t retained = 0;
    if (max and *max <= 0)
        throw myexception()<<"Maximum sample count must be positive.";

    bool done = false;
    while(not done)
    {
        done = true;
        // Do one round of reading.
        for(int i=0;i<readers.size();i++)
        {
            if (not readers[i]->done() and (not last or tree_positions[i] <= *last))
            {
                done = false;
                // Positions count pairs after the user's per-chain thinning. The shared grid
                // remains anchored at zero when the adaptive factor changes between rounds.
                if (positions[i] % factor == 0)
                {
                    A_T_strings[i].push_back(readers[i]->read());
                    retained++;
                }
                positions[i]++;
                // Advance whole pairs to preserve alignment/tree correspondence and bounds.
                std::uint64_t stride = thin;
                for(std::uint64_t j=0; j<stride and not readers[i]->done(); j++)
                {
                    readers[i]->next();
                    tree_positions[i] += alignment_thin_factor;
                    if (last and tree_positions[i] > *last) break;
                }
            }
        }

        // Each chain can contribute one rounding remainder when halved. Allow N records
        // of slack so singleton chains cannot cause repeated halvings without freeing space.
        if (max and retained > 2 * std::size_t(*max) + N)
        {
            auto old_size = retained;
            retained = 0;
            for (auto& chain: A_T_strings)
            {
                vector<pair<string,string>> thinned;
                thinned.reserve((chain.size() + 1) / 2);
                for (std::size_t j = 0; j < chain.size(); j += 2)
                    thinned.push_back(std::move(chain[j]));
                chain.swap(thinned);
                retained += chain.size();
            }
            // Retained positions are precisely multiples of the new factor in each chain.
            factor *= 2;
            if (log_verbose)
                std::cerr<<"Halving: Shrinking from "<<old_size<<" to "<<retained<<" samples.\n";
        }
    }
    if (max)
        thin_joint_samples(A_T_strings, *max);

    vector<alignment> A;
    vector<SequenceTree> T;
    shared_ptr<const alphabet> alph;
    vector<string> names;
    // Parse a retained pair, reusing the first alignment's alphabet and sequence ordering.
    auto f = [&](const pair<string,string>& at)
        {
            auto& [a,t] = at;
            auto tt = parse_sequence_tree(t);
            if (not tt) return;

            std::istringstream astringfile(a);
            alignment aa;
            if (not alph)
            {
                try
                {
                    aa = load_next_alignment(astringfile, alphabet_name);
                    names = sequence_names(aa);
                    alph = shared_ptr<const alphabet>(aa.get_alphabet().clone());
                }
                catch(myexception& e)
                {
                    names = {};
                    alph = {};
                    std::cerr<<"Warning: Skipping bad alignment:"<<endl;
                    std::cerr<<"  Exception: "<<e.what()<<endl;
                    return;
                }
            }
            else
            {
                try
                {
                    aa = load_next_alignment(astringfile, *alph, names);
                }
                catch(myexception& e)
                {
                    std::cerr<<"Warning: Skipping bad alignment:"<<endl;
                    std::cerr<<"  Exception: "<<e.what()<<endl;
                    return;
                }
            }

            // FIXME: We could get all the sequence names, but miss part of the last sequence.
            //        So technically we should maybe check that the sequences are the right length.
            //
            //        We COULD require that there is a blank line after the alignment.  Then we'd know
            //        We got the whole thing.  Or an END mark, like //.
            A.push_back(aa);
            T.push_back(*tt);
        };

    for(auto& chain: A_T_strings)
        for(auto& at: chain)
            f(at);

    joint_A_T J;
    J.load(A,T,internal);

    return J;
}
