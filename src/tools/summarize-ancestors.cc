/*
  Copyright (C) 2005,2008-2009,2019 Benjamin Redelings

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
#include "util/myexception.hh"
#include "util/log-level.hh"
#include "util/rng.hh"
#include "util/io.hh"
#include "util/cmdline.hh"
#include "alignment/alignment.hh"
#include "alignment/index-matrix.hh"
#include "tree-align/link.hh"
#include "alignment/load.hh"

#include "tree/tree-util.hh"
#include "alignment/alignment-util.hh"
#include "distance-methods.hh"
#include "joint-A-T.hh"
#include "ancestor-queries.hh"
#include <optional>
#include <cstdint>
#include <map>

#include <boost/dynamic_bitset.hpp>

using std::cin;
using std::cout;
using std::cerr;
using std::pair;
using std::istream;
using std::ifstream;
using std::vector;
using std::string;
using std::endl;
using std::optional;
using std::map;
using boost::dynamic_bitset;
//using namespace optimize;

namespace po = boost::program_options;
using po::variables_map;

//TODO: add flag to affect gap_must_be_half.

//TODO: sort rows, including internal nodes, by tree... how?
//      -- see alignment-cat --reorder-by-tree
//      We could sort the leaf and internal nodes I think.
//      We could also sort tree branches, like node1,node1=>node2,node2=>node1,node2
//      Group queries not directly derived from the tree would be more complicated to sort.

struct profile
{
    vector<vector<int>> observations; // observations[site][i]
    ambiguity_database ambiguities;
    int count = 0;

    void count_alignment(const alignment& A, int row, const vector<pair<int,int>>& corresponding_columns);
    int max_for_position(int i, bool gap_must_be_half=true) const;
    profile(const alignment& A): observations(A.length()),
        ambiguities(A.get_alphabet().n_letters()) {}
};


// Choose the most frequent observation; ambiguity ties use state sets, not local code assignment.
int argmax(const map<int,int>& counts, const ambiguity_database& ambiguities)
{
    auto max_x = counts.begin()->first;
    auto max_count = counts.begin()->second;

    for(auto& [x,count]: counts)
    {
        if (count > max_count or
            (count == max_count and alphabet::is_ambiguity(x) and alphabet::is_ambiguity(max_x) and
             ambiguities.mask(x) < ambiguities.mask(max_x)))
        {
            max_x = x;
            max_count = count;
        }
    }

    return max_x;
}

// NOTE: `gap_must_be_half` means that we first consider "-" versus "N",
// and then secondly consider which letter the N will be.
//
// For example, if we have {"-":0.45, "A":35, "T":"0.2"} then we would
// first reject "-" because "N" is more probable, and then choose "A"
// as the most likely letter value.

int profile::max_for_position(int pos, bool gap_must_be_half) const
{
    map<int,int> counts;
    if (observations[pos].size() < 1)
        return alphabet::unknown;

    // 1. Find the counts - and each letter.
    int total = 0;
    int total_gap = 0;
    for(int letter: observations[pos])
    {
        total++;
        if (letter == alphabet::gap and gap_must_be_half)
            total_gap++;
        else if (counts.count(letter))
            counts.at(letter)++;
        else
            counts[letter] = 1;
    }

    // 2. Return gap only if its a 50% probability or higher
    if (total_gap*2 >= total and gap_must_be_half)
        return alphabet::gap;

    // 3. Otherwise return the most frequent letter.
    return argmax(counts, ambiguities);
}

// Translate alignment-local ambiguities into this profile's database before counting observations.
void profile::count_alignment(const alignment& A, int row, const vector<pair<int,int>>& corresponding_columns)
{
    count++;
    for(auto [template_c, sample_c]: corresponding_columns)
    {
        int code = A(sample_c,row);
        if (alphabet::is_ambiguity(code))
            code = ambiguities.encode_mask(A.get_ambiguities().mask(code));
        observations[template_c].push_back(code);
    }
}


vector<pair<int,int>> get_characters(const matrix<int>& template_m, int c)
{
    assert(0 <= c);
    assert(c < template_m.size1());

    vector<pair<int,int>> characters;
    for(int i=0; i<template_m.size2(); i++)
        if (int x = template_m(c,i); x >= 0)
            characters.push_back({i,x});

    return characters;
}

// Choose a character that anchors the column; missing-only columns have no anchor.
optional<pair<int,int>> choose_representative_character(const matrix<int>& template_m, int c)
{
    assert(template_m.size2() > 0);

    auto characters = get_characters(template_m, c);

    if (characters.empty()) return {};

    int n = characters.size();

    return characters[uniform_int(0,n-1)];
}


vector<pair<int,int>> map_columns_random(const alignment& template_A, const alignment& sample_A)
{
    auto template_m = M(template_A);

    // 1. Construct list of (template column, sample column)
    index_matrix sample_m( sample_A );

    vector<pair<int,int>> columns;
    for(int c=0; c<template_A.length(); c++)
    {
        auto representative = choose_representative_character(template_m, c);
        if (not representative) continue;
        auto [seq,index] = *representative;

        int c2 = sample_m.column(seq,index);
        columns.push_back({c,c2});
    }

    return columns;
}


pair<vector<profile>,vector<profile>> extract_sequence(const joint_A_T& samples, const alignment& template_A,
                                                       const optional<SequenceTree>& node_queries,
                                                       const optional<vector<pair<string,dynamic_bitset<>>>>& branch_queries)
{
    vector<int> node_counts;
    vector<profile> node_profiles;
    if (node_queries)
    {
        node_counts.resize(node_queries->n_nodes());
        node_profiles = vector<profile>(node_queries->n_nodes(), profile(template_A));
    }

    vector<profile> group_profiles;
    vector<int> group_counts;
    if (branch_queries)
    {
        group_counts.resize(branch_queries->size());
        group_profiles = vector<profile>(branch_queries->size(), profile(template_A));
    }

    for(auto& [A,T]: samples)
    {
        // 1. Find out which columns of the alignment template are in the alignment sample
        auto corresponding_columns = map_columns_random(template_A, A);

        map<string,int> internal_nodes;
        
        // 2. Find ancestors from node queries
        if (node_queries)
        {
            for(auto& [q_node, t_node]: find_and_name_nodes(T, *node_queries))
            {
                node_counts[q_node]++;

                node_profiles[q_node].count_alignment(A, t_node, corresponding_columns);

                auto name = node_queries->get_labels()[q_node];
                if (not name.empty())
                    internal_nodes.insert({name, t_node});
            }
        }

        // 3. Find ancestors from branch queries.
        if (branch_queries)
        {
            for(auto& [g, t_node]: find_and_name_nodes(T,*branch_queries))
            {
                group_counts[g]++;

                group_profiles[g].count_alignment(A, t_node, corresponding_columns);

                auto& [name,_] = (*branch_queries)[g];
                internal_nodes.insert({name, t_node});
            }
        }

        if (log_verbose)
        {
            // 4. Write leaf sequences
            for(int node=0;node<samples.leaf_names().size();node++)
                write_alignment_row(std::cerr, samples.leaf_names()[node], A, node);

            // 5. Write named ancestors sequences corresponding to node and branch queries
            for(auto& [name,node]: internal_nodes)
                write_alignment_row(std::cerr, name, A, node);

            std::cerr<<"\n\n";
        }
    }

    // Report statistics on the frequency of queried nodes
    if (node_queries)
    {
        for(int q_node=node_queries->n_leaves(); q_node<node_queries->n_nodes(); q_node++)
        {
            auto q_node_name = node_queries->get_label(q_node);
            if (q_node_name.empty()) continue;

            double fraction = double(node_counts[q_node])/samples.size();
            std::cerr<<"Node '"<<q_node_name<<"': present in "<<node_counts[q_node]<<"/"<<samples.size()<<" = "<<fraction*100<<"% of samples.\n";
        }
    }

    // Report statistics on the frequency of queried branches
    if (branch_queries)
    {
        for(int i=0;i<branch_queries->size();i++)
        {
            auto& [name,_] = (*branch_queries)[i];

            std::cerr<<"Group '"<<name<<"': present in "<<group_counts[i]<<"/"<<samples.size()<<" = "<<double(group_counts[i])/samples.size()*100<<"% of samples.\n";
        }
    }

    return {node_profiles,group_profiles};
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // invisible options
    options_description invisible("Invisible options");
    invisible.add_options()
        ("template-alignment,a", value<string>(), "File with template alignment")
        ;

    // named options
    options_description general("General options");
    general.add_options()
        ("help,h", "produces help message")
        ("verbose,V",value<int>()->implicit_value(1),"Show more log messages on stderr.")
        ;

    options_description input("Input options");
    input.add_options()
        ("alignments,A", value<vector<string>>()->composing(),"File of alignment samples")
        ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
        ("trees,T", value<vector<string>>()->composing(), "File of corresponding tree samples")
        ("subsample,x",value<unsigned>()->default_value(10),"Tree stride used to match alignment records (1 for already-paired files)")
        ("skip",value<std::int64_t>()->default_value(0),"First original tree-record position to retain (zero-based, inclusive)")
        ("until",value<std::int64_t>(),"Last original tree-record position to retain (zero-based, inclusive)")
        ("thin",value<int>()->default_value(1),"Keep every kth eligible alignment/tree pair, independently per input chain")
        ("max,m",value<unsigned>()->default_value(500),"Thin (alignment,tree) pairs down to this number of samples.")
        ;

    options_description ancestors("Ancestor query options");
    ancestors.add_options()
        ("nodes,n",value<string>(),"Newick tree with labelled ancestors")
        ("groups,g",value<string>(),"File with named groups")
        ("nodes-min",value<double>()->default_value(0.34,"0.34"),"Minimum fraction to include a node.")
        ("groups-min",value<double>()->default_value(0.34,"0.34"),"Minimum fraction to include a group.")
        ;

    options_description all("All options");
    all.add(invisible).add(general).add(input).add(ancestors);

    options_description visible("All options");
    visible.add(general).add(input).add(ancestors);

    // positional options
    positional_options_description p;
    p.add("template-alignment", 1);

    variables_map args;     
    store(command_line_parser(argc, argv).options(all).positional(p).run(), args);
    notify(args);    

    if (args.count("help")) {
        cout<<"Construct alignments with internal sequences for labeled nodes in query tree.\n\n";
        cout<<"Usage: summarize-ancestors <alignment> -A <fastas1> -T <trees1> [OPTIONS]\n";

        cout<<visible<<"\n";
        cout<<"Examples:\n\n";
        cout<<" Add ancestral sequences to summary alignment:\n";
        cout<<"   % summarize-ancestors summary.fasta -A C1.P1.fastas -T C1.trees --nodes query.tree --groups query.tree\n\n";
        exit(0);
    }

    if (args.count("verbose")) log_verbose = args["verbose"].as<int>();

    if (not args.count("template-alignment"))
        throw myexception()<<"No template alignment given!";

    if (not args.count("alignments"))
        throw myexception()<<"No alignment samples given! (Use -A alignments_file)";

    if (not args.count("trees"))
        throw myexception()<<"No tree sampls given! (Use -T trees_file)";

    if (args["subsample"].as<unsigned>() == 0 or args["thin"].as<int>() <= 0)
        throw myexception()<<"--subsample and --thin must be positive.";
    if (args["skip"].as<std::int64_t>() < 0 or
        (args.count("until") and args["until"].as<std::int64_t>() < 0))
        throw myexception()<<"--skip and --until must be non-negative.";
    if (args.count("until") and args["until"].as<std::int64_t>() < args["skip"].as<std::int64_t>())
        throw myexception()<<"--until must be at least --skip.";

    return args;
}

int main(int argc,char* argv[])
{
    myrand_init();

    try {
        variables_map args = parse_cmd_line(argc,argv);
        //Arguments args;
        //args.read(argc,argv);
        //args.print(cerr);

        // 1. Load alignment and tree samples
        if (log_verbose) cerr<<"summarize-ancestors: Loading alignments and trees...\n";
        joint_A_T samples = get_multiple_joint_A_T(
            args["alignments"].as<vector<string>>(), args["trees"].as<vector<string>>(),
            get_alphabet_name(args), true, args["subsample"].as<unsigned>(),
            args["skip"].as<std::int64_t>(), get_arg<std::int64_t>(args, "until"),
            args["thin"].as<int>(), args["max"].as<unsigned>());

        if (samples.size() == 0)
            throw myexception()<<"No usable alignment/tree pairs remain after sample selection.";
        else
            if (log_verbose) cerr<<"read "<<samples.size()<<" (A,T) pairs.\n\n";

        // 2. Load template alignment
        alignment template_A;
        if (auto filename = get_arg<string>(args,"template-alignment"))
        {
            if (log_verbose) cerr<<"summarize-ancestors: Loading template alignment...\n";

            vector<sequence> sequences = sequence_format::load_from_file(*filename);
            auto a_name = get_arg_default<string>(args,"alphabet", "");
            // Preserve the supplied columns, including those without an ancestral anchor.
            template_A = load_alignment(sequences, a_name, false);

            template_A = remap_A_indices(template_A, samples.leaf_names(), samples.T(0).n_leaves(), samples.T(0).n_nodes());

            if (log_verbose) cerr<<"done.\n";
        }

        // 3. Load queries to find ancestor nodes on the tree
        auto node_queries = get_node_queries(get_arg<string>(args, "nodes"), samples.leaf_names());
        auto branch_queries = get_branch_queries(get_arg<string>(args, "groups"), samples.leaf_names());
        if (not node_queries and not branch_queries)
            std::cerr<<"WARNING: no ancestors defined!\n";
        
        // 4. Extract profiles
        auto [node_profiles, branch_profiles] = extract_sequence(samples, template_A, node_queries, branch_queries);

        // 5. Write profiles for template alignments
        auto& a = template_A.get_alphabet();

        for(int node=0;node<samples.leaf_names().size();node++)
            write_alignment_row(std::cout, samples.leaf_names()[node], template_A, node);

        if (node_queries)
        {
            double nodes_min = args["nodes-min"].as<double>();
            for(int node=node_queries->n_leaves();node<node_queries->n_nodes();node++)
            {
                auto name = node_queries->get_label(node);
                if (name.empty()) continue;

                // Skip nodes if they occur less that 33.3% of the time.
                double fraction = double(node_profiles[node].count)/samples.size();
                if (fraction < nodes_min) continue;

                std::cout<<">"<<name<<"\n";
                for(int col=0; col<template_A.length(); col++)
                    std::cout<<node_profiles[node].ambiguities.decode(a, node_profiles[node].max_for_position(col)).first;
                std::cout<<"\n";
            }
        }

        if (branch_queries)
        {
            double groups_min = args["groups-min"].as<double>();
            for(int i=0;i < branch_queries->size(); i++)
            {
                auto& [name,_] = (*branch_queries)[i];

                double fraction = double(branch_profiles[i].count)/samples.size();
                if (fraction < groups_min) continue;

                std::cout<<">"<<name<<"\n";
                for(int col=0; col<template_A.length(); col++)
                    std::cout<<branch_profiles[i].ambiguities.decode(a, branch_profiles[i].max_for_position(col)).first;
                std::cout<<"\n";
            }
        }

    }
    catch (std::exception& e) {
        cerr<<"summarize-ancestors: Error! "<<e.what()<<endl;
        exit(1);
    }
    return 0;
}
