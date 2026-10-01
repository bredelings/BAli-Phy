/*
  Copyright (C) 2005-20013,2017-2018 Benjamin Redelings

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
#include <memory>
#include <string>
#include <optional>
#include "alignment/alignment.hh"
#include "alignment/alignment-util.hh"
#include "util/set.hh"
#include "util/mapping.hh"
#include "util/io.hh"
#include "util/range.hh"
#include "util/cmdline.hh"
#include "util/log-level.hh"
#include "sequence/sequence.hh"
#include "sequence/sequence-format.hh"
#include <CLI/CLI.hpp>
#include "findroot.hh"

extern int log_verbose;

using namespace sequence_format;

using std::istream;
using std::vector;
using std::string;
using std::optional;

using std::cin;
using std::cout;
using std::cerr;
using std::endl;

namespace {
class AlignmentCatFormatter : public CLI::Formatter
{
public:
    // The description and following option section already supply the blank-line separation.
    // Remove the extra boundary newlines from CLI11's generated usage.
    string make_usage(const CLI::App* app, string name) const override
    {
        auto usage = CLI::Formatter::make_usage(app, name);
        if (!usage.empty() and usage.front() == '\n')
            usage.erase(0, 1);
        if (usage.ends_with("\n\n"))
            usage.pop_back();
        return usage;
    }
};
}

vector<int> get_mapping(const vector<sequence>& S1, const vector<sequence>& S2)
{

    vector<string> names1(S1.size());
    for(int i=0;i<S1.size();i++)
	names1[i] = S1[i].name;

    vector<string> names2(S2.size());
    for(int i=0;i<S2.size();i++)
	names2[i] = S2[i].name;

    vector<int> mapping;

    try {
	mapping = compute_mapping(names1,names2);
    }
    catch (const bad_mapping<string>& b) {
	bad_mapping<string> b2 = b;
	b2.clear();
	if (b.from == 0)
	    b2<<"Couldn't find sequence '"<<b2.missing<<"'.";
	else
	    b2<<"Extra sequence '"<<b2.missing<<"' not contained in earlier alignments.";
	throw b2;    
    }
    return mapping;
}

sequence strip_gaps(const sequence& s1, const vector<char>& missing)
{
    sequence s2 = s1;
    int L=0;
    for(int c=0;c<s2.size();c++)
	if (not includes(missing,s2[c]))
	    s2[L++] = s2[c];
    s2.resize(L);
    return s2;
}

vector<sequence> strip_gaps(const vector<sequence>& S1, const vector<char>& missing)
{
    vector<sequence> S2 = S1;
    for(auto& seq: S2)
	seq = strip_gaps(seq, missing);
    return S2;
}

vector<sequence> concatenate(const vector<sequence>& S1, const vector<sequence>& S2)
{
    if (not S1.size())
	return S2;

    if (S1.size() != S2.size())
        throw myexception()<<"Cannot concatenate alignments with "<<S1.size()<<" and "<<S2.size()<<" sequences.";

    vector<int> mapping = get_mapping(S1,S2);
    vector<sequence> S = S1;
    for(int i=0;i<S1.size();i++)
	(string&)S[i] = S1[i] + S2[mapping[i]];

    return S;
}


void check_all_same_length(const vector<sequence>& s, const string& reason,
                           const string& advice = "Consider option -p to pad them to the same length.")
{
    for(int i=1;i<s.size();i++)
	if (s[i].size() != s[0].size())
	{
	    myexception e;
	    e<<"All sequences in an alignment must have the same length "<<reason<<"\n";
	    e<<"Alignment file: sequence #"<<i+1<<" '"<<s[i].name<<"' has length "<<s[i].size()<<" != "<<s[0].size()<<"\n";
	    e<<advice;
	    throw e;
	}
}

vector<sequence> remove_empty_columns(const vector<sequence>& s,const vector<char>& missing)
{
    check_all_same_length(s, "in order to remove empty columns.");

    // All sequences have the same length after the check above.
    int L = s[0].size();

    // find non-empty columns
    vector<int> columns;
    for(int c=0;c<L;c++)
    {
	bool empty = true;
	for(int j=0;j<s.size() and empty;j++)
	    if (not includes(missing,s[j][c]))
		empty=false;

	if (not empty)
	    columns.push_back(c);
    }

    // select the non-empty columns
    return select(s,columns);
}

vector<sequence> load_file(istream& file,bool pad)
{
    vector<sequence> s = sequence_format::read_guess(file);
    if (s.size() == 0)
	throw myexception()<<"Alignment file didn't contain any sequences!";

    if (pad)
	pad_to_same_length(s);

    return s;
}

// Load through the stream implementation while retaining filename context in errors.
vector<sequence> load_file(const string& filename,bool pad)
{
    checked_ifstream file(filename,"alignment file");
    try
    {
        return load_file(file,pad);
    }
    catch (std::exception& e)
    {
        throw myexception()<<"Alignment file '"<<filename<<"': "<<e.what();
    }
}

vector<sequence> select_taxa(const vector<sequence>& S,const vector<string>& names)
{
    vector<sequence> S2;

    vector<int> mapping(names.size(),-1);
    for(int i=0;i<names.size();i++) {
	for(int j=0;j<S.size() and mapping[i] == -1;j++)
	    if (names[i] == S[j].name)
		mapping[i] = j;
    }

    bool ok=true;
    myexception error;
    for(int i=0;i<mapping.size();i++)
	if (mapping[i] == -1) {
	    if (not ok)
		error<<"\n";
	    error<<"Alignment contains no sequence named '"<<names[i]<<"'";
	    ok = false;
	}

    if (not ok) throw error;

    for(int i=0;i<mapping.size();i++)
	S2.push_back(S[mapping[i]]);

    return S2;
}


struct branch_order {
    const Tree& T;

    bool operator()(int b1,int b2) const {
        int h1 = subtree_height(T,b1), h2 = subtree_height(T,b2);
	if (h1 < h2)
	    return true;
	if (h1 > h2)
	    return false;
	return T.partition(b1).find_first() < T.partition(b2).find_first();
    }

    branch_order(const Tree& T_): T(T_) {}
};


/// get an ordered list of leaves under T[n]
vector<int> get_leaf_order(const Tree& T,int b) 
{
    vector<int> mapping;

    if (T.directed_branch(b).target().is_leaf_node()) {
	mapping.push_back( T.directed_branch(b).target() );
	return mapping;
    }

    // get sorted list of branches
    vector<const_branchview> branches;
    append(T.directed_branch(b).branches_after(),branches);
    std::sort(branches.begin(),branches.end(),branch_order(T));

    // accumulate results
    for(int branch: branches) {
	vector<int> sub_mapping = get_leaf_order(T,branch);
	mapping.insert(mapping.end(),sub_mapping.begin(),sub_mapping.end());
    }

    return mapping;
}

/// get an ordered list of the leaves of T
vector<int> get_leaf_order(const RootedTree& RT) 
{
    vector<int> mapping;

    // get sorted list of branches
    vector<const_branchview> branches;
    append(RT.root().branches_out(),branches);
    std::sort(branches.begin(),branches.end(),branch_order(RT));

    // accumulate results
    for(int branch: branches) {
	vector<int> sub_mapping = get_leaf_order(RT,branch);
	mapping.insert(mapping.end(),sub_mapping.begin(),sub_mapping.end());
    }

    assert(mapping.size() == RT.n_leaves());
    return mapping;
}

vector<string> get_names_from_tree(RootedSequenceTree T, bool use_root)
{
    //------- Re-root the tree appropriately  --------//
    if (not use_root)
    {
        bool missing_length = false, nonzero_length = false;
        for(int b=0;b<T.n_branches();b++)
        {
            if (not T.branch(b).has_length())
                missing_length = true;
            else if (T.branch(b).length() != 0)
                nonzero_length = true;
        }

        // Fallback: incomplete or all-zero lengths provide no usable metric for rooting.
        // Give every edge unit length before the existing root search, preserving its tie rules.
        // Keep this policy unless a different topology-only rooting rule is explicitly chosen.
        if (missing_length or not nonzero_length)
            for(int b=0;b<T.n_branches();b++)
                T.branch(b).set_length(1.0);

	int rootb=-1;
	double rootd = -1;
	find_root(T,rootb,rootd);
	if (log_verbose) {
	    std::cerr<<"alignment-cat: root branch = "<<rootb<<std::endl;
	    std::cerr<<"alignment-cat: x = "<<rootd<<std::endl;
	    for(int i=0;i<T.n_leaves();i++)
		std::cerr<<"alignment-cat: "<<T.get_label(i)<<"  "<<rootdistance(T,i,rootb,rootd)<<std::endl;
	}
    
	T = add_root((SequenceTree)T,rootb);  // we don't care about the lengths anymore
    }
  
    //----- Standardize initial leaf order by alphabetical order of names ----//
    vector<string> names = T.get_leaf_labels();
  
    std::sort(names.begin(),names.end());
  
    vector<int> mapping1 = compute_mapping(T.get_leaf_labels(),names);
  
    T.standardize(mapping1);
  
    //-------- Compute the mapping  -------//
    vector<int> order = get_leaf_order(T);

    //-------- Compute the ordered list of names ------//
    names.clear();
    for(int l: order)
	names.push_back(T.get_label(l));

    return names;
}

vector<string> get_names(const vector<sequence>& S)
{
    vector<string> names;
    for(const auto& s: S)
	names.push_back(s.name);
    return names;
}

vector<sequence> align_by_amino_acids(const vector<sequence>& S1, const string& filename, const vector<char>& missing)
{
    // 1. Load the amino acid sequence alignment, and pad it.
    vector<sequence> aminos = load_file(filename,true);

    // 2. Check that there are the same number of amino and nucleotide sequence.
    if (S1.size() != aminos.size())
	throw myexception()<<"Amino acid alignment has "<<aminos.size()<<" sequences, but there are "<<S1.size()<<" nucleotide sequences.";

    // 3. Rearrange nucleotide sequences in the same order as the amino acid sequences.
    vector<sequence> S2 = select_taxa(S1, get_names(aminos));

    for(int i=0;i<S2.size();i++)
    {
	sequence nuc = strip_gaps(S2[i],missing);
	const sequence& aa = aminos[i];
	assert(nuc.name == aa.name);
	int aa_length = strip_gaps(aa,missing).size();
	// Each non-missing amino acid consumes three nucleotides; missing positions consume none.
	// Exact equality after stripping guarantees that the loop neither loses nor overruns nucleotides.
	if (nuc.size() != 3*aa_length)
	    throw myexception()<<"Sequence '"<<nuc.name<<"' has "<<nuc.size()<<" nucleotides - cannot match 3*"<<aa_length<<"="<<3*aa_length<<" amino acids.";

	S2[i].resize(3*aa.size());
	for(int j=0,k=0,l=0;j<aa.size();j++)
	{
	    if (includes(missing,aa[j]))
	    {
		S2[i][k++] = aa[j];
		S2[i][k++] = aa[j];
		S2[i][k++] = aa[j];
	    }
	    else
	    {
		S2[i][k++] = nuc[l++];
		S2[i][k++] = nuc[l++];
		S2[i][k++] = nuc[l++];
	    }
	}
    }

    return S2;
}


int main(int argc,char* argv[]) 
{ 

    try {
	//---------- Parse command line  -------//
        CLI::App app{"Concatenate, select, reorder, and reformat aligned sequences.", "alignment-cat"};
        app.formatter(std::make_shared<AlignmentCatFormatter>());
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        string output = "fasta", columns, taxa, missing_characters = "-?=";
        string tree_file, alignment_file, amino_file;
        bool pad = false, reverse = false, erase_empty_columns = false;
        bool do_strip_gaps = false, use_root = false;
        vector<string> filenames;

        app.add_option("--output", output, "Output format: fasta or phylip")->capture_default_str();
        app.add_option("-c,--columns", columns, "Columns to keep, e.g. 1-10,30- or 1-/3");
        app.add_option("-t,--taxa", taxa, "Taxa to keep in order: comma-separated names or @filename");
        app.add_flag("-p,--pad", pad, "Pad each input's shorter sequences with gaps");
        app.add_flag("-r,--reverse", reverse, "Reverse each sequence without complementing it");
        app.add_flag("-e,--erase-empty-columns", erase_empty_columns, "Remove columns containing only missing characters");
        app.add_option("--missing", missing_characters, "Characters treated as missing")->capture_default_str();
        app.add_flag("--strip-gaps", do_strip_gaps, "Remove missing characters from each sequence");
        app.add_option("--reorder-by-tree", tree_file, "Select and order sequences using a tree");
        app.add_flag("--use-root", use_root, "Use the specified tree root for ordering");
        app.add_option("--reorder-by-alignment", alignment_file, "Select and order sequences using another alignment");
        app.add_option("--align-by-amino", amino_file, "Arrange nucleotides using an amino-acid alignment");
        auto* verbosity = app.add_option("-V,--verbose", log_verbose, "Diagnostic verbosity (1 if no value is given)")
            ->type_name("LEVEL")->expected(0, 1);

        app.add_option("file,--file", filenames, "Input alignments (default: stdin; '-' reads stdin)")
            ->type_size(1)->expected(-1);
        // The examples are preformatted; preserve their indentation and line breaks.
        app.get_formatter()->enable_footer_formatting(false);
        app.footer("Examples:\n"
                   "  alignment-cat -c1-10,50-100,600- alignment.fasta > selected.fasta\n"
                   "  alignment-cat -c1-/3 codons.fasta > position1.fasta\n"
                   "  alignment-cat gene1.fasta gene2.fasta > combined.fasta\n");
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

	optional<vector<string>> names;
	if (app.count("--taxa"))
	    names = get_string_list(taxa);
	else if (app.count("--reorder-by-tree"))
	{
	    RootedSequenceTree RT;
	    RT.read(tree_file);
	    names = get_names_from_tree(RT, use_root);
	}
	else if (app.count("--reorder-by-alignment"))
	{
	    vector<sequence> sequences = load_file(alignment_file, false);
            names = get_names(sequences);
	}

	//------- Determine filenames --------//
	if (filenames.empty())
	    filenames = {"-"};

	//------- Try to load sequences --------//
	vector<sequence> S;
	vector<sequence> s_in;
	bool cin_read = false;
	for(int i=0;i<filenames.size();i++) 
	{
	    // Read the sequences
	    vector<sequence> s;
	    if (filenames[i] == "-")
	    {
		if (not cin_read) {
		    s_in = load_file(cin,pad);
		    cin_read = true;
		}
		s = s_in;
	    }
	    else
		s = load_file(filenames[i],pad);

	    // Add the sequences to what we have so far
	    try {
		if (filenames.size() > 1)
		    check_all_same_length(s, "in order to concatenate two or more alignments.");
	
		if (names)
		    s = select_taxa(s,*names);
		S = concatenate(S,s);
	    }
	    catch (std::exception& e) {
		throw myexception()<<"File '"<<filenames[i]<<"': "<<e.what();
	    }
	}

	// determine which chars are not characters
	vector<char> missing(missing_characters.begin(), missing_characters.end());

        // Empty selections have no sequence transformations, but still reach output validation.
        if (not S.empty())
        {
            if (app.count("--align-by-amino"))
            {
                S = align_by_amino_acids(S,amino_file,missing);
            }

            if (app.count("--columns"))
            {
                S = select(S,columns);
            }

            if (erase_empty_columns)
                S = remove_empty_columns(S,missing);

            if (do_strip_gaps)
                S = strip_gaps(S, missing);

            // Reverse each sequence, if asked.
            if (reverse)
                for(sequence& s: S)
                    std::reverse(s.begin(), s.end());
        }

	if (output == "phylip")
        {
            if (S.empty())
                throw myexception()<<"PHYLIP output requires at least one sequence.";
            check_all_same_length(S, "in order to write PHYLIP.",
                                  "Use FASTA for unequal lengths; --pad runs before --strip-gaps.");
            write_phylip(cout,S);
        }
	else if (output == "fasta")
	    write_fasta(cout,S);
	else
	    throw myexception()<<"I don't recognize requested format '"<<output<<"'";
    }
    catch (std::exception& e) {
	std::cerr<<"alignment-cat: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
