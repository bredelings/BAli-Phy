#include "A-T-model.H"
#include <algorithm>                                // for max, find, min
#include <filesystem>                               // for path, operator/
#include <boost/dynamic_bitset.hpp>                 // for dynamic_bitset
#include <boost/program_options.hpp>                // for program_options
#include <boost/algorithm/string/join.hpp>          // for join
#include <map>                                      // for map, map<>::mappe...
#include <optional>                                 // for optional
#include <set>                                      // for set
#include <utility>                                  // for tuple_element<>::...
#include "alignment/alignment-constraint.H"         // for load_alignment_co...
#include "alignment/alignment.H"                    // for alignment, sequen...
#include "alignment/index-matrix.H"                 // for get_alignment
#include "alignment/load.H"                         // for load_alignment
#include "computation/expression/expression_ref.H"  // for expression_ref
#include "computation/expression/lambda.H"          // for lambda_quantify
#include "computation/expression/var.H"             // for var
#include "computation/object.H"                     // for intrusive_ptr_rel...
#include "computation/program.H"                    // for class Program
#include "link-partitions.H"                        // for shared_items, get...
#include "models/TreeInterface.H"                   // for TreeInterface
#include "models/parameters.H"                      // for Parameters, param...
#include "models/A-T-prog.H"                        // for gen_atmodel_program
#include "models/rules.H"                           // for Rules
#include "models/compile.H"                         // for model_t, compile_model
#include "models/parse.H"                           // for parse_type( )
#include "sequence/alphabet.H"                      // for alphabet
#include "sequence/doublets.H"                      // for Doublets
#include "sequence/RNAEdits.H"                      // for RNAEdits
#include "sequence/codons.H"                        // for Codons
#include "sequence/sequence-format.H"               // for load_from_file
#include "sequence/sequence.H"                      // for sequence, select
#include "tree-align/link.H"                        // for link
#include "tree/tree-branchnode.H"                   // for branchview, const...
#include "tree/tree-util.H"                         // for extends, load_T
#include "tree/tree.H"                              // for RandomTree, branc...
#include "util/io.H"                                // for portable_getline
#include "util/io/optional.H"                       // for operator<<
#include "util/mapping.H"                           // for bad_mapping, find...
#include "util/matrix.H"                            // for matrix
#include "util/myexception.H"                       // for myexception
#include "util/ptree.H"                             // for ptree
#include "util/settings.H"                          // for load_settings
#include "util/string/convert.H"                    // for convertTo, conver...
#include "util/string/join.H"                       // for join
#include "util/string/split.H"                      // for split, split_on_last
#include "util/text.H"                              // for bold, bold_blue
#include <range/v3/all.hpp>                         // for ranges::transform

class module_loader;

namespace views = ranges::views;

extern int log_verbose;

namespace fs = std::filesystem;

namespace po = boost::program_options;
using po::variables_map;

using boost::dynamic_bitset;

using std::cout;
using std::cerr;
using std::clog;
using std::endl;
using std::ostream;
using std::string;
using std::vector;
using std::pair;
using std::optional;
using std::map;
using std::set;

/// Replace negative or zero branch lengths with saner values.
void sanitize_branch_lengths(SequenceTree& T)
{
    double min_branch = 1e-6;
    for(int i=0;i<T.n_branches();i++)
        if (T.branch(i).has_length() and T.branch(i).length() > 0)
            min_branch = std::min(min_branch,T.branch(i).length());

    for(int i=0;i<T.n_branches();i++)
        if (T.branch(i).has_length())
        {
            double L = T.branch(i).length();
            if (L <= 0)
            {
                if (log_verbose > 0)
                {
                    if (L < 0)
                        std::cerr<<"Read in negative branch length "<<L<<": ";
                    else if (L == 0)
                        std::cerr<<"Read in zero branch length: ";
                    std::cerr<<"setting length to "<<min_branch;
                }
                T.branch(i).set_length(min_branch);
            }
        }
}

vector<double> get_geometric_heating_levels(const string& s)
{
    vector<double> levels;

    vector<string> parse = split(s,'/');

    if (parse.size() != 2) return levels;

    try
    {
        int n_levels = convertTo<int>(parse[1]);
        levels.resize(n_levels);
    
        parse = split(parse[0],'-');
        levels[0] = convertTo<double>(parse[0]);
        levels.back() = convertTo<double>(parse[1]);
        double factor = pow(levels.back()/levels[0], 1.0/(n_levels-1));
    
        for(int i=1;i<levels.size()-1;i++)
            levels[i] = levels[i-1]*factor;
    
        return levels;
    }
    catch (...)
    {
        throw myexception()<<"I don't understand beta level string '"<<s<<"'";
    }
}

/*
void setup_heating(int proc_id, const variables_map& args, Parameters& P) 
{
    if (args.count("beta")) 
    {
        string beta_s = args["beta"].as<string>();

        vector<double> beta = get_geometric_heating_levels(beta_s);
        if (not beta.size())
            beta = convertTo<double>(split(beta_s,','));

        P.PC->all_betas = beta;

        if (proc_id >= beta.size())
            throw myexception()<<"not enough temperatures given: only got "<<beta.size()<<", wanted at least "<<proc_id+1;

        P.set_beta(beta[proc_id]);

        P.PC->beta_series.push_back(beta[proc_id]);
    }

    if (args.count("dbeta")) {
        vector<string> deltas = split(args["dbeta"].as<string>(),',');
        for(auto& delta: deltas) {
            vector<double> D = convertTo<double>(split(delta,'*'));
            if (D.size() != 2)
                throw myexception()<<"Couldn't parse beta increment '"<<delta<<"'";
            int D1 = (int)D[0];
            double D2 = D[1];
            for(int i=0;i<D1;i++) {
                double next = P.PC->beta_series.back() + D2;
                next = std::max(0.0,next);
                P.PC->beta_series.push_back(next);
            }
        }
    }
    for(double b:P.PC->beta_series)
        std::cout<<b<<"\n";
}
*/

vector<model_t>
compile_imodels(const Rules& R, TypecheckingState TC, CodeGenState code_gen_state, const shared_items<string>& imodel_names_mapping)
{
    map<string,pair<string,ptree>> imodel_states = {{"topology",{"topology",parse_type("Topology")}}};

    TC.add_states(imodel_states);

    vector<model_t> imodels;
    for(int i=0;i<imodel_names_mapping.n_unique_items();i++)
    {
        string what = "indel model " + std::to_string(i+1);

        auto TC2 = TC;
        imodels.push_back( compile_model(R, TC2, code_gen_state, parse_type("IndelModel"), imodel_names_mapping.unique(i), what, {}, imodel_states) );
    }
    return imodels;
}

template <typename T>
json::value optional_to_json(const std::optional<T>& o)
{
    if (not o)
        return nullptr;
    else
        return *o;
}


// FIXME - maybe we should try to make a single giant model so that we get S1/parameter
//           exactly when this occurs for the logged parameter names, themselves.
//
//           This would also help it we have some variables that are outside of models, because
//             they are shared between them.

string tag(string s, int i)
{
    return s+convertToString(i+1);
}

string show_main_bold(const model_t& m)
{
    string s = indent_and_wrap(0,12,1000,m.show_main(false));
    return s.substr(0,2) + bold(s.substr(2));
}

json::object log_summary(const vector<model_t>& IModels,
			 const vector<model_t>& SModels,
			 const vector<model_t>& ScaleModels,
			 const model_t& tree_model,
                         const model_t& subst_rates,
                         const model_t& indel_rates,
			 std::vector<std::optional<int>>& smodel_index_for_partition,
			 std::vector<std::optional<int>>& imodel_index_for_partition,
			 std::vector<std::optional<int>>& scale_index_for_partition,
                         int n_sequences, int n_data_partitions,
                         const vector<string>& alphabet_names,
                         const vector<string>& smodel_conditions,
                         const variables_map& args)
{
    string tree;
    string m_subst_rates;
    string m_indel_rates;
    if (n_sequences >= 2 and not tree_model.empty())
    {
        cout<<"tree "<<tree_model.show()<<endl;
        tree = tree_model.show(false);

        if (subst_rates.empty())
        {
            cout<<"subst rates = constant"<<endl;
            m_subst_rates = "constant";
        }
        else
        {
            cout<<"subst rates "<<subst_rates.show()<<endl;
            m_subst_rates = subst_rates.show(false);
        }

        if (indel_rates.empty())
        {
            cout<<"indel rates = constant"<<endl;
            m_indel_rates = "constant";
        }
        else
        {
            cout<<"indel rates "<<indel_rates.show()<<endl;
            m_indel_rates = indel_rates.show(false);
        }

        cout<<endl;
    }

    //-------- Log some stuff -----------//
    auto filename_ranges = args["align"].as<vector<string> >();

    vector<pair<fs::path,string>> alignment_files;
    for(const auto& [filename,range]: args["align"].as<vector<string>>()
                                      | views::transform([&](auto& x) {
                                          return split_on_last_regex(":", "[-\\d.\\/]*", x);
                                      }))
        alignment_files.push_back( {fs::path(filename), range} );

    json::array partitions;
    for(int i=0;i<n_data_partitions;i++)
    {
	json::object partition;

        cout<<"Partition "<<magenta(tag("P",i))<<":\n";
        // 1. filename 
        auto [filename,range] = alignment_files[i];
        string file = bold(filename.string());
        if (range.size())
            file += ":"+range;
        cout<<"    file = "<<bold(file)<<endl;
        partition["filename"] = filename.string();
        partition["range"] = range;

        // 2. alphabet
        string a_name = alphabet_names[i];
        cout<<"    alphabet = "<<bold(a_name)<<"\n";
        partition["alphabet"] = a_name;

        // 3. substitution model
        auto s_index = smodel_index_for_partition[i];
        cout<<"    subst "<<show_main_bold(SModels[*s_index]);
        auto condition = smodel_conditions[*s_index];
        partition["smodel"] = optional_to_json( smodel_index_for_partition[i] );
        if (condition.empty())
            partition["condition"] = nullptr;
        else
        {
            cout<<bold(" | "+smodel_conditions[i]);
            partition["condition"] = condition;
        }
        cout<<" ("<<bold_blue(tag("S",*s_index))<<")\n";

        // 4. indel model
        if (auto i_index = imodel_index_for_partition[i])
            cout<<"    indel "<<show_main_bold(IModels[*i_index])<<" ("<<bold_red(tag("I",*i_index))<<")\n";
        else
            cout<<"    indel = "<<bold("none")<<"\n";
        partition["imodel"] = optional_to_json( imodel_index_for_partition[i] );

        // 5. scale model
        auto scale_index = scale_index_for_partition[i];
        cout<<"    scale "<<show_main_bold(ScaleModels[*scale_index])<<" ("<<green(tag("Scale",*scale_index))<<")\n";
        partition["scale"] = optional_to_json( scale_index_for_partition[i] );

        cout<<endl;
        partitions.push_back(partition);
    }

    json::array smodels;
    for(int i=0;i<SModels.size();i++)
    {
        //    out_cache<<"subst model"<<i+1<<" = "<<P.SModel(i).name()<<endl<<endl;
        smodels.push_back( convert_to_json(SModels[i].pretty_model()) );
        string e = SModels[i].show_extracted();
        if (e.size())
            cout<<"Substitution model "<<bold_blue(tag("S",i))<<" priors:"<<e<<"\n\n";
    }

    json::array imodels;
    for(int i=0;i<IModels.size();i++)
    {
        imodels.push_back( convert_to_json(IModels[i].pretty_model()) );
        string e = IModels[i].show_extracted();
        if (e.size())
            cout<<"Insertion/deletion model "<<bold_red(tag("I",i))<<" priors:"<<e<<"\n\n";
    }

    json::array scales;
    for(int i=0;i<ScaleModels.size();i++)
    {
        scales.push_back( convert_to_json(ScaleModels[i].pretty_model()) );
        string e = ScaleModels[i].show_extracted();
        if (e.size())
            cout<<"Scale model "<<green(tag("Scale",i))<<" priors:"<<e<<"\n\n";
    }
    cout.flush();

    json::object info;
    info["partitions"] = partitions;
    info["smodels"] = smodels;
    info["imodels"] = imodels;
    info["scales"] = scales;
    info["tree"] = tree;
    info["subst rates"] = m_subst_rates;
    info["indel rates"] = m_indel_rates;
    return info;
}

void warn_if_newick_chars_in_alignment_names(const alignment& A)
{
    const string special = "();:,[]&'";

    bool warn = false;
    for(int i=0;i<A.n_sequences();i++)
    {
        // 1. Get the sequence name.
        const string& name = A.seq(i).name;

        // 2. Find special chars that it contains.
        std::set<char> chars;
        for(char c: special)
            if (name.find(c) != -1)
                chars.insert(c);

        // 3. Warning about the name if it contains special chars.
        if (not chars.empty())
        {
            warn = true;
            std::cerr<<"WARNING: Sequence name \""<<name<<"\" contains Newick special character";
            if (chars.size() > 1)
                std::cerr<<'s';
            std::cerr<<" ";

            vector<string> cs;
            for(auto c: chars)
                cs.push_back(string(1,char('"'))+c+char('"'));
            std::cerr<<join(cs,", ")<<std::endl;
        }
    }

    // 4. Explain
    if (warn)
        std::cerr<<"WARNING: Some phylogenetics software may not correctly read Newick trees with these sequence names."<<std::endl<<std::endl;
}

void check_alignment_values(const alignment& A,const pair<fs::path,string>& filename_range)
{
    const alphabet& a = A.get_alphabet();

    for(int i=0;i<A.n_sequences();i++)
    {
        string name = A.seq(i).name;

        for(int j=0;j<A.length();j++) 
            if (A.unknown(j,i))
            {
                auto [filepath, range] = filename_range;
                string filename = filepath.string();
                if (range.size())
                    filename = filename + ":" + range;
                throw myexception()<<"Alignment file '"<<filename<<"' has a '"<<a.unknown_letter()<<"' in sequence '"<<name<<"'.\n (Please replace with gap character '"<<a.gap_letter<<"' or wildcard '"<<a.wildcard<<"'.)";
            }
    }
}

void write_branch_numbers(ostream& o, SequenceTree T)
{
    // Write out a tree 
    auto flags = o.flags();
    o.unsetf(std::ios::floatfield);
    for(int b=0;b<T.n_branches();b++)
        T.branch(b).set_length(b);
    o<<"branch numbers = "<<T<<"\n\n";
    o.flags(flags);
}

// return the list of constrained branches
vector<int> load_alignment_branch_constraints(const string& filename, const SequenceTree& TC)
{
    // open file
    checked_ifstream file(filename,"alignment-branch constraint file");

    // read file
    string line;
    vector<vector<string> > name_groups;
    while(portable_getline(file,line)) {
        vector<string> names = split(line,' ');
        for(int i=names.size()-1;i>=0;i--)
            if (names[i].size() == 0)
                names.erase(names.begin()+i);

        if (names.size() == 0) 
            continue;
        else if (names.size() == 1)
            throw myexception()<<"In alignment constraint file: you must specify more than one sequence per group.";
    
        name_groups.push_back(names);
    }

    // parse the groups into mask_groups;
    vector< dynamic_bitset<> > mask_groups(name_groups.size());
    for(int i=0;i<mask_groups.size();i++) 
    {
        mask_groups[i].resize(TC.n_leaves());
        mask_groups[i].reset();

        for(int j=0;j<name_groups[i].size();j++)
        {
            if (auto index = find_index(TC.get_leaf_labels(), name_groups[i][j]))
                mask_groups[i][*index] = true;
            else
                throw myexception()<<"Reading alignment constraint file '"<<filename<<"':\n"
                                   <<"   Can't find leaf taxon '"<<name_groups[i][j]<<"' in the tree.";
        }
    }

    // 1. check that each group is a fully resolved clade in the constraint tree (no polytomies)
    // 2. construct the list of constrained branches
    // FIXME - what if the user specifies nested clades?  Won't we get branches twice, then?
    //       - SOLUTION: use a bitmask.
    vector<int> branches;
    for(int i=0;i<mask_groups.size();i++) 
    {
        // find the branch that corresponds to a mask
        boost::dynamic_bitset<> mask(TC.n_leaves());
        int found = -1;
        for(int b=0;b<2*TC.n_branches() and found == -1;b++) 
        {
            mask = TC.partition(b);

            if (mask_groups[i] == mask)
                found = b;
        }

        // complain if we can't find it
        if (found == -1) 
            throw myexception()<<"Alignment constraint: clade '"
                               <<join(name_groups[i],' ')
                               <<"' not found in topology constraint tree.";
    
        // mark branch and child branches as constrained
        for(auto branch: branches_after_inclusive(TC, found))
        {
            if (branch.target().degree() > 3)
                throw myexception()<<"Alignment constraint: clade '"
                                   <<join(name_groups[i],' ')
                                   <<"' has a polytomy in the topology constraint tree.";
            branches.push_back(branch.undirected_name());
        }
    }


    return branches;
}

alignment unalign_A(const alignment& A)
{
    vector<int> L;
    for(int i=0;i<A.n_sequences();i++)
        L.push_back(A.seqlength(i));

    return get_alignment(unaligned_matrix(L),A);
}

bool needs_constant_alignment(const alphabet& a)
{
    if (dynamic_cast<const Doublets*>(&a)) return true;
    if (dynamic_cast<const Numeric*>(&a))  return true;

    return false;
}

bool can_share_imodel(const alphabet& a1, const alphabet& a2)
{
    if (needs_constant_alignment(a1) != needs_constant_alignment(a2)) return false;

    if (a1.width() != a2.width()) return false;

    if (dynamic_cast<const Nucleotides*>(&a1)) return bool(dynamic_cast<const Nucleotides*>(&a2));

    if (dynamic_cast<const Doublets*>(&a1))    return bool(dynamic_cast<const Doublets*>(&a2));

    if (dynamic_cast<const Triplets*>(&a1))    return bool(dynamic_cast<const Triplets*>(&a2));

    if (dynamic_cast<const AminoAcids*>(&a1))  return bool(dynamic_cast<const AminoAcids*>(&a2));

    return a1.name == a2.name;
}


model_t compile_smodel(const Rules& R, TypecheckingState TC, CodeGenState code_gen_state, const std::string& model, const string& what)
{
    map<string,pair<string,ptree>> smodel_states = {
        {"alphabet",{"alpha",parse_type("a")}},
        {"branch_categories",{"branch_categories",parse_type("List<Int>")}},
        {"tree",{"tree",parse_type("Tree<t>")}},
    };

    TC.add_states(smodel_states);

    try {
        auto TC2 = TC;
        return compile_model(R, TC2, code_gen_state, parse_type("CTMC<a>"), model, what, {}, smodel_states);
    }
    catch (myexception& e) {};

    try {
        auto TC2 = TC;
        return compile_model(R, TC2, code_gen_state, parse_type("DiscreteDist<CTMC<a>>"), model, what, {}, smodel_states);
    }
    catch (myexception& e) {};

    auto TC2 = TC;
    return compile_model(R, TC, code_gen_state, parse_type("MultiMixtureModel<a>"), model, what, {}, smodel_states);
}


int num_distinct(const vector<optional<int>>& v)
{
    int m = -1;
    for(auto& x: v)
        if (x)
            m = std::max(m,*x);
    return m+1;
}

int get_num_models(const vector<optional<int>>& mapping)
{
    int m = -1;
    set<int> models;
    for(auto& model: mapping)
        if (model)
        {
            assert(*model >= 0);
            m = std::max(m,*model);
            models.insert(*model);
        }

    int n = models.size();
    assert(m+1 == n);
    return n;
}

string get_alphabet_type(ptree type)
{
    auto [head,args] = get_type_apps(type);

    if (head == "MultiMixtureModel")
	return get_type_head(args[0]);
    else if (head == "DiscreteDist")
	return get_alphabet_type(args[0]);
    else if (head == "CTMC")
	return get_type_head(args[0]);
    else
	throw myexception()<<"get_alphabet_type: I don't understand type "<<type.show();
}

std::vector<string> get_default_alphabet_names(const shared_items<string>& smodel_names_mapping, const vector<model_t>& full_smodels, const shared_items<string>& alphabet_names_mapping)
{
    //------------- Get alphabet names -------------------
    vector<string> alphabet_names;
    for(int i=0;i<alphabet_names_mapping.n_partitions();i++)
        alphabet_names.push_back(alphabet_names_mapping[i]);

    // 2. For all SPECIFIED smodels, try to determine the alphabet.
    //    (Alphabets for SPECIFIED smodel groups must be all specified or all unspecified).
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
        if (smodel_names_mapping.unique(i).empty()) continue;

        auto type = full_smodels[i].type;
        auto& constraints = full_smodels[i].constraints;

        auto alphabet_type = get_alphabet_type(type);

        vector<int> a_specified;
        vector<int> a_unspecified;
        for(int j: smodel_names_mapping.partitions_for_item[i])
        {
            if (alphabet_names[j].empty())
                a_unspecified.push_back(j);
            else
                a_specified.push_back(j);
        }
        if (a_specified.size() and a_unspecified.size())
            throw myexception()<<"SModel "<<i+1<<" applied to partition "<<a_specified[i]+1<<" (alphabet specified) and partition "
                               <<a_unspecified[0]+1<<" (alphabet not specified).";

        if (a_specified.size())
        {
        }
        else if (alphabet_type == "Codons")
        {
            for(int j: smodel_names_mapping.partitions_for_item[i])
                alphabet_names[j] = "Codons";
        }
        else if (alphabet_type == "RNAEdits")
        {
            for(int j: smodel_names_mapping.partitions_for_item[i])
                alphabet_names[j] = "RNAEdits";
        }
        else if (alphabet_type == "Triplets")
        {
            for(int j: smodel_names_mapping.partitions_for_item[i])
                alphabet_names[j] = "Triplets";
        }
        else if (alphabet_type == "Doublets")
        {
            for(int j: smodel_names_mapping.partitions_for_item[i])
                alphabet_names[j] = "Doublets";
        }
        else
        {
            bool triplet_constraint = false;
            bool doublet_constraint = false;
            for(auto& constraint: constraints)
	    {
		auto [head,args] = get_type_apps(constraint);
                if (head == "Triplets" and args[0] == alphabet_type)
                    triplet_constraint = true;
                if (head == "Doublets" and args[0] == alphabet_type)
                    doublet_constraint = true;
	    }

            if (triplet_constraint)
                for(int j: smodel_names_mapping.partitions_for_item[i])
                    alphabet_names[j] = "Triplets";

            if (doublet_constraint)
                for(int j: smodel_names_mapping.partitions_for_item[i])
                    alphabet_names[j] = "Doublets";
        }
//      Use the auto-detected alphabet right now -- it leads to better error messages.
//      else if (alphabet_type == "AA")
//      {
//          for(int j: smodel_names_mapping.partitions_for_item[i])
//              alphabet_names[j] = "AA";
//      }
    }

    return alphabet_names;
}

// Read the alignments and update the alphabet names to what we actually see.
vector<alignment> read_alignments(const vector<pair<fs::path,string>>& filename_ranges, vector<string>& alphabet_names)
{
    vector<alignment> A(filename_ranges.size());

    map<fs::path,vector<sequence>> sequences_for_filename;
    for(auto& [filename, range]: filename_ranges)
        if (not sequences_for_filename.count(filename))
            sequences_for_filename[filename] = sequence_format::load_from_file(filename);

    for(int i=0;i<filename_ranges.size();i++)
    {
        auto [filename, range] = filename_ranges[i];
        try
        {
            A[i] = load_alignment(select(sequences_for_filename[filename], range), alphabet_names[i]);
        }
        catch (myexception& e)
        {
            if (range.empty())
                e.prepend("In file '"+filename.string()+"': ");
            else
                e.prepend("In file '"+filename.string()+"' columns "+range+": ");
            throw;
        }

        // Record the final guessed alphabet name, in case the alphabet was unspecified (e.g. "")
        // or not fully specified (e.g. "Codons", "Codons(,standard)", "Codons(DNA)").
        alphabet_names[i] = A[i].get_alphabet().name;
    }

    for(int i=0;i<A.size();i++) {
        warn_if_newick_chars_in_alignment_names(A[i]);
        check_alignment_values(A[i],filename_ranges[i]);
    }

    return A;
}

string findAndReplaceAll( std::string data,
                          const std::string& match,
                          const std::string& replace)
{
   // Get the first occurrence
   size_t pos = data.find(match);

   // Repeat till end is reached
   while( pos != std::string::npos )
   {
        data.replace(pos, match.size(), replace);

       // Get the next occurrence from the current position
        pos = data.find(match, pos+replace.size());
   }

   return data;
}

void get_default_imodels(shared_items<string>& imodel_names_mapping, const vector<alignment>& A)
{
    //-- Check that we're not estimating the alignment for things that aren't sequences --//
    for(int i = imodel_names_mapping.n_unique_items() - 1; i >= 0; i--)
    {
        auto& value = imodel_names_mapping.unique(i);

        // Maybe complain based on needs_constant_alignment(a)

        // Check that all partitions with the same indel model have compatible alphabets

        bool need_constant_a = false;
        optional<int> j0;
        for(int j: imodel_names_mapping.partitions_for_item[i])
        {
            auto& a = A[j].get_alphabet();

            if (not j0) j0 = j;

            if (value.empty() and needs_constant_alignment(A[*j0].get_alphabet()) != needs_constant_alignment(a))
                throw myexception()<<"Can't guess shared indel model for incompatible partitions "<<*j0+1<<" ("<<A[*j0].get_alphabet().name<<") and "<<j+1<<" ("<<a.name<<").\n";

            if (not can_share_imodel(A[*j0].get_alphabet(), a))
                std::cerr<<"Warning! Linking indel model '"<<value<<"' for partition "<<*j0+1<<" ("<<A[*j0].get_alphabet().name<<") and partition "<<j+1<<" ("<<a.name<<") -- is this right?\n";

            if (needs_constant_alignment(a))
            {
                if (not value.empty())
                    throw myexception()<<"Data of type '"<<a.name<<"' requires a constant alignment in partition "<<j+1<<", but got indel model '"<<value<<"'";
                need_constant_a = true;
            }
        }

        // Choose default indel model based on needs_constant_alignment(a)
        if (value == "")
        {
            if (need_constant_a)
            {
                imodel_names_mapping.remove_nth_item(i);
                continue;
            }
            else
                value = "rs07";
        }
    }
}


std::tuple<std::unique_ptr<Program>, json::object>
create_A_and_T_model(const Rules& R, variables_map& args, const std::shared_ptr<module_loader>& L,
		     int /* proc_id */, const fs::path& dir)
{
    // 1. --- Determine number of partitions
    vector<pair<fs::path,string>> filename_ranges;
    for(const auto& [filename,range]: args["align"].as<vector<string>>()
                                      | views::transform([&](auto& x) {
                                          return split_on_last_regex(":","[-\\d.\\/]*",x);
                                      }))
        filename_ranges.push_back( {fs::path(filename), range});

    const int n_partitions = filename_ranges.size();

    // 2. --- Find out what is fixed
    auto fixed = get_fixed(args);

    // 3. --- Compile declarations
    model_t decls;
    vector<pair<string,ptree>> extra_vars = {{"taxa",parse_type("List<Text>")}};
    if (fixed.count("topology"))
	extra_vars.push_back({"topology",parse_type("Tree<Nothing>")});

    auto TC = makeTypechecker(R, extra_vars, {});
    CodeGenState code_gen_state(R);
    string var_str;
    if (args.count("variables"))
	var_str = boost::algorithm::join( args.at("variables").as<vector<string>>(), "");

    decls = compile_decls(R, TC, code_gen_state, var_str, extra_vars, {});

    // QUESTION: How do we access the list of declared variables, their type, and their haskell name?

    // 4. --- Get smodels for all SPECIFIED smodel names 
    auto smodel_names_mapping = get_mapping(args, "smodel", n_partitions);

    vector<string> smodel_conditions;
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
        auto fullname = smodel_names_mapping.unique(i);
        auto [name,condition] = split_on_last_regex("\\|","[a-zA-Z0-9_]*", fullname);

        if (not condition.empty())
        {
            if (condition != "variable")
                throw myexception()<<"Unknown ascertainment condition '"<<condition<<"' in substitution model '"<<fullname<<"'.\n  Implemented conditions include: variable";
        }
        smodel_names_mapping.unique(i) = name;
        smodel_conditions.push_back(condition);
    }

    auto& smodel_mapping = smodel_names_mapping.item_for_partition;

    vector<model_t> full_smodels(smodel_names_mapping.n_unique_items());

    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
        if (not smodel_names_mapping.unique(i).empty())
            full_smodels[i] = compile_smodel(R, TC, code_gen_state, smodel_names_mapping.unique(i), "substitution model " + std::to_string(i+1));

    // 5. --- Get unspecified alphabet names from specified substitution models types.
    shared_items<string> alphabet_names_mapping = get_mapping(args, "alphabet", filename_ranges.size());

    vector<string> alphabet_names = get_default_alphabet_names(smodel_names_mapping, full_smodels, alphabet_names_mapping);

    // 6. -- Load alignments and determine alphabets (for SPECIFIED, UNSPECIFIED, and PARTIALLY SPECIFIED alphabets)
    //       The ONLY thing we use these alignments for is their alphabet.
    vector<alignment> A = read_alignments(filename_ranges, alphabet_names);

    // 7. --- Check that all smodel-linked partitions end up with the same alphabet.
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
	int first_partition = smodel_names_mapping.partitions_for_item[i][0];
	string a1 = alphabet_names[first_partition];
	string a1b = findAndReplaceAll(a1, "RNA", "DNA");

        for(int partition: smodel_names_mapping.partitions_for_item[i])
	{
	    string a2 = alphabet_names[partition];
	    string a2b = findAndReplaceAll(a2, "RNA", "DNA");
	    if (a1b != a2b)
		throw myexception()<<"Partitions "<<first_partition+1<<" ("<<a1<<") and "<<partition+1<<" ("<<a2<<") have different alphabets, but are given the same substitution model!";
	}

    }

    // 8. --- Default unspecified substitution models based on the alphabet, compile them.
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
        if (smodel_names_mapping.unique(i).empty())
        {
            // Check that all the alphabets are the same before using the alphabet to get a default model.
            int first_index = smodel_names_mapping.partitions_for_item[i][0];
            const alphabet& a = A[first_index].get_alphabet();

            smodel_names_mapping.unique(i) = default_markov_model(a);

            if (smodel_names_mapping.unique(i) == "")
                throw myexception()<<"You must specify a substitution model - there is no default substitution model for alphabet '"<<a.name<<"'";

            full_smodels[i] = compile_smodel(R, TC, code_gen_state, smodel_names_mapping.unique(i), "substitution model " + std::to_string(i+1));
        }

    // 9. Check that alignment alphabet fits requirements from smodel.
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
        int first_index = smodel_names_mapping.partitions_for_item[i][0];
        const alphabet& a = A[first_index].get_alphabet();

        auto type = full_smodels[i].type;
//      FIXME: Actually we need to look at constraints for Nucleotides<a>, Doublets<a,b>, Triplets<a,b>
//      auto& constraints = full_smodels[i].constraints;
        auto alphabet_type = type.begin()->second;

        if (alphabet_type == "Codons" and not dynamic_cast<const Codons*>(&a))
            throw myexception()<<"Substitution model S"<<i+1<<" requires a codon alphabet, but sequences are '"<<a.name<<"'";;

        if (alphabet_type == "Triplets" and not dynamic_cast<const Triplets*>(&a))
            throw myexception()<<"Substitution model S"<<i+1<<" requires a triplet alphabet, but sequences are '"<<a.name<<"'";;

        if (alphabet_type == "Doublets" and not dynamic_cast<const Doublets*>(&a))
            throw myexception()<<"Substitution model S"<<i+1<<" requires a doublet alphabet, but sequences are '"<<a.name<<"'";;

        if (alphabet_type == "RNAEdits" and not dynamic_cast<const RNAEdits*>(&a))
            throw myexception()<<"Substitution model S"<<i+1<<" requires a RNAEdits alphabet, but sequences are '"<<a.name<<"'";;

        if (alphabet_type == "AA" and not dynamic_cast<const AminoAcids*>(&a))
            throw myexception()<<"Substitution model S"<<i+1<<" requires an amino-acid alphabet, but sequences are '"<<a.name<<"'";;
    }

    // 10. --- Default and compile indel models
    auto imodel_names_mapping = get_mapping(args, "imodel", n_partitions);
    auto& imodel_mapping = imodel_names_mapping.item_for_partition;

    get_default_imodels(imodel_names_mapping, A);

    auto full_imodels = compile_imodels(R, TC, code_gen_state, imodel_names_mapping);

    // Check that there are no substitution conditions on variable partitions.
    for(int i=0;i<imodel_mapping.size();i++)
    {
        int smodel_index = *smodel_mapping[i];
        auto s_condition = smodel_conditions[smodel_index];

        if (imodel_mapping[i] and not s_condition.empty())
        {
            throw myexception()<<"Partition "<<i+1<<" has both a variable alignment and an ascertainment condition '"<<s_condition<<"'.";
        }
    }

    // 11. --- Default and compile scale models
    shared_items<string> scale_names_mapping = get_mapping(args, "scale", A.size());

    auto scale_mapping = scale_names_mapping.item_for_partition;

    vector<model_t> full_scale_models(scale_names_mapping.n_unique_items());

    for(int i=0;i<scale_names_mapping.n_unique_items();i++)
    {
        auto scale_model = scale_names_mapping.unique(i);
        if (scale_model.empty())
        {
            if (fixed.count("tree"))
                scale_model = "1";
            else
                scale_model = "~gamma(0.5, 2)";
        }
        full_scale_models[i] = compile_model(R, TC, code_gen_state, parse_type("Double"), scale_model, "scale model " + std::to_string(i+1));
    }

    // 12. Default and compile tree model
    model_t tree_model;
    ptree tree_type = parse_type("Tree<t>");
    if (not fixed.count("tree"))
    {
        string M;
        if (args.count("tree"))
            M = args["tree"].as<string>();
        else if (fixed.count("topology"))
            M = "~fixed_topology_tree(topology)";
        else
            M = "~uniform_tree(taxa)";

        tree_model = compile_model(R, TC, code_gen_state, tree_type, M, "tree model", {});
        tree_type = tree_model.type;
    }

    // 13. Default and compile subst rates
    model_t subst_rates_model;
    if (args.count("subst-rates") and args.at("subst-rates").as<string>() != "constant")
    {
        string M = args.at("subst-rates").as<string>();
        if (M == "relaxed")
        {
            // FIXME -- allow automatically modifying downstream vars when changing sigma
            // M = "~iidMap(branches(tree), logNormal(0,sigma)) where {sigma~logLaplace(-3,1)}";
            M = "map(|x:pow(x,sigma)|,~iidMap(branches(tree), logNormal(0,1))) where {sigma~logLaplace(-3,1)}";
        }

        auto TC2 = TC;
        subst_rates_model = compile_model(R, TC, code_gen_state, parse_type("IntMap<Double>"), M, "substitution rates", {{"tree", tree_type}});
    }

    // 14. Default and compile indel rates
    model_t indel_rates_model;
    if (args.count("indel-rates") and args.at("indel-rates").as<string>() != "constant")
    {
        string M = args.at("indel-rates").as<string>();
        if (M == "relaxed")
        {
            // FIXME -- allow automatically modifying downstream vars when changing sigma
            // M = "~iidMap(branches(tree), logNormal(0,sigma)) where {sigma~logLaplace(-3,1)}";
            M = "map(|x:pow(x,sigma)|,~iidMap(branches(tree), logNormal(0,1))) where {sigma~logLaplace(-3,1)}";
        }

        indel_rates_model = compile_model(R, TC, code_gen_state, parse_type("IntMap<Double>"), M, "indel rates", {{"tree", tree_type}});
    }

    //-------------- Likelihood calculator types -----------//
    vector<int> likelihood_calculators(A.size(), 0);
    for(int i=0;i<imodel_mapping.size();i++)
        if (not imodel_mapping[i])
            likelihood_calculators[i] = 1;

    if (args.count("likelihood-calculators"))
    {
        likelihood_calculators = convertTo<int>(split(args["likelihood-calculators"].as<string>(), ","));
        if (likelihood_calculators.size() == 1)
            likelihood_calculators = vector<int>(A.size(), likelihood_calculators[0]);
        if (likelihood_calculators.size() != A.size())
            throw myexception()<<"We have "<<A.size()<<" partitions, but only got "<<likelihood_calculators.size()<<" likelihood calculator types.";
        for(int i=0; i<A.size();i++)
        {
            int c = likelihood_calculators[i];
            if (c != 0 and c != 1)
                throw myexception()<<"Calculator "<<c<<" not recognized!";
            if (c != 0 and imodel_mapping[i])
                throw myexception()<<"Calculator "<<c<<" does not work with a variable alignment!";
        }
    }

    bool unalign = args.count("unalign");
    for(int i=0;i<A.size();i++)
        if (unalign and imodel_mapping[i])
            if (likelihood_calculators[i] != 0)
                throw myexception()<<"Can't unalign with calculator "<<likelihood_calculators[i]<<"!";

    //--------------- Create the Parameters object---------------//
    if (args.count("set"))
        load_settings(args["set"].as<vector<string> >());

    // check that smodel mapping has correct size.
    if (smodel_mapping.size() != filename_ranges.size())
        throw myexception()<<"There are "<<filename_ranges.size()
                           <<" data partitions, but you mapped smodels onto "
                           <<smodel_mapping.size();

    int n_smodels = get_num_models(smodel_mapping);

    // check that we only map existing smodels to data partitions
    for(int i=0;i<smodel_mapping.size();i++) {
        int m = *smodel_mapping[i];
        if (m >= n_smodels)
            throw myexception()<<"You can't use smodel "<<m+1<<" for data partition "<<i+1
                               <<" because there are only "<<n_smodels<<" smodels.";
    }

    //-------------------- Log model -------------------------//
    auto info = log_summary(full_imodels, full_smodels, full_scale_models,
                            tree_model, subst_rates_model, indel_rates_model,
                            smodel_mapping, imodel_mapping, scale_mapping,
                            A[0].n_sequences(), filename_ranges.size(),
                            alphabet_names,
                            smodel_conditions,
                            args);

    //------------------- Handle heating ---------------------//
    // setup_heating(proc_id, args, M);

    //------------------- create the program -----------------//
    fs::path program_filename = dir / "BAliPhy.Main.hs";
    vector<expression_ref> alphabet_exps;
    for(int i=0;i<n_partitions;i++)
        alphabet_exps.push_back(get_alphabet_expression(A[i].get_alphabet()));

    // Can we pass the code_gen_state, to avoid stomping on generated haskell var names?
    auto prog = gen_atmodel_program(args,
                                    L, 
                                    program_filename,
                                    alphabet_exps, filename_ranges, A[0].n_sequences(),
                                    decls,
                                    full_smodels, smodel_mapping, smodel_conditions,
                                    full_imodels, imodel_mapping,
                                    full_scale_models, scale_mapping,
                                    tree_model,
                                    subst_rates_model, indel_rates_model,
                                    likelihood_calculators);

    return {std::move(prog), info};
}

void write_initial_alignments(variables_map& args, int proc_id, const fs::path& dir)
{
    string base = "C" + convertToString(proc_id+1);

    int i=1;
    for(const auto& [filename, range]: args["align"].as<vector<string> >()
                                       | views::transform([&](auto& x) {
                                           return split_on_last_regex(":","[-\\d.\\/]*",x);
                                       }))
    {
        auto sequences = load_sequences_with_range(filename, range);

        auto target = fs::path(base+".P"+convertToString(i)+".initial.fasta");
        checked_ofstream out(dir/target, false);
        sequence_format::write_fasta(out, sequences);

        i++;
    }
}

/// Construct a multifurcating tree representing topology constraints from file \a filename.
///
/// \param filename The name of the file to load the tree from.
/// \param names The order of the leaf labels.
/// \return a multifurcating tree.
///
SequenceTree load_constraint_tree(const string& filename,const vector<string>& names)
{
    RootedSequenceTree RT;
    RT.read(filename);

    SequenceTree constraint = RT;
      
    remove_sub_branches(constraint);
  
    try{
        remap_T_leaf_indices(constraint,names);
    }
    catch(const bad_mapping<string>& b) {
        bad_mapping<string> b2 = b;
        b2.clear();
        if (b.from == 0)
            b2<<"Constraint tree leaf sequence '"<<b2.missing<<"' not found in the alignment.";
        else
            b2<<"Alignment sequence '"<<b2.missing<<"' not found in the constraint tree.";
        throw b2;
    }
    return constraint;
}

