#include "A-T-model.H"
#include <algorithm>                                // for max, find, min
#include <boost/dynamic_bitset.hpp>                 // for dynamic_bitset
#include <boost/filesystem/path.hpp>                // for path, operator/
#include <boost/program_options.hpp>                // for program_options
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
#include "link-partitions.H"                        // for shared_items, get...
#include "models/TreeInterface.H"                   // for TreeInterface
#include "models/parameters.H"                      // for Parameters, param...
#include "models/rules.H"                           // for Rules
#include "models/setup.H"                           // for model_t, get_model
#include "sequence/alphabet.H"                      // for alphabet, Doublets
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
#include "util/string/convert.H"                    // for convertTo, conver...
#include "util/string/join.H"                       // for join
#include "util/string/split.H"                      // for split, split_on_last
#include "util/text.H"                              // for bold, bold_blue
class module_loader;


extern int log_verbose;

namespace fs = boost::filesystem;

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
        for(int i=0;i<deltas.size();i++) {
            vector<double> D = convertTo<double>(split(deltas[i],'*'));
            if (D.size() != 2)
                throw myexception()<<"Couldn't parse beta increment '"<<deltas[i]<<"'";
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

vector<model_t>
get_imodels(const Rules& R, const shared_items<string>& imodel_names_mapping, const SequenceTree&)
{
    map<string,pair<string,string>> imodel_states = {{"topology",{"topology","Topology"}}};

    vector<model_t> imodels;
    for(int i=0;i<imodel_names_mapping.n_unique_items();i++) 
        imodels.push_back( get_model(R, "IndelModel", imodel_names_mapping.unique(i), {}, imodel_states) );
    return imodels;
}

template <typename T>
json optional_to_json(const std::optional<T>& o)
{
    if (not o)
        return nullptr;
    else
        return *o;
}


vector<pair<string,string>> split_on_last(char sep, const vector<string>& v1)
{
    vector<pair<string,string>> v2;
    for(auto& s: v1)
        v2.push_back(split_on_last(sep, s));
    return v2;
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

string show_model(const model_t& m)
{
    string s = indent_and_wrap(0,12,1000,m.show_main(false));
    return s.substr(0,2) + bold(s.substr(2));
}

json log_summary(ostream& out_cache, ostream& out_screen,ostream& out_both,
                 const vector<model_t>& IModels,
                 const vector<model_t>& SModels,
                 const vector<model_t>& ScaleModels,
                 const model_t& branch_length_model,
                 std::vector<std::optional<int>>& smodel_index_for_partition,
                 std::vector<std::optional<int>>& imodel_index_for_partition,
                 std::vector<std::optional<int>>& scale_index_for_partition,
                 const Parameters& P,const variables_map& args)
{
    json info;
    json partitions;

    json tree;
    if (P.t().n_branches() > 1)
    {
        out_both<<"T:topology ~ uniform on tree topologies\n";
        tree["topology"] = "uniform";
    }

    if (P.t().n_branches() > 0)
    {
        out_both<<"T:lengths "<<branch_length_model.show()<<endl<<endl;
        tree["lengths"] = branch_length_model.show(false);
    }

    //-------- Log some stuff -----------//
    auto filename_ranges = args["align"].as<vector<string> >();
    auto alignment_files = split_on_last(':',filename_ranges);

    for(int i=0;i<P.n_data_partitions();i++)
    {
        json partition;

        out_screen<<"Partition "<<magenta(tag("P",i))<<":\n";
        // 1. filename 
        out_cache<<"data"<<i+1<<" = "<<filename_ranges[i]<<endl;
        auto [filename,range] = alignment_files[i];
        string file = bold(filename);
        if (range.size())
            file += ":"+range;
        out_screen<<"    file = "<<bold(file)<<endl;
        partition["filename"] = filename;
        partition["range"] = range;

        // 2. alphabet
        string a_name = P[i].get_alphabet()->name;
        out_screen<<"    alphabet = "<<bold(a_name)<<"\n";
        out_cache<<"alphabet"<<i+1<<" = "<<a_name<<endl;
        partition["alphabet"] = a_name;

        // 3. substitution model
        auto s_index = smodel_index_for_partition[i];
        out_screen<<"    subst "<<show_model(SModels[*s_index])<<" ("<<bold_blue(tag("S",*s_index))<<")\n";
        out_cache<<"smodel-index"<<i+1<<" = "<<smodel_index_for_partition[i]<<endl;
        partition["smodel"] = optional_to_json( smodel_index_for_partition[i] );

        // 4. indel model
        if (auto i_index = imodel_index_for_partition[i])
            out_screen<<"    indel "<<show_model(IModels[*i_index])<<" ("<<bold_red(tag("I",*i_index))<<")\n";
        else
            out_screen<<"    indel = "<<bold("none")<<"\n";
        out_cache<<"imodel-index"<<i+1<<" = "<<imodel_index_for_partition[i]<<endl;
        partition["imodel"] = optional_to_json( imodel_index_for_partition[i] );

        // 5. scale model
        auto scale_index = scale_index_for_partition[i];
        out_screen<<"    scale "<<show_model(ScaleModels[*scale_index])<<" ("<<green(tag("Scale",*scale_index))<<")\n";
        out_cache<<"scale-index"<<i+1<<" = "<<scale_index_for_partition[i]<<endl;
        partition["scale"] = optional_to_json( scale_index_for_partition[i] );

        out_screen<<endl;
        out_cache<<endl;
        partitions.push_back(partition);
    }

    json smodels = json::array();
    for(int i=0;i<SModels.size();i++)
    {
        //    out_cache<<"subst model"<<i+1<<" = "<<P.SModel(i).name()<<endl<<endl;
        out_cache<<"subst model"<<i+1<<" "<<SModels[i].show()<<endl<<endl;
        smodels.push_back(SModels[i].pretty_model());
        string e = SModels[i].show_extracted();
        if (e.size())
            out_screen<<"Substitution model "<<bold_blue(tag("S",i))<<" priors:"<<e<<"\n\n";
    }

    json imodels = json::array();
    for(int i=0;i<IModels.size();i++)
    {
        out_cache<<"indel model"<<i+1<<" "<<IModels[i].show()<<endl<<endl;
        imodels.push_back(IModels[i].pretty_model());
        string e = IModels[i].show_extracted();
        if (e.size())
            out_screen<<"Insertion/deletion model "<<bold_red(tag("I",i))<<" priors:"<<e<<"\n\n";
    }

    json scales = json::array();
    for(int i=0;i<P.n_branch_scales();i++)
    {
        out_cache<<"scale model"<<i+1<<" "<<ScaleModels[i].show()<<endl<<endl;
        scales.push_back(ScaleModels[i].pretty_model());
        string e = ScaleModels[i].show_extracted();
        if (e.size())
            out_screen<<"Scale model "<<green(tag("Scale",i))<<" priors:"<<e<<"\n\n";
    }

    info["partitions"] = partitions;
    info["smodels"] = smodels;
    info["imodels"] = imodels;
    info["scales"] = scales;
    info["tree"] = tree;
    return info;
}

void check_alignment_names(const alignment& A)
{
    const string forbidden = "();:\"'[]&,";

    for(int i=0;i<A.n_sequences();i++) {
        const string& name = A.seq(i).name;
        for(int j=0;j<name.size();j++)
            for(int c=0;c<forbidden.size();c++)
                for(int pos=0;pos<name.size();pos++)
                    if (name[pos] == forbidden[c])
                        throw myexception()<<"Sequence name '"<<name<<"' contains illegal character '"<<forbidden[c]<<"'";
    }
}

void check_alignment_values(const alignment& A,const pair<string,string>& filename_range)
{
    const alphabet& a = A.get_alphabet();

    for(int i=0;i<A.n_sequences();i++)
    {
        string name = A.seq(i).name;

        for(int j=0;j<A.length();j++) 
            if (A.unknown(j,i))
            {
                auto [filename, range] = filename_range;
                if (range.size())
                    filename = filename + ":" + range;
                throw myexception()<<"Alignment file '"<<filename<<"' has a '"<<a.unknown_letter<<"' in sequence '"<<name<<"'.\n (Please replace with gap character '"<<a.gap_letter<<"' or wildcard '"<<a.wildcard<<"'.)";
            }
    }
}

/// If the tree has any foreground branch attributes, then set the corresponding branch to foreground, here.
void set_foreground_branches(Parameters& P, const SequenceTree& T)
{
    if (auto attribute_index = T.maybe_find_undirected_branch_attribute_index_by_name("foreground"))
    {
        for(int b=0;b<T.n_branches();b++)
        {
            boost::any value = T.branch(b).undirected_attribute(*attribute_index);
            if (value.empty()) continue;

            int foreground_level = convertTo<int>( boost::any_cast<string>( value) );

            P.set_branch_category(b, foreground_level);
            std::cerr<<"Setting branch '"<<b<<"' to foreground level "<<foreground_level<<"\n";;
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
        vector<const_branchview> b2 = branches_after_inclusive(TC,found); 
        for(int j=0;j<b2.size();j++) {
            if (b2[j].target().degree() > 3)
                throw myexception()<<"Alignment constraint: clade '"
                                   <<join(name_groups[i],' ')
                                   <<"' has a polytomy in the topology constraint tree.";
            branches.push_back(b2[j].undirected_name());
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


model_t get_smodel(const Rules& R, const std::string& model)
{
    map<string,pair<string,string>> smodel_states = {{"alphabet",{"alpha","a"}},
                                                     {"branch_categories",{"branch_categories","List[Int]"}}};

    try {
        return get_model(R, "RevCTMC[a]", model, {}, smodel_states);
    }
    catch (myexception& e) {};

    try {
        return get_model(R, "MixtureModel[a]", model, {}, smodel_states);
    }
    catch (myexception& e) {};

    return get_model(R, "MultiMixtureModel[a]", model, {}, smodel_states);
}


owned_ptr<Model> create_A_and_T_model(const Rules& R, variables_map& args, const std::shared_ptr<module_loader>& L,
                                      ostream& out_cache, ostream& out_screen, ostream& out_both, json& info,
                                      int proc_id, const string& dir)
{
    //------ Determine number of partitions ------//
    auto filename_ranges = split_on_last(':', args["align"].as<vector<string> >() );

    const int n_partitions = filename_ranges.size();

    //------------- Get smodel names -------------------
    auto smodel_names_mapping = get_mapping(args, "smodel", n_partitions);
    auto& smodel_mapping = smodel_names_mapping.item_for_partition;

    vector<model_t> full_smodels(smodel_names_mapping.n_unique_items());

    // 1. Get smodels for all SPECIFIED smodel names.
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
        if (not smodel_names_mapping.unique(i).empty())
            full_smodels[i] = get_smodel(R, smodel_names_mapping.unique(i));

    //------------- Get alphabet names -------------------
    shared_items<string> alphabet_names_mapping = get_mapping(args, "alphabet", filename_ranges.size());
    vector<string> alphabet_names;
    for(int i=0;i<alphabet_names_mapping.n_partitions();i++)
        alphabet_names.push_back(alphabet_names_mapping[i]);

    // 2. For all SPECIFIED smodels, try to determine the alphabet.
    //    (Alphabets for SPECIFIED smodel groups must all specified or all unspecified).
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
        if (smodel_names_mapping.unique(i).empty()) continue;

        auto type = full_smodels[i].type;
        auto& constraints = full_smodels[i].constraints;
        auto alphabet_type = type.begin()->second;

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
            for(int j: a_specified)
                if (alphabet_names[j] != alphabet_names[a_specified[0]])
                    throw myexception()<<"Partitions "<<a_specified[0]+1<<" and "<<j+1<<" have different alphabets, but are given the same substitution model!";
            string a = alphabet_names[a_specified[0]];
            if (alphabet_type.get_value<string>() == "Codons")
            {
                if (a != "Codons")
                    throw myexception()<<"Partition "<<a_specified[0]+1<<" has specified alphabet '"<<a<<"' but the substitution model requires a codon alphabet!";
            }
            else if (alphabet_type.get_value<string>() == "Triplets")
            {
                if (a != "Triplets")
                    throw myexception()<<"Partition "<<a_specified[0]+1<<" has specified alphabet '"<<a<<"' but the substitution model requires a triplet alphabet!";
            }
            else if (alphabet_type.get_value<string>() == "Doublets")
            {
                if (a != "Doublets")
                    throw myexception()<<"Partition "<<a_specified[0]+1<<" has specified alphabet '"<<a<<"' but the substitution model requires a doublet alphabet!";
            }
            else if (alphabet_type.get_value<string>() == "AA")
            {
                if (a != "AA" and a != "Amino-Acids" and a != "Amino-AcidsWithStop")
                    throw myexception()<<"Partition "<<a_specified[0]+1<<" has specified alphabet '"<<a<<"' but the substitution model requires an amino-acid alphabet!";
            }
        }
        else if (alphabet_type.get_value<string>() == "Codons")
        {
            for(int j: smodel_names_mapping.partitions_for_item[i])
                alphabet_names[j] = "Codons";
        }
        else if (alphabet_type.get_value<string>() == "Triplets")
        {
            for(int j: smodel_names_mapping.partitions_for_item[i])
                alphabet_names[j] = "Triplets";
        }
        else if (alphabet_type.get_value<string>() == "Doublets")
        {
            for(int j: smodel_names_mapping.partitions_for_item[i])
                alphabet_names[j] = "Doublets";
        }
        else
        {
            bool triplet_constraint = false;
            for(auto& constraint: constraints)
                if (constraint.get_value<string>() == "Triplets" and constraint.begin()->second.get_value<string>() == "a")
                    triplet_constraint = true;

            if (triplet_constraint)
                for(int j: smodel_names_mapping.partitions_for_item[i])
                    alphabet_names[j] = "Triplets";

            bool doublet_constraint = false;
            for(auto& constraint: constraints)
                if (constraint.get_value<string>() == "Doublets" and constraint.begin()->second.get_value<string>() == "a")
                    doublet_constraint = true;

            if (doublet_constraint)
                for(int j: smodel_names_mapping.partitions_for_item[i])
                    alphabet_names[j] = "Doublets";
        }
//      Use the auto-detected alphabet right now -- it leads to better error messages.
//      else if (alphabet_type.get_value<string>() == "AA")
//      {
//          for(int j: smodel_names_mapping.partitions_for_item[i])
//              alphabet_names[j] = "AA";
//      }
    }

    //----------- Load alignments  ---------//
    vector<alignment> A(filename_ranges.size());

    // 3. -- load alignments for SPECIFIED and UNSPECIFIED alphabets
    {
        map<string,vector<sequence>> sequences_for_filename;
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
                    e.prepend("In file '"+filename+"': ");
                else
                    e.prepend("In file '"+filename+"' columns "+range+": ");
                throw;
            }

            if (alphabet_names[i].empty())
                alphabet_names[i] = A[i].get_alphabet().name;
        }
    }

    for(int i=0;i<A.size();i++) {
        check_alignment_names(A[i]);
        check_alignment_values(A[i],filename_ranges[i]);
    }

    //----------- Load tree and link to alignments ---------//
    SequenceTree T;

    if (args.count("tree"))
    {
        T = load_T(args);
    }
    else
    {
        SequenceTree TC = star_tree(sequence_names(A[0]));
        if (args.count("t-constraint"))
            TC = load_constraint_tree(args["t-constraint"].as<string>(),sequence_names(A[0]));

        T = TC;
        RandomTree(T);
    }

    clean_T(T);

    //--------- Handle branch lengths <= 0 and very short branch lengths --------//
    sanitize_branch_lengths(T);

    //--------- Set up indel model --------//
    auto imodel_names_mapping = get_mapping(args, "imodel", n_partitions);
    auto& imodel_mapping = imodel_names_mapping.item_for_partition;

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
                break;
            }
            else
                value = "rs07";
        }
    }
    auto full_imodels = get_imodels(R, imodel_names_mapping, T);

    // 5. ----- Check that all smodel-linked partitions end up with the same alphabet. -----
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
        int first_index = smodel_names_mapping.partitions_for_item[i][0];
        for(int j: smodel_names_mapping.partitions_for_item[i])
            if (alphabet_names[j] != alphabet_names[first_index])
                throw myexception()<<"Partitions "<<first_index+1<<" and "<<j+1<<" have different alphabets '"<<alphabet_names[first_index]<<"' and '"<<alphabet_names[j]<<"', but share a linked substitution model!";
    }

    // 6. ------ Check that all alphabet-linked partitions end up with the same alphabet. -----
    for(int i=0;i<alphabet_names_mapping.n_unique_items();i++)
    {
        int first_index = alphabet_names_mapping.partitions_for_item[i][0];
        for(int other_index: alphabet_names_mapping.partitions_for_item[i])
        {
            if (alphabet_names[first_index] != alphabet_names[other_index])
                throw myexception()<<"Partitions "<<first_index+1<<" and "<<other_index+1<<" have different alphabets, but are specified as being linked!";
        }
    }
    // 7. --------- Get UNSPECIFIED substitution models, which depend on the alphabet --------//
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
        if (smodel_names_mapping.unique(i).empty())
        {
            // Check that all the alphabets are the same before using the alphabet to get a default model.
            int first_index = smodel_names_mapping.partitions_for_item[i][0];
            const alphabet& a = A[first_index].get_alphabet();

            smodel_names_mapping.unique(i) = default_markov_model(a);

            if (smodel_names_mapping.unique(i) == "")
                throw myexception()<<"You must specify a substitution model - there is no default substitution model for alphabet '"<<a.name<<"'";

            full_smodels[i] = get_smodel(R, smodel_names_mapping.unique(i));
        }
    
    // 8. Check that alignment alphabet fits requirements from smodel.
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
        int first_index = smodel_names_mapping.partitions_for_item[i][0];
        const alphabet& a = A[first_index].get_alphabet();

        auto type = full_smodels[i].type;
        auto alphabet_type = type.begin()->second;

        if (alphabet_type.get_value<string>() == "Codons" and not dynamic_cast<const Codons*>(&a))
            throw myexception()<<"Substitution model S"<<i+1<<" requires a codon alphabet, but sequences are '"<<a.name<<"'";;

        if (alphabet_type.get_value<string>() == "Triplets" and not dynamic_cast<const Triplets*>(&a))
            throw myexception()<<"Substitution model S"<<i+1<<" requires a triplet alphabet, but sequences are '"<<a.name<<"'";;

        if (alphabet_type.get_value<string>() == "Doublets" and not dynamic_cast<const Doublets*>(&a))
            throw myexception()<<"Substitution model S"<<i+1<<" requires a doublet alphabet, but sequences are '"<<a.name<<"'";;

        if (alphabet_type.get_value<string>() == "AA" and not dynamic_cast<const AminoAcids*>(&a))
            throw myexception()<<"Substitution model S"<<i+1<<" requires an amino-acid alphabet, but sequences are '"<<a.name<<"'";;
    }

    //-------------- Which partitions share a scale? -----------//
    shared_items<string> scale_names_mapping = get_mapping(args, "scale", A.size());

    auto scale_mapping = scale_names_mapping.item_for_partition;

    vector<model_t> full_scale_models(scale_names_mapping.n_unique_items());

    for(int i=0;i<scale_names_mapping.n_unique_items();i++)
    {
        auto scale_model = scale_names_mapping.unique(i);
        if (scale_model.empty())
            scale_model = "~gamma[0.5,2]";
        full_scale_models[i] = get_model(R, "Double", scale_model);
    }

    //-------------- Branch length model --------------------//
    model_t branch_length_model;
    {
        string M;
        if (args.count("branch-lengths"))
            M = args["branch-lengths"].as<string>();
        else
            M = "~iid[num_branches[tree],gamma[0.5,div[2,num_branches[tree]]]]";

        branch_length_model = get_model(R, "List[Double]", M, {{"tree","Tree"}});
        branch_length_model.code.E = lambda_quantify(var("tree"), branch_length_model.code.E);
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
    Model::key_map_t keys;
    if (args.count("set"))
        keys = parse_key_map(args["set"].as<vector<string> >());

    fs::path program_filename = fs::path(dir) / "BAliPhy.Main.hs";
    vector<expression_ref> alphabet_exps;
    for(int i=0;i<n_partitions;i++)
        alphabet_exps.push_back(get_alphabet_expression(A[i].get_alphabet()));

    auto prog = gen_atmodel_program(L, keys, program_filename,
                                    alphabet_exps, filename_ranges, T.n_leaves(),
                                    full_smodels, smodel_mapping,
                                    full_imodels, imodel_mapping,
                                    full_scale_models, scale_mapping,
                                    branch_length_model,
                                    likelihood_calculators);

    Parameters P(prog, keys, A, filename_ranges, T, smodel_mapping, imodel_mapping, scale_mapping, likelihood_calculators);

    P.evaluate_program();
    //-------- Set the alignments for variable partitions ---------//
    for(int i=0;i<P.n_data_partitions();i++)
        if (unalign and P.get_data_partition(i).has_IModel())
            P.get_data_partition(i).set_alignment(unalign_A(A[i]));
    P.evaluate_program();

    // If the tree has any foreground branch attributes, then set the corresponding branch to foreground, here.
    set_foreground_branches(P, T);

    //------------- Write out a tree with branch numbers as branch lengths------------- //
    write_branch_numbers(out_cache, T);

    //-------------------- Log model -------------------------//
    info.update( log_summary(out_cache, out_screen, out_both,
                             full_imodels, full_smodels, full_scale_models, branch_length_model,
                             smodel_mapping, imodel_mapping, scale_mapping,
                             P,args) );

    //------------------- Handle heating ---------------------//
    setup_heating(proc_id,args,P);

    return P;
}

void write_initial_alignments(variables_map& args, int proc_id, const string& dir_name)
{
    auto filename_ranges = split_on_last(':', args["align"].as<vector<string> >() );

    fs::path dir(dir_name);

    string base = "C" + convertToString(proc_id+1);

    int i=1;
    for(auto& [filename, range]: filename_ranges)
    {
        auto sequences = load_sequences_with_range(filename, range);

        auto target = fs::path(base+".P"+convertToString(i)+".initial.fasta");
        target = dir/target;
        checked_ofstream out(target.string(), false);
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

