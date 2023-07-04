#include "A-T-prog.H"

#include <set>
#include <map>

#include "util/set.H"
#include "util/io.H"
#include "models/setup.H"   // for model_t

#include "computation/loader.H"
#include "computation/expression/list.H"
#include "computation/expression/tuple.H"
#include "computation/expression/var.H"

#include "sequence/genetic_code.H"
#include "sequence/codons.H"
#include "sequence/doublets.H"

using std::vector;
using std::string;
using std::pair;
using std::set;
using std::cerr;
using std::endl;
using std::ostream;
using std::map;
using std::tuple;
using std::optional;

using std::optional;
namespace fs = std::filesystem;

expression_ref get_genetic_code_expression(const Genetic_Code& code)
{
    if (code.name() == "standard")
        return var("standard_code");
    else
        throw myexception()<<"Need to add a Haskell function for genetic code '"<<code.name()<<"'";
}

expression_ref get_alphabet_expression(const alphabet& a)
{
    if (a.name == "DNA")
        return  var("dna");
    else if (a.name == "RNA")
        return var("rna");
    else if (a.name == "Amino-Acids")
        return var("aa");
    else if (auto codons = dynamic_cast<const Codons*>(&a))
    {
        auto nucs = get_alphabet_expression(codons->getNucleotides());
        auto code = get_genetic_code_expression(codons->getGenetic_Code());
        return {var("codons"), nucs, code};
    }
    else if (auto triplets = dynamic_cast<const Triplets*>(&a))
    {
        auto nucs = get_alphabet_expression(triplets->getNucleotides());
        return {var("triplets"),nucs};
    }
    else if (auto doublets = dynamic_cast<const Doublets*>(&a))
    {
        auto nucs = get_alphabet_expression(doublets->getNucleotides());
        return {var("doublets"),nucs};
    }
    else
    {
        throw myexception()<<"Can't translate alphabet "<<a.name;
    }
}

string maybe_emit_code(map<string,string>& code_to_name, const string& name, const expression_ref& E)
{
    auto code = print_equals_function(E);
    if (code_to_name.count(code))
        code = code_to_name.at(code);
    else
        code_to_name.insert({code," = " + name});
    return name + code + "\n";
}


var bind_and_log(bool do_log, const var& x, const var& log_x, const string& name, const expression_ref& E, bool is_action, bool has_loggers, do_block& block, vector<expression_ref>& loggers, bool is_referenced=true)
{
    perform_action_simplified(block.get_stmts(), x, log_x, is_referenced, E, is_action, has_loggers);
    maybe_log(loggers, name, do_log?x:expression_ref{}, has_loggers?log_x:expression_ref{});
    return x;
}

var bind_and_log(bool do_log, const string& name, const expression_ref& E, bool is_action, bool has_loggers, do_block& block, vector<expression_ref>& loggers, bool is_referenced=true)
{
    string var_name = name;
    if (var_name.empty() or not std::islower(var_name[0]))
        var_name = "_"+var_name;
    var x(var_name);
    var log_x("log_"+name);
    return bind_and_log(do_log, x, log_x, name, E, is_action, has_loggers, block, loggers, is_referenced);
}


// Given a collection of different functions, check if the code is the same to avoid printing the same code twice.
// Only subscript the functions if there is more than one unique code fragment.
vector<string> print_models(const string& tag, const vector<model_t>& models, std::ostringstream& file)
{
    map<string,int> functions;
    vector<string> function_for_index;
    for(int i=0;i<models.size();i++)
    {
        auto code = print_equals_function(models[i].code.generate());
        if (not functions.count(code))
            functions.insert({code,functions.size()});
    }
    int printed = 0;
    for(int i=0;i<models.size();i++)
    {
        auto code = print_equals_function(models[i].code.generate());
        int index = functions.at(code);
        string name = tag;
        if (functions.size() > 1) name += "_"+std::to_string(index+1);
        function_for_index.push_back(name);
        if (index >= printed)
        {
            file<<name<<" "<<code<<"\n\n";
            printed++;
        }
    }
    return function_for_index;
}

std::string generate_atmodel_program(const set<string>& fixed,
                                     int n_sequences,
                                     const vector<expression_ref>& alphabet_exps,
                                     const vector<pair<string,string>>& filename_ranges,
                                     const optional<fs::path>& tree_filename,
                                     const vector<model_t>& SMs,
                                     const vector<optional<int>>& s_mapping,
                                     const vector<model_t>& IMs,
                                     const vector<optional<int>>& i_mapping,
                                     const vector<model_t>& scaleMs,
                                     const vector<optional<int>>& scale_mapping,
                                     const model_t& branch_length_model,
                                     const std::vector<int>& like_calcs)
{
    int n_partitions = filename_ranges.size();

    int n_leaves   = n_sequences;
    int n_branches = (n_leaves==1)?0:2*n_leaves - 3;

    set<string> imports;
    imports.insert("Bio.Alignment");                         // for Alignment.load_alignment
    imports.insert("Bio.Alphabet");                          // for Bio.Alphabet.dna, etc.
    imports.insert("BAliPhy.ATModel");                       // for ATModel
    imports.insert("Effect");                                // for getProperties
    imports.insert("MCMC");                                  // for scale_means_only_slice
    imports.insert("Probability.Distribution.OnTree");       // for ctmc_on_tree{,fixed_A}
    imports.insert("Tree.Newick");                           // for write_newick
    for(auto& m: SMs)
        add(imports, m.imports);
    for(auto& m: IMs)
        add(imports, m.imports);
    for(auto& m: scaleMs)
        add(imports, m.imports);
    add(imports, branch_length_model.imports);

    std::ostringstream program_file;
    program_file<<"-- Use the program `ormolu` (or `brittany` or `hindent`) to indent this file for readability\n";
    program_file<<"module Main where";
    for(auto& mod: imports)
        program_file<<"\nimport "<<mod;
    program_file<<"\nimport qualified Data.IntMap as IntMap";

    // F1. Substitution models
    map<string,string> code_to_name;

    program_file<<"\n\n";

    auto SM_function_for_index = print_models("sample_smodel", SMs, program_file);
    auto IM_function_for_index = print_models("sample_imodel", IMs, program_file);
    vector<string> scaleM_function_for_index;
    if (n_branches > 0)
    {
        scaleM_function_for_index = print_models("sample_scale", scaleMs, program_file);

        // F4. Branch lengths
        if (not fixed.count("tree"))
        {
            program_file<<"sample_branch_lengths"<<print_equals_function(branch_length_model.code.generate())<<"\n";
        }
    }

    // F5. Topology
    if (not fixed.count("topology") and not fixed.count("tree"))
        program_file<<"\nsample_topology taxa = uniform_labelled_topology taxa\n";

    /* --------------------------------------------------------------- */
    do_block program;

    // FIXME: We can't load the alignments to read their names until we know the alphabets!
    // FIXME: Can we load the alignments as SEQUENCES first?
    var taxon_names_var("taxa");

    // ATModel smodels imodels scales branch_lengths
    // Loggers = [(string,(Maybe a,Loggers)]
    vector<expression_ref> program_loggers;
    // Therefore, we are constructing a list with values [(prefix1,(Just value1, loggers1)), (prefix1, (Just value1, loggers2))

    // M1. Taxa

    if (n_partitions > 0)
    {
        expression_ref sequence_data1 = var("sequence_data");
        if (n_partitions > 1)
            sequence_data1 = {var("!!"),sequence_data1,0};
        program.let(taxon_names_var, {var("map"),var("sequence_name"),sequence_data1});
        program.empty_stmt();
    }

    // We could fix the whole tree or just the topology.
    auto tree_var = var("tree'");
    expression_ref branch_lengths = var("IntMap.empty");

    // M2. Topology
    auto topology_var = var("topology");
    if (not fixed.count("topology") and not fixed.count("tree"))
        program.perform(topology_var, {var("RanSamplingRate"),0.0,{var("sample_topology"),taxon_names_var}});

    // M3. Branch lengths
    if (n_branches > 0 and not fixed.count("tree"))
    {
        string var_name = "branch_lengths";
        auto code = branch_length_model.code;
        expression_ref E = {var("sample_"+var_name),topology_var};
        E = {var("RanSamplingRate"),0.0,E};

        branch_lengths = bind_and_log(false, var_name, E, code.is_action(), code.has_loggers(), program, program_loggers);
    }
    // M4. Branch-length tree
    if (not fixed.count("tree"))
        program.let(tree_var, {var("branch_length_tree"),topology_var,branch_lengths});
    else
        program.let(tree_var, {var("tree")});
    branch_lengths = {var("IntMap.elems"),branch_lengths};
    if (not fixed.count("tree"))
        program_loggers.push_back( {var("%=%"), String("tree"), {var("write_newick"), {var("make_rooted"), {var("addInternalLabels"),tree_var}}}} );

    program.perform({var("RanSamplingRate"), 1.0, {var("PerformTKEffect"), {var("add_tree_alignment_moves"), tree_var}}});


    set<string> used_states;
    for(int i=0;i<SMs.size();i++)
        add(used_states, SMs[i].code.used_states);

    // M5. Branch categories
    expression_ref maybe_branch_categories = var("Nothing");
    expression_ref branch_categories;
    if (used_states.count("branch_categories"))
    {
        var branch_categories_var("branch_categories");
        program.let(branch_categories_var, { var("map"), var("modifiable"), {var("replicate"), n_branches, 0} });
        branch_categories = branch_categories_var;
        maybe_branch_categories = {var("Just"),branch_categories_var};
    }

    // M6. Scales
    vector<expression_ref> scales;
    if (n_branches > 0)
    {
        // define tree_length
        var tree_length_var("tlength");
        program.let(tree_length_var, {var("tree_length"),tree_var});
        // log |T|
        program_loggers.push_back( {var("%=%"), String("|T|"), tree_length_var} );

        for(int i=0; i<scaleMs.size(); i++)
        {
            // FIXME: Ideally we would actually join these models together using a Cons operation and prefix.
            //        This would obviate the need to create a Scale1 (etc) prefix here.
            string indexsuffix = (scaleMs.size()>1)?convertToString(i+1):"";
            string index_suffix = (scaleMs.size()>1)?"_"+convertToString(i+1):"";
            string var_name = "scale"+indexsuffix;

            auto code = scaleMs[i].code;
            expression_ref E = var(scaleM_function_for_index[i]);

            // This should still log sub-loggers of the scales, I think.
            auto scale_var = bind_and_log(false, var_name, E, code.is_action(), code.has_loggers(), program, program_loggers);

            scales.push_back(scale_var);

            // log scale[i]
            program_loggers.push_back( {var("%=%"), String(var_name), scale_var} );

            // log scale[i]*|T|
            program_loggers.push_back( {var("%=%"), String(var_name+"*|T|"), {var("*"),scale_var,tree_length_var}} );
        }

        if (not fixed.count("tree"))
        {
            program.perform({var("RanSamplingRate"), 2.0, {var("PerformTKEffect"), {var("add_move"), {var("scale_means_only_slice"), get_list(scales), branch_lengths}}}});
            program.perform({var("RanSamplingRate"), 1.0, {var("PerformTKEffect"), {var("add_move"), {var("scale_means_only_MH"), get_list(scales), branch_lengths}}}});
        }
    }

    // M7. Substitution models
    vector<expression_ref> smodels;
    for(int i=0;i<SMs.size();i++)
    {
        string prefix = "S" + convertToString(i+1);
        string _suffix = (SMs.size()>1)?"_"+convertToString(i+1):"";
        string suffix = (SMs.size()>1)?convertToString(i+1):"";

        optional<int> first_partition;
        for(int j=0;j<s_mapping.size();j++)
            if (s_mapping[j] and *s_mapping[j] == i)
                first_partition = j;

        auto code = SMs[i].code;

        expression_ref smodel = var(SM_function_for_index[i]);
        for(auto& state_name: code.used_states)
        {
            if (state_name == "alphabet")
                smodel = {smodel, alphabet_exps[*first_partition]};
            else if (state_name == "branch_categories")
            {
                assert(branch_categories);
                smodel = {smodel, branch_categories};
            }
            else
                throw myexception()<<"Don't know how to supply variable for state '"<<state_name<<"'";
        }

        auto smodel_var = var("smodel" + suffix);
        auto log_smodel = var("log_"+smodel_var.name);
        bind_and_log(false, smodel_var, log_smodel, prefix, smodel, code.is_action(), code.has_loggers(), program, program_loggers);
        smodels.push_back(smodel_var);
    }


    // M8. Indel models
    vector<expression_ref> imodels;
    for(int i=0;i<IMs.size();i++)
    {
        string prefix = "I" + convertToString(i+1);
        string _suffix = (IMs.size()>1)?"_"+convertToString(i+1):"";
        string suffix = (IMs.size()>1)?convertToString(i+1):"";

        auto code = IMs[i].code;

        expression_ref imodel = var(IM_function_for_index[i]);
        for(auto& state_name: code.used_states)
        {
            if (state_name == "topology")
                imodel = {imodel, tree_var};
        }

        auto imodel_var = var("imodel" + suffix);
        auto log_imodel = var("log_"+imodel_var.name);
        bind_and_log(false, imodel_var, log_imodel, prefix, imodel, code.is_action(), code.has_loggers(), program, program_loggers);
        imodels.push_back(imodel_var);
    }
    program.empty_stmt();

    vector<expression_ref> alignment_lengths;
    vector<expression_ref> total_num_indels;
    vector<expression_ref> total_length_indels;
    vector<expression_ref> total_substs;
    vector<expression_ref> total_prior_A;

    for(int i=0; i < n_partitions; i++)
    {
        string part = std::to_string(i+1);
        string part_suffix = (n_partitions>1) ? part : "";
        int scale_index = *scale_mapping[i];
        int smodel_index = *s_mapping[i];
        auto imodel_index = i_mapping[i];
        expression_ref smodel = smodels[smodel_index];
        expression_ref sequence_data_var = var("sequence_data");
        if (n_partitions > 1)
            sequence_data_var = {var("!!"),sequence_data_var,i};

        // Model.Partition.1. tree_part<i> = scale_branch_lengths scale tree
        var branch_dist_tree("tree" + part_suffix);
        if (n_branches > 0)
        {
            expression_ref scale = scales[scale_index];
            program.let(branch_dist_tree, {var("scale_branch_lengths"), scale, tree_var});
        }
        else
            program.let(branch_dist_tree, tree_var);

        // Model.Partition.2. Sample the alignment
        var alignment_on_tree("alignment" + part_suffix);
        vector<expression_ref> sub_loggers;
        if (imodel_index)
        {
            assert(like_calcs[i] == 0);
            expression_ref imodel = imodels[*imodel_index];

            expression_ref alphabet = {var("getAlphabet"),smodel};
            if (fixed.count("alignment"))
            {
                program.let(alignment_on_tree, {var("alignmentOnTreeFromSequences"), tree_var, sequence_data_var, alphabet});
            }
            else
            {
                var leaf_sequence_lengths("sequence_lengths" + part_suffix);
                program.let(leaf_sequence_lengths, {var("get_sequence_lengths"), alphabet,  sequence_data_var});
                program.perform(alignment_on_tree, {var("sample"),{var("random_alignment"), branch_dist_tree, imodel, leaf_sequence_lengths}});
            }
        }

        // Model.Partition.3. Observe the sequence data from the distribution
        expression_ref distribution;
        if (like_calcs[i] == 0)
            distribution = {var("ctmc_on_tree"), branch_dist_tree, alignment_on_tree, smodel};
        else
            distribution = {var("ctmc_on_tree_fixed_A"), branch_dist_tree, smodel};
        program.perform({var("observe"),sequence_data_var,distribution});

        program.empty_stmt();

        // Model.Partition.4 Logging.
        if (imodel_index)
        {
            var properties("properties"+part_suffix);
            program.let(properties,Hs::TypedExp({noloc,{var("getProperties"), sequence_data_var}},{noloc,Hs::TypeCon("CTMCOnTreeProperties")}));

            if (n_branches > 0)
            {
                var alignment_length("alignment_length"+part_suffix);
                program.let(alignment_length, {var("alignment_on_tree_length"), alignment_on_tree} );
                alignment_lengths.push_back(alignment_length);
                var num_indels("num_indels"+part_suffix);
                program.let(num_indels, {var("totalNumIndels"), alignment_on_tree} );
                total_num_indels.push_back(num_indels);
                var length_indels("total_length_indels"+part_suffix);
                program.let(length_indels, {var("totalLengthIndels"), alignment_on_tree} );
                total_length_indels.push_back(length_indels);

                var substs("substs"+part_suffix);
                program.let(substs, {var("prop_n_muts"), properties});
                total_substs.push_back(substs);

                sub_loggers.push_back({var("%=%"), String("|A|"), alignment_length });
                sub_loggers.push_back({var("%=%"), String("#indels"), num_indels });
                sub_loggers.push_back({var("%=%"), String("|indels|"), length_indels} );
                sub_loggers.push_back({var("%=%"), String("#substs"), substs });
            }

            if (not fixed.count("alignment"))
            {
                var properties_A("properties_A"+part_suffix);
                program.let(properties_A,Hs::TypedExp({noloc,{var("getProperties"), alignment_on_tree}},{noloc,Hs::TypeCon("RandomAlignmentProperties")}));
                var prior_A("prior_A" + part_suffix);
                program.let(prior_A, {var("probability"),properties_A});
                sub_loggers.push_back({var("%=%"), String("prior_A"), {var("ln"),prior_A}});
            }

            sub_loggers.push_back({var("%=%"), String("likelihood"), {var("ln"),{var("prop_likelihood"),properties}}});
        }
        else
        {
            var properties("properties"+part_suffix);
            program.let(properties,Hs::TypedExp({noloc,{var("getProperties"), sequence_data_var}},{noloc,Hs::TypeCon("CTMCOnTreeFixedAProperties")}));

            sub_loggers.push_back({var("%=%"), String("likelihood"), {var("ln"),{var("prop_fa_likelihood"),properties}}});

            if (n_branches > 0)
            {
                var substs("substs"+part_suffix);
                program.let(substs, {var("prop_fa_n_muts"), properties});
                sub_loggers.push_back({var("%=%"), String("#substs"), substs });
                total_substs.push_back(substs);
            }
        }

        var part_loggers("p"+part+"_loggers");
        program.let(part_loggers,get_list(sub_loggers));
        program_loggers.push_back( {var("%>%"), String("P"+part), part_loggers} );
        program.empty_stmt();
    }
    if (not alignment_lengths.empty())
        program_loggers.push_back( {var("%=%"), String("|A|"), {var("sum"),get_list(alignment_lengths) }} );
    if (not total_num_indels.empty())
        program_loggers.push_back( {var("%=%"), String("#indels"), {var("sum"),get_list(total_num_indels) }} );
    if (not total_length_indels.empty())
        program_loggers.push_back( {var("%=%"), String("|indels|"), {var("sum"),get_list(total_length_indels) }} );
    if (not total_substs.empty())
        program_loggers.push_back( {var("%=%"), String("#substs"), {var("sum"),get_list(total_substs) }} );
    if (not total_prior_A.empty())
        program_loggers.push_back( {var("%=%"), String("prior_A"), {var("sum"),get_list(total_prior_A) }} );

    
    var loggers_var("loggers");
    program.let(loggers_var, get_list(program_loggers));
    program.empty_stmt();

    var atmodel_var("atmodel");
    program.let(atmodel_var, {var("ATModel"), tree_var, maybe_branch_categories});
    program.empty_stmt();

    expression_ref sequence_data_list = var("sequence_data");
    if (n_partitions == 1)
        sequence_data_list = List(sequence_data_list);
    program.finish_return(
        Tuple(
            Tuple(
                var("atmodel"),
                sequence_data_list
                ),
            var("loggers")
            )
        );

    auto model = var("model");
    auto sequence_data = var("sequence_data");
    auto topology = var("topology");
    auto tree = var("tree");
    expression_ref model_fn = {model,sequence_data};
    if (fixed.count("tree"))
        model_fn = {model_fn,tree};
    else if (fixed.count("topology"))
        model_fn = {model_fn, topology};
    program_file<<"\n";
    program_file<<model_fn<<" = "<<program.get_expression().print()<<"\n";

    do_block main;

    if (n_partitions == 1)
    {
        auto [filename, range] = filename_ranges[0];
        expression_ref E = {var("load_sequences"),String(filename)};
        if (not range.empty())
            E = {var("<$>"), {var("select_range"),String(range)}, E};
        main.empty_stmt();

        main.perform(sequence_data, E);
    }
    else
    {
        // Main.1: Emit let filenames = ...
        var filenames_var("filenames");
        bool any_ranges = false;
        map<string,int> index_for_filename;
        {
            vector<expression_ref> filenames_;
            for(auto& [filename,range]: filename_ranges)
            {
                if (not index_for_filename.count(filename))
                {
                    index_for_filename.insert({filename,filenames_.size()});
                    filenames_.push_back(String(filename));
                }
                if (not range.empty())
                    any_ranges = true;
            }
            main.let(filenames_var,get_list(filenames_));
        }

        if (index_for_filename.size() == n_partitions and not any_ranges)
            main.perform(sequence_data,{var("mapM"), var("load_sequences"), filenames_var});
        else
        {
            // Main.2: Emit let filenames_to_seqs = ...
            var filename_to_seqs("seqs");
            {
                main.perform(filename_to_seqs,{var("mapM"), var("load_sequences"), filenames_var});
            }
            main.empty_stmt();

            // Main.3. Emit let sequence_data<n> = 
            vector<expression_ref> partition_sequence_data;
            for(int i=0;i<n_partitions;i++)
            {
                string part = std::to_string(i+1);
                var partition_sequence_data_var("sequence_data"+part);
                int index = index_for_filename.at( filename_ranges[i].first );
                expression_ref loaded_sequences = {var("!!"),filename_to_seqs,index};
                if (not filename_ranges[i].second.empty())
                    loaded_sequences = {var("select_range"), String(filename_ranges[i].second), loaded_sequences};
                main.let(partition_sequence_data_var, loaded_sequences);
                partition_sequence_data.push_back(partition_sequence_data_var);
                main.empty_stmt();
            }

            // Main.4. Emit let sequence_data = ...
            main.let(sequence_data, get_list(partition_sequence_data));
            main.empty_stmt();
        }
    }

    if ((fixed.count("tree") or fixed.count("topology")) and not tree_filename)
    {
        throw myexception()<<"The tree is fixed, but no tree file is given. (Use --tree)";
    }

    if (fixed.count("tree"))
    {
        main.perform(tree, {var("readBranchLengthTree"),String(tree_filename->string())});
    }
    else if (fixed.count("topology"))
    {
        main.perform(topology, {var("readTreeTopology"),String(tree_filename->string())});
    }

    // Main.5. Emit mcmc $ model sequence_data
    main.perform({var("$"),var("mcmc"),model_fn});

    program_file<<"\nmain = "<<main.get_expression().print()<<"\n";

    return program_file.str();
}

Program gen_atmodel_program(const std::shared_ptr<module_loader>& L,
                            const Model::key_map_t&,
                            const set<string>& fixed,
                            const fs::path& program_filename,
                            const std::optional<fs::path>& tree_filename,
                            const vector<expression_ref>& alphabet_exps,
                            const vector<pair<string,string>>& filename_ranges,
                            int n_leaves,
                            const vector<model_t>& SMs,
                            const vector<optional<int>>& s_mapping,
                            const vector<model_t>& IMs,
                            const vector<optional<int>>& i_mapping,
                            const vector<model_t>& scaleMs,
                            const vector<optional<int>>& scale_mapping,
                            const model_t& branch_length_model,
                            const std::vector<int>& like_calcs)
{
    // FIXME! Make likelihood_calculators for 1- and 2-sequence alignments handle compressed alignments.
    {
        checked_ofstream program_file(program_filename);
        program_file<<generate_atmodel_program(fixed,
                                               n_leaves,
                                               alphabet_exps,
                                               filename_ranges,
                                               tree_filename,
                                               SMs, s_mapping,
                                               IMs, i_mapping,
                                               scaleMs, scale_mapping,
                                               branch_length_model,
                                               like_calcs);
    }

    Program P(L, Program::exe_type::log_pair);
    auto m = P.get_module_loader()->load_module_from_file(program_filename);
    P.add(m);
    P.main = "Main.main";
    return P;
}

