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
#include "computation/module.H"

#include "sequence/genetic_code.H"
#include "sequence/codons.H"
#include "sequence/doublets.H"
#include "bali-phy/cmd_line.H"                                // for get_log_formats

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

namespace po = boost::program_options;
using po::variables_map;

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

std::string generate_atmodel_program(const variables_map& args,
                                     int n_sequences,
                                     const vector<expression_ref>& alphabet_exps,
                                     const vector<pair<fs::path,string>>& filename_ranges,
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
    Model::key_map_t keys;
    if (args.count("set"))
        keys = parse_key_map(args["set"].as<vector<string> >());

    set<string> fixed;
    if (args.count("fix"))
        for(auto& f: args.at("fix").as<vector<string>>())
            fixed.insert(f);

    for(auto& f: fixed)
        if (f != "topology" and f != "tree" and f != "alignment")
            throw myexception()<<"--fix: parameter '"<<f<<"' not recognized";

    auto log_formats = get_log_formats(args, args.count("align"));

    int n_partitions = filename_ranges.size();

    int n_leaves   = n_sequences;
    int n_branches = (n_leaves==1)?0:2*n_leaves - 3;

    set<string> imports;
    imports.insert("Bio.Alignment");                         // for Alignment.load_alignment
    imports.insert("Bio.Alphabet");                          // for Bio.Alphabet.dna, etc.
    imports.insert("Bio.Sequence");                          // for mkAlignedCharacterData, mkUnalignedCharacterData
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
    program_file<<"{-# LANGUAGE ExtendedDefaultRules #-}\n";
    program_file<<"module Main where";
    for(auto& mod: imports)
        program_file<<"\nimport "<<mod;
    program_file<<"\nimport qualified Data.IntMap as IntMap";
    program_file<<"\nimport qualified Data.JSON as J";
    program_file<<"\nimport qualified Data.Text.IO as T";
    program_file<<"\nimport Probability.Logger";
    program_file<<"\nimport System.Environment";
    program_file<<"\nimport System.FilePath";

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
    // Partitions are classified into n groups.
    // Currently n = 2, and groups are {unaligned, aligned}.
    vector<int> partition_index(n_partitions);
    vector<int> partition_group(n_partitions);
    vector<int> partition_group_size(2);

    for(int i=0;i<n_partitions;i++)
    {
	int g = (i_mapping[i])?0:1;
	partition_group[i] = g;
	partition_index[i] = partition_group_size[g]++;
    }

    auto unaligned_sequence_data = var("sequenceData");
    auto aligned_sequence_data = var("sequenceData");

    if (partition_group_size[0] > 0 and partition_group_size[1] > 0)
    {
	unaligned_sequence_data = var("unalignedSequenceData");
	aligned_sequence_data = var("alignedSequenceData");
    }

    auto getSequenceData = [&](int p) -> expression_ref
    {
	assert(n_partitions > 0);

	int group = partition_group[p];

	auto sequenceData = (group == 0) ? unaligned_sequence_data : aligned_sequence_data;

	// Only subscript if the group contains more than one element.
	if (partition_group_size[group] == 1)
	    return sequenceData;
	else
	    return {var("!!"), sequenceData, partition_index[p]};
    };

    if (n_partitions > 0)
    {
        program.let(taxon_names_var, {var("getTaxa"), getSequenceData(0)});
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

    program.perform({var("RanSamplingRate"), 1.0, {var("PerformTKEffect"), {var("add_tree_moves"), tree_var}}});


    set<string> used_states;
    for(int i=0;i<SMs.size();i++)
        add(used_states, SMs[i].code.used_states);

    // M5. Branch categories
    expression_ref branch_categories;
    if (used_states.count("branch_categories"))
    {
        var branch_categories_var("branch_categories");
        program.let(branch_categories_var, { var("foregroundBranches"), tree_var, String("foreground")});
        branch_categories = branch_categories_var;
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

    vector<tuple<int,expression_ref,expression_ref>> alignments; // partition, alignment var, alignment logger
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
        expression_ref sequence_data_var = getSequenceData(i);

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
                program.let(alignment_on_tree, {var("alignmentOnTreeFromSequences"), tree_var, sequence_data_var});
            }
            else
            {
                var leaf_sequence_lengths("sequence_lengths" + part_suffix);
                program.let(leaf_sequence_lengths, {var("get_sequence_lengths"), alphabet,  sequence_data_var});

                var properties_A("properties_A"+part_suffix);
		program.perform(Tuple(alignment_on_tree, properties_A), {var("sampleWithProps"),{var("random_alignment"), branch_dist_tree, imodel, leaf_sequence_lengths}});
            }
        }

        // Model.Partition.3. Observe the sequence data from the distribution
        expression_ref distribution;
        if (like_calcs[i] == 0)
            distribution = {var("ctmc_on_tree"), branch_dist_tree, alignment_on_tree, smodel};
        else
            distribution = {var("ctmc_on_tree_fixed_A"), branch_dist_tree, smodel};
	var properties("properties"+part_suffix);
	program.perform(properties, {var("observe"),sequence_data_var,distribution});

        program.empty_stmt();

        // Model.Partition.4 Logging.
        expression_ref alignment_exp;
        if (imodel_index)
        {
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
                var anc_alignment("anc_alignment"+part_suffix);
                program.let(anc_alignment, {var("prop_anc_seqs"), properties} );
                alignment_exp = anc_alignment;

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
                var prior_A("prior_A" + part_suffix);
                program.let(prior_A, {var("probability"),properties_A});
                sub_loggers.push_back({var("%=%"), String("prior_A"), {var("ln"),prior_A}});
            }

            sub_loggers.push_back({var("%=%"), String("likelihood"), {var("ln"),{var("prop_likelihood"),properties}}});
        }
        else
        {
            sub_loggers.push_back({var("%=%"), String("likelihood"), {var("ln"),{var("prop_fa_likelihood"),properties}}});

            if (n_branches > 0)
            {
                var substs("substs"+part_suffix);
                program.let(substs, {var("prop_fa_n_muts"), properties});
                sub_loggers.push_back({var("%=%"), String("#substs"), substs });
                total_substs.push_back(substs);

                if (load_value(keys,"write-fixed-alignments",false))
                {
                    // This should affect whether we allow modifying leaf sequences.
                    // bool infer_ambiguous_observed = load_value(keys, "infer-ambiguous-observed",false);

                    var anc_alignment("anc_alignment"+part_suffix);
                    program.let(anc_alignment, {var("prop_fa_anc_seqs"), properties} );
                    alignment_exp = anc_alignment;
                }
            }
        }

        var part_loggers("p"+part+"_loggers");
        program.let(part_loggers,get_list(sub_loggers));
        program_loggers.push_back( {var("%>%"), String("P"+part), part_loggers} );
        program.empty_stmt();

        if (alignment_exp)
            alignments.push_back({i,alignment_exp,var("logA"+part_suffix)});
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

    vector<expression_ref> alignment_loggers;
    for(auto& [i,a,l]: alignments)
        alignment_loggers.push_back(l);

    var sequences("sequences");
    auto model = var("model");

    expression_ref model_fn = model;

    // Pass in the sequence data for the two groups.
    if (partition_group_size[0] > 0)
	model_fn = {model_fn, unaligned_sequence_data};
    if (partition_group_size[1] > 0)
	model_fn = {model_fn, aligned_sequence_data};

    // Pass in the fixed tree or topology
    auto tree = var("tree");
    auto topology = var("topology");
    if (fixed.count("tree"))
        model_fn = {model_fn, tree};
    else if (fixed.count("topology"))
        model_fn = {model_fn, topology};

    // Pass in the loggers
    var jsonLogger("logParamsJSON");
    var tsvLogger("logParamsTSV");
    auto treeLogger = var("logTree");
    if (not args.count("test"))
    {
	if (log_formats.count("tsv"))
	    model_fn = {model_fn, tsvLogger};
	if (log_formats.count("json"))
	    model_fn = {model_fn, jsonLogger};
        if (not fixed.count("tree"))
            model_fn = {model_fn, treeLogger};
        if (not alignment_loggers.empty())
        model_fn = {model_fn, get_list(alignment_loggers)};
    }

    var loggers_var("loggers");
    program.let(loggers_var, get_list(program_loggers));
    program.empty_stmt();

    // Add the logger for scalar parameters
    if (not args.count("test"))
    {
	if (log_formats.count("tsv"))
	{
	    program.empty_stmt();
	    program.perform({var("$"),var("addLogger"),{tsvLogger,loggers_var}});
	}

	if (log_formats.count("json"))
	{
	    program.empty_stmt();
	    program.perform({var("$"),var("addLogger"),{jsonLogger,loggers_var}});
	}

        // Add the tree logger
        if (not fixed.count("tree"))
        {
            program.empty_stmt();
            program.perform({var("$"),var("addLogger"),{treeLogger,{var("addInternalLabels"),tree_var}}});
        }

        // Add the alignment loggers.
        if (not alignments.empty())
            program.empty_stmt();
        for(auto& [i,a,l]: alignments)
            program.perform({var("$"),var("addLogger"),{{var("$"),{var("every"),10},{l,a}}}});
    }
    
    program.empty_stmt();
    program.finish_return( loggers_var );
    program_file<<"\n";
    program_file<<model_fn<<" = "<<program.get_expression().print()<<"\n";

    do_block main;

    expression_ref directory = var("directory");
    vector<expression_ref> prog_args = {directory};
    if (not args.count("test"))
        main.perform(get_list(prog_args), var("getArgs"));

    auto unaligned_partitions = unaligned_sequence_data;
    auto aligned_partitions = aligned_sequence_data;
    if (n_partitions == 1)
    {
        auto [filename, range] = filename_ranges[0];

	// Load the sequences
	expression_ref E = {var("load_sequences"),String(filename.string())};

	// Select range
	if (not range.empty())
            E = {var("<$>"), {var("select_range"),String(range)}, E};

	// Convert to CharacterData
	if (i_mapping[0])
	    E = {var("<$>"),{var("mkUnalignedCharacterData"),alphabet_exps[0]}, E};
	else
	    E = {var("<$>"),{var("mkAlignedCharacterData"),alphabet_exps[0]}, E};

        main.perform(var("sequenceData"), E);
    }
    else
    {
        // Main.1: Emit let filenames = ...
        var filenames_var("filenames");
        bool any_ranges = false;
        map<fs::path,int> index_for_filename;
        {
            vector<expression_ref> filenames_;
            for(auto& [filename,range]: filename_ranges)
            {
                if (not index_for_filename.count(filename))
                {
                    index_for_filename.insert({filename,filenames_.size()});
                    filenames_.push_back(String(filename.string()));
                }
                if (not range.empty())
                    any_ranges = true;
            }
            main.let(filenames_var,get_list(filenames_));
        }

        {
            // Main.2: Emit let filenames_to_seqs = ...
            var filename_to_seqs("seqs");
            {
                main.perform(filename_to_seqs,{var("mapM"), var("load_sequences"), filenames_var});
            }
            main.empty_stmt();

            // Main.3. Emit let sequence_data<n> = 
            vector<var> unaligned_sequence_partitions;
            vector<var> aligned_sequence_partitions;
            for(int i=0;i<n_partitions;i++)
            {
		int group = partition_group[i];
                string part = std::to_string(i+1);

                var partition_sequence_data_var("sequenceData"+part);
		if (partition_group_size[group] == 1)
		    partition_sequence_data_var = (group==0) ? unaligned_sequence_data : aligned_sequence_data;

                int index = index_for_filename.at( filename_ranges[i].first );
                expression_ref loaded_sequences = {var("!!"),filename_to_seqs,index};
                if (not filename_ranges[i].second.empty())
                    loaded_sequences = {var("select_range"), String(filename_ranges[i].second), loaded_sequences};
		if (i_mapping[i])
		{
		    loaded_sequences = {var("mkUnalignedCharacterData"),alphabet_exps[i],loaded_sequences};
		    unaligned_sequence_partitions.push_back(partition_sequence_data_var);
		}
		else
		{
		    loaded_sequences = {var("mkAlignedCharacterData"),alphabet_exps[i],loaded_sequences};
		    aligned_sequence_partitions.push_back(partition_sequence_data_var);
		}
                main.let(partition_sequence_data_var, loaded_sequences);
                main.empty_stmt();
            }

            // Main.4. Emit let sequence_data = ...
	    if (unaligned_sequence_partitions.size() > 1)
		main.let(unaligned_partitions, get_list(unaligned_sequence_partitions));

	    if (aligned_sequence_partitions.size() > 1)
		main.let(aligned_partitions, get_list(aligned_sequence_partitions));

            main.empty_stmt();
        }
    }

    if ((fixed.count("tree") or fixed.count("topology")) and not tree_filename)
    {
        throw myexception()<<"The tree is fixed, but no tree file is given. (Use --tree)";
    }

    if (fixed.count("tree"))
    {
        main.empty_stmt();
        main.perform(tree, {var("<$>"),var("dropInternalLabels"),{var("readBranchLengthTree"),String(tree_filename->string())}});
    }
    else if (fixed.count("topology"))
    {
        main.empty_stmt();
        main.perform(topology, {var("<$>"),var("dropInternalLabels"),{var("readTreeTopology"),String(tree_filename->string())}});
    }

    if (not args.count("test"))
    {
        // Initialize the parameters logger
	if (log_formats.count("tsv"))
	{
	    main.empty_stmt();
	    main.perform(tsvLogger, {var("tsvLogger"),{var("</>"), directory, String("C1.log")},get_list(vector<String>{"iter"})});
	}

	if (log_formats.count("json"))
	{
	    main.empty_stmt();
	    main.perform(jsonLogger, {var("jsonLogger"),{var("</>"), directory, String("C1.log.json")}});
	}

        // Initialize the tree logger
        if (not fixed.count("tree"))
        {
            main.empty_stmt();
            main.perform(treeLogger,{var("treeLogger"), {var("</>"), directory, String("C1.trees")} });
        }

        // Initialize the alignment loggers
        if (not alignments.empty())
        {
            main.empty_stmt();

            // Create alignment loggers.
            for(auto& [i,a,logger]: alignments)
            {
                string filename = "C1.P"+std::to_string(i+1)+".fastas";
                main.perform(logger,{var("alignmentLogger"), {var("</>"), directory, String(filename)}});
            }
        }
    }

    // Main.5. Emit mymodel <- makeMCMCModel $ model sequence_data
    main.empty_stmt();
    main.perform(var("mymodel"),{var("$"),var("makeMCMCModel"),model_fn});

    // Main.6. Emit runMCMC iterations mymodel
    if (args.count("test"))
    {
        if (log_formats.count("tsv"))
        {
            main.empty_stmt();
            main.perform(var("line"), {var("logTableLine"), var("mymodel"), 0});
            main.empty_stmt();
            main.perform({var("T.putStrLn"), var("line")});
        }

        if (log_formats.count("json"))
        {
            main.empty_stmt();
            main.perform(var("jline"), {var("logJSONLine"), var("mymodel"), 0});
            main.empty_stmt();
            main.perform({var("T.putStrLn"), var("jline")});
        }

        if (args.count("verbose"))
        {
            main.perform({var("writeTraceGraph"),var("mymodel")});
//            M->write_factor_graph();
        }
    }
    else
    {
        main.empty_stmt();
        // int subsample = args["subsample"].as<int>();
        int max_iterations = 200000;
        if (args.count("iterations"))
            max_iterations = args["iterations"].as<long int>();
        main.perform({var("runMCMC"), max_iterations, var("mymodel")});
    }

    program_file<<"\nmain = "<<main.get_expression().print()<<"\n";

    return program_file.str();
}

Program gen_atmodel_program(const boost::program_options::variables_map& args,
                            const std::shared_ptr<module_loader>& L,
                            const fs::path& output_directory,
                            const fs::path& program_filename,
                            const std::optional<fs::path>& tree_filename,
                            const vector<expression_ref>& alphabet_exps,
                            const vector<pair<fs::path,string>>& filename_ranges,
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
        program_file<<generate_atmodel_program(args,
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

    Program P(L);
    auto m = P.get_module_loader()->load_module_from_file(program_filename);
    P.add(m);
    P.main = "Main.main";
    L->args = {output_directory.string()};
    return P;
}

string generate_model_program(const boost::program_options::variables_map& args,
                              const string& model_module_name,
                              const fs::path& output_directory)
{
    std::ostringstream program_file;
    for(auto& mod: {"System.FilePath","Probability","Probability.Logger","MCMC"})
        program_file<<"import "<<mod<<"\n";
    program_file<<"import qualified Data.Text.IO as T\n";
    program_file<<"\nimport qualified "<<model_module_name<<" as Model\n";
    program_file<<"\n";
    program_file<<"main = do\n";

    auto log_formats = get_log_formats(args, false);

    string addLogCmds;
    if (not args.count("test"))
    {
	if (log_formats.count("tsv"))
	{
	    program_file<<"  logParamsTSV <- tsvLogger "<<output_directory / "C1.log" <<" [\"iter\"]\n";
	    program_file<<"\n";
	    addLogCmds += "addLogger $ logParamsTSV j;";
	}

	if (log_formats.count("json"))
	{
	    program_file<<"  logParamsJSON <- jsonLogger $ "<<output_directory / "C1.log.json" <<"\n";
	    program_file<<"\n";
	    addLogCmds += "addLogger $ logParamsJSON j;";
	}
    }
    program_file<<"  model <- Model.main\n";
    program_file<<"\n";
    if (args.count("test"))
    {
	program_file<<"  mymodel <- makeMCMCModel $ model\n";
    }
    else
    {
	program_file<<"  mymodel <- makeMCMCModel $ do { j <- model; "<<addLogCmds<<" return j }\n";
    }
    if (args.count("test"))
    {
        auto log_formats = get_log_formats(args, false);

        if (log_formats.count("tsv"))
        {
            program_file<<"\n";
            program_file<<"  line <- logTableLine mymodel 0\n";
            program_file<<"\n";
            program_file<<"  T.putStrLn line\n";
        }

        if (log_formats.count("json"))
        {
            program_file<<"\n";
            program_file<<"  jline <- logJSONLine mymodel 0\n";
            program_file<<"\n";
            program_file<<"  T.putStrLn jline\n";
        }
    }
    else
    {
        int iterations = 200000;
        if (args.count("iterations"))
            iterations = args["iterations"].as<long int>();

        program_file<<"\n";
        program_file<<"  runMCMC "<<iterations<<" mymodel\n";
    }

    if (args.count("verbose"))
    {
        program_file<<"\n";
	program_file<<"  writeTraceGraph mymodel\n";
    }

    return program_file.str();

}

Program gen_model_program(const boost::program_options::variables_map& args,
                          const std::shared_ptr<module_loader>& L,
                          const fs::path& output_directory,
                          const fs::path& model_filepath)
{
    // 1. Check that the model filepath is valid.
    if (not fs::exists(model_filepath))
	throw myexception()<<"The model file "<<model_filepath<<" does not exist!";

    if (fs::is_directory(model_filepath))
	throw myexception()<<"The model file "<<model_filepath<<" is a directory!";
	
    if (not fs::is_regular_file(model_filepath))
	throw myexception()<<"The model file "<<model_filepath<<" is not a regular file!";
	
    fs::path main_file = "Main.hs";
    if (output_directory.empty())
	main_file = "Test.Main.hs";

    if (model_filepath.filename() == main_file)
        throw myexception()<<"The model file may not be called "<<main_file<<".";

    // 2. Save a copy of the model file.
    auto dest_model_filepath = output_directory / model_filepath.filename();
    if (not output_directory.empty())
	fs::copy_file(model_filepath, dest_model_filepath);

    // 3. Load the model module.
    Program P(L);
    auto m = P.get_module_loader()->load_module_from_file(dest_model_filepath);
    if (m->name == "Main")
        throw myexception()<<"The module name for the model file "<<model_filepath<<" may not be 'Main'\n";
    P.add(m);

    // 4. Generate and write the Main module.
    auto program_filepath = output_directory / main_file;
    {
        checked_ofstream program_file(program_filepath);
        program_file<<generate_model_program(args, m->name, output_directory);
    }

    // 5. Load the Main module.
    auto m2 = P.get_module_loader()->load_module_from_file(program_filepath);
    P.add(m2);
    P.main = "Main.main";

    return P;
}
