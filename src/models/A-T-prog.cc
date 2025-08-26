#include "A-T-prog.H"

#include <set>
#include <map>

#include "util/set.H"
#include "util/io.H"
#include "util/string/split.H"
#include "util/settings.H"    // for get_setting_or( )
#include "models/compile.H"   // for model_t
#include "models/parse.H"   // for unparse_type

#include "computation/loader.H"
#include "computation/expression/list.H"
#include "computation/expression/tuple.H"
#include "computation/expression/var.H"
#include "computation/module.H"

#include "sequence/genetic_code.H"
#include "sequence/codons.H"
#include "sequence/doublets.H"
#include "sequence/RNAEdits.H"
#include "bali-phy/cmd_line.H"                                // for get_log_formats
#include "bali-phy/files.H"                                   // for run_name

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

/* NOTE: Fixing the alignment
 *
 * Currently we compute a fixed alignment on the tree using `alignmentOnTreeFromSequences`.
 * This assumes the leaf characters are minimally connected (I think) in each column,
 * instead of actually inferring whether internal nodes have gaps or not.  Also, currently
 * the alignment isn't modifiable, and walk_tree_sample_NNI_and_A complains and crashes.
 *
 * In order to do this correctly, we would need to draw the alignment from some kind of
 * "fixedAlignment(aligned_sequences,tree)" distribution.  The pairwise alignments would need
 * to be modifiables, but we need to only change them in ways that don't alter the
 * projection to the leaves.
 *
 * - How would the MCMC moves detect that they shouldn't change the (projected) alignment?
 * - How can we do SPR moves without changing the projected alignment?
 */

std::map<std::string, std::string> get_fixed(const boost::program_options::variables_map& args)
{
    map<string, string> fixed;
    if (args.count("fix"))
        for(auto& f: args.at("fix").as<vector<string>>())
	{
	    auto [key,value] = split_on_first('=',f);
	    fixed.insert({key,value});
	}

    for(auto& [key,value]: fixed)
        if (key != "topology" and key != "tree" and key != "alignment")
            throw myexception()<<"--fix: parameter '"<<key<<"' not recognized";

    if (fixed.count("tree") and fixed.count("topology"))
        throw myexception()<<"Can't fix both 'tree' and 'topology'";

    if (fixed.count("alignment") and not args.count("test"))
        throw myexception()<<"Currently --fix=alignment only works with --test.\n  You can fix the alignment for MCMC by disabling the indel model with -Inone, which disables the indel model.\n  Using the indel information from a fixed alignment during MCMC is not implemented.";

    for(auto&& word: {"tree","topology"})
        if (fixed.count(word) and args.count("tree"))
            throw myexception()<<"Can't specify --tree=<prior> if the "<<word<<" is fixed!";

    for(auto&& word: {"tree","topology"})
        if (fixed.count(word) and fixed.at(word).empty())
            throw myexception()<<"Fixed "<<word<<" but did not specify "<<word<<" file!  Use --fix "<<word<<"=<filename>";

    return fixed;
}

expression_ref get_genetic_code_expression(const Genetic_Code& code)
{
    return {var("geneticCode"),String(code.name())};
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
        return {var("mkCodons"), nucs, code};
    }
    else if (auto triplets = dynamic_cast<const Triplets*>(&a))
    {
        auto nucs = get_alphabet_expression(triplets->getNucleotides());
        return {var("mkTriplets"),nucs};
    }
    else if (auto doublets = dynamic_cast<const Doublets*>(&a))
    {
        auto nucs = get_alphabet_expression(doublets->getNucleotides());
        return {var("mkDoublets"),nucs};
    }
    else if (auto doublets = dynamic_cast<const RNAEdits*>(&a))
    {
        auto nucs = get_alphabet_expression(doublets->getNucleotides());
        return {var("mkRNAEdits"),nucs};
    }
    else if (auto num = dynamic_cast<const Numeric*>(&a))
    {
	int n = num->size();
	return {var("mkNumeric"),n};
    }
    else
    {
	throw myexception()<<"Can't translate C++ alphabet object "<<a.name<<" into Haskell expression";
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

vector<expression_ref> generate_scale_models(const vector<model_t>& scaleMs,
					     const vector<string>& scaleM_function_for_index,
					     const expression_ref& tree_var,
					     do_block& model,
					     vector<expression_ref>& model_loggers)
{
    // define tree_length
    var tree_length_var("tlength");
    model.let(tree_length_var, {var("treeLength"),tree_var});
    // log |T|
    model_loggers.push_back( {var("%=%"), String("|T|"), tree_length_var} );

    vector<expression_ref> scales;

    for(int i=0; i<scaleMs.size(); i++)
    {
	// FIXME: Ideally we would actually join these models together using a Cons operation and prefix.
	//        This would obviate the need to create a Scale1 (etc) prefix here.
	string var_name = "scale" + convertToString(i+1);

	auto code = scaleMs[i].code;
	expression_ref E = var(scaleM_function_for_index[i]);
	E = code.add_arguments(E, {});

	// This should still log sub-loggers of the scales, I think.
	auto scale_var = bind_and_log(false, var_name, E, code.is_action(), code.has_loggers(), model, model_loggers);

	scales.push_back(scale_var);

	// log scale[i]
	model_loggers.push_back( {var("%=%"), String(var_name), scale_var} );

	// log scale[i]*|T|
	model_loggers.push_back( {var("%=%"), String(var_name+"*|T|"), {var("*"),scale_var,tree_length_var}} );
    }

    return scales;
}

vector<expression_ref> generate_substitution_models(const vector<model_t>& SMs,
						    const vector<optional<int>>& s_mapping,
						    const vector<string>& SM_function_for_index,
						    const vector<expression_ref>& alphabet_exps,
						    const expression_ref& branch_categories,
						    const expression_ref& tree,
						    do_block& model,
						    vector<expression_ref>& model_loggers)
{
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
        smodel = code.add_arguments(smodel, {
                {"alphabet",alphabet_exps[*first_partition]},
                {"branch_categories",branch_categories},
                {"tree",tree}
            });

        auto smodel_var = var("smodel" + suffix);
        auto log_smodel = var("log_"+smodel_var.name);
        bind_and_log(false, smodel_var, log_smodel, prefix, smodel, code.is_action(), code.has_loggers(), model, model_loggers);
        smodels.push_back(smodel_var);
    }
    return smodels;
}

vector<expression_ref> generate_indel_models(const vector<model_t>& IMs,
					     const vector<string>& IM_function_for_index,
					     const expression_ref& tree_var,
					     do_block& model,
					     vector<expression_ref>& model_loggers)
{
    // M8. Indel models
    vector<expression_ref> imodels;
    for(int i=0;i<IMs.size();i++)
    {
        string prefix = "I" + convertToString(i+1);
        string _suffix = (IMs.size()>1)?"_"+convertToString(i+1):"";
        string suffix = (IMs.size()>1)?convertToString(i+1):"";

        auto code = IMs[i].code;

        expression_ref imodel = var(IM_function_for_index[i]);
        imodel = code.add_arguments(imodel, {{"topology",tree_var}});

        auto imodel_var = var("imodel" + suffix);
        auto log_imodel = var("log_"+imodel_var.name);
        bind_and_log(false, imodel_var, log_imodel, prefix, imodel, code.is_action(), code.has_loggers(), model, model_loggers);
        imodels.push_back(imodel_var);
    }
    return imodels;
}

do_block generate_main(const variables_map& args,
		       const vector<pair<fs::path,string>>& filename_ranges,
		       const vector<expression_ref>& alphabet_exps,
		       const vector<int>& partition_group,
		       const vector<int>& partition_group_size,
		       const var& unaligned_sequence_data,
		       const var& aligned_sequence_data,
		       const expression_ref& tree,
		       const expression_ref& topology,
		       const expression_ref& tsvLogger,
		       const expression_ref& jsonLogger,
		       const expression_ref& treeLogger,
		       const expression_ref& model_fn,
		       vector<tuple<int,expression_ref,expression_ref>>& alignments)
{
    auto fixed = get_fixed(args);

    auto log_formats = get_log_formats(args, args.count("align"));

    int n_partitions = filename_ranges.size();

    do_block main;

    expression_ref directory = var("directory");
    if (not args.count("test"))
    {
        main.perform(directory, {var("createUniqueDirectory"), String(run_name(args))});
        // putStrLn $ "Created directory " ++ show directory ++ " for output files"
        main.perform({var("$"),var("putStrLn"),{var("++"),String("Created directory "),{var("++"),{var("show"),directory},{String(" for output files")}}}});
        main.empty_stmt();

        // files <- getArgs
        expression_ref files = var("files");
        main.perform(files, var("getArgs"));
        main.empty_stmt();

        // storeFiles files
        main.perform({var("storeFiles"), directory, files});
        main.empty_stmt();
    }

    auto unaligned_partitions = unaligned_sequence_data;
    auto aligned_partitions = aligned_sequence_data;
    if (n_partitions == 1)
    {
        auto [filename, range] = filename_ranges[0];

	// Load the sequences
	expression_ref E = {var("loadSequences"),String(filename.string())};

	// Select range
	if (not range.empty())
            E = {var("<$>"), {var("selectRange"),String(range)}, E};

	// Convert to CharacterData
	if (partition_group[0] == 0)
	    E = {var("<$>"),{var("mkUnalignedCharacterData"),alphabet_exps[0]}, E};
	else
	    E = {var("<$>"),{var("mkAlignedCharacterData"),alphabet_exps[0]}, E};

        main.perform(var("sequenceData"), E);
    }
    else
    {
        // Main.1: Emit let filenames = ...
        var filenames_var("filenames");
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
            }
            main.let(filenames_var,get_list(filenames_));
        }

        {
            // Main.2: Emit let filenames_to_seqs = ...
            var filename_to_seqs("seqs");
            {
                main.perform(filename_to_seqs,{var("mapM"), var("loadSequences"), filenames_var});
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
                    loaded_sequences = {var("selectRange"), String(filename_ranges[i].second), loaded_sequences};
		if (partition_group[i] == 0)
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
    optional<fs::path> tree_filename;
    if (fixed.count("tree"))
	tree_filename = fixed.at("tree");
    else if (fixed.count("topology"))
	tree_filename = fixed.at("topology");

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

    return main;
}


void write_header(std::ostream& program_file,
		  const model_t& decls,
		  const vector<model_t>& SMs,
		  const vector<model_t>& IMs,
		  const vector<model_t>& scaleMs,
		  const model_t& tree_model)
{
    set<string> imports;
    imports.insert("Bio.Alignment");                         // for Alignment.load_alignment
    imports.insert("Bio.Alphabet");                          // for Bio.Alphabet.dna, etc.
    imports.insert("Bio.Sequence");                          // for mkAlignedCharacterData, mkUnalignedCharacterData
    imports.insert("MCMC");                                  // for scaleGroups{Slice,MH}
    imports.insert("Tree");                                  // for Tree
    imports.insert("Tree.Newick");                           // for writeNewick
    imports.insert("SModel.Parsimony");                      // for parsimony
    imports.insert("Probability");                           // for prop_likelihood, dropInternalLabels(?)
    imports.insert("Probability.Random");                    // for makeMCMCModel
    add(imports, decls.imports);
    for(auto& m: SMs)
        add(imports, m.imports);
    for(auto& m: IMs)
        add(imports, m.imports);
    for(auto& m: scaleMs)
        add(imports, m.imports);
    add(imports, tree_model.imports);

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
    program_file<<"\nimport BAliPhy.Util";
    program_file<<"\nimport Data.Maybe (listToMaybe)";
}

vector<expression_ref>
compute_logged_quantities(do_block& model,
			  int n_branches,
			  int n_partitions,
			  const map<string,string>& fixed,
			  int i,
			  const expression_ref& tree,
			  const expression_ref& alignment_on_tree,
			  const expression_ref& properties,
			  const expression_ref& alphabet_exp,
			  const expression_ref& sequence_data,
			  const expression_ref& smodel,
			  std::optional<int> imodel_index,
			  vector<expression_ref>& alignment_lengths,
			  vector<expression_ref>& total_num_indels,
			  vector<expression_ref>& total_length_indels,
			  vector<expression_ref>& total_substs,
			  vector<expression_ref>& total_prior_A,
			  vector<tuple<int,expression_ref,expression_ref>>& alignments)
{
    string part = std::to_string(i+1);
    string part_suffix = (n_partitions>1) ? part : "";

    vector<expression_ref> sub_loggers;
    expression_ref alignment_exp;
    if (imodel_index)
    {
	var alignment_length("alignment_length"+part_suffix);
	model.let(alignment_length, {var("alignmentLength"), alignment_on_tree} );
	alignment_lengths.push_back(alignment_length);

	if (n_branches > 0)
	{
	    var num_indels("num_indels"+part_suffix);
	    model.let(num_indels, {var("totalNumIndels"), alignment_on_tree} );
	    total_num_indels.push_back(num_indels);
	    var length_indels("total_length_indels"+part_suffix);
	    model.let(length_indels, {var("totalLengthIndels"), alignment_on_tree} );
	    total_length_indels.push_back(length_indels);

	    sub_loggers.push_back({var("%=%"), String("|A|"), alignment_length });
	    sub_loggers.push_back({var("%=%"), String("#indels"), num_indels });
	    sub_loggers.push_back({var("%=%"), String("|indels|"), length_indels} );
	}

	if (not fixed.count("alignment"))
	{
	    var properties_A("properties_A"+part_suffix);
	    var prior_A("prior_A" + part_suffix);
	    model.let(prior_A, {var("ln"),{var("probability"),properties_A}});
	    total_prior_A.push_back(prior_A);
	    sub_loggers.push_back({var("%=%"), String("prior_A"), prior_A});
	}
    }
    else
    {
	// For fixed-alignment partitions, the alignment length comes from the observed-data matrix.
	var alignment_length("alignment_length"+part_suffix);
	model.let(alignment_length, {var("alignmentLength"), sequence_data} );
	alignment_lengths.push_back(alignment_length);
    }

    sub_loggers.push_back({var("%=%"), String("likelihood"), {var("ln"),{var("prop_likelihood"),properties}}});

    if (n_branches > 0)
    {
	if (imodel_index or get_setting_or("write-fixed-alignments",false))
	{
            // FIXME: This should affect whether we allow modifying leaf sequences.
	    // bool infer_ambiguous_observed = get_setting_or(keys, "infer-ambiguous-observed",false);

            // Get the alignment variable
            auto alignment = alignment_on_tree;
            if (not imodel_index)
            {
                alignment = var("alignment" + part_suffix);
                model.let(alignment, {var("leafAlignment"), tree, sequence_data});
            }

            var anc_states("ancStates" + part_suffix);
            model.let(anc_states,{var("prop_anc_cat_states"), properties});
        
	    var anc_alignment("ancAlignment"+part_suffix);
	    model.let(anc_alignment, {var("toFasta"),{var("ancestralAlignment"), tree, alignment, {var("getSMap"),smodel}, alphabet_exp, anc_states}});
	    alignment_exp = anc_alignment;
	}

	var substs("substs"+part_suffix);
	expression_ref costs = {var("unitCostMatrix"),alphabet_exp};
	expression_ref aligned_data = sequence_data;
	if (imodel_index)
	    aligned_data = Tuple(sequence_data, alignment_on_tree);

	model.let(substs, {var("parsimony"), tree, costs, aligned_data});
	sub_loggers.push_back({var("%=%"), String("#substs"), substs });
	if (alphabet_exp.print().starts_with("mkRNA"))
	{
	    string suffix = part_suffix;
	    if (not suffix.empty())
		suffix = "_"+suffix;

	    var substs_pos2("substsRNA"+suffix);
	    expression_ref costs_pos2 = {var("pos2CostMatrix"),alphabet_exp};
	    model.let(substs_pos2, {var("parsimony"), tree, costs_pos2, aligned_data});
	    sub_loggers.push_back({var("%=%"), String("#substsRNA"), substs_pos2 });
	}

	total_substs.push_back(substs);
    }

    if (alignment_exp)
	alignments.push_back({i,alignment_exp,var("logA"+part_suffix)});

    return sub_loggers;
}

bool is_reversible(const ptree& t)
{
    return true;

    if (get_type_head(t) == "RevCTMC")
	return true;
    else if (get_type_head(t) == "CTMC")
	return false;
    else if (get_type_head(t) == "DiscreteDist")
	return is_reversible(t[0].second);
    else if (get_type_head(t) == "MultiMixtureModel")
	return is_reversible(t[0].second);
    else
	throw myexception()<<"is_reversible: unrecognized type "<<unparse_type(t)<<"!";
}

bool is_reversible(const vector<model_t>& SMs)
{
    for(auto& SM: SMs)
	if (not is_reversible(SM.type))
	    return false;

    return true;
}

std::string generate_atmodel_program(const variables_map& args,
                                     int n_sequences,
                                     const vector<expression_ref>& alphabet_exps,
                                     const vector<pair<fs::path,string>>& filename_ranges,
                                     const model_t& decls,
                                     const vector<model_t>& SMs,
                                     const vector<optional<int>>& s_mapping,
                                     const vector<string>& s_conditions,
                                     const vector<model_t>& IMs,
                                     const vector<optional<int>>& i_mapping,
                                     const vector<model_t>& scaleMs,
                                     const vector<optional<int>>& scale_mapping,
                                     const model_t& tree_model,
                                     const model_t& subst_rates_model,
                                     const model_t& indel_rates_model,
                                     const std::vector<int>& like_calcs)
{
    auto fixed = get_fixed(args);

    auto log_formats = get_log_formats(args, args.count("align"));

    int n_partitions = filename_ranges.size();

    int n_leaves   = n_sequences;
    int n_branches = (n_leaves==1)?0:2*n_leaves - 3;

    // Write pragmas, module, imports.
    std::ostringstream program_file;
    write_header(program_file, decls, SMs, IMs, scaleMs, tree_model);
    program_file<<"\n\n";

    auto SM_function_for_index = print_models("sample_smodel", SMs, program_file);
    auto IM_function_for_index = print_models("sample_imodel", IMs, program_file);
    vector<string> scaleM_function_for_index;
    if (n_branches > 0)
        scaleM_function_for_index = print_models("sample_scale", scaleMs, program_file);

    // F5. Topology / Tree
    if (not fixed.count("tree"))
	program_file<<"sampleTree"<<print_equals_function(tree_model.code.generate())<<"\n";

    /* --------------------------------------------------------------- */
    do_block model;

    // FIXME: We can't load the alignments to read their names until we know the alphabets!
    // FIXME: Can we load the alignments as SEQUENCES first?
    var taxon_names_var("taxa");

    // Loggers = [(string,(Maybe a,Loggers)]
    vector<expression_ref> model_loggers;
    // Therefore, we are constructing a list with values [(prefix1,(Just value1, loggers1)), (prefix1, (Just value1, loggers2))

    // M1. Taxa
    // Partitions are classified into n groups.
    // Currently n = 2, and groups are {unaligned, aligned}.
    vector<int> partition_index(n_partitions);
    vector<int> partition_group(n_partitions);
    vector<int> partition_group_size(2);

    for(int i=0;i<n_partitions;i++)
    {
	int g = (i_mapping[i] and not fixed.contains("alignment"))?0:1;
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
        model.let(taxon_names_var, {var("getTaxa"), getSequenceData(0)});
        model.empty_stmt();
    }

    // We could fix the whole tree or just the topology.
    expression_ref branch_lengths = var("IntMap.empty");

    // M1. Declarations -- This is similar to use_block(model, ??, decls, "decls");
    for(auto& stmt: decls.code.stmts)
	model.get_stmts().push_back(stmt);
    auto decl_loggers = decls.code.loggers;
    simplify(decl_loggers);
    for(auto& logger: generate_loggers(model, decl_loggers))
	model_loggers.push_back(logger);

    model.empty_stmt();

    // M4. Branch-length tree
    auto tree_var = var("tree");
    if (not fixed.count("tree"))
    {
//      if (not is_reversible(SMs) and not fixed.count("topology") and not fixed.count("tree"))
//      {
//          model.perform(var("root"), {var("sample"),{var("uniformCategoricalOn"),{var("nodes"), var("topology")}}});
//          tree_exp = {var("addRoot"),var("root"),tree_exp};
//      }

        string var_name = "tree";

        auto code = tree_model.code;

        expression_ref E = var("sampleTree");
        E = code.add_arguments(E,{{"taxa",taxon_names_var}});

        tree_var = bind_and_log(false, var_name, E, code.is_action(), code.has_loggers(), model, model_loggers);
        branch_lengths = {var("branchLengths"), tree_var};
    }

    expression_ref subst_tree=tree_var;
    auto subst_rates_var = var("substRates");
    if (not subst_rates_model.empty())
    {
        string var_name = "substRates";
        auto code = subst_rates_model.code;
        code.haskell_lambda_vars.clear(); // This isn't a function, these vars should be in scope.
        subst_rates_var = bind_and_log(false, var_name, code.generate(), code.is_action(), code.has_loggers(), model, model_loggers);

        auto subst_tree_var = var("substTree");
        model.let(subst_tree_var, {var("addBranchRates"),var("substRates"),tree_var});
        subst_tree = subst_tree_var;
        model.empty_stmt();
    }

    expression_ref indel_tree=tree_var;
    auto indel_rates_var = var("indelRates");
    if (not indel_rates_model.empty())
    {
        string var_name = "indelRates";
        auto code = indel_rates_model.code;
        code.haskell_lambda_vars.clear(); // This isn't a function, these vars should be in scope.
        indel_rates_var = bind_and_log(false, var_name, code.generate(), code.is_action(), code.has_loggers(), model, model_loggers);

        auto indel_tree_var = var("indelTree");
        model.let(indel_tree_var, {var("addBranchRates"),var("indelRates"),tree_var});
        indel_tree = indel_tree_var;
        model.empty_stmt();
    }

    set<string> used_states;
    for(int i=0;i<SMs.size();i++)
        add(used_states, SMs[i].code.used_states);

    // M5. Branch categories
    expression_ref branch_categories;
    if (used_states.count("branch_categories"))
    {
        var branch_categories_var("branch_categories");
        model.let(branch_categories_var, { var("foregroundBranches"), tree_var, String("foreground")});
        branch_categories = branch_categories_var;
    }

    // M6. Scales
    vector<expression_ref> scales;
    if (n_branches > 0)
    {
	scales = generate_scale_models(scaleMs, scaleM_function_for_index, tree_var, model, model_loggers);

        if (not fixed.count("tree"))
        {
            model.perform({var("addMove"), 2, {var("scaleGroupsSlice"), get_list(scales), branch_lengths}});
	    model.perform({var("addMove"), 1, {var("scaleGroupsMH"), get_list(scales), branch_lengths}});
        }
    }

    auto smodels = generate_substitution_models(SMs, s_mapping, SM_function_for_index, alphabet_exps, branch_categories, tree_var, model, model_loggers);
    auto imodels = generate_indel_models(IMs, IM_function_for_index, tree_var, model, model_loggers);
    model.empty_stmt();

    vector<tuple<int,expression_ref,expression_ref>> alignments; // partition, alignment var, alignment logger
    vector<expression_ref> alignment_lengths;
    vector<expression_ref> total_num_indels;
    vector<expression_ref> total_length_indels;
    vector<expression_ref> total_substs;
    vector<expression_ref> total_prior_A;
    vector<expression_ref> partition_scales;

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
	expression_ref scale = 1;
        if (n_branches > 0)
            scale = scales[scale_index];
	partition_scales.push_back(scale);
	
        // Model.Partition.2. Sample the alignment
        var alignment_on_tree("alignment" + part_suffix);
        if (imodel_index)
        {
            assert(like_calcs[i] == 0);
            expression_ref imodel = imodels[*imodel_index];

            if (fixed.count("alignment"))
            {
                model.let(alignment_on_tree, {var("alignmentOnTreeFromSequences"), tree_var, sequence_data_var});
            }
            else
            {
                var leaf_sequence_lengths("sequence_lengths" + part_suffix);
                model.let(leaf_sequence_lengths, {var("getSequenceLengths"), sequence_data_var});

                var properties_A("properties_A"+part_suffix);
		model.perform(Tuple(alignment_on_tree, properties_A), {var("sampleWithProps"),{var("phyloAlignment"), indel_tree, imodel, scale, leaf_sequence_lengths}});
            }
        }

        // Model.Partition.3. Observe the sequence data from the distribution
        expression_ref distribution;
        string s_condition = s_conditions[smodel_index];
        if (like_calcs[i] == 0)
        {
            assert(s_condition.empty());
            distribution = {var("phyloCTMC"), subst_tree, alignment_on_tree, smodel, scale};
        }
        else
	{
	    expression_ref alignment_length = {var("alignmentLength"), sequence_data_var};
            distribution = {var("phyloCTMC"), subst_tree, alignment_length, smodel, scale};
            if (not s_condition.empty())
            {
                if (s_condition == "variable")
                    distribution = {var("variable"), distribution};
                else
                    throw myexception()<<"Unrecognized ascertainment condition '"<<s_condition<<"'";
            }
	}
	var properties("properties"+part_suffix);
	expression_ref sequence_data = sequence_data_var;
	if (fixed.contains("alignment") and i_mapping[i])
	    sequence_data = {var("unalign"), sequence_data};
	model.perform(properties, {var("observe"),sequence_data,distribution});

        model.empty_stmt();

        // Model.Partition.4 Logging.
	auto sub_loggers = compute_logged_quantities(model,
						     n_branches,
						     n_partitions,
						     fixed,
						     i,
						     tree_var,
						     alignment_on_tree,
						     properties,
						     alphabet_exps[i],
						     sequence_data,
                                                     smodel,
						     imodel_index,
						     alignment_lengths,
						     total_num_indels,
						     total_length_indels,
						     total_substs,
						     total_prior_A,
						     alignments);

        var part_loggers("part"+part+"Loggers");
        model.let(part_loggers,get_list(sub_loggers));
        model_loggers.push_back( {var("%>%"), String("P"+part), part_loggers} );
        model.empty_stmt();
    }
    bool has_a_variable_alignment = not total_num_indels.empty();
    model.let(var("alignmentLengths"),get_list(alignment_lengths));
    if (n_branches > 0)
    {
	if (n_partitions > 1)
	{
	    model.let(var("scales"),get_list(partition_scales));
	    expression_ref a_lengths = {var("fmap"),var("fromIntegral"),var("alignmentLengths")};
	    model.let(var("scale"),{var("weightedAverage"), a_lengths, var("scales")});
	}
	else
	    model.let(var("scale"),{var("scale1")});
	model_loggers.push_back( {var("%=%"), String("scale"), var("scale")});
	model_loggers.push_back( {var("%=%"), String("scale*|T|"), {var("*"),var("scale"),var("tlength")}});
    }

    if (not alignment_lengths.empty() and has_a_variable_alignment)
        model_loggers.push_back( {var("%=%"), String("|A|"), {var("sum"),var("alignmentLengths") }} );
    if (not total_num_indels.empty())
        model_loggers.push_back( {var("%=%"), String("#indels"), {var("sum"),get_list(total_num_indels) }} );
    if (not total_length_indels.empty())
        model_loggers.push_back( {var("%=%"), String("|indels|"), {var("sum"),get_list(total_length_indels) }} );
    if (not total_substs.empty())
        model_loggers.push_back( {var("%=%"), String("#substs"), {var("sum"),get_list(total_substs) }} );
    if (not total_prior_A.empty())
        model_loggers.push_back( {var("%=%"), String("prior_A"), {var("sum"),get_list(total_prior_A) }} );

    vector<expression_ref> alignment_loggers;
    for(auto& [i,a,l]: alignments)
        alignment_loggers.push_back(l);

    var sequences("sequences");

    expression_ref model_fn = var("model");

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
    model.let(loggers_var, get_list(model_loggers));
    model.empty_stmt();

    // Add the logger for scalar parameters
    if (not args.count("test"))
    {
	if (log_formats.count("tsv"))
	{
	    model.empty_stmt();
	    model.perform({var("$"),var("addLogger"),{tsvLogger,loggers_var}});
	}

	if (log_formats.count("json"))
	{
	    model.empty_stmt();
	    model.perform({var("$"),var("addLogger"),{jsonLogger,loggers_var}});
	}

        // Add the tree logger
        if (not fixed.count("tree"))
        {
            model.empty_stmt();
	    expression_ref scaled_tree = tree_var;
	    if (n_branches > 0)
		scaled_tree = {var("scaleBranchLengths"),var("scale"), scaled_tree};
            model.perform({var("$"),var("addLogger"),{treeLogger,{var("addInternalLabels"), scaled_tree}}});
        }

        // Add the alignment loggers.
        if (not alignments.empty())
            model.empty_stmt();
        for(auto& [i,a,l]: alignments)
            model.perform({var("$"),var("addLogger"),{{var("$"),{var("every"),10},{l,a}}}});
    }

    model.empty_stmt();
    model.finish_return( loggers_var );
    program_file<<"\n";
    program_file<<model_fn<<" = "<<model.get_expression().print()<<"\n";

    auto main = generate_main(args,
			      filename_ranges,
			      alphabet_exps,
			      partition_group,
			      partition_group_size,
			      unaligned_sequence_data,
			      aligned_sequence_data,
			      tree,
			      topology,
			      tsvLogger,
			      jsonLogger,
			      treeLogger,
			      model_fn,
			      alignments);

    program_file<<"\nmain = "<<main.get_expression().print()<<"\n";

    return program_file.str();
}

std::unique_ptr<Program>
gen_atmodel_program(const boost::program_options::variables_map& args,
		    const std::shared_ptr<module_loader>& L,
		    const fs::path& output_directory,
		    const fs::path& program_filename,
		    const vector<expression_ref>& alphabet_exps,
		    const vector<pair<fs::path,string>>& filename_ranges,
		    int n_leaves,
		    const model_t& decls,
		    const vector<model_t>& SMs,
		    const vector<optional<int>>& s_mapping,
                    const vector<string>& s_conditions,
		    const vector<model_t>& IMs,
		    const vector<optional<int>>& i_mapping,
		    const vector<model_t>& scaleMs,
		    const vector<optional<int>>& scale_mapping,
		    const model_t& tree_model,
		    const model_t& subst_rates_model,
		    const model_t& indel_rates_model,
		    const std::vector<int>& like_calcs)
{
    // FIXME! Make likelihood_calculators for 1- and 2-sequence alignments handle compressed alignments.
    {
        checked_ofstream program_file(program_filename);
        program_file<<generate_atmodel_program(args,
                                               n_leaves,
                                               alphabet_exps,
                                               filename_ranges,
					       decls,
                                               SMs, s_mapping, s_conditions,
                                               IMs, i_mapping,
                                               scaleMs, scale_mapping,
                                               tree_model,
                                               subst_rates_model,
                                               indel_rates_model,
                                               like_calcs);
    }

    auto m = L->load_module_from_file(program_filename);
    auto P = std::make_unique<Program>(L,vector{m}, "Main.main");
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
    program_file<<"  model <- Model.main "<<output_directory<<"\n";
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

std::unique_ptr<Program>
gen_model_program(const boost::program_options::variables_map& args,
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
    auto model_module = L->load_module_from_file(model_filepath);
    if (model_module->name == "Main")
        throw myexception()<<"The module name for the model file "<<model_filepath<<" may not be 'Main'\n";

    // 4. Generate and write the Main module.
    auto program_filepath = output_directory / main_file;
    {
        checked_ofstream program_file(program_filepath);
        program_file<<generate_model_program(args, model_module->name, output_directory);
    }

    // 5. Load the Main module.
    auto main_module = L->load_module_from_file(program_filepath);

    return std::make_unique<Program>(L, vector{model_module, main_module}, "Main.main");
}
