#include "A-T-prog.H"

#include <set>
#include <map>

#include "util/set.H"
#include "util/io.H"
#include "util/string/split.H"
#include "util/string/convert.H"
#include "util/settings.H"    // for get_setting_or( )
#include "models/compile.H"   // for model_t
#include "models/parse.H"   // for unparse_type

#include "computation/loader.H"
#include "computation/haskell/generated.H"
#include "computation/haskell/var.H"
#include "computation/module.H"

#include "sequence/genetic_code.H"
#include "sequence/codons.H"
#include "sequence/doublets.H"
#include "sequence/RNAEdits.H"
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
namespace HsG = Haskell::Generated;

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

Hs::Exp get_genetic_code_expression(const Genetic_Code& code)
{
    return HsG::Apply(Hs::Var("geneticCode"), {Hs::Literal(Hs::String(code.name()))});
}

Hs::Exp get_alphabet_expression(const alphabet& a)
{
    if (a.name == "DNA")
        return Hs::Var("dna");
    else if (a.name == "RNA")
        return Hs::Var("rna");
    else if (a.name == "Amino-Acids")
        return Hs::Var("aa");
    else if (auto codons = dynamic_cast<const Codons*>(&a))
    {
        auto nucs = get_alphabet_expression(codons->getNucleotides());
        auto code = get_genetic_code_expression(codons->getGenetic_Code());
        return HsG::Apply(Hs::Var("mkCodons"), {nucs, code});
    }
    else if (auto triplets = dynamic_cast<const Triplets*>(&a))
    {
        auto nucs = get_alphabet_expression(triplets->getNucleotides());
        return HsG::Apply(Hs::Var("mkTriplets"), {nucs});
    }
    else if (auto doublets = dynamic_cast<const Doublets*>(&a))
    {
        auto nucs = get_alphabet_expression(doublets->getNucleotides());
        return HsG::Apply(Hs::Var("mkDoublets"), {nucs});
    }
    else if (auto doublets = dynamic_cast<const RNAEdits*>(&a))
    {
        auto nucs = get_alphabet_expression(doublets->getNucleotides());
        return HsG::Apply(Hs::Var("mkRNAEdits"), {nucs});
    }
    else if (auto num = dynamic_cast<const Numeric*>(&a))
    {
	int n = num->size();
	return HsG::Apply(Hs::Var("mkNumeric"), {Hs::Literal(Hs::Integer{integer(n)})});
    }
    else
    {
	throw myexception()<<"Can't translate C++ alphabet object "<<a.name<<" into Haskell expression";
    }
}

// Emits the generated binding needed for an action or pure expression result.
static void perform_action_simplified(Hs::Stmts& block, const Hs::Var& x, const Hs::Var& log_x, bool is_referenced, Hs::Exp E, bool is_action, bool has_loggers)
{
    if (is_action)
    {
        if (not has_loggers)
            // x <- code
            HsG::Bind(block, HsG::VarPat(x), E);
        else
            // (x, log_x) <- code
            HsG::Bind(block, HsG::TuplePat({HsG::VarPat(x), HsG::VarPat(log_x)}), E);
    }
    else
    {
        if (has_loggers)
            HsG::Let(block, HsG::TuplePat({HsG::VarPat(x), HsG::VarPat(log_x)}), E);
        else if (is_referenced)
            HsG::Let(block, x, E);
    }
}

// Binds a generated result and appends its logger expression when requested.
Hs::Var bind_and_log(bool do_log, const Hs::Var& x, const Hs::Var& log_x, const string& name, const Hs::Exp& E, bool is_action, bool has_loggers, Hs::Stmts& block, vector<Hs::Exp>& loggers, bool is_referenced=true)
{
    perform_action_simplified(block, x, log_x, is_referenced, E, is_action, has_loggers);
    maybe_log(loggers, name, do_log ? Hs::Exp(x) : Hs::Exp{}, has_loggers ? Hs::Exp(log_x) : Hs::Exp{});
    return x;
}

// Chooses the generated variable names before binding and logging a result.
Hs::Var bind_and_log(bool do_log, const string& name, const Hs::Exp& E, bool is_action, bool has_loggers, Hs::Stmts& block, vector<Hs::Exp>& loggers, bool is_referenced=true)
{
    string var_name = name;
    if (var_name.empty() or not std::islower(var_name[0]))
        var_name = "_"+var_name;
    Hs::Var x(var_name);
    Hs::Var log_x("log_"+name);
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
        auto code = print_generated_function_decl("_generated_model", models[i].code.generate());
        if (not functions.count(code))
            functions.insert({code,functions.size()});
    }
    int printed = 0;
    for(int i=0;i<models.size();i++)
    {
        auto code = print_generated_function_decl("_generated_model", models[i].code.generate());
        int index = functions.at(code);
        string name = tag;
        if (functions.size() > 1) name += "_"+std::to_string(index+1);
        function_for_index.push_back(name);
        if (index >= printed)
        {
            file<<print_generated_function_decl(name, models[i].code.generate())<<"\n\n";
            printed++;
        }
    }
    return function_for_index;
}

vector<Hs::Exp> generate_scale_models(const vector<model_t>& scaleMs,
					     const vector<string>& scaleM_function_for_index,
					     const Hs::Exp& tree_var,
					     Hs::Stmts& model,
					     vector<Hs::Exp>& model_loggers)
{
    // define tree_length
    Hs::Var tree_length_var("tlength");
    HsG::Let(model, tree_length_var, HsG::Apply(Hs::Var("treeLength"), {tree_var}));
    // log |T|
    maybe_log(model_loggers, "|T|", tree_length_var, {});

    vector<Hs::Exp> scales;

    for(int i=0; i<scaleMs.size(); i++)
    {
	// FIXME: Ideally we would actually join these models together using a Cons operation and prefix.
	//        This would obviate the need to create a Scale1 (etc) prefix here.
	string var_name = "scale" + convertToString(i+1);

	auto code = scaleMs[i].code;
	Hs::Exp E = Hs::Var(scaleM_function_for_index[i]);
	E = code.add_arguments(E, {});

	// This should still log sub-loggers of the scales, I think.
	auto scale_var = bind_and_log(false, var_name, E, code.is_action(), code.has_loggers(), model, model_loggers);

	scales.push_back(scale_var);

	// log scale[i]
	maybe_log(model_loggers, var_name, scale_var, {});

	// log scale[i]*|T|
	maybe_log(model_loggers, var_name+"*|T|", HsG::Apply(Hs::Var("*"), {scale_var, tree_length_var}), {});
    }

    return scales;
}

vector<Hs::Exp> generate_substitution_models(const vector<model_t>& SMs,
						    const vector<optional<int>>& s_mapping,
						    const vector<string>& SM_function_for_index,
						    const vector<Hs::Exp>& alphabet_exps,
						    const Hs::Exp& branch_categories,
						    const Hs::Exp& tree,
						    Hs::Stmts& model,
						    vector<Hs::Exp>& model_loggers)
{
    // M7. Substitution models
    vector<Hs::Exp> smodels;
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

        Hs::Exp smodel = Hs::Var(SM_function_for_index[i]);
        smodel = code.add_arguments(smodel, {
                {"alphabet",alphabet_exps[*first_partition]},
                {"branch_categories",branch_categories},
                {"tree",tree}
            });

        auto smodel_var = Hs::Var("smodel" + suffix);
        auto log_smodel = Hs::Var("log_"+smodel_var.name);
        bind_and_log(false, smodel_var, log_smodel, prefix, smodel, code.is_action(), code.has_loggers(), model, model_loggers);
        smodels.push_back(smodel_var);
    }
    return smodels;
}

vector<Hs::Exp> generate_indel_models(const vector<model_t>& IMs,
					     const vector<string>& IM_function_for_index,
					     const Hs::Exp& tree_var,
					     Hs::Stmts& model,
					     vector<Hs::Exp>& model_loggers)
{
    // M8. Indel models
    vector<Hs::Exp> imodels;
    for(int i=0;i<IMs.size();i++)
    {
        string prefix = "I" + convertToString(i+1);
        string _suffix = (IMs.size()>1)?"_"+convertToString(i+1):"";
        string suffix = (IMs.size()>1)?convertToString(i+1):"";

        auto code = IMs[i].code;

        Hs::Exp imodel = Hs::Var(IM_function_for_index[i]);
        imodel = code.add_arguments(imodel, {{"topology",tree_var}});

        auto imodel_var = Hs::Var("imodel" + suffix);
        auto log_imodel = Hs::Var("log_"+imodel_var.name);
        bind_and_log(false, imodel_var, log_imodel, prefix, imodel, code.is_action(), code.has_loggers(), model, model_loggers);
        imodels.push_back(imodel_var);
    }
    return imodels;
}

Hs::Stmts generate_main(const variables_map& args,
		       const vector<pair<fs::path,string>>& filename_ranges,
		       const vector<Hs::Exp>& alphabet_exps,
		       const vector<int>& partition_group,
		       const vector<int>& partition_group_size,
		       const Hs::Var& unaligned_sequence_data,
		       const Hs::Var& aligned_sequence_data,
		       const Hs::Var& tree,
		       const Hs::Var& topology,
		       const Hs::Var& tsvLogger,
		       const Hs::Var& jsonLogger,
		       const Hs::Var& treeLogger,
		       const Hs::Exp& model_fn,
		       vector<tuple<int,Hs::Exp,Hs::Exp>>& alignment_loggers,
		       vector<tuple<int,Hs::Exp,Hs::Exp>>& category_state_loggers)
{
    auto fixed = get_fixed(args);

    auto log_formats = get_log_formats(args, args.count("align"));

    int n_partitions = filename_ranges.size();

    Hs::Stmts main;

    Hs::Var directory("directory");
    if (not args.count("test"))
        HsG::Bind(main, HsG::ListPat({HsG::VarPat(directory)}), Hs::Var("getArgs"));

    auto unaligned_partitions = unaligned_sequence_data;
    auto aligned_partitions = aligned_sequence_data;
    if (n_partitions == 1)
    {
        auto [filename, range] = filename_ranges[0];

	// Load the sequences
	Hs::Exp E = HsG::Apply(Hs::Var("loadSequences"), {Hs::Literal(Hs::String(filename.string()))});

	// Select range
	if (not range.empty())
            E = HsG::Apply(Hs::Var("<$>"), {HsG::Apply(Hs::Var("selectRange"), {Hs::Literal(Hs::String(range))}), E});

	// Convert to CharacterData
	if (partition_group[0] == 0)
	    E = HsG::Apply(Hs::Var("<$>"), {HsG::Apply(Hs::Var("mkUnalignedCharacterData"), {alphabet_exps[0]}), E});
	else
	    E = HsG::Apply(Hs::Var("<$>"), {HsG::Apply(Hs::Var("mkAlignedCharacterData"), {alphabet_exps[0]}), E});

        HsG::Bind(main, HsG::VarPat(Hs::Var("sequenceData")), E);
    }
    else
    {
        // Main.1: Emit let filenames = ...
        Hs::Var filenames_var("filenames");
        map<fs::path,int> index_for_filename;
        {
            vector<Hs::Exp> filenames_;
            for(auto& [filename,range]: filename_ranges)
            {
                if (not index_for_filename.count(filename))
                {
                    index_for_filename.insert({filename,filenames_.size()});
                    filenames_.push_back(Hs::Literal(Hs::String(filename.string())));
                }
            }
            HsG::Let(main, filenames_var, HsG::List(filenames_));
        }

        {
            // Main.2: Emit let filenames_to_seqs = ...
            Hs::Var filename_to_seqs("seqs");
            {
                HsG::Bind(main, HsG::VarPat(filename_to_seqs), HsG::Apply(Hs::Var("mapM"), {Hs::Var("loadSequences"), filenames_var}));
            }

            // Main.3. Emit let sequence_data<n> = 
            vector<Hs::Exp> unaligned_sequence_partitions;
            vector<Hs::Exp> aligned_sequence_partitions;
            for(int i=0;i<n_partitions;i++)
            {
		int group = partition_group[i];
                string part = std::to_string(i+1);

                Hs::Var partition_sequence_data_var("sequenceData"+part);
		if (partition_group_size[group] == 1)
		    partition_sequence_data_var = (group==0) ? unaligned_sequence_data : aligned_sequence_data;

                int index = index_for_filename.at( filename_ranges[i].first );
                Hs::Exp loaded_sequences = HsG::Apply(Hs::Var("!!"), {filename_to_seqs, Hs::Literal(Hs::Integer{integer(index)})});
                if (not filename_ranges[i].second.empty())
                    loaded_sequences = HsG::Apply(Hs::Var("selectRange"), {Hs::Literal(Hs::String(filename_ranges[i].second)), loaded_sequences});
		if (partition_group[i] == 0)
		{
		    loaded_sequences = HsG::Apply(Hs::Var("mkUnalignedCharacterData"), {alphabet_exps[i], loaded_sequences});
		    unaligned_sequence_partitions.push_back(partition_sequence_data_var);
		}
		else
		{
		    loaded_sequences = HsG::Apply(Hs::Var("mkAlignedCharacterData"), {alphabet_exps[i], loaded_sequences});
		    aligned_sequence_partitions.push_back(partition_sequence_data_var);
		}
                HsG::Let(main, partition_sequence_data_var, loaded_sequences);
            }

            // Main.4. Emit let sequence_data = ...
	    if (unaligned_sequence_partitions.size() > 1)
		HsG::Let(main, unaligned_partitions, HsG::List(unaligned_sequence_partitions));

	    if (aligned_sequence_partitions.size() > 1)
		HsG::Let(main, aligned_partitions, HsG::List(aligned_sequence_partitions));
        }
    }

    if (fixed.count("tree"))
    {
        auto tree_filename = fixed.at("tree");
        HsG::Bind(main, HsG::VarPat(tree), HsG::Apply(Hs::Var("<$>"), {Hs::Var("dropInternalLabels"), HsG::Apply(Hs::Var("readBranchLengthTree"), {Hs::Literal(Hs::String(tree_filename))})}));
    }
    else if (fixed.count("topology"))
    {
        auto tree_filename = fixed.at("topology");
        HsG::Bind(main, HsG::VarPat(topology), HsG::Apply(Hs::Var("<$>"), {Hs::Var("dropInternalLabels"), HsG::Apply(Hs::Var("readTreeTopology"), {Hs::Literal(Hs::String(tree_filename))})}));
    }

    if (not args.count("test"))
    {
        // Initialize the parameters logger
	if (log_formats.count("tsv"))
	{
	    HsG::Bind(main, HsG::VarPat(tsvLogger), HsG::Apply(Hs::Var("tsvLogger"), {HsG::Apply(Hs::Var("</>"), {directory, Hs::Literal(Hs::String("C1.log"))}), HsG::List({Hs::Literal(Hs::String("iter"))})}));
	}

	if (log_formats.count("json"))
	{
	    HsG::Bind(main, HsG::VarPat(jsonLogger), HsG::Apply(Hs::Var("jsonLogger"), {HsG::Apply(Hs::Var("</>"), {directory, Hs::Literal(Hs::String("C1.log.json"))})}));
	}

        // Initialize the tree logger
        if (not fixed.count("tree"))
        {
            HsG::Bind(main, HsG::VarPat(treeLogger), HsG::Apply(Hs::Var("treeLogger"), {HsG::Apply(Hs::Var("</>"), {directory, Hs::Literal(Hs::String("C1.trees"))})}));
        }

        // Initialize the alignment loggers
        if (not alignment_loggers.empty())
        {
            // Create alignment loggers.
            for(auto& [i,a,logger]: alignment_loggers)
            {
                string filename = "C1.P"+std::to_string(i+1)+".fastas";
                HsG::Bind(main, HsG::VarPat(logger.as_<Hs::Var>()), HsG::Apply(Hs::Var("alignmentLogger"), {HsG::Apply(Hs::Var("</>"), {directory, Hs::Literal(Hs::String(filename))})}));
            }
        }

        // Initialize the category-state loggers
        if (not category_state_loggers.empty())
        {
            // Create alignment loggers.
            for(auto& [i, cs, logger]: category_state_loggers)
            {
                string filename = "C1.catStates"+std::to_string(i+1)+".json";
                HsG::Bind(main, HsG::VarPat(logger.as_<Hs::Var>()), HsG::Apply(Hs::Var("ejsonLogger"), {HsG::Apply(Hs::Var("</>"), {directory, Hs::Literal(Hs::String(filename))})}));
            }
        }
    }

    // Main.5. Emit mymodel <- makeMCMCModel $ model sequence_data
    HsG::Bind(main, HsG::VarPat(Hs::Var("mymodel")), HsG::Apply(Hs::Var("$"), {Hs::Var("makeMCMCModel"), model_fn}));

    // Main.6. Emit runMCMC iterations mymodel
    if (args.count("test"))
    {
        if (log_formats.count("tsv"))
        {
            HsG::Bind(main, HsG::VarPat(Hs::Var("line")), HsG::Apply(Hs::Var("logTableLine"), {Hs::Var("mymodel"), Hs::Literal(Hs::Integer{integer(0)})}));
            HsG::Expr(main, HsG::Apply(Hs::Var("T.putStrLn"), {Hs::Var("line")}));
        }

        if (log_formats.count("json"))
        {
            HsG::Bind(main, HsG::VarPat(Hs::Var("jline")), HsG::Apply(Hs::Var("logJSONLine"), {Hs::Var("mymodel"), Hs::Literal(Hs::Integer{integer(0)})}));
            HsG::Expr(main, HsG::Apply(Hs::Var("T.putStrLn"), {Hs::Var("jline")}));
        }

        if (args.count("verbose"))
        {
            HsG::Expr(main, HsG::Apply(Hs::Var("writeTraceGraph"), {Hs::Var("mymodel")}));
//            M->write_factor_graph();
        }
    }
    else
    {
        // int subsample = args["subsample"].as<int>();
        int max_iterations = 200000;
        if (args.count("iterations"))
            max_iterations = args["iterations"].as<long int>();
        HsG::Expr(main, HsG::Apply(Hs::Var("runMCMC"), {Hs::Literal(Hs::Integer{integer(max_iterations)}), Hs::Var("mymodel")}));
    }

    return main;
}


void write_header(std::ostream& program_file,
		  const model_t& decls,
		  const vector<model_t>& SMs,
		  const vector<model_t>& IMs,
		  const vector<model_t>& scaleMs,
		  const model_t& subst_rates_model,
		  const model_t& indel_rates_model,
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
    add(imports, subst_rates_model.imports);
    add(imports, indel_rates_model.imports);
    add(imports, tree_model.imports);

    program_file<<"{-# LANGUAGE ExtendedDefaultRules #-}\n";
    program_file<<"module Main where";
    for(auto& mod: imports)
        program_file<<"\nimport "<<mod;
    program_file<<"\nimport qualified Data.IntMap as IntMap";
    program_file<<"\nimport qualified Data.JSON as J";
    program_file<<"\nimport Data.JSON ((.=))";
    program_file<<"\nimport qualified Data.Text.IO as T";
    program_file<<"\nimport Probability.Logger";
    program_file<<"\nimport System.Environment";
    program_file<<"\nimport System.FilePath";
}

vector<Hs::Exp>
compute_logged_quantities(Hs::Stmts& model,
			  int n_branches,
			  int n_partitions,
			  const map<string,string>& fixed,
			  int i,
			  const Hs::Exp& tree,
			  const Hs::Exp& alignment_on_tree,
			  const Hs::Exp& properties,
			  const Hs::Exp& alphabet_exp,
			  const Hs::Exp& sequence_data,
			  const Hs::Exp& smodel,
			  std::optional<int> imodel_index,
			  vector<Hs::Exp>& alignment_lengths,
			  vector<Hs::Exp>& total_num_indels,
			  vector<Hs::Exp>& total_length_indels,
			  vector<Hs::Exp>& total_substs,
			  vector<Hs::Exp>& total_prior_A,
			  vector<tuple<int,Hs::Exp,Hs::Exp>>& alignment_loggers,
			  vector<tuple<int,Hs::Exp,Hs::Exp>>& category_state_loggers)
{
    string part = std::to_string(i+1);
    string part_suffix = (n_partitions>1) ? part : "";

    vector<Hs::Exp> sub_loggers;
    if (imodel_index)
    {
	Hs::Var alignment_length("alignment_length"+part_suffix);
	HsG::Let(model, alignment_length, HsG::Apply(Hs::Var("alignmentLength"), {alignment_on_tree}));
	alignment_lengths.push_back(alignment_length);

	if (n_branches > 0)
	{
	    Hs::Var num_indels("num_indels"+part_suffix);
	    HsG::Let(model, num_indels, HsG::Apply(Hs::Var("totalNumIndels"), {alignment_on_tree}));
	    total_num_indels.push_back(num_indels);
	    Hs::Var length_indels("total_length_indels"+part_suffix);
	    HsG::Let(model, length_indels, HsG::Apply(Hs::Var("totalLengthIndels"), {alignment_on_tree}));
	    total_length_indels.push_back(length_indels);

	    maybe_log(sub_loggers, "|A|", alignment_length, {});
	    maybe_log(sub_loggers, "#indels", num_indels, {});
	    maybe_log(sub_loggers, "|indels|", length_indels, {});
	}

	if (not fixed.count("alignment"))
	{
	    Hs::Var properties_A("properties_A"+part_suffix);
	    Hs::Var prior_A("prior_A" + part_suffix);
	    HsG::Let(model, prior_A, HsG::Apply(Hs::Var("ln"), {HsG::Apply(Hs::Var("probability"), {properties_A})}));
	    total_prior_A.push_back(prior_A);
	    maybe_log(sub_loggers, "prior_A", prior_A, {});
	}
    }
    else
    {
	// For fixed-alignment partitions, the alignment length comes from the observed-data matrix.
	Hs::Var alignment_length("alignment_length"+part_suffix);
	HsG::Let(model, alignment_length, HsG::Apply(Hs::Var("alignmentLength"), {sequence_data}));
	alignment_lengths.push_back(alignment_length);
    }

    maybe_log(sub_loggers, "likelihood", HsG::Apply(Hs::Var("ln"), {HsG::Apply(Hs::Var("prop_likelihood"), {properties})}), {});

    if (n_branches > 0)
    {
        std::optional<Hs::Var> anc_states;
	if (imodel_index or get_setting_or("write-fixed-alignments",false) or get_setting_or("write-properties", false))
        {
            anc_states = Hs::Var("ancStates" + part_suffix);
            HsG::Let(model, *anc_states, HsG::Apply(Hs::Var("prop_anc_cat_states"), {properties}));
        }
        
	if (imodel_index or get_setting_or("write-fixed-alignments",false))
	{
            // FIXME: This should affect whether we allow modifying leaf sequences.
	    // bool infer_ambiguous_observed = get_setting_or(keys, "infer-ambiguous-observed",false);

            // Get the alignment variable
            auto alignment = alignment_on_tree;
            if (not imodel_index)
            {
                Hs::Var fixed_alignment("alignment" + part_suffix);
                alignment = fixed_alignment;
                HsG::Let(model, fixed_alignment, HsG::Apply(Hs::Var("leafAlignment"), {tree, sequence_data}));
            }

	    Hs::Var anc_alignment("ancAlignment"+part_suffix);
	    HsG::Let(model, anc_alignment, HsG::Apply(Hs::Var("toFasta"), {HsG::Apply(Hs::Var("ancestralAlignment"), {tree, alignment, HsG::Apply(Hs::Var("getSMap"), {smodel}), alphabet_exp, *anc_states})}));
            alignment_loggers.push_back({i, anc_alignment, Hs::Var("logA"+part_suffix)});
	}

        if (get_setting_or("write-properties", false))
        {
            Hs::Var cat_states("catStates" + part_suffix);
            HsG::Let(model, cat_states, HsG::Apply(Hs::Var("labeledNodeMap"), {tree, *anc_states}));
            Hs::Exp smodel_properties = HsG::Apply(Hs::Var("prop_smodel_properties"), {properties});
            Hs::Exp cat_states_key = HsG::Apply(Hs::Var("J.toJSONKey"), {Hs::Literal(Hs::String("catStates"))});
            Hs::Exp properties_key = HsG::Apply(Hs::Var("J.toJSONKey"), {Hs::Literal(Hs::String("properties"))});
            Hs::Exp cat_state_fields = HsG::Apply(Hs::Var("<>"),
                {HsG::Apply(Hs::Var(".="), {cat_states_key, cat_states}),
                 HsG::Apply(Hs::Var(".="), {properties_key, smodel_properties})});
            Hs::Exp cat_state_encoding = HsG::Apply(Hs::Var("J.pairs"), {cat_state_fields});
            category_state_loggers.push_back({i, cat_state_encoding, Hs::Var("logCatStates"+part_suffix)});
	}
        
	Hs::Var substs("substs"+part_suffix);
	Hs::Exp costs = HsG::Apply(Hs::Var("unitCostMatrix"), {alphabet_exp});
	Hs::Exp aligned_data = sequence_data;
	if (imodel_index)
	    aligned_data = HsG::Tuple({sequence_data, alignment_on_tree});

	HsG::Let(model, substs, HsG::Apply(Hs::Var("parsimony"), {tree, costs, aligned_data}));
	maybe_log(sub_loggers, "#substs", substs, {});
	if (alphabet_exp.print().starts_with("mkRNA"))
	{
	    string suffix = part_suffix;
	    if (not suffix.empty())
		suffix = "_"+suffix;

	    Hs::Var substs_pos2("substsRNA"+suffix);
	    Hs::Exp costs_pos2 = HsG::Apply(Hs::Var("pos2CostMatrix"), {alphabet_exp});
	    HsG::Let(model, substs_pos2, HsG::Apply(Hs::Var("parsimony"), {tree, costs_pos2, aligned_data}));
	    maybe_log(sub_loggers, "#substsRNA", substs_pos2, {});
	}

	total_substs.push_back(substs);
    }

    return sub_loggers;
}

bool is_reversible(const type_t& t)
{
    return true;

    if (get_type_head(t) == "RevCTMC")
	return true;
    else if (get_type_head(t) == "CTMC")
	return false;
    else if (get_type_head(t) == "DiscreteDist")
    {
        auto [head,args] = get_type_apps(t);
	return is_reversible(args[0]);
    }
    else if (get_type_head(t) == "MultiMixtureModel")
    {
        auto [head,args] = get_type_apps(t);
	return is_reversible(args[0]);
    }
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
                                     const vector<Hs::Exp>& alphabet_exps,
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
    write_header(program_file, decls, SMs, IMs, scaleMs, subst_rates_model, indel_rates_model, tree_model);
    program_file<<"\n\n";

    auto SM_function_for_index = print_models("sample_smodel", SMs, program_file);
    auto IM_function_for_index = print_models("sample_imodel", IMs, program_file);
    vector<string> scaleM_function_for_index;
    if (n_branches > 0)
        scaleM_function_for_index = print_models("sample_scale", scaleMs, program_file);

    // F5. Topology / Tree
    if (not fixed.count("tree"))
	program_file<<print_generated_function_decl("sampleTree", tree_model.code.generate())<<"\n";

    /* --------------------------------------------------------------- */
    Hs::Stmts model;

    // FIXME: We can't load the alignments to read their names until we know the alphabets!
    // FIXME: Can we load the alignments as SEQUENCES first?
    Hs::Var taxon_names_var("taxa");

    // Loggers = [(string,(Maybe a,Loggers)]
    vector<Hs::Exp> model_loggers;
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

    Hs::Var unaligned_sequence_data("sequenceData");
    Hs::Var aligned_sequence_data("sequenceData");

    if (partition_group_size[0] > 0 and partition_group_size[1] > 0)
    {
	unaligned_sequence_data = Hs::Var("unalignedSequenceData");
	aligned_sequence_data = Hs::Var("alignedSequenceData");
    }

    auto getSequenceData = [&](int p) -> Hs::Exp
    {
	assert(n_partitions > 0);

	int group = partition_group[p];

	auto sequenceData = (group == 0) ? unaligned_sequence_data : aligned_sequence_data;

	// Only subscript if the group contains more than one element.
	if (partition_group_size[group] == 1)
	    return sequenceData;
	else
	    return HsG::Apply(Hs::Var("!!"), {sequenceData, Hs::Literal(Hs::Integer{integer(partition_index[p])})});
    };

    if (n_partitions > 0)
    {
        HsG::Let(model, taxon_names_var, HsG::Apply(Hs::Var("getTaxa"), {getSequenceData(0)}));
    }

    // We could fix the whole tree or just the topology.
    Hs::Exp branch_lengths = Hs::Var("IntMap.empty");

    for(auto& stmt: decls.code.stmts.stmts)
	model.stmts.push_back(stmt);
    auto decl_loggers = decls.code.loggers;
    simplify(decl_loggers);
    for(auto& logger: generate_loggers(model, decl_loggers))
	model_loggers.push_back(logger);

    // M4. Branch-length tree
    auto tree_var = Hs::Var("tree");
    if (not fixed.count("tree"))
    {
        string var_name = "tree";

        auto code = tree_model.code;

        Hs::Exp E = Hs::Var("sampleTree");
        E = code.add_arguments(E,{{"taxa",taxon_names_var}});

        tree_var = bind_and_log(false, var_name, E, code.is_action(), code.has_loggers(), model, model_loggers);
        branch_lengths = HsG::Apply(Hs::Var("branchLengths"), {tree_var});
    }

    Hs::Exp subst_tree=tree_var;
    auto subst_rates_var = Hs::Var("substRates");
    if (not subst_rates_model.empty())
    {
        string var_name = "substRates";
        auto code = subst_rates_model.code;
        code.haskell_lambda_vars.clear(); // This isn't a function, these vars should be in scope.
        subst_rates_var = bind_and_log(false, var_name, code.generate(), code.is_action(), code.has_loggers(), model, model_loggers);

        auto subst_tree_var = Hs::Var("substTree");
        HsG::Let(model, subst_tree_var, HsG::Apply(Hs::Var("addBranchRates"), {Hs::Var("substRates"), tree_var}));
        subst_tree = subst_tree_var;
    }

    Hs::Exp indel_tree=tree_var;
    auto indel_rates_var = Hs::Var("indelRates");
    if (not indel_rates_model.empty())
    {
        string var_name = "indelRates";
        auto code = indel_rates_model.code;
        code.haskell_lambda_vars.clear(); // This isn't a function, these vars should be in scope.
        indel_rates_var = bind_and_log(false, var_name, code.generate(), code.is_action(), code.has_loggers(), model, model_loggers);

        auto indel_tree_var = Hs::Var("indelTree");
        HsG::Let(model, indel_tree_var, HsG::Apply(Hs::Var("addBranchRates"), {Hs::Var("indelRates"), tree_var}));
        indel_tree = indel_tree_var;
    }

    set<string> used_states;
    for(int i=0;i<SMs.size();i++)
        add(used_states, SMs[i].code.used_states);

    // M5. Branch categories
    Hs::Exp branch_categories;
    if (used_states.count("branch_categories"))
    {
        Hs::Var branch_categories_var("branch_categories");
        HsG::Let(model, branch_categories_var, HsG::Apply(Hs::Var("foregroundBranches"), {tree_var, Hs::Literal(Hs::String("foreground"))}));
        branch_categories = branch_categories_var;
    }

    // M6. Scales
    vector<Hs::Exp> scales;
    if (n_branches > 0)
    {
	scales = generate_scale_models(scaleMs, scaleM_function_for_index, tree_var, model, model_loggers);

        if (not fixed.count("tree"))
        {
            HsG::Expr(model, HsG::Apply(Hs::Var("addMove"), {Hs::Literal(Hs::Integer{integer(2)}), HsG::Apply(Hs::Var("scaleGroupsSlice"), {HsG::List(scales), branch_lengths})}));
	    HsG::Expr(model, HsG::Apply(Hs::Var("addMove"), {Hs::Literal(Hs::Integer{integer(1)}), HsG::Apply(Hs::Var("scaleGroupsMH"), {HsG::List(scales), branch_lengths})}));
        }
    }

    auto smodels = generate_substitution_models(SMs, s_mapping, SM_function_for_index, alphabet_exps, branch_categories, tree_var, model, model_loggers);
    auto imodels = generate_indel_models(IMs, IM_function_for_index, tree_var, model, model_loggers);

    vector<tuple<int,Hs::Exp,Hs::Exp>> alignment_loggers; // partition, alignment var, alignment logger
    vector<tuple<int,Hs::Exp,Hs::Exp>> category_state_loggers; // partition, category-state var, category-state logger
    vector<Hs::Exp> alignment_lengths;
    vector<Hs::Exp> total_num_indels;
    vector<Hs::Exp> total_length_indels;
    vector<Hs::Exp> total_substs;
    vector<Hs::Exp> total_prior_A;
    vector<Hs::Exp> partition_scales;

    for(int i=0; i < n_partitions; i++)
    {
        string part = std::to_string(i+1);
        string part_suffix = (n_partitions>1) ? part : "";
        int scale_index = *scale_mapping[i];
        int smodel_index = *s_mapping[i];
        auto imodel_index = i_mapping[i];
        Hs::Exp smodel = smodels[smodel_index];
        Hs::Exp sequence_data_var = getSequenceData(i);

        // Model.Partition.1. tree_part<i> = scale_branch_lengths scale tree
	Hs::Exp scale = Hs::Literal(Hs::Integer{integer(1)});
        if (n_branches > 0)
            scale = scales[scale_index];
	partition_scales.push_back(scale);
	
        // Model.Partition.2. Sample the alignment
        Hs::Var alignment_on_tree("alignment" + part_suffix);
        if (imodel_index)
        {
            assert(like_calcs[i] == 0);
            Hs::Exp imodel = imodels[*imodel_index];

            if (fixed.count("alignment"))
            {
                HsG::Let(model, alignment_on_tree, HsG::Apply(Hs::Var("alignmentOnTreeFromSequences"), {tree_var, sequence_data_var}));
            }
            else
            {
                Hs::Var leaf_sequence_lengths("sequence_lengths" + part_suffix);
                HsG::Let(model, leaf_sequence_lengths, HsG::Apply(Hs::Var("getSequenceLengths"), {sequence_data_var}));

                Hs::Var properties_A("properties_A"+part_suffix);
		HsG::Bind(model,
                          HsG::TuplePat({HsG::VarPat(alignment_on_tree), HsG::VarPat(properties_A)}),
                          HsG::Apply(Hs::Var("sampleWithProps"), {HsG::Apply(Hs::Var("phyloAlignment"), {indel_tree, imodel, scale, leaf_sequence_lengths})}));
            }
        }

        // Model.Partition.3. Observe the sequence data from the distribution
        Hs::Exp distribution;
        string s_condition = s_conditions[smodel_index];
        if (like_calcs[i] == 0)
        {
            assert(s_condition.empty());
            distribution = HsG::Apply(Hs::Var("phyloCTMC"), {subst_tree, alignment_on_tree, smodel, scale});
        }
        else
	{
	    Hs::Exp alignment_length = HsG::Apply(Hs::Var("alignmentLength"), {sequence_data_var});
            distribution = HsG::Apply(Hs::Var("phyloCTMC"), {subst_tree, alignment_length, smodel, scale});
            if (not s_condition.empty())
            {
                if (s_condition == "variable")
                    distribution = HsG::Apply(Hs::Var("variable"), {distribution});
                else
                    throw myexception()<<"Unrecognized ascertainment condition '"<<s_condition<<"'";
            }
	}
	Hs::Var properties("properties"+part_suffix);
	Hs::Exp sequence_data = sequence_data_var;
	if (fixed.contains("alignment") and i_mapping[i])
	    sequence_data = HsG::Apply(Hs::Var("unalign"), {sequence_data});
	HsG::Bind(model, HsG::VarPat(properties), HsG::Apply(Hs::Var("observe"), {sequence_data, distribution}));

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
						     alignment_loggers,
						     category_state_loggers);

        Hs::Var part_loggers("part"+part+"Loggers");
        HsG::Let(model, part_loggers, HsG::List(sub_loggers));
        maybe_log(model_loggers, "P"+part, {}, part_loggers);
    }
    bool has_a_variable_alignment = not total_num_indels.empty();
    HsG::Let(model, Hs::Var("alignmentLengths"), HsG::List(alignment_lengths));
    if (n_branches > 0)
    {
	if (n_partitions > 1)
	{
	    HsG::Let(model, Hs::Var("scales"), HsG::List(partition_scales));
	    Hs::Exp a_lengths = HsG::Apply(Hs::Var("fmap"), {Hs::Var("fromIntegral"), Hs::Var("alignmentLengths")});
	    HsG::Let(model, Hs::Var("scale"), HsG::Apply(Hs::Var("weightedAverage"), {a_lengths, Hs::Var("scales")}));
	}
	else
	    HsG::Let(model, Hs::Var("scale"), Hs::Var("scale1"));
	maybe_log(model_loggers, "scale", Hs::Var("scale"), {});
	maybe_log(model_loggers, "scale*|T|", HsG::Apply(Hs::Var("*"), {Hs::Var("scale"), Hs::Var("tlength")}), {});
    }

    if (not alignment_lengths.empty() and has_a_variable_alignment)
        maybe_log(model_loggers, "|A|", HsG::Apply(Hs::Var("sum"), {Hs::Var("alignmentLengths")}), {});
    if (not total_num_indels.empty())
        maybe_log(model_loggers, "#indels", HsG::Apply(Hs::Var("sum"), {HsG::List(total_num_indels)}), {});
    if (not total_length_indels.empty())
        maybe_log(model_loggers, "|indels|", HsG::Apply(Hs::Var("sum"), {HsG::List(total_length_indels)}), {});
    if (not total_substs.empty())
        maybe_log(model_loggers, "#substs", HsG::Apply(Hs::Var("sum"), {HsG::List(total_substs)}), {});
    if (not total_prior_A.empty())
        maybe_log(model_loggers, "prior_A", HsG::Apply(Hs::Var("sum"), {HsG::List(total_prior_A)}), {});

    Hs::Exp model_fn = Hs::Var("model");

    // Pass in the sequence data for the two groups.
    if (partition_group_size[0] > 0)
	model_fn = HsG::Apply(model_fn, {unaligned_sequence_data});
    if (partition_group_size[1] > 0)
	model_fn = HsG::Apply(model_fn, {aligned_sequence_data});

    // Pass in the fixed tree or topology
    auto tree = Hs::Var("tree");
    auto topology = Hs::Var("topology");
    if (fixed.count("tree"))
        model_fn = HsG::Apply(model_fn, {tree});
    else if (fixed.count("topology"))
        model_fn = HsG::Apply(model_fn, {topology});

    // Pass in the loggers
    Hs::Var jsonLogger("logParamsJSON");
    Hs::Var tsvLogger("logParamsTSV");
    auto treeLogger = Hs::Var("logTree");
    if (not args.count("test"))
    {
	if (log_formats.count("tsv"))
	    model_fn = HsG::Apply(model_fn, {tsvLogger});
	if (log_formats.count("json"))
	    model_fn = HsG::Apply(model_fn, {jsonLogger});
        if (not fixed.count("tree"))
            model_fn = HsG::Apply(model_fn, {treeLogger});
        if (not alignment_loggers.empty())
        {
            vector<Hs::Exp> alignment_loggers_vec;
            for(auto& [i,a,l]: alignment_loggers)
                alignment_loggers_vec.push_back(l);

            model_fn = HsG::Apply(model_fn, {HsG::List(alignment_loggers_vec)});
        }
        if (not category_state_loggers.empty())
        {
            vector<Hs::Exp> category_state_loggers_vec;
            for(auto& [i,a,l]: category_state_loggers)
                category_state_loggers_vec.push_back(l);

            model_fn = HsG::Apply(model_fn, {HsG::List(category_state_loggers_vec)});
        }
    }

    Hs::Var loggers_var("loggers");
    HsG::Let(model, loggers_var, HsG::List(model_loggers));

    // Add the logger for scalar parameters
    if (not args.count("test"))
    {
	if (log_formats.count("tsv"))
	{
	    HsG::Expr(model, HsG::Apply(Hs::Var("$"), {Hs::Var("addLogger"), HsG::Apply(tsvLogger, {loggers_var})}));
	}

	if (log_formats.count("json"))
	{
	    HsG::Expr(model, HsG::Apply(Hs::Var("$"), {Hs::Var("addLogger"), HsG::Apply(jsonLogger, {loggers_var})}));
	}

        // Add the tree logger
        if (not fixed.count("tree"))
        {
	    Hs::Exp scaled_tree = tree_var;
	    if (n_branches > 0)
		scaled_tree = HsG::Apply(Hs::Var("scaleBranchLengths"), {Hs::Var("scale"), scaled_tree});
            HsG::Expr(model, HsG::Apply(Hs::Var("$"), {Hs::Var("addLogger"), HsG::Apply(treeLogger, {HsG::Apply(Hs::Var("addInternalLabels"), {scaled_tree})})}));
        }

        // Add the alignment loggers.
        for(auto& [i,a,l]: alignment_loggers)
            HsG::Expr(model, HsG::Apply(Hs::Var("$"), {Hs::Var("addLogger"), HsG::Apply(HsG::Apply(Hs::Var("$"), {HsG::Apply(Hs::Var("every"), {Hs::Literal(Hs::Integer{integer(10)})})}), {HsG::Apply(l, {a})})}));

        // Add the category-state loggers
        for(auto& [i,cs,l]: category_state_loggers)
            HsG::Expr(model, HsG::Apply(Hs::Var("$"), {Hs::Var("addLogger"), HsG::Apply(HsG::Apply(Hs::Var("$"), {HsG::Apply(Hs::Var("every"), {Hs::Literal(Hs::Integer{integer(10)})})}), {HsG::Apply(l, {cs})})}));
    }

    HsG::Return(model, loggers_var);
    program_file<<"\n";
    program_file<<model_fn<<" = "<<HsG::Do(model).print()<<"\n";

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
			      alignment_loggers,
			      category_state_loggers);

    program_file<<"\nmain = "<<HsG::Do(main).print()<<"\n";

    return program_file.str();
}

std::unique_ptr<Program>
gen_atmodel_program(const boost::program_options::variables_map& args,
		    const std::shared_ptr<module_loader>& L,
		    const fs::path& output_directory,
		    const fs::path& program_filename,
		    const vector<Hs::Exp>& alphabet_exps,
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
    L->set_user_source_root_for_file(model_filepath, model_module->name);
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
