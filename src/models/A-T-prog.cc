#include "A-T-prog.hh"

#include <set>
#include <map>

#include "util/set.hh"
#include "util/io.hh"
#include "util/string/split.hh"
#include "util/string/convert.hh"
#include "util/settings.hh"    // for get_setting_or( )
#include "models/compile.hh"   // for model_t
#include "models/parse.hh"   // for unparse_type

#include "computation/loader.hh"
#include "computation/haskell/generated.hh"
#include "computation/haskell/var.hh"
#include "computation/module.hh"

#include "sequence/genetic_code.hh"
#include "sequence/codons.hh"
#include "sequence/doublets.hh"
#include "sequence/RNAEdits.hh"
#include "bali-phy/cmd_line.hh"                                // for get_log_formats
#include "bali-phy/files.hh"                                   // for run_name

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

namespace HsG = Haskell::Generated;

struct LoggerExpressions
{
    vector<Hs::Exp> parameters;
    vector<Hs::Exp> context;
};

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

std::map<std::string, std::string> get_fixed(const InferOptions& options)
{
    map<string, string> fixed;
    for(auto& f: options.fixed)
	{
	    auto [key,value] = split_on_first('=',f);
	    fixed.insert({key,value});
	}

    for(auto& [key,value]: fixed)
        if (key != "topology" and key != "tree" and key != "alignment")
            throw myexception()<<"--fix: parameter '"<<key<<"' not recognized";

    if (fixed.count("tree") and fixed.count("topology"))
        throw myexception()<<"Can't fix both 'tree' and 'topology'";

    if (fixed.count("alignment") and not options.test)
        throw myexception()<<"Currently --fix=alignment only works with --test.\n"
                           <<"  You can fix the alignment for MCMC by disabling the indel model with -Inone.\n"
                           <<"  Using indel information from a fixed alignment during MCMC is not implemented.";

    for(auto&& word: {"tree","topology"})
        if (fixed.count(word) and options.tree)
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

// Binds a generated result and appends both projections of its logger values.
Hs::Var bind_and_log(bool do_log,
                     const Hs::Var& x,
                     const Hs::Var& log_x,
                     const string& name,
                     const Hs::Exp& E,
                     bool is_action,
                     bool has_parameter_loggers,
                     bool has_context_loggers,
                     Hs::Stmts& block,
                     LoggerExpressions& loggers,
                     bool is_referenced = true)
{
    bool has_loggers = has_parameter_loggers or has_context_loggers;
    perform_action_simplified(block, x, log_x, is_referenced, E, is_action, has_loggers);

    if (do_log)
        maybe_log(loggers.parameters, name, x, {});
    if (has_parameter_loggers)
        maybe_log(loggers.parameters,
                  name,
                  {},
                  HsG::Apply(Hs::Var("parameterLogValues"), {log_x}));
    if (has_context_loggers)
        loggers.context.push_back(HsG::Apply(Hs::Var("%>!"),
                                             {Hs::Literal(Hs::String{name}),
                                              HsG::Apply(Hs::Var("contextLogValues"), {log_x})}));

    return x;
}

// Chooses the generated variable names before binding and logging a result.
Hs::Var bind_and_log(bool do_log,
                     const string& name,
                     const Hs::Exp& E,
                     bool is_action,
                     bool has_parameter_loggers,
                     bool has_context_loggers,
                     Hs::Stmts& block,
                     LoggerExpressions& loggers,
                     bool is_referenced = true)
{
    string var_name = name;
    if (var_name.empty() or not std::islower(var_name[0]))
        var_name = "_"+var_name;
    Hs::Var x(var_name);
    Hs::Var log_x("log_"+name);
    return bind_and_log(do_log,
                        x,
                        log_x,
                        name,
                        E,
                        is_action,
                        has_parameter_loggers,
                        has_context_loggers,
                        block,
                        loggers,
                        is_referenced);
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
					     LoggerExpressions& model_loggers)
{
    // define tree_length
    Hs::Var tree_length_var("tlength");
    HsG::Let(model, tree_length_var, HsG::Apply(Hs::Var("treeLength"), {tree_var}));
    // log |T|
    maybe_log(model_loggers.parameters, "|T|", tree_length_var, {});

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
	auto scale_var = bind_and_log(false,
                                      var_name,
                                      E,
                                      code.is_action(),
                                      code.has_parameter_loggers(),
                                      code.has_context_loggers(),
                                      model,
                                      model_loggers);

	scales.push_back(scale_var);

	// log scale[i]
	maybe_log(model_loggers.parameters, var_name, scale_var, {});

	// log scale[i]*|T|
	maybe_log(model_loggers.parameters,
                  var_name+"*|T|",
                  HsG::Apply(Hs::Var("*"), {scale_var, tree_length_var}),
                  {});
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
						    LoggerExpressions& model_loggers)
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
        bind_and_log(false,
                     smodel_var,
                     log_smodel,
                     prefix,
                     smodel,
                     code.is_action(),
                     code.has_parameter_loggers(),
                     code.has_context_loggers(),
                     model,
                     model_loggers);
        smodels.push_back(smodel_var);
    }
    return smodels;
}

vector<Hs::Exp> generate_indel_models(const vector<model_t>& IMs,
					     const vector<string>& IM_function_for_index,
					     const Hs::Exp& tree_var,
					     Hs::Stmts& model,
					     LoggerExpressions& model_loggers)
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
        bind_and_log(false,
                     imodel_var,
                     log_imodel,
                     prefix,
                     imodel,
                     code.is_action(),
                     code.has_parameter_loggers(),
                     code.has_context_loggers(),
                     model,
                     model_loggers);
        imodels.push_back(imodel_var);
    }
    return imodels;
}

Hs::Stmts generate_main(const InferOptions& options,
		       const vector<pair<fs::path,string>>& filename_ranges,
		       const vector<Hs::Exp>& alphabet_exps,
		       const vector<int>& partition_group,
		       const vector<Hs::Var>& partition_sequence_data_vars,
		       const Hs::Var& tree,
		       const Hs::Var& topology,
		       const Hs::Var& tsvLogger,
		       const Hs::Var& jsonLogger,
		       const Hs::Var& treeLogger,
		       const Hs::Exp& model_fn,
		       vector<tuple<int,Hs::Exp,Hs::Exp>>& alignment_loggers,
		       vector<tuple<int,Hs::Exp,Hs::Exp>>& category_state_loggers)
{
    auto fixed = get_fixed(options);

    int n_partitions = filename_ranges.size();

    Hs::Stmts main;

    Hs::Var run_options("options");
    Hs::Var run_info("runInfo");
    Hs::Var is_test("isTest");
    Hs::Var logging_enabled("loggingEnabled");
    Hs::Var tsv_enabled("tsvEnabled");
    Hs::Var json_enabled("jsonEnabled");
    Hs::Var output_directory("outputDirectory");
    auto run_mode = HsG::Apply(Hs::Var("runMode"), {run_options});
    auto selected_log_formats = HsG::Apply(Hs::Var("logFormats"), {run_options});

    HsG::Bind(main,
              HsG::VarPat(run_options),
              HsG::Apply(Hs::Var("execParser"), {Hs::Var("runOptions")}));

    if (fixed.count("alignment"))
    {
        Hs::Stmts reject_fixed_alignment;
        HsG::Expr(reject_fixed_alignment,
                  HsG::Apply(Hs::Var("hPutStrLn"),
                             {Hs::Var("stderr"),
                              Hs::Literal(Hs::String(
                                  "Currently --fix=alignment only works with --test.\n"
                                  "  You can fix the alignment for MCMC by disabling the indel model with "
                                  "-Inone.\n"
                                  "  Using indel information from a fixed alignment during MCMC is not "
                                  "implemented."))}));
        HsG::Expr(reject_fixed_alignment, Hs::Var("exitFailure"));
        auto normal_mode = HsG::Apply(Hs::Var("/="), {run_mode, Hs::Var("TestMode")});
        HsG::Expr(main,
                  HsG::Apply(Hs::Var("when"), {normal_mode, HsG::Do(reject_fixed_alignment)}));
    }

    HsG::Bind(main,
              HsG::VarPat(run_info),
              HsG::Apply(Hs::Var("initializeModelRun"), {run_mode}));
    HsG::Let(main,
             is_test,
             HsG::Apply(Hs::Var("=="), {run_mode, Hs::Var("TestMode")}));
    HsG::Let(main, logging_enabled, HsG::Apply(Hs::Var("not"), {is_test}));
    HsG::Let(main,
             tsv_enabled,
             HsG::Apply(Hs::Var("&&"),
                        {logging_enabled,
                         HsG::Apply(Hs::Var("elem"), {Hs::Var("TSV"), selected_log_formats})}));
    HsG::Let(main,
             json_enabled,
             HsG::Apply(Hs::Var("&&"),
                        {logging_enabled,
                         HsG::Apply(Hs::Var("elem"), {Hs::Var("JSON"), selected_log_formats})}));
    HsG::Let(main,
             output_directory,
             HsG::Apply(Hs::Var("modelRunDirectory"), {run_info}));

    auto output_file = [&](const string& filename) {
        return HsG::Apply(Hs::Var("</>"), {output_directory, Hs::Literal(Hs::String(filename))});
    };

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

        HsG::Bind(main, HsG::VarPat(partition_sequence_data_vars[0]), E);
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

            // Main.3. Emit let sequenceData<n> = ...
            for(int i=0;i<n_partitions;i++)
            {
                const auto& partition_sequence_data_var = partition_sequence_data_vars[i];

                int index = index_for_filename.at( filename_ranges[i].first );
                Hs::Exp loaded_sequences = HsG::Apply(Hs::Var("!!"), {filename_to_seqs, Hs::Literal(Hs::Integer{integer(index)})});
                if (not filename_ranges[i].second.empty())
                    loaded_sequences = HsG::Apply(Hs::Var("selectRange"), {Hs::Literal(Hs::String(filename_ranges[i].second)), loaded_sequences});
		if (partition_group[i] == 0)
		    loaded_sequences = HsG::Apply(Hs::Var("mkUnalignedCharacterData"), {alphabet_exps[i], loaded_sequences});
		else
		    loaded_sequences = HsG::Apply(Hs::Var("mkAlignedCharacterData"), {alphabet_exps[i], loaded_sequences});
                HsG::Let(main, partition_sequence_data_var, loaded_sequences);
            }
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

    auto no_logger = HsG::Apply(Hs::Var("return"), {Hs::Var("noLogger")});
    // Choose a real output logger only when its runtime mode and format enable it.
    auto choose_logger = [&](const Hs::Exp& enabled, const Hs::Exp& logger) -> Hs::Exp {
        return Hs::If(HsG::Loc(enabled), HsG::Loc(logger), HsG::Loc(no_logger));
    };

    HsG::Bind(main,
              HsG::VarPat(tsvLogger),
              choose_logger(tsv_enabled,
                            HsG::Apply(Hs::Var("tsvLogger"),
                                       {output_file("C1.log"),
                                        HsG::List({Hs::Literal(Hs::String("iter"))})})));
    HsG::Bind(main,
              HsG::VarPat(jsonLogger),
              choose_logger(json_enabled,
                            HsG::Apply(Hs::Var("jsonLogger"), {output_file("C1.log.json")})));

    if (not fixed.count("tree"))
        HsG::Bind(main,
                  HsG::VarPat(treeLogger),
                  choose_logger(logging_enabled,
                                HsG::Apply(Hs::Var("treeLogger"), {output_file("C1.trees")})));

    for(auto& [i, a, logger]: alignment_loggers)
    {
        string filename = "C1.P"+std::to_string(i+1)+".fastas";
        HsG::Bind(main,
                  HsG::VarPat(logger.as_<Hs::Var>()),
                  choose_logger(logging_enabled,
                                HsG::Apply(Hs::Var("alignmentLogger"), {output_file(filename)})));
    }

    for(auto& [i, cs, logger]: category_state_loggers)
    {
        string filename = "C1.P"+std::to_string(i+1)+".site-property-samples.jsonl";
        HsG::Bind(main,
                  HsG::VarPat(logger.as_<Hs::Var>()),
                  choose_logger(logging_enabled,
                                HsG::Apply(Hs::Var("ejsonLogger"), {output_file(filename)})));
    }

    Hs::Stmts report;
    // Emit each message from the same runtime condition and filename used to construct its logger.
    auto report_output = [&](const Hs::Exp& enabled,
                             const string& description,
                             const string& filename,
                             const string& suffix = "") {
        auto action = HsG::Apply(Hs::Var("reportOutput"),
                                 {Hs::Literal(Hs::String(description)),
                                  output_file(filename),
                                  Hs::Literal(Hs::String(suffix))});
        HsG::Expr(report, HsG::Apply(Hs::Var("when"), {enabled, action}));
    };
    // Keep the sequence of generated startup lines readable in the C++ generator.
    auto put_line = [&](const string& line) {
        HsG::Expr(report, HsG::Apply(Hs::Var("putStrLn"), {Hs::Literal(Hs::String(line))}));
    };

    put_line("");
    put_line("Beginning MCMC computations.");
    report_output(tsv_enabled, "numerical parameters", "C1.log", " as TSV");
    report_output(json_enabled, "numerical parameters", "C1.log.json", " as JSON");
    if (not fixed.count("tree"))
        report_output(logging_enabled, "trees", "C1.trees");
    for(auto& [i, a, logger]: alignment_loggers)
        report_output(logging_enabled, "alignments", "C1.P"+std::to_string(i+1)+".fastas");
    for(auto& [i, cs, logger]: category_state_loggers)
        report_output(logging_enabled,
                      "character properties",
                      "C1.P"+std::to_string(i+1)+".site-property-samples.jsonl");

    put_line("");
    put_line("BAli-Phy does NOT detect how many iterations is sufficient:");
    put_line("   You need to monitor convergence and kill it when done.");
    auto iteration_count = HsG::Apply(Hs::Var("iterations"), {run_options});
    auto iterations_message = HsG::Apply(
        Hs::Var("++"),
        {Hs::Literal(Hs::String("   Maximum number of iterations set to ")),
         HsG::Apply(Hs::Var("++"),
                    {HsG::Apply(Hs::Var("show"), {iteration_count}),
                     Hs::Literal(Hs::String("."))})});
    HsG::Expr(report, HsG::Apply(Hs::Var("putStrLn"), {iterations_message}));
    put_line("");
    auto tsv_help = HsG::Apply(
        Hs::Var("putStrLn"),
        {Hs::Literal(Hs::String(
            "You can examine 'C1.log' using BAli-Phy tool statreport (command-line) or the "
            "BEAST program Tracer (graphical)."))});
    HsG::Expr(report, HsG::Apply(Hs::Var("when"), {tsv_enabled, tsv_help}));
    put_line("See the manual at http://www.bali-phy.org/README.xhtml for further information.");
    HsG::Expr(report, HsG::Apply(Hs::Var("hFlush"), {Hs::Var("stdout")}));
    HsG::Expr(main, HsG::Apply(Hs::Var("unless"), {is_test, HsG::Do(report)}));

    // Main.5. Emit mcmcState <- makeMCMCState $ model sequence_data
    HsG::Bind(main, HsG::VarPat(Hs::Var("mcmcState")), HsG::Apply(Hs::Var("$"), {Hs::Var("makeMCMCState"), model_fn}));

    // Main.6. Inspect the initial state or run MCMC according to the parsed runtime mode.
    auto inspect_model = HsG::Apply(Hs::Var("printInitialModel"),
                                    {selected_log_formats, Hs::Var("mcmcState")});
    auto run_mcmc = HsG::Apply(Hs::Var("runMCMC"),
                               {iteration_count, Hs::Var("mcmcState")});
    HsG::Expr(main,
              Hs::If(HsG::Loc(is_test), HsG::Loc(inspect_model), HsG::Loc(run_mcmc)));

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
    imports.insert("Probability.Random");                    // for makeMCMCState
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
    imports.insert("BAliPhy.Run");
    imports.insert("Options.Applicative");
    imports.insert("System.Exit");
    imports.insert("System.FilePath");
    imports.insert("System.IO");

    program_file<<"{-# LANGUAGE ExtendedDefaultRules #-}\n";
    program_file<<"{-# LANGUAGE OverloadedStrings #-}\n";
    program_file<<"module Main where";
    for(auto& mod: imports)
        program_file<<"\nimport "<<mod;
    program_file<<"\nimport qualified Data.IntMap as IntMap";
    program_file<<"\nimport qualified Data.Map as Map";
    program_file<<"\nimport qualified Data.Set as Set";
    program_file<<"\nimport qualified Data.JSON as J";
    program_file<<"\nimport Data.JSON ((.=))";
    program_file<<"\nimport qualified Data.Text.IO as T";
    program_file<<"\nimport Probability.Logger";
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
	if (imodel_index or get_setting_or("write-fixed-alignments",false) or get_setting_or("write-properties", true))
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

        if (get_setting_or("write-properties", true))
        {
            Hs::Var cat_states("catStates" + part_suffix);
            HsG::Let(model, cat_states, HsG::Apply(Hs::Var("labeledNodeMap"), {tree, *anc_states}));
            Hs::Exp smodel_properties = HsG::Apply(Hs::Var("prop_smodel_properties"), {properties});
            Hs::Exp smodel_conditions = HsG::Apply(Hs::Var("prop_smodel_conditions"), {properties});
            Hs::Exp cat_states_key = HsG::Apply(Hs::Var("J.toJSONKey"), {Hs::Literal(Hs::String("catStates"))});
            Hs::Exp properties_key = HsG::Apply(Hs::Var("J.toJSONKey"), {Hs::Literal(Hs::String("properties"))});
            Hs::Exp conditions_key = HsG::Apply(Hs::Var("J.toJSONKey"), {Hs::Literal(Hs::String("conditions"))});
            Hs::Exp cat_state_and_property_fields = HsG::Apply(Hs::Var("<>"),
                {HsG::Apply(Hs::Var(".="), {cat_states_key, cat_states}),
                 HsG::Apply(Hs::Var(".="), {properties_key, smodel_properties})});
            Hs::Exp cat_state_fields = HsG::Apply(Hs::Var("<>"),
                {cat_state_and_property_fields,
                 HsG::Apply(Hs::Var(".="), {conditions_key, smodel_conditions})});
            category_state_loggers.push_back({i, cat_state_fields, Hs::Var("logCatStates"+part_suffix)});
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

std::string generate_atmodel_program(const InferOptions& options,
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
                                     const model_t& indel_rates_model)
{
    auto fixed = get_fixed(options);

    auto log_formats = get_log_formats(options, not options.alignments.empty());

    int n_partitions = filename_ranges.size();

    int n_leaves   = n_sequences;
    int n_branches = (n_leaves==1)?0:2*n_leaves - 3;

    // Write pragmas, module, imports.
    std::ostringstream program_file;
    write_header(program_file, decls, SMs, IMs, scaleMs,
                 subst_rates_model, indel_rates_model, tree_model);
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

    LoggerExpressions model_loggers;

    // M1. Taxa
    // Sequence data remains classified as unaligned or aligned so main can use the correct constructor.
    vector<int> partition_group(n_partitions);
    vector<Hs::Var> partition_sequence_data_vars;

    for(int i=0;i<n_partitions;i++)
    {
	partition_group[i] = (i_mapping[i] and not fixed.contains("alignment")) ? 0 : 1;
        string var_name = (n_partitions == 1) ? "sequenceData" : "sequenceData" + std::to_string(i+1);
        partition_sequence_data_vars.emplace_back(var_name);
    }

    if (n_partitions > 0)
    {
        HsG::Let(model,
                 taxon_names_var,
                 HsG::Apply(Hs::Var("getTaxa"), {partition_sequence_data_vars[0]}));
    }

    // We could fix the whole tree or just the topology.
    Hs::Exp branch_lengths = Hs::Var("IntMap.empty");

    for(auto& stmt: decls.code.stmts.stmts)
	model.stmts.push_back(stmt);
    auto decl_loggers = decls.code.loggers;
    simplify(decl_loggers);
    Hs::Var declaration_loggers("declarationLoggers");
    if (not decl_loggers.empty())
        HsG::Let(model, declaration_loggers, generate_logger_values(model, decl_loggers));

    // M4. Branch-length tree
    auto tree_var = Hs::Var("tree");
    if (not fixed.count("tree"))
    {
        string var_name = "tree";

        auto code = tree_model.code;

        Hs::Exp E = Hs::Var("sampleTree");
        E = code.add_arguments(E,{{"taxa",taxon_names_var}});

        tree_var = bind_and_log(false,
                                var_name,
                                E,
                                code.is_action(),
                                code.has_parameter_loggers(),
                                code.has_context_loggers(),
                                model,
                                model_loggers);
        branch_lengths = HsG::Apply(Hs::Var("branchLengths"), {tree_var});
    }

    Hs::Exp subst_tree=tree_var;
    auto subst_rates_var = Hs::Var("substRates");
    if (not subst_rates_model.empty())
    {
        string var_name = "substRates";
        auto code = subst_rates_model.code;
        code.haskell_lambda_vars.clear(); // This isn't a function, these vars should be in scope.
        subst_rates_var = bind_and_log(false,
                                      var_name,
                                      code.generate(),
                                      code.is_action(),
                                      code.has_parameter_loggers(),
                                      code.has_context_loggers(),
                                      model,
                                      model_loggers);

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
        indel_rates_var = bind_and_log(false,
                                      var_name,
                                      code.generate(),
                                      code.is_action(),
                                      code.has_parameter_loggers(),
                                      code.has_context_loggers(),
                                      model,
                                      model_loggers);

        auto indel_tree_var = Hs::Var("indelTree");
        HsG::Let(model, indel_tree_var, HsG::Apply(Hs::Var("addBranchRates"), {Hs::Var("indelRates"), tree_var}));
        indel_tree = indel_tree_var;
    }

    set<string> used_states;
    for(int i=0;i<SMs.size();i++)
        add(used_states, SMs[i].code.used_states);

    // Foreground categories come from attributes on the supplied topology, so they cannot retain
    // their intended branch identities while topology changes.
    if (used_states.contains("branch_categories") and
        not fixed.contains("tree") and not fixed.contains("topology"))
        throw myexception()<<"Models using foreground branch categories require a fixed tree topology.\n"
                           <<"  Use --fix topology=<treefile> or --fix tree=<treefile>.";

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
        Hs::Exp sequence_data_var = partition_sequence_data_vars[i];

        // Model.Partition.1. tree_part<i> = scale_branch_lengths scale tree
	Hs::Exp scale = Hs::Literal(Hs::Integer{integer(1)});
        if (n_branches > 0)
            scale = scales[scale_index];
	partition_scales.push_back(scale);
	
        // Model.Partition.2. Sample the alignment
        Hs::Var alignment_on_tree("alignment" + part_suffix);
        if (imodel_index)
        {
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
        if (imodel_index)
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
        maybe_log(model_loggers.parameters, "P"+part, {}, part_loggers);
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
	maybe_log(model_loggers.parameters, "scale", Hs::Var("scale"), {});
	maybe_log(model_loggers.parameters,
                  "scale*|T|",
                  HsG::Apply(Hs::Var("*"), {Hs::Var("scale"), Hs::Var("tlength")}),
                  {});
    }

    if (not alignment_lengths.empty() and has_a_variable_alignment)
        maybe_log(model_loggers.parameters,
                  "|A|",
                  HsG::Apply(Hs::Var("sum"), {Hs::Var("alignmentLengths")}),
                  {});
    if (not total_num_indels.empty())
        maybe_log(model_loggers.parameters,
                  "#indels",
                  HsG::Apply(Hs::Var("sum"), {HsG::List(total_num_indels)}),
                  {});
    if (not total_length_indels.empty())
        maybe_log(model_loggers.parameters,
                  "|indels|",
                  HsG::Apply(Hs::Var("sum"), {HsG::List(total_length_indels)}),
                  {});
    if (not total_substs.empty())
        maybe_log(model_loggers.parameters,
                  "#substs",
                  HsG::Apply(Hs::Var("sum"), {HsG::List(total_substs)}),
                  {});
    if (not total_prior_A.empty())
        maybe_log(model_loggers.parameters,
                  "prior_A",
                  HsG::Apply(Hs::Var("sum"), {HsG::List(total_prior_A)}),
                  {});

    Hs::Exp model_fn = Hs::Var("model");

    // Pass each partition's sequence data as a separate argument in partition order.
    for(const auto& sequence_data_var: partition_sequence_data_vars)
	model_fn = HsG::Apply(model_fn, {sequence_data_var});

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
    model_fn = HsG::Apply(model_fn, {tsvLogger, jsonLogger});
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

    Hs::Exp parameter_loggers = HsG::List(model_loggers.parameters);
    Hs::Exp context_loggers = HsG::List(model_loggers.context);
    if (has_loggers(decl_loggers, LogValueKind::parameter))
        parameter_loggers = HsG::Apply(Hs::Var("++"),
                                       {HsG::Apply(Hs::Var("parameterLogValues"), {declaration_loggers}),
                                        parameter_loggers});
    if (has_loggers(decl_loggers, LogValueKind::context))
        context_loggers = HsG::Apply(Hs::Var("++"),
                                     {HsG::List({HsG::Apply(Hs::Var("contextLogValues"),
                                                           {declaration_loggers})}),
                                      context_loggers});

    vector<Hs::Exp> standard_context_loggers = {
        HsG::Apply(Hs::Var("%=!"), {Hs::Literal(Hs::String{"prior"}), Hs::Var("logPrior")}),
        HsG::Apply(Hs::Var("%=!"), {Hs::Literal(Hs::String{"likelihood"}), Hs::Var("logLikelihood")}),
        HsG::Apply(Hs::Var("%=!"), {Hs::Literal(Hs::String{"posterior"}), Hs::Var("logPosterior")})
    };
    context_loggers = HsG::Apply(Hs::Var("++"), {HsG::List(standard_context_loggers), context_loggers});
    context_loggers = HsG::Apply(Hs::Var("contextFields"), {context_loggers});

    Hs::Var logger_values_var("loggerValues");
    HsG::Let(model,
             logger_values_var,
             HsG::Apply(Hs::Var("LoggerValues"), {parameter_loggers, context_loggers}));

    // Register real and null loggers uniformly.
    // Runtime mode was resolved when each logger was constructed.
    auto add_logger = [&](const Hs::Exp& logger) {
        auto add_logger_action = HsG::Apply(Hs::Var("$"), {Hs::Var("addLogger"), logger});
        HsG::Expr(model, add_logger_action);
    };

    // Each scalar format evaluates context fields independently when both formats are enabled.
    add_logger(HsG::Apply(tsvLogger, {logger_values_var}));
    add_logger(HsG::Apply(jsonLogger, {logger_values_var}));

    if (not fixed.count("tree"))
    {
	Hs::Exp scaled_tree = tree_var;
	if (n_branches > 0)
	    scaled_tree = HsG::Apply(Hs::Var("scaleBranchLengths"), {Hs::Var("scale"), scaled_tree});
        add_logger(HsG::Apply(treeLogger,
                             {HsG::Apply(Hs::Var("addInternalLabels"), {scaled_tree})}));
    }

    for(auto& [i,a,l]: alignment_loggers)
        add_logger(HsG::Apply(HsG::Apply(Hs::Var("$"),
                                        {HsG::Apply(Hs::Var("every"),
                                                    {Hs::Literal(Hs::Integer{integer(10)})})}),
                              {HsG::Apply(l, {a})}));

    for(auto& [i,cs,l]: category_state_loggers)
        add_logger(HsG::Apply(HsG::Apply(Hs::Var("$"),
                                        {HsG::Apply(Hs::Var("every"),
                                                    {Hs::Literal(Hs::Integer{integer(10)})})}),
                              {HsG::Apply(l, {cs})}));

    HsG::Return(model, HsG::Apply(Hs::Var("parameterLogValues"), {logger_values_var}));
    program_file<<"\n";
    program_file<<model_fn<<" = "<<HsG::Do(model).print()<<"\n";

    long int max_iterations = options.iterations.value_or(200000);
    vector<Hs::Exp> default_log_formats;
    if (log_formats.count("tsv"))
        default_log_formats.push_back(Hs::Var("TSV"));
    if (log_formats.count("json"))
        default_log_formats.push_back(Hs::Var("JSON"));

    // Keep the generated option interface next to the analysis-specific program.
    program_file<<"\n\nrunOptions = info\n"
                <<"  (modelRunOptions "<<Hs::Literal(Hs::String(run_name(options))).print()
                <<" "<<max_iterations<<" "<<HsG::List(default_log_formats).print()<<" <**> helper)\n"
                <<"  (fullDesc <> progDesc \"Run this generated BAli-Phy analysis\")\n"
                <<R"(

-- Test mode never evaluates logger paths, so an empty placeholder keeps logger setup uniform.
modelRunDirectory TestRun = ""
modelRunDirectory (MCMCRun directory) = directory

-- Describe a logger only after its destination has been opened successfully.
reportOutput description filename suffix =
  putStrLn $ "   - Sampled " ++ description ++ " logged to " ++ show filename ++ suffix
)";

    auto main = generate_main(options,
			      filename_ranges,
			      alphabet_exps,
			      partition_group,
			      partition_sequence_data_vars,
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
gen_atmodel_program(const InferOptions& options,
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
		    const model_t& indel_rates_model)
{
    // FIXME! Make fixed-alignment likelihoods for 1- and 2-sequence alignments handle compressed alignments.
    {
        checked_ofstream program_file(program_filename);
        program_file<<generate_atmodel_program(options,
                                               n_leaves,
                                               alphabet_exps,
                                               filename_ranges,
					       decls,
                                               SMs, s_mapping, s_conditions,
                                               IMs, i_mapping,
                                               scaleMs, scale_mapping,
                                               tree_model,
                                               subst_rates_model,
                                               indel_rates_model);
    }

    auto m = L->load_module_from_file(program_filename);
    auto P = std::make_unique<Program>(L,vector{m}, "Main.main");
    if (options.test)
        L->args = {"--test"};
    else
        L->args = {"--output-dir", output_directory.string()};
    return P;
}
