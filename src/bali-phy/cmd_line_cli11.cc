#include "cmd_line.H"

#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <CLI/CLI.hpp>

#include "command_config.H"
#include "command_line_help.H"
#include "help.hh"
#include "paths.H"
#include "util/myexception.H"
#include "version.H"

using std::string;
using std::vector;

namespace
{

/// Recognize the cumulative help-level topic names without accepting prefixes.
std::optional<CommandHelpLevel> help_level(const string& name)
{
    if (name == "basic") return CommandHelpLevel::basic;
    if (name == "advanced") return CommandHelpLevel::advanced;
    if (name == "expert") return CommandHelpLevel::expert;
    if (name == "developer") return CommandHelpLevel::developer;
    return {};
}

/// Preserve BAli-Phy's one-value-per-occurrence composing options despite CLI11 vectors accepting
/// adjacent values by default. TakeAll accumulates CLI occurrences and marks options for config merging below.
CLI::Option* composing_option(CLI::Option* option)
{
    return option->type_size(1)->expected(1)->allow_extra_args(false)->take_all();
}

/// Adapt BAli-Phy's command files to CLI11 while retaining repeated-value merge order.
/// CLI11 normally drops config values after a command-line occurrence; the deferred values are
/// additional machinery needed to preserve BAli-Phy's command-line-then-config combination.
class BaliPhyConfig : public CLI::Config
{
    struct Target
    {
        CLI::Option* option;
        vector<string> parents;
    };

    CLI::Option* config_option;
    std::map<string, Target> targets;
    mutable vector<std::pair<CLI::Option*, vector<string>>> deferred_values;

    /// Register every configurable spelling with the parent path CLI11 expects in a ConfigItem.
    void add_targets(CLI::App& app, vector<string> parents)
    {
        for(auto* option: app.get_options())
        {
            if (not option->get_configurable()) continue;
            for(const auto& name: option->get_lnames())
                targets.emplace(name, Target{option, parents});
            for(const auto& name: option->get_snames())
                targets.emplace(name, Target{option, parents});
        }
    }

    /// Recover the active filename because CLI11's Config interface receives only its stream.
    string filename() const
    {
        if (config_option->results().empty()) return "<config>";
        return config_option->results().front();
    }

public:
    /// Record the global and inference option namespaces used to resolve exact command-file names.
    BaliPhyConfig(CLI::App& root, CLI::App& infer, CLI::Option* config)
        :config_option(config)
    {
        add_targets(root, {});
        add_targets(infer, {"infer"});
    }

    /// BAli-Phy supports reading command files, but does not use CLI11 to generate them.
    string to_config(const CLI::App*, bool, bool, string) const override
    {
        return {};
    }

    /// Convert one command file and remember composing values that CLI11 would otherwise ignore.
    vector<CLI::ConfigItem> from_config(std::istream& input) const override
    {
        deferred_values.clear();
        auto config = read_command_config(input, filename());
        config.options["variables"].push_back(config.model_source);

        vector<CLI::ConfigItem> items;
        for(auto& [name, values]: config.options)
        {
            auto target = targets.find(name);
            if (target == targets.end())
            {
                auto line = config.option_lines.at(name);
                throw CLI::ConfigError(filename()+":"+std::to_string(line)+
                                       ": unknown option '"+name+"'");
            }

            auto* option = target->second.option;
            if (option->count() and option->get_multi_option_policy() == CLI::MultiOptionPolicy::TakeAll)
                deferred_values.emplace_back(option, values);

            items.push_back({target->second.parents, name, values});
        }
        return items;
    }

    /// Append config values skipped because the same composing option occurred on the command line.
    void append_deferred_values() const
    {
        for(auto& [option, values]: deferred_values)
        {
            option->add_result(values);
            option->run_callback();
        }
        deferred_values.clear();
    }
};

/// Hold CLI11's command tree and its command-specific destinations for one parse operation.
class CLI11CommandParser
{
    CLI::App app{"Bayesian Inference of Alignment and Phylogeny", "bali-phy"};
    std::shared_ptr<CommandLineHelpFormatter> help_formatter;
    CommandLine result;
    InferOptions infer;
    RunCommand run;
    PrintCommand print;
    TypeCommand type;
    TestModuleCommand test_module;
    HelpCommand help;

    CLI::Option* verbosity_option = nullptr;
    CLI::Option* config_option = nullptr;
    std::shared_ptr<BaliPhyConfig> config_reader;
    CLI::App* infer_app = nullptr;
    CLI::App* run_app = nullptr;
    CLI::App* print_app = nullptr;
    CLI::App* type_app = nullptr;
    CLI::App* test_module_app = nullptr;
    CLI::App* help_app = nullptr;

    /// Record one option's minimum cumulative help level and return it for CLI11 modifiers.
    CLI::Option* show_at(CLI::Option* option, CommandHelpLevel level)
    {
        help_formatter->show_at(option, level);
        return option;
    }

    /// Record one command's minimum cumulative help level and return it for further setup.
    CLI::App* show_at(CLI::App* command, CommandHelpLevel level)
    {
        help_formatter->show_at(command, level);
        return command;
    }

    /// Register one command's cumulative level and user-facing invocation syntax.
    CLI::App* show_command(CLI::App* command, CommandHelpLevel level, string synopsis)
    {
        show_at(command, level);
        help_formatter->set_synopsis(command, std::move(synopsis));
        return command;
    }

    /// Register options that affect process execution and Haskell compilation across commands.
    void add_global_options()
    {
        auto& global = result.global;
        auto& compiler = global.compiler;

        verbosity_option = show_at(app.add_option("-V,--verbose", global.verbosity,
                                                  "Print additional diagnostic output"),
                                   CommandHelpLevel::advanced)->type_name("LEVEL")->expected(0, 1);
        show_at(app.add_option("-s,--seed", global.seed, "Random seed"),
                CommandHelpLevel::advanced)->type_name("SEED");
        auto* package_paths = composing_option(show_at(app.add_option("-P,--package-path", global.package_paths,
                                                                      "Directories to search for packages"),
                                                       CommandHelpLevel::expert)->type_name("DIR"));
        help_formatter->set_package_paths(package_paths);
        composing_option(show_at(app.add_option("--set", global.settings, "Set key=<value>"),
                                 CommandHelpLevel::expert)->type_name("KEY=VALUE"));

        show_at(app.add_flag("--dump-parsed", compiler.dump_parsed, "Show parser output"),
                CommandHelpLevel::developer);
        show_at(app.add_flag("--dump-rn", compiler.dump_renamed, "Show renamed output"),
                CommandHelpLevel::developer);
        show_at(app.add_flag("--dump-tc", compiler.dump_typechecked, "Show typechecked output"),
                CommandHelpLevel::developer);
        show_at(app.add_flag("--dump-ds", compiler.dump_desugared, "Show desugared output"),
                CommandHelpLevel::developer);
        show_at(app.add_flag("--dump-opt", compiler.dump_optimized, "Show optimized output"),
                CommandHelpLevel::developer);
        show_at(app.add_option("--recompile", compiler.recompile, "Rerun compilation of selected or all modules"),
                CommandHelpLevel::developer)->type_name("MODULE")->expected(0, 1);
        show_at(app.add_option("--optimize", compiler.optimize, "Run optimization passes"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--fully-lazy", compiler.fully_lazy,
                               "Run fully lazy lambda lifting transformation"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--pre-inline", compiler.pre_inline, "Pre-inline unconditionally"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--post-inline", compiler.post_inline, "Post-inline unconditionally"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--let-float-from-case", compiler.let_float_from_case, "Let float from case"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--let-float-from-apply", compiler.let_float_from_apply, "Let float from apply"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--let-float-from-let", compiler.let_float_from_let, "Let float from let"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--case-of-constant", compiler.case_of_constant, "Case of constant"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--case-of-variable", compiler.case_of_variable, "Case of constant"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--case-of-case", compiler.case_of_case, "Case of case"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--inline-threshold", compiler.inline_threshold, "Inline threshold"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--beta-reduction", compiler.beta_reduction, "Beta reduction"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_option("--simplifier-max-iterations", compiler.simplifier_max_iterations,
                               "Bound on iterating the simplifier"),
                CommandHelpLevel::developer)->capture_default_str();
        show_at(app.add_flag("--dump-ffi", compiler.dump_ffi, "Show grouped foreign-import ABI information"),
                CommandHelpLevel::developer);
        show_at(app.add_flag("--cpp", compiler.force_cpp,
                             "Conditionally preprocess every Haskell source module"), CommandHelpLevel::developer);
        composing_option(show_at(app.add_option("-D,--cpp-define", compiler.cpp_definitions,
                                                "Define a CPP macro as NAME[=TEXT]"),
                                 CommandHelpLevel::developer)->type_name("NAME[=TEXT]"));
        composing_option(show_at(app.add_option("--cpp-undefine", compiler.cpp_undefinitions,
                                                "Remove an initial CPP macro definition"),
                                 CommandHelpLevel::developer)->type_name("NAME"));
        show_at(app.add_flag("--dump-cpp", compiler.dump_cpp,
                             "Show Haskell source after conditional preprocessing"), CommandHelpLevel::developer);
    }

    /// Register the inference command without applying configuration-file contents yet.
    void add_infer_command()
    {
        infer_app = show_command(app.add_subcommand("infer", "Infer a phylogeny and related model parameters"),
                                 CommandHelpLevel::basic,
                                 "infer [INFER-OPTIONS] SEQUENCE-FILE [SEQUENCE-FILE ...]");
        infer_app->fallthrough();
        show_at(infer_app->get_help_ptr(), CommandHelpLevel::basic);

        auto append_alignments = [this](const vector<string>& alignments)
        {
            infer.alignments.insert(infer.alignments.end(), alignments.begin(), alignments.end());
        };
        // Separate callbacks keep --align single-valued while positional SEQUENCE-FILE remains variadic.
        // Appending during parsing preserves the established ordering when the two forms are mixed.
        show_at(infer_app->add_option_function<vector<string>>("SEQUENCE-FILE", append_alignments,
                                                               "Sequence data files"),
                CommandHelpLevel::basic)->type_name("")->take_all()->trigger_on_parse();
        composing_option(show_at(infer_app->add_option_function<vector<string>>("--align", append_alignments,
                                                                                "Sequence data files"),
                                  CommandHelpLevel::basic)->type_name("SEQUENCE-FILE"))->trigger_on_parse();
        show_at(infer_app->add_flag("-t,--test", infer.test, "Analyze initial values and exit"),
                CommandHelpLevel::basic);
        show_at(infer_app->add_option("-i,--iterations", infer.iterations, "Number of MCMC iterations"),
                CommandHelpLevel::basic)->type_name("N");
        show_at(infer_app->add_option("-n,--name", infer.name, "Name for the output directory"),
                CommandHelpLevel::basic)->type_name("NAME");
        show_at(infer_app->add_option("-x,--subsample", infer.subsample, "Factor by which to subsample"),
                CommandHelpLevel::advanced)->type_name("N")->capture_default_str();
        show_at(infer_app->add_option("-l,--log-format", infer.log_format,
                                      "Log format: tsv, json, or tsv,json"),
                CommandHelpLevel::advanced)->type_name("FORMAT");
        show_at(infer_app->add_option("--pre-burnin", infer.pre_burnin,
                                      "Iterations to refine the initial tree"),
                CommandHelpLevel::advanced)->type_name("N")->capture_default_str();
        show_at(infer_app->add_option("-T,--tree", infer.tree, "Tree prior"),
                CommandHelpLevel::basic)->type_name("MODEL");
        show_at(infer_app->add_flag("-U,--unalign", infer.unalign,
                                    "Unalign alignments that are not fixed"), CommandHelpLevel::advanced);
        composing_option(show_at(infer_app->add_option("-A,--alphabet", infer.alphabets, "Alphabet"),
                                 CommandHelpLevel::basic)->type_name("ALPHABET"));
        composing_option(show_at(infer_app->add_option("-S,--smodel", infer.smodels, "Substitution model"),
                                 CommandHelpLevel::basic)->type_name("MODEL"));
        composing_option(show_at(infer_app->add_option("-I,--imodel", infer.imodels, "Insertion-deletion model"),
                                 CommandHelpLevel::basic)->type_name("MODEL"));
        composing_option(show_at(infer_app->add_option("-R,--scale", infer.scales, "Prior on the scale"),
                                 CommandHelpLevel::basic)->type_name("MODEL"));
        composing_option(show_at(infer_app->add_option("-F,--fix", infer.fixed,
                                                       "Fix topology, tree, or alignment"),
                                 CommandHelpLevel::basic)->type_name("TARGET"));
        composing_option(show_at(infer_app->add_option("--variables", infer.variables,
                                                       "Variable definitions"),
                                 CommandHelpLevel::basic)->type_name("DEFINITIONS"));
        composing_option(show_at(infer_app->add_option("-L,--link", infer.links, "Link partitions"),
                                 CommandHelpLevel::basic)->type_name("PARTITIONS"));
        show_at(infer_app->add_option("--subst-rates", infer.subst_rates, "Substitution rates model"),
                CommandHelpLevel::basic)->type_name("MODEL")->capture_default_str();
        show_at(infer_app->add_option("--indel-rates", infer.indel_rates, "Indel rates model"),
                CommandHelpLevel::basic)->type_name("MODEL")->capture_default_str();
    }

    /// CLI11 processes config files at the root; present this option as part of the inference interface.
    void add_config_file()
    {
        infer_app->get_help_ptr()->configurable(false);
        config_option = app.set_config("-c,--config", "", "Command file to read");
        config_option->type_name("FILE");
        help_formatter->show_with(infer_app, config_option, CommandHelpLevel::basic);
        config_reader = std::make_shared<BaliPhyConfig>(app, *infer_app, config_option);
        app.config_formatter(config_reader);
    }

    /// Register non-inference commands, including run's option-before-program boundary.
    void add_other_commands()
    {
        run_app = show_command(app.add_subcommand("run", "Run a Haskell program"),
                               CommandHelpLevel::advanced, "run PROGRAM [ARGUMENT ...]");
        run_app->fallthrough();
        run_app->positionals_at_end();
        show_at(run_app->add_option("PROGRAM", run.program, "Haskell program"),
                CommandHelpLevel::advanced)->type_name("")->required();
        show_at(run_app->add_option("ARGUMENT", run.arguments, "Program arguments"),
                CommandHelpLevel::advanced)->type_name("");

        print_app = show_command(app.add_subcommand("print", "Evaluate and print a model-language expression"),
                                 CommandHelpLevel::advanced, "print [PRINT-OPTIONS] EXPRESSION");
        print_app->fallthrough();
        show_at(print_app->add_option("EXPRESSION", print.expression, "Expression to evaluate"),
                CommandHelpLevel::advanced)->type_name("")->required();
        composing_option(show_at(print_app->add_option("-A,--alphabet", print.alphabets, "Alphabet"),
                                 CommandHelpLevel::advanced)->type_name("ALPHABET"));

        type_app = show_command(app.add_subcommand("type", "Show the type of a qualified Haskell name"),
                                CommandHelpLevel::developer, "type NAME");
        type_app->fallthrough();
        show_at(type_app->add_option("NAME", type.name, "Qualified Haskell name"),
                CommandHelpLevel::developer)->type_name("")->required();

        test_module_app = show_command(app.add_subcommand("test-module", "Compile and inspect a Haskell module"),
                                       CommandHelpLevel::developer, "test-module MODULE");
        test_module_app->fallthrough();
        show_at(test_module_app->add_option("MODULE", test_module.module, "Module name or source file"),
                CommandHelpLevel::developer)->type_name("")->required();

        help_app = show_command(app.add_subcommand("help", "Show command or model-language help"),
                                CommandHelpLevel::basic, "help [TOPIC]");
        show_at(help_app->add_option("TOPIC", help.topic, "Help topic"),
                CommandHelpLevel::basic)->type_name("");
    }

    /// Print level or command help requested through the help subcommand; other topics continue to main.
    void show_requested_cli_help()
    {
        if (not help_app->parsed()) return;

        if (not help.topic)
        {
            help_formatter->set_level(CommandHelpLevel::basic);
            std::cout<<help_formatter->make_help(&app, "bali-phy", CLI::AppFormatMode::Normal);
            std::exit(0);
        }
        if (auto level = help_level(*help.topic))
        {
            help_formatter->set_level(*level);
            std::cout<<help_formatter->make_help(&app, "bali-phy", CLI::AppFormatMode::Normal);
            std::exit(0);
        }
        if (auto* command = app.get_subcommand_no_throw(*help.topic))
        {
            std::cout<<command->help("bali-phy");
            std::exit(0);
        }

        show_help(*help.topic, get_package_paths(result.global.package_paths));
        std::exit(0);
    }

    /// Select the typed payload after CLI11 has validated that exactly one subcommand was supplied.
    void select_command()
    {
        if (infer_app->parsed())
        {
            if (config_option->count() > 1)
                throw myexception()<<"infer accepts only one --config file";
            if (infer.alignments.empty())
                throw myexception()<<"infer requires sequence data";
            result.command = std::move(infer);
        }
        else if (run_app->parsed())
        {
            if (run.program.extension() != ".hs")
                run.program += ".hs";
            result.command = std::move(run);
        }
        else if (print_app->parsed())
            result.command = std::move(print);
        else if (type_app->parsed())
            result.command = std::move(type);
        else if (test_module_app->parsed())
            result.command = std::move(test_module);
        else
            result.command = std::move(help);

        if (result.global.compiler.dump_ffi and not test_module_app->parsed())
            throw myexception()<<"--dump-ffi requires the test-module command";
    }

public:
    /// Build the complete command tree once so CLI11 metadata and parsing share one declaration.
    CLI11CommandParser()
    {
        help_formatter = std::make_shared<CommandLineHelpFormatter>(&app);
        app.formatter(help_formatter);
        app.require_subcommand(1);
#ifdef _WIN32
        app.allow_windows_style_options();
#endif
        add_global_options();
        add_infer_command();
        add_other_commands();

        show_at(app.get_help_ptr(), CommandHelpLevel::basic)->configurable(false)->disable_flag_override();
        show_at(app.set_version_flag("-v,--version", [] {
            std::ostringstream out;
            print_version_info(out);
            return out.str();
        }), CommandHelpLevel::basic)->configurable(false);
        add_config_file();
    }

    /// Parse one argv vector and return the parser-independent command value.
    CommandLine parse(int argc, char* argv[])
    {
        try
        {
            app.parse(argc, argv);

            // CLI11 records a value-less optional argument as an empty result.  Preserve Boost's
            // implicit verbosity of one while still allowing attached values such as `-V4`.
            if (verbosity_option->count() and verbosity_option->results().front().empty())
                result.global.verbosity = 1;

            config_reader->append_deferred_values();

            if (config_option->count() and not infer_app->parsed())
                throw myexception()<<"--config requires the infer command";
            show_requested_cli_help();
        }
        catch (const CLI::ParseError& error)
        {
            std::exit(app.exit(error));
        }
        select_command();
        return std::move(result);
    }
};

}

/// Parse the new subcommand interface without exposing CLI11 to command consumers.
CommandLine parse_cli11_command_line(int argc, char* argv[])
{
    return CLI11CommandParser{}.parse(argc, argv);
}
