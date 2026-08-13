#include <filesystem>
#include <boost/program_options.hpp>
#include <boost/program_options/option.hpp>
#include "command_config.H"
#include "cmd_line.H"
#include "paths.H"
#include "util/file-paths.H"
#include "util/io.H"
#include "util/log-level.H"
#include "util/myexception.H"
#include "util/string/join.H"
#include "util/string/split.H"
#include "util/text.H"
#include "version.H"
#include "models/rules.H"
#include "models/parse.H"
#include "help.hh"


using std::string;
using std::map;
using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::optional;
using std::set;

namespace po = boost::program_options;
using po::variables_map;

const string trailing_args_separator = "--";

namespace fs = std::filesystem;

po::parsed_options bali_config_file(std::istream& file, const po::options_description& options_desc,
                                    const string& filename)
{
    auto config = read_command_config(file, filename);
    config.options["variables"].push_back(config.model_source);

    po::parsed_options options(&options_desc);
    for(auto& [key,values]: config.options)
	options.options.push_back(po::basic_option<char>(key,values));

    return options;
}

/// Parse the file $HOME/.bali-phy and add the options it contains to the command line arguments.
///
/// \param args The command line arguments.
/// \param options The allowed options.
///
void load_bali_phy_rc(po::variables_map& args,const po::options_description& options)
{
    if (auto home_dir = get_home_dir())
    {
	if (not fs::exists(*home_dir))
	    cerr<<"Home directory "<<*home_dir<<" does not exist!"<<endl;
	else if (not fs::is_directory(*home_dir))
	    cerr<<"Home directory "<<*home_dir<<" is not a directory!"<<endl;
	else
        {
	    auto filename = *home_dir / ".bali-phy";

	    if (fs::exists(filename))
            {
		if (log_verbose >= 1)
		    cerr<<"Reading ~/.bali-phy ...";
		checked_ifstream file(filename, "config file");

		store(parse_config_file(file, options), args);
		notify(args);
		if (log_verbose >= 1)
		    cerr<<" done."<<endl;
	    }
	}
    }
    else
	cerr<<"Environment variables HOME and USERPROFILE not set!"<<endl;
}

vector<string> drop_trailing_args(int argc, char* argv[], const string& separator)
{
    vector<string> args;
    for(int i=1;i<argc;i++)
    {
	string arg = argv[i];
	if (arg == separator) break;
	args.push_back(arg);
    }
    return args;
}

vector<string> trailing_args(int argc, char* argv[], const string& separator)
{
    vector<string> args;
    int i = 1;
    for(;i<argc;i++)
    {
	string arg = argv[i];
	if (arg == separator) break;
    }
    for(i++;i<argc;i++)
    {
	string arg = argv[i];
	args.push_back(arg);
    }
    return args;
}

po::options_description general_options(int level)
{
    using namespace po;

    // named options
    options_description general("General options");
    general.add_options()
	("help,h", value<string>()->implicit_value("basic"),"Print usage information.")
	("version,v", "Print version information.")
	("test,t","Analyze the initial values and exit.")
	("config,c", value<string>(),"Command file to read.")
	;
    if (level >= 1)
	general.add_options()
	    ("verbose,V",value<int>()->implicit_value(1),"Print extra output in case of error.");
    if (level >= 2)
	general.add_options()
	    ("package-path,P",value<vector<string> >()->composing(),"Directories to search for packages.")
	    ("set",value<vector<string> >()->composing(),"Set key=<value>");
    return general;
}

po::options_description mcmc_options(int level)
{
    using namespace po;

    options_description mcmc("MCMC options");
    mcmc.add_options()
	("iterations,i",value<long int>(),"The number of iterations to run.")
	("name,n", value<string>(),"Name for the output directory to create.")
	;

    if (level >= 1)
	mcmc.add_options()
	    ("subsample,x",value<int>()->default_value(1),"Factor by which to subsample.")
	    ("seed,s", value<unsigned long>(),"Random seed.")
            ("log-format,l", value<string>(),"Log-format: `tsv` or `json` or `tsv,json`")
	    ("pre-burnin",value<int>()->default_value(3),"Iterations to refine initial tree.");

    if (level >= 2)
	mcmc.add_options()
	    ("enable",value<string>(),"Comma-separated list of kernels to enable.")
	    ("disable",value<string>(),"Comma-separated list of kernels to disable.");

    if (level >= 3)
	mcmc.add_options()
	    ("beta",value<string>(),"MCMCMC temperature")
	    ("dbeta",value<string>(),"MCMCMC temperature changes");

    return mcmc;
}


po::options_description haskell_optimization()
{
    using namespace po;

    options_description optimization("Haskell optimization options");
    optimization.add_options()
	("dump-parsed","Show parser output")
	("dump-rn","Show renamed output")
	("dump-tc","Show typechecked output")
	("dump-ds","Show desugared output")
	("dump-opt","Show optimized output")
        ("recompile",value<string>()->implicit_value(""),"Rerun the compilation of specified modules, or all modules")
        ("optimize",value<bool>()->default_value(true),"Run optimization passes")
	("fully-lazy",value<bool>()->default_value(true), "Run fully lazy lambda lifting transformation")
	("pre-inline",value<bool>()->default_value(true),"Pre-inline unconditionally")
	("post-inline",value<bool>()->default_value(true),"Post-inline unconditionally")
	("let-float-from-case",value<bool>()->default_value(true),"Let float from case")
	("let-float-from-apply",value<bool>()->default_value(true),"Let float from apply")
	("let-float-from-let",value<bool>()->default_value(true),"Let float from let")
	("case-of-constant",value<bool>()->default_value(true),"Case of constant")
	("case-of-variable",value<bool>()->default_value(true),"Case of constant")
	("case-of-case",value<bool>()->default_value(true),"Case of case")
	("inline-threshold",value<int>()->default_value(8),"Inline threshold")
	("beta-reduction",value<bool>()->default_value(true),"Beta-reduction")
	("simplifier-max-iterations",value<int>()->default_value(4),"Bound on iterating simplifier")
	;
    return optimization;
}

po::options_description parameters_options(int level)
{
    using namespace po;

    options_description parameters("Parameter options");
    parameters.add_options()
	("align", value<vector<string> >()->composing(),"Sequence file & initial alignment.")
	("tree,T",value<string>(),"Tree prior: ~UniformTree(taxa), ~UniformRootedTree(taxa), ~Yule(taxa), etc.");

    if (level >= 1)
	parameters.add_options()
	    ("unalign,U","Unalign alignments that aren't fixed.");

    return parameters;
}

po::options_description model_options(int level)
{
    using namespace po;

    options_description model("Model options");
    model.add_options()
        ("alphabet,A",value<vector<string> >()->composing(),"The alphabet.")
        ("smodel,S",value<vector<string> >()->composing(),"Substitution model.")
        ("imodel,I",value<vector<string> >()->composing(),"Insertion-deletion model.")
        ("scale,R",value<vector<string> >()->composing(),"Prior on the scale.")
        ("fix,F",value<vector<string>>()->composing(),"Fix topology,tree,alignment")
        ("variables",value<vector<string>>()->composing(),"Variable definitions");
    model.add_options()
        ("link,L",value<vector<string>>()->composing(),"Link partitions.")
        ("subst-rates",value<string>()->default_value("constant"),"Subst rates: *constant, relaxed, or an expression.")
        ("indel-rates",value<string>()->default_value("relaxed"),"Indel rates: constant, *relaxed, or an expression.");

    if (level >= 2)
        model.add_options()
            ("print,p",value<string>(),"Evaluate and print expression.");
    return model;
}

po::options_description developer_options()
{
    using namespace po;

    options_description developer("Developer options");
    developer.add_options()
	("test-module",value<string>(),"Parse and optimize the given module")
	("dump-ffi","Show grouped foreign-import ABI information")
	("cpp","Conditionally preprocess every Haskell source module")
	("cpp-define,D",value<vector<string>>()->composing(),"Define a CPP macro as NAME[=TEXT]")
	("cpp-undefine",value<vector<string>>()->composing(),"Remove an initial CPP macro definition")
	("dump-cpp","Show Haskell source after conditional preprocessing")
	("run,r",value<vector<string>>()->multitoken(),"Run the given module")
	("type",value<string>(),"Get the type of a qualified haskell function")
	("partition-weights",value<string>(),"File containing tree with partition weights")
	("t-constraint",value<string>(),"File with m.f. tree representing topology and branch-length constraints.")
	("a-constraint",value<string>(),"File with groups of leaf taxa whose alignment is constrained.")
	("align-constraint",value<string>(),"File with alignment constraints.")
	("likelihood-calculators",value<string>(),"comma-separated integers")
	;
    return developer;
}

string short_description()
{
    return "Bayesian Inference of Alignment and Phylogeny";
}

string usage()
{
    return "Usage: bali-phy <sequence-file1> [<sequence-file2> ...] [OPTIONS]";
}

variables_map parse_boost_options(int argc,char* argv[])
{ 
    using namespace po;

    map<string,options_description> help_levels;

    map<string,string> next_level = {{"basic","advanced"},{"advanced","expert"}};
    map<string,string> prev_level = {{"advanced","basic"},{"expert","advanced"}};

    options_description all(bold("Developer")+" options - " + bold_red("use at your own risk!"));
    all.add(general_options(3)).add(mcmc_options(3)).add(parameters_options(3)).add(model_options(3)).add(haskell_optimization()).add(developer_options());
    help_levels.insert({"developer",all});
    
    options_description expert(bold("Expert")+" options");
    expert.add(general_options(2)).add(mcmc_options(2)).add(parameters_options(2)).add(model_options(2));
    help_levels.insert({"expert", expert});
    
    options_description advanced(bold("Advanced")+" options");
    advanced.add(general_options(1)).add(mcmc_options(1)).add(parameters_options(1)).add(model_options(1));
    help_levels.insert({"advanced", advanced});
    
    options_description basic(bold("Basic")+" options");
    basic.add(general_options(0)).add(mcmc_options(0)).add(parameters_options(0)).add(model_options(0));
    help_levels.insert({"basic", basic});

    // positional options
    positional_options_description p;
    p.add("align", -1);

    vector<string> cargs = drop_trailing_args(argc, argv, trailing_args_separator);
    if (cargs.size()>=1 and cargs[0] == "help")
    {
        cargs[0] = "--help";
    }
    else if (cargs.size()>=1 and cargs[0] == "print")
    {
        cargs[0] = "--print";
    }
    else if (cargs.size()>=1 and cargs[0] == "run")
    {
        cargs[0] = "--run";
    }
    else if (cargs.size()>=1 and cargs[0] == "type")
    {
        cargs[0] = "--type";
    }
    variables_map args;
    store(command_line_parser(cargs).options(all).positional(p).run(), args);
    notify(args);    

    if (args.count("version")) {
	print_version_info(cout);
	exit(0);
    }

    if (args.count("verbose")) log_verbose = args["verbose"].as<int>();

    if (args.count("help"))
    {
        string topic = args.count("help")?args["help"].as<string>():"basic";

	vector<string> path_arguments;
	if (args.count("package-path"))
	    path_arguments = args["package-path"].as<vector<string>>();
	auto package_paths = get_package_paths(path_arguments);
	if (help_levels.count(topic))
	{
	    cout<<short_description()<<"\n";
	    cout<<usage()<<"\n";
	    cout<<help_levels[topic]<<"\n";
	    cout<<"Showing "<<bold(topic)<<" command line options.";
	    if (next_level.count(topic))
		cout<<"  Not all options are shown!\n";
	    else
		cout<<"\n";
	    if (next_level.count(topic))
		cout<<"  * See `bali-phy help "<<bold(next_level.at(topic))<<"` to see more options.\n";
	    if (prev_level.count(topic))
	    {
		if (prev_level.at(topic) == "basic")
		    cout<<"  * See `bali-phy help` to see fewer options.\n";
		else
		    cout<<"  * See `bali-phy help "<<bold(prev_level.at(topic))<<"` to see fewer options.\n";
	    }
	    cout<<"\n";
	    cout<<"See `bali-phy help "<<underline("option")<<"` for help on "<<underline("option")<<".  For example,\n";
	    cout<<"  * `bali-phy help "<<bold("alphabet")<<"` shows help on the "<<bold("--alphabet")<<" command.\n";
	    cout<<"  * `bali-phy help "<<bold("Normal")<<"` shows help on the normal distribution.\n";
	    cout<<"  * `bali-phy help "<<bold("TN93")<<"` shows help on the TN93 model.\n";
	    cout<<"  * `bali-phy help "<<bold("log")<<"` shows help on the log function.\n\n";

	    help_topics(cout, package_paths);
	}
	else
	    show_help(topic, package_paths);
	exit(0);
    }

    if (args.count("config")) 
    {
	string filename = args["config"].as<string>();
	checked_ifstream file(filename,"config file");

	store(bali_config_file(file, all, filename), args);
	notify(args);
    }

    load_bali_phy_rc(args,all);

    if (args.count("dump-ffi") and not args.count("test-module"))
        throw myexception()<<"--dump-ffi requires --test-module";

    std::set<string> commands;
    for(auto word : {"align", "print", "test-module", "run", "type"})
	if (args.count(word))
	    commands.insert(word);

    if (commands.empty())
	throw myexception()<<"You must specify alignment files or a command such as `run'.\n\n"
                           <<"Try `"<<argv[0]<<" --help' for more information.";

    if (commands.size() > 1)
    {
	if (commands.count("align"))
	{
	    commands.erase(commands.find("align"));
	    throw myexception()<<"You cannot specify both sequence files and \"--"<<*commands.begin()<<"\".\n\nTry `"<<argv[0]<<" --help' for more information.";
	}
	auto first = commands.begin();
	auto second = first;
	second++;
	throw myexception()<<"You cannot specify both \"--"<<*first<<"\" and \"--"<<*second<<"\".\n\nTry `"<<argv[0]<<" --help' for more information.";
    }

    return args;
}

/// Return an explicitly supplied scalar option without replacing absence by a default value.
template <typename T>
optional<T> optional_value(const variables_map& args, const string& name)
{
    if (args.count(name))
        return args.at(name).as<T>();
    return {};
}

/// Return a composing option's values in the ordering established by Boost.Program_options.
template <typename T>
vector<T> vector_value(const variables_map& args, const string& name)
{
    if (args.count(name))
        return args.at(name).as<vector<T>>();
    return {};
}

/// Convert the temporary Boost representation to the parser-independent command records.
/// This adapter exists only for the staged CLI11 migration and should disappear with Boost.
CommandLine adapt_boost_options(const variables_map& args, int argc, char* argv[])
{
    CommandLine command_line;
    auto& global = command_line.global;
    global.verbosity = optional_value<int>(args, "verbose").value_or(0);
    global.test = args.count("test");
    global.package_paths = vector_value<string>(args, "package-path");
    global.settings = vector_value<string>(args, "set");
    global.seed = optional_value<unsigned long>(args, "seed");

    auto& compiler = global.compiler;
    compiler.dump_parsed = args.count("dump-parsed");
    compiler.dump_renamed = args.count("dump-rn");
    compiler.dump_typechecked = args.count("dump-tc");
    compiler.dump_desugared = args.count("dump-ds");
    compiler.dump_optimized = args.count("dump-opt");
    compiler.recompile = optional_value<string>(args, "recompile");
    compiler.optimize = args.at("optimize").as<bool>();
    compiler.fully_lazy = args.at("fully-lazy").as<bool>();
    compiler.pre_inline = args.at("pre-inline").as<bool>();
    compiler.post_inline = args.at("post-inline").as<bool>();
    compiler.let_float_from_case = args.at("let-float-from-case").as<bool>();
    compiler.let_float_from_apply = args.at("let-float-from-apply").as<bool>();
    compiler.let_float_from_let = args.at("let-float-from-let").as<bool>();
    compiler.case_of_constant = args.at("case-of-constant").as<bool>();
    compiler.case_of_variable = args.at("case-of-variable").as<bool>();
    compiler.case_of_case = args.at("case-of-case").as<bool>();
    compiler.inline_threshold = args.at("inline-threshold").as<int>();
    compiler.beta_reduction = args.at("beta-reduction").as<bool>();
    compiler.simplifier_max_iterations = args.at("simplifier-max-iterations").as<int>();
    compiler.dump_ffi = args.count("dump-ffi");
    compiler.force_cpp = args.count("cpp");
    compiler.cpp_definitions = vector_value<string>(args, "cpp-define");
    compiler.cpp_undefinitions = vector_value<string>(args, "cpp-undefine");
    compiler.dump_cpp = args.count("dump-cpp");

    if (args.count("run"))
    {
        auto run_arguments = args.at("run").as<vector<string>>();
        if (run_arguments.empty())
            throw myexception()<<"--run requires at least one argument";

        fs::path program = run_arguments.front();
        if (program.extension() != ".hs")
            program += ".hs";
        run_arguments.erase(run_arguments.begin());
        auto program_arguments = trailing_args(argc, argv, trailing_args_separator);
        run_arguments.insert(run_arguments.end(), program_arguments.begin(), program_arguments.end());
        command_line.command = RunCommand{std::move(program), std::move(run_arguments)};
    }
    else if (args.count("print"))
    {
        command_line.command = PrintCommand{
            args.at("print").as<string>(),
            vector_value<string>(args, "alphabet")
        };
    }
    else if (args.count("type"))
        command_line.command = TypeCommand{args.at("type").as<string>()};
    else if (args.count("test-module"))
        command_line.command = TestModuleCommand{args.at("test-module").as<string>()};
    else
    {
        InferOptions infer;
        infer.alignments = vector_value<string>(args, "align");
        infer.iterations = optional_value<long int>(args, "iterations");
        infer.name = optional_value<string>(args, "name");
        infer.subsample = args.at("subsample").as<int>();
        infer.log_format = optional_value<string>(args, "log-format");
        infer.pre_burnin = args.at("pre-burnin").as<int>();
        infer.enable = optional_value<string>(args, "enable");
        infer.disable = optional_value<string>(args, "disable");
        infer.beta = optional_value<string>(args, "beta");
        infer.dbeta = optional_value<string>(args, "dbeta");
        infer.tree = optional_value<string>(args, "tree");
        infer.unalign = args.count("unalign");
        infer.alphabets = vector_value<string>(args, "alphabet");
        infer.smodels = vector_value<string>(args, "smodel");
        infer.imodels = vector_value<string>(args, "imodel");
        infer.scales = vector_value<string>(args, "scale");
        infer.fixed = vector_value<string>(args, "fix");
        infer.variables = vector_value<string>(args, "variables");
        infer.links = vector_value<string>(args, "link");
        infer.subst_rates = args.at("subst-rates").as<string>();
        infer.indel_rates = args.at("indel-rates").as<string>();
        infer.partition_weights = optional_value<string>(args, "partition-weights");
        infer.topology_constraint = optional_value<string>(args, "t-constraint");
        infer.alignment_constraint = optional_value<string>(args, "a-constraint");
        infer.align_constraint = optional_value<string>(args, "align-constraint");
        infer.likelihood_calculators = optional_value<string>(args, "likelihood-calculators");
        command_line.command = std::move(infer);
    }

    return command_line;
}

/// Parse with the existing Boost interface, then expose only the parser-independent representation.
CommandLine parse_cmd_line(int argc, char* argv[])
{
    auto args = parse_boost_options(argc, argv);
    return adapt_boost_options(args, argc, argv);
}

string get_command_line(int argc, char* argv[])
{
    vector<string> args;
    for(int i=0;i<argc;i++)
	args.push_back(argv[i]);

    return join(args," ");
}

set<string> get_log_formats(const InferOptions& options, bool is_A_T_model)
{
    string log_format = is_A_T_model ? "tsv" : "json";
    if (options.log_format)
        log_format = *options.log_format;
    auto log_formats_vec = split(log_format,',');
    set<string> log_formats;
    for(auto& format: log_formats_vec)
        log_formats.insert(format);
    return log_formats;
}
