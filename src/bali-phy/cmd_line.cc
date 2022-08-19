#include <filesystem>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string.hpp>
#include "cmd_line.H"
#include "paths.H"
#include "util/string/join.H"
#include "util/text.H"
#include "util/io.H"
#include "util/file-paths.H"
#include "version.H"
#include "models/rules.H"
#include "models/parse.H"
#include "help.hh"
#include "util/ptree.H"
#include "util/log-level.H"

using std::string;
using std::map;
using std::vector;
using std::cout;
using std::cerr;
using std::endl;
using std::optional;

namespace po = boost::program_options;
using po::variables_map;

const string trailing_args_separator = "--";

namespace fs = std::filesystem;

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
	    ("package-path,P",value<string>(),"Directories to search for packages.")
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
	    ("disable",value<string>(),"Comma-separated list of kernels to disable.")
	    ("Rao-Blackwellize",value<string>(),"Parameter names to print Rao-Blackwell averages for.");

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
	("dump-ds","Show desugared output")
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
	("keenness",value<double>()->default_value(1.5),"Keenness factor")
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
	("tree,T",value<string>(),"File with initial tree");

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
	("scale,R",value<vector<string> >()->composing(),"Prior on the scale.");
    if (level >= 1)
	model.add_options()
	("branch-lengths,B",value<string>(),"Prior on branch lengths.");
    model.add_options()
	("link,L",value<vector<string>>()->composing(),"Link partitions.");

    if (level >= 2)
	model.add_options()
	    ("model,m",value<vector<string>>()->multitoken(),"File containing hierarchical model.")
            ("Model,M",value<vector<string>>()->multitoken(),"Module containing hierarchical model.")
	    ("print,p",value<string>(),"Evaluate and print expression.");
    return model;
}

po::options_description developer_options()
{
    using namespace po;

    options_description developer("Developer options");
    developer.add_options()
	("test-module",value<string>(),"Parse and optimize the given module")
	("--no-type-check","Type-check modules")
	("run-module,r",value<vector<string>>()->multitoken(),"Run the given module")
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

variables_map parse_cmd_line(int argc,char* argv[]) 
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

    string command;
    string topic;
    vector<string> cargs = drop_trailing_args(argc, argv, trailing_args_separator);
    if (cargs.size()>=1 and cargs[0] == "help")
    {
	command = "help";
	cargs.erase(cargs.begin());
	if (cargs.size() >= 1)
	{
	    topic = cargs[0];
	    cargs.erase(cargs.begin());
	}
    }
    variables_map args;
    store(command_line_parser(cargs).options(all).positional(p).run(), args);
    notify(args);    

    if (args.count("version")) {
	print_version_info(cout);
	exit(0);
    }

    if (args.count("verbose")) log_verbose = args["verbose"].as<int>();

    if (args.count("help") or command == "help")
    {
	if (topic.empty())
	    topic = args.count("help")?args["help"].as<string>():"basic";

	auto package_paths = get_package_paths(argv[0], args);
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
	    cout<<"  * `bali-phy help "<<bold("normal")<<"` shows help on the normal distribution.\n";
	    cout<<"  * `bali-phy help "<<bold("tn93")<<"` shows help on the TN93 model.\n";
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

	store(parse_config_file(file, all), args);
	notify(args);
    }

    load_bali_phy_rc(args,all);

    std::set<string> commands;
    for(auto word : {"align", "Model", "model", "print", "test-module", "run-module"})
	if (args.count(word))
	    commands.insert(word);

    if (commands.empty())
	throw myexception()<<"You must specify alignment files or a generic model (--model or --Model).\n\nTry `"<<argv[0]<<" --help' for more information.";

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

string get_command_line(int argc, char* argv[])
{
    vector<string> args;
    for(int i=0;i<argc;i++)
	args.push_back(argv[i]);

    return join(args," ");
}

