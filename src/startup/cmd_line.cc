#include <boost/algorithm/string/replace.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/algorithm/string.hpp>
#include "startup/cmd_line.H"
#include "startup/paths.H"
#include "util.H"
#include "../io.H"
#include "setup.H"
#include "version.H"
#include "models/rules.H"
#include "models/parse.H"
#include "help.hh"
#include "util/ptree.H"

using std::string;
using std::map;
using std::vector;
using std::cout;
using boost::optional;

namespace po = boost::program_options;
using po::variables_map;

const string trailing_args_separator = "---";

namespace fs = boost::filesystem;

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
	("help,h", value<string>()->implicit_value("simple"),"Print usage information.")
	("version,v", "Print version information.")
	("test,t","Analyze the initial values and exit.")
	;
    if (level >= 1)
	general.add_options()
	    ("verbose,V",value<int>()->implicit_value(1),"Print extra output in case of error.")
	    ("config,c", value<string>(),"Config file to read.");
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
	("pre-inline",value<bool>()->default_value(true),"Pre-inline unconditionally")
	("post-inline",value<bool>()->default_value(true),"Post-inline unconditionally")
	("let-float-from-case",value<bool>()->default_value(true),"Let float from case")
	("let-float-from-apply",value<bool>()->default_value(true),"Let float from apply")
	("let-float-from-let",value<bool>()->default_value(true),"Let float from let")
	("case-of-constant",value<bool>()->default_value(true),"Case of constant")
	("case-of-variable",value<bool>()->default_value(true),"Case of constant")
	("case-of-case",value<bool>()->default_value(true),"Case of case")
	("inline-threshhold",value<int>()->default_value(8),"Inline threshhold")
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
	    ("unalign,U","Unalign sequences if variable-A");

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
	    ("model,m",value<string>(),"File containing hierarchical model.")
	    ("Model,M",value<string>(),"Module containing hierarchical model.")
	    ("print,p",value<string>(),"Evaluate and print expression.")
	    ("initial-value",value<vector<string> >()->composing(),"Set parameter=<initial value>");
    return model;
}

po::options_description developer_options()
{
    using namespace po;

    options_description developer("Developer options");
    developer.add_options()
	("test-module",value<string>(),"Parse and optimize the given module")
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
    return "Usage: bali-phy <sequence-file1> [<sequence-file2> [OPTIONS]]";
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    options_description all("Developer options - some may not work!");
    all.add(general_options(3)).add(mcmc_options(3)).add(parameters_options(3)).add(model_options(3)).add(haskell_optimization()).add(developer_options());
    
    options_description expert("Expert options - some may not work!");
    expert.add(general_options(2)).add(mcmc_options(2)).add(parameters_options(2)).add(model_options(2));
    
    options_description advanced("Advanced options");
    advanced.add(general_options(1)).add(mcmc_options(1)).add(parameters_options(1)).add(model_options(1));
    
    options_description simple("Simple options");
    simple.add(general_options(0)).add(mcmc_options(0)).add(parameters_options(0)).add(model_options(0));

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
	    topic = args.count("help")?args["help"].as<string>():"simple";

	auto package_paths = get_package_paths(argv[0], args);
	if (topic == "simple")
	{
	    cout<<short_description()<<"\n";
	    cout<<usage()<<"\n";
	    cout<<simple<<"\n";
	    cout<<"Try --help=help for a list of topics to ask for help on.\n\n";
	}
	else if (topic == "advanced")
	{
	    cout<<short_description()<<"\n";
	    cout<<usage()<<"\n";
	    cout<<advanced<<"\n";
	    cout<<"Try --help=help for a list of topics to ask for help on.\n\n";
	}
	else if (topic == "expert")
	{
	    cout<<short_description()<<"\n";
	    cout<<usage()<<"\n";
	    cout<<expert<<"\n";
	    cout<<"Try --help=help for a list of topics to ask for help on.\n\n";
	}
	else if (topic == "developer")
	{
	    cout<<short_description()<<"\n";
	    cout<<usage()<<"\n";
	    cout<<all<<"\n";
	    cout<<"Try --help=help for a list of topics to ask for help on.\n\n";
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
    for(auto word : {"align", "Model", "model", "print", "test-module"})
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

