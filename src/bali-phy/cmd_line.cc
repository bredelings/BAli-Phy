#include <filesystem>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/program_options/option.hpp>
#include <regex>
#include "cmd_line.H"
#include "paths.H"
#include "util/string/join.H"
#include "util/string/split.H"
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
using std::set;

namespace po = boost::program_options;
using po::variables_map;

const string trailing_args_separator = "--";

namespace fs = std::filesystem;

const std::regex rgx_simple( R""(:([^ \t]+)[ \t]+([^ \t"].*)[ \t]*)"" );
const std::regex rgx_quoted( R""(:([^ \t]+)[ \t]+"(([^\\"]|\\.)*)"[ \t]*)"" );
const std::regex rgx_no_arg( R""(:([^ \t]+)[ \t]*)"" );
const std::regex rgx_comment( R""([ \t]*#.*)"" );


string unescape_value(const string& line)
{
    std::ostringstream output;
    for(int i=0;i<line.size();i++)
    {
	char c = line[i];
	if (c == '\\')
	{
	    i++;
	    c = line[i];
	    if (c == 'n')
		c = '\n';
	    else if (c == 't')
		c = '\t';
	    else if (c == '\\')
		;
	    else if (c == '"')
		;
	    else
		throw myexception()<<"Invalid escape sequence '\\"<<c<<"' in option value \""<<line<<"\"";
	}
	output<<c;
    }
    return output.str();
}


po::parsed_options bali_config_file(std::istream& file, const po::options_description& options_desc)
{
    std::map<string,vector<string>> options_map;
    po::parsed_options options(&options_desc);
    std::ostringstream model_lines;

    string line;
    while(portable_getline(file, line))
    {
	std::smatch m;

	if (std::regex_match(line, m, rgx_comment))
	    continue;
	else if (not line.starts_with(':'))
	    model_lines<<line<<"\n";
	else if (std::regex_match(line, m, rgx_no_arg))
	{
	    string key = m[1];
	    options_map[key];
	}
	else if (std::regex_match(line, m, rgx_quoted))
	{
	    string key = m[1];
	    string value = unescape_value(m[2]);
	    options_map[key].push_back(value);
	}
	else if (std::regex_match(line, m, rgx_simple))
	{
	    string key = m[1];
	    string value = m[2];
	    options_map[key].push_back(value);
	}
	else
	    throw myexception()<<"Malformed line '"<<line<<"'. It should have the form '<key> <value>' or '<key> \"<value>\"";
    }
    options_map["variables"].push_back(model_lines.str());

    for(auto& [key,values]: options_map)
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
	("tree,T",value<string>(),"Tree prior: ~uniform_tree(taxa), ~uniform_rooted_tree(taxa), ~yule(taxa), etc.")
	("initial-tree",value<string>(),"Initial tree for MCMC (kept modifiable)");

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
            ("model,m",value<vector<string>>()->multitoken(),"File containing hierarchical model.")
            ("print,p",value<string>(),"Evaluate and print expression.");
    return model;
}

po::options_description developer_options()
{
    using namespace po;

    options_description developer("Developer options");
    developer.add_options()
	("test-module",value<string>(),"Parse and optimize the given module")
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
    else if (cargs.size()>=1 and cargs[0] == "model")
    {
        cargs[0] = "--model";
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

	auto package_paths = get_package_paths(args);
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

	store(bali_config_file(file, all), args);
	notify(args);
    }

    load_bali_phy_rc(args,all);

    std::set<string> commands;
    for(auto word : {"align", "model", "print", "test-module", "run", "type"})
	if (args.count(word))
	    commands.insert(word);

    if (commands.empty())
	throw myexception()<<"You must specify alignment files or a generic model (with --model).\n\nTry `"<<argv[0]<<" --help' for more information.";

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

set<string> get_log_formats(const boost::program_options::variables_map& args, bool is_A_T_model)
{
    string log_format = is_A_T_model ? "tsv" : "json";
    if (args.count("log-format"))
        log_format = args["log-format"].as<string>();
    auto log_formats_vec = split(log_format,',');
    set<string> log_formats;
    for(auto& format: log_formats_vec)
        log_formats.insert(format);
    return log_formats;
}
