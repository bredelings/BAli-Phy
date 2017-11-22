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

using std::string;
using std::map;
using std::vector;
using std::cout;
using boost::optional;

namespace po = boost::program_options;
using po::variables_map;

const string trailing_args_separator = "---";

namespace fs = boost::filesystem;

namespace pt = boost::property_tree;
using pt::ptree;

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

string get_topic_from_string(const string& s)
{
    string s2 = s;
    int index = s2.find_first_of("\r\n");
    if (index != string::npos)
	s2 = s2.substr(0,index);
    if (s2.empty() or s2[s2.size()-1] == '.')
	;
    else if (s2[s2.size()-1] == ':')
	s2[s2.size()-1] = '.';
    else
	s2.push_back('.');
    return s2;
}

string pad(const string& s, int n)
{
    if (s.size() < n)
	return s+string(n-s.size(),' ');
    return s;
}


void help_on_help(std::ostream& o, const map<string,string>& help)
{
    o<<"Help topics via --help=arg are available for:\n";
    o<<"  =commands            Command-line flags and a short description.\n";
    o<<"  =advanced            Extra command-line flags and a short description.\n\n";
    o<<"  =functions           A list of functions and result type.\n";
    o<<"  =<function name>     Function type, description, and argument names.\n\n";
    o<<"  =help                This list of topics.\n\n";
    for(auto& x: help)
	o<<"  ="<<pad(x.first,15)<<"     "<<get_topic_from_string(x.second)<<"\n";
    o<<"\n";
}

std::map<string,string> load_help_files(const std::vector<fs::path>& package_paths)
{
    map<string,string> help;

    for(auto& package_path: package_paths)
    {
	auto path = package_path / "help";

	if (fs::exists(path))
	    for(auto& dir_entry: fs::directory_iterator(path))
	    {
		auto abs_path = fs::canonical(dir_entry.path() );
		string topic = abs_path.stem().string();
		
		if (abs_path.extension() == ".txt" and not help.count(topic))
		    help[topic] = boost::trim_copy(read_file(abs_path.string(), "help file"));
	    }
    }

    return help;
}


string indent_lines(const string& lines, int n)
{
    std::ostringstream s;
    for(auto& line: split(lines,"\n"))
    {
	if (line.empty())
	    s<<std::endl;
	else
	    s<<string(n,' ')<<line<<std::endl;
    }
    return s.str();
}

optional<string> get_citation(const Rule& rule)
{
    auto citation = rule.get_child_optional("citation");
    if (not citation)
	return boost::none;

    if (not citation->get_value<string>().empty())
	return citation->get_value<string>();

    vector<string> cite;
    auto title = citation->get_optional<string>("title");
    auto year = citation->get_optional<string>("year");
    vector<string> authors;
    if (auto authors_ = citation->get_child_optional("author"))
	for(auto& author: *authors_)
	    if (auto name = author.second.get_optional<string>("name"))
		authors.push_back(*name);

    if (authors.size())
	cite.push_back(join(authors,",")+".");
    if (year)
	cite.push_back("("+*year+")");
    if (title)
	cite.push_back(*title);

    return join(cite," ");
}



optional<string> get_citation_doi(const Rule& rule)
{
    optional<string> url;

    // 1. Check if there is a citation field.
    auto citation = rule.get_child_optional("citation");
    if (not citation) return boost::none;

    // 2. Try to get the DOI
    if (auto identifiers = citation->get_child_optional("identifier"))
    {
	for(auto& identifier: *identifiers)
	{
	    auto type = identifier.second.get_child_optional("type");
	    if (not type or type->get_value<string>() != "doi") continue;

	    auto id = identifier.second.get_child_optional("id");
	    if (id)
		return id->get_value<string>();
	}
    }
    return boost::none;
}

optional<string> get_citation_url(const Rule& rule)
{
    // 1. Check if there is a citation field.
    auto citation = rule.get_child_optional("citation");
    if (not citation) return boost::none;

    // 2. Try to get the URL from the "link" field.
    if (auto links = citation->get_child_optional("link"))
    {
	for(auto& link: *links)
	{
	    auto url = link.second.get_child_optional("url");
	    if (not url) continue;

	    auto anchor = link.second.get_child_optional("anchor");
	    if (anchor)
		return url->get_value<string>()+"/#"+anchor->get_value<string>();
	    else
		return url->get_value<string>();
	}
	
    }

    if (auto doi = get_citation_doi(rule))
    {
	return "https://doi.org/"+*doi;
    }

    return boost::none;
}

string get_help_for_rule(const Rule& rule)
{
    std::ostringstream help;
    if (auto title = rule.get_optional<string>("title"))
	help<<*title<<std::endl<<std::endl;

    if (auto citation = get_citation(rule))
    {
	help<<*citation<<std::endl;
	if (auto url = get_citation_url(rule))
	    help<<*url<<std::endl;
	help<<std::endl;
    }
    
    string name = rule.get<string>("name");
    string result_type = unparse_type(rule.get_child("result_type"));
    auto args = rule.get_child("args");
    vector<string> args_names_types;
    // Actually, we may have a problem here...
    if (auto constraints = rule.get_child_optional("constraints"))
    {
	vector<string> cs;
	for(auto& x: *constraints)
	    cs.push_back(unparse_type(x.second));
//		help<<join(cs,", ")<<" => ";
    }
    for(auto& argpair: args)
    {
	auto& arg = argpair.second;
	if (arg.get_child_optional("no_apply")) continue;
	args_names_types.push_back(arg.get<string>("arg_name") + " :: " + unparse_type(arg.get_child("arg_type")));
    }
    help<<name;
    if (args_names_types.size()) help<<"["<<join(args_names_types,", ")<<"]";
    help<<" -> "<<result_type << std::endl<<std::endl;

    for(auto& argpair: args)
    {
	auto& arg = argpair.second;
	if (arg.get_child_optional("no_apply")) continue;
	auto default_value = arg.get_child_optional("default_value");
	optional<string> description = arg.get_optional<string>("description");
	if (default_value or description)
	{
	    help<<"   "<<arg.get<string>("arg_name");
	    if (default_value)
		help<<" "<<show_model(*default_value)<<std::endl;
	    else
		help<<":"<<std::endl;
	    if (description)
		help<<indent_lines(*description,7);
	    help<<std::endl;
	}
    }
    if (auto description = rule.get_optional<string>("description"))
	help<<indent_lines(*description,1)<<std::endl;

    if (auto examples = rule.get_child_optional("examples"))
    {
	help<<"Examples:\n";
	for(auto& x: *examples)
	{
	    help<<"   "<<x.second.get_value<string>()<<std::endl;
	}
    }
    return help.str();
}
	
variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    options_description advanced("Advanced options");
    advanced.add_options()
	("unalign,U","Unalign sequences (if variable-A)")
	("unalign-all","Unalign sequences")
	("pre-burnin",value<int>()->default_value(3),"Iterations to refine initial tree.")
	("beta",value<string>(),"MCMCMC temperature")
	("package-path,P",value<string>(),"Directories to search for packages (':'-separated)")
	("enable",value<string>(),"Comma-separated list of kernels to enable.")
	("disable",value<string>(),"Comma-separated list of kernels to disable.")
	("set",value<vector<string> >()->composing(),"Set key=<value>")
	("initial-value",value<vector<string> >()->composing(),"Set parameter=<initial value>")
	("model,m",value<string>(),"File containing hierarchical model.")
	("Model,M",value<string>(),"Module containing hierarchical model.")
	("test-module",value<string>(),"Parse and optimize the given module")
	("Rao-Blackwellize",value<string>(),"Parameter names to print Rao-Blackwell averages for.")
	("likelihood-calculators",value<string>(),"comma-separated integers")
	;

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
    
    options_description developer("Developer options");
    developer.add_options()
	("partition-weights",value<string>(),"File containing tree with partition weights")
	("dbeta",value<string>(),"MCMCMC temperature changes")
	("t-constraint",value<string>(),"File with m.f. tree representing topology and branch-length constraints.")
	("a-constraint",value<string>(),"File with groups of leaf taxa whose alignment is constrained.")
	("align-constraint",value<string>(),"File with alignment constraints.")
	;

    // named options
    options_description general("General options");
    general.add_options()
	("help,h", value<string>()->implicit_value("simple"),"Print usage information.")
	("version,v", "Print version information.")
	("config,c", value<string>(),"Config file to read.")
	("test,t","Analyze the initial values and exit.")
	("verbose,V",value<int>()->implicit_value(1),"Print extra output in case of error.")
	;

    options_description mcmc("MCMC options");
    mcmc.add_options()
	("iterations,i",value<long int>(),"The number of iterations to run.")
	("subsample,x",value<int>()->default_value(1),"Factor by which to subsample.")
	("seed,s", value<unsigned long>(),"Random seed.")
	("name,n", value<string>(),"Name for the output directory to create.")
	;

    options_description parameters("Parameter options");
    parameters.add_options()
	("align", value<vector<string> >()->composing(),"Sequence file & initial alignment.")
	("tree,T",value<string>(),"File with initial tree")
	;

    options_description model("Model options");
    model.add_options()
	("alphabet,A",value<vector<string> >()->composing(),"The alphabet.")
	("smodel,S",value<vector<string> >()->composing(),"Substitution model.")
	("imodel,I",value<vector<string> >()->composing(),"Insertion-deletion model.")
	("scale,R",value<vector<string> >()->composing(),"Prior on the scale.")
	("branch-length,B",value<string>(),"Prior on branch lengths.")
	("link,L",value<vector<string>>()->composing(),"Link partitions.")
	;
    options_description all("Advanced options");
    all.add(general).add(mcmc).add(parameters).add(model).add(advanced).add(optimization).add(developer);
    options_description some("Simple options");
    some.add(general).add(mcmc).add(parameters).add(model);

    // positional options
    positional_options_description p;
    p.add("align", -1);
  

    vector<string> cargs = drop_trailing_args(argc, argv, trailing_args_separator);
    variables_map args;
    store(command_line_parser(cargs).options(all).positional(p).run(), args);
    notify(args);    

    if (args.count("version")) {
	print_version_info(cout);
	exit(0);
    }

    if (args.count("verbose")) log_verbose = 1;

    if (args.count("help"))
    {
	string topic = args["help"].as<string>();
	if (topic == "simple")
	{
	    cout<<"Usage: bali-phy <sequence-file1> [<sequence-file2> [OPTIONS]]\n";
	    cout<<some<<"\n";
	    cout<<"Try --help=help for a list of topics to ask for help on.\n\n";
	    exit(0);
	}
	if (topic == "advanced")
	{
	    cout<<"Usage: bali-phy <sequence-file1> [<sequence-file2> [OPTIONS]]\n";
	    cout<<all<<"\n";
	    cout<<"Try --help=help for a list of topics to ask for help on.\n\n";
	    exit(0);
	}
	auto package_paths = get_package_paths(argv[0], args);
	auto help = load_help_files(package_paths);
	
	if (topic == "help")
	{
	    help_on_help(std::cout, help);
	    exit(0);
	}
	if (help.count(topic))
	{
	    std::cout<<help[topic];
	    std::cout<<std::endl;
	    exit(0);
	}

	Rules R(package_paths);
	if (topic == "functions")
	{
	    for(auto& rule: R)
	    {
		string name = rule.get_child("name").get_value<string>();
		string result_type = unparse_type(rule.get_child("result_type"));
		std::cout<<name<<" :: "<<result_type << std::endl;
	    }
	    exit(0);
	}

	if (auto rule = R.get_rule_for_func(topic))
	{
	    std::cout<<get_help_for_rule(*rule);
	    exit(0);
	}
	else
	{
	    cout<<"Help topic '"<<args["help"].as<string>()<<"' not found.\n\n";
	    help_on_help(cout,help);
	    exit(0);
	}
    }

    if (args.count("config")) 
    {
	string filename = args["config"].as<string>();
	checked_ifstream file(filename,"config file");

	store(parse_config_file(file, all), args);
	notify(args);
    }

    load_bali_phy_rc(args,all);

    if (args.count("align") and (args.count("model") or args.count("Model")))
	throw myexception()<<"You cannot specify both sequence files and a generic model.\n\nTry `"<<argv[0]<<" --help' for more information.";

    if (not args.count("align") and not args.count("model") and not args.count("Model") and not args.count("test-module"))
	throw myexception()<<"You must specify alignment files or a generic model (--model or --Model).\n\nTry `"<<argv[0]<<" --help' for more information.";

    if (args.count("model") and args.count("Model"))
	throw myexception()<<"You cannot specify both --model and --Model.\n\nTry `"<<argv[0]<<" --help' for more information.";

    return args;
}

string get_command_line(int argc, char* argv[])
{
    vector<string> args;
    for(int i=0;i<argc;i++)
	args.push_back(argv[i]);

    return join(args," ");
}

