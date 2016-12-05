#include "startup/cmd_line.H"
#include "util.H"
#include "../io.H"
#include "setup.H"
#include "version.H"

using std::string;
using std::vector;
using std::cout;

namespace po = boost::program_options;
using po::variables_map;

const string trailing_args_separator = "---";

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

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  options_description advanced("Advanced options");
  advanced.add_options()
    ("pre-burnin",value<int>()->default_value(3),"Iterations to refine initial tree.")
    ("Rao-Blackwellize",value<string>(),"Parameter names to print Rao-Blackwell averages for.")
    ("randomize-alignment","Randomly realign the sequences before use.")
    ("package-path,P",value<string>(),"Directories to search for packages (':'-separated)")
    ("internal",value<string>(),"If set to '+', then make all internal node entries wildcards")
    ("align-constraint",value<string>(),"File with alignment constraints.")
    ("enable",value<string>(),"Comma-separated list of kernels to enable.")
    ("disable",value<string>(),"Comma-separated list of kernels to disable.")
    ("model,m",value<string>(),"File containing hierarchical model.")
    ("Model,M",value<string>(),"Module containing hierarchical model.")
    ;

  options_description developer("Developer options");
  developer.add_options()
    ("partition-weights",value<string>(),"File containing tree with partition weights")
    ("beta",value<string>(),"MCMCMC temperature")
    ("dbeta",value<string>(),"MCMCMC temperature changes")
    ("t-constraint",value<string>(),"File with m.f. tree representing topology and branch-length constraints.")
    ("a-constraint",value<string>(),"File with groups of leaf taxa whose alignment is constrained.")
    ;

  // named options
  options_description general("General options");
  general.add_options()
    ("help,h", "Print usage information.")
    ("Help,H", "Print advanced usage information.")
    ("version,v", "Print version information.")
    ("config,c", value<string>(),"Config file to read.")
    ("test,T","Analyze the initial values and exit.")
    ("verbose,V","Print extra output in case of error.")
    ;

  options_description mcmc("MCMC options");
  mcmc.add_options()
    ("iterations,i",value<long int>()->default_value(100000),"The number of iterations to run.")
    ("subsample,x",value<int>()->default_value(1),"Factor by which to subsample.")
    ("seed,s", value<unsigned long>(),"Random seed")
    ("name,n", value<string>(),"Name for the analysis directory to create.")
    ;

  options_description parameters("Parameter options");
  parameters.add_options()
    ("align", value<vector<string> >()->composing(),"Sequence file & initial alignment.")
    ("tree",value<string>(),"File with initial tree")
    ("initial-value",value<vector<string> >()->composing(),"Set parameter=<initial value>")
    ("set",value<vector<string> >()->composing(),"Set key=<value>")
    ("unalign-all,U","Unalign all sequences before use.")
    ;

  options_description model("Model options");
  model.add_options()
    ("alphabet",value<vector<string> >()->composing(),"DNA, RNA, Amino-Acids, Codons, etc.")
    ("smodel,S",value<vector<string> >()->composing(),"Substitution model.")
    ("imodel,I",value<vector<string> >()->composing(),"Indel model: none, RS07, RS05, RS07-no-T.")
    ("traditional,t","Fix the alignment and don't model indels.")
    ("branch-prior",value<string>()->default_value("Gamma"),"Exponential or Gamma.")
    ("same-scale",value<vector<string> >()->composing(),"Which partitions have the same scale?")
    ;
  options_description all("All options");
  all.add(general).add(mcmc).add(parameters).add(model).add(advanced).add(developer);
  options_description some("All options");
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

  if (args.count("verbose"))
    log_verbose = 1;

  if (args.count("Help"))
  {
    cout<<"Usage: bali-phy <sequence-file1> [<sequence-file2> [OPTIONS]]\n";
    cout<<all<<"\n";
    exit(0);
  }
  if (args.count("help")) {
    cout<<"Usage: bali-phy <sequence-file1> [<sequence-file2> [OPTIONS]]\n";
    cout<<some<<"\n";
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

  if (args.count("align") and (args.count("model") or args.count("Model")))
      throw myexception()<<"You cannot specify both sequence files and a generic model.\n\nTry `"<<argv[0]<<" --help' for more information.";

  if (not args.count("align") and not args.count("model") and not args.count("Model"))
    throw myexception()<<"You must specify alignment files or a generic model (--model or --Model).\n\nTry `"<<argv[0]<<" --help' for more information.";

  if (args.count("model") and args.count("Model"))
    throw myexception()<<"You cannot specify both --model and --Model.\n\nTry `"<<argv[0]<<" --help' for more information.";

  if (not args.count("iterations"))
    throw myexception()<<"The number of iterations was not specified.\n\nTry `"<<argv[0]<<" --help' for more information.";

  return args;
}

string get_command_line(int argc, char* argv[])
{
  vector<string> args;
  for(int i=0;i<argc;i++)
    args.push_back(argv[i]);

  return join(args," ");
}

