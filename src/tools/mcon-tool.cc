#include "mcon/mcon.H"

#include <boost/program_options.hpp>
#include <fstream>
#include <filesystem>

namespace po = boost::program_options;

using std::string;
using std::vector;

po::variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description invisible("Invisible options");
    invisible.add_options()
	("filename",value<string>(),"file to convert")
	;

    // named options
    options_description visible("Allowed options");
    visible.add_options()
	("help,h", "produce help message")
	("unnest", "file with alignment to annotate")
	("atomize","file with tree")
	("split",value<string>(),"split and write to filenames with prefix=arg")
	("drop",value<vector<string>>()->multitoken(),"paths to drop")
	("output,O",value<string>()->default_value("MCON"),"output format (TSV or MCON)")
	;

    options_description all("All options");
    all.add(invisible).add(visible);

    // positional options
    positional_options_description p;
    p.add("filename", -1);
  
    variables_map args;     
    store(command_line_parser(argc, argv).
	  options(all).positional(p).run(), args);
    // store(parse_command_line(argc, argv, desc), args);
    notify(args);    

    if (args.count("help")) {
	std::cout<<"Convert MCON files\n\n";
	std::cout<<"Usage: mcon-tool <infile> [OPTIONS]\n\n";
	std::cout<<visible<<"\n";
	exit(0);
    }

    return args;
}

int main(int argc,char* argv[])
{
    try {
	//---------- Parse command line  -------//
	auto args = parse_cmd_line(argc,argv);

	std::istream* instream = nullptr;
	std::filesystem::path filename;
	std::ifstream infile;
	if (args.count("filename"))
	{
	    filename = args.at("filename").as<string>();
	    infile.open(filename);
	    if (not infile)
	    {
		std::cerr<<"Error: can't open file "<<filename<<"\n";
		exit(1);
	    }
	    instream = &infile;
	}
	else
	{
	    instream = &std::cin;
	    filename = "<STDIN>";
	}

	auto logfile = MCON::read_logfile(*instream, filename);

	if (args.count("drop"))
	    for(auto& name: args.at("drop").as<vector<string>>())
		logfile.drop(name);

	if (args.count("unnest"))
	    logfile.unnest();

	if (args.count("atomize"))
	    logfile.atomize();

	auto output = args.at("output").as<string>();

	if (args.count("split"))
	{
	    std::filesystem::path split_filename = args.at("split").as<string>();
	    auto logs = logfile.split();
	    for(int i=0;i<logs.size();i++)
	    {
		auto tmp_filename = split_filename;
		tmp_filename += "." + std::to_string(i+1);
		if (output == "mcon" or output=="MCON")
		{
		    tmp_filename += ".json";
		    std::ofstream outfile(tmp_filename);
		    logs[i].dump_MCON(outfile);
		    outfile.close();
		}
		else if (output == "tsv" or output == "TSV")
		{
		    tmp_filename += ".tsv";
		    std::ofstream outfile(tmp_filename);
		    logs[i].dump_TSV(outfile);
		    outfile.close();
		}
	    }
	}

	if (output == "mcon" or output == "MCON")
	    logfile.dump_MCON(std::cout);
	else if (output == "tsv" or output == "TSV")
	    logfile.dump_TSV(std::cout);
	else
	{
	    std::cerr<<"Error: Unrecognized output format '"<<output<<"'"<<std::endl;
	    exit(1);
	}
    }
    catch (std::exception& e) {
	std::cerr<<"mcon-tool: Error! "<<e.what()<<std::endl;
	exit(1);
    }
    return 0;
}

