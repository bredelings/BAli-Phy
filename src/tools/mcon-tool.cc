#include "mcon/mcon.hh"

#include <CLI/CLI.hpp>
#include <optional>
#include <fstream>
#include <stdexcept>
#include <filesystem>

using std::string;
using std::vector;

int main(int argc,char* argv[])
{
    try {
        CLI::App app{"Convert MCON files.", "mcon-tool"};
        app.usage("Usage: mcon-tool [FILE] [OPTIONS]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        std::optional<string> input, split, time_field;
        string output = "MCON";
        vector<string> drop;
        bool unnest = false, atomize = false;
        app.add_option("FILE", input, "file to convert (default: STDIN)")->type_name("");
        app.add_flag("--unnest", unnest, "flatten nested field names");
        app.add_flag("--atomize", atomize, "expand structured values into scalar fields");
        app.add_option("--split", split, "split and write to filenames with this prefix");
        app.add_option("--drop", drop, "paths to drop");
        app.add_option("-T,--add-time-field", time_field, "add field indicating the record number");
        app.add_option("-O,--output", output, "output format (TSV or MCON)")->capture_default_str();
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

	std::istream* instream = nullptr;
	std::filesystem::path filename;
	std::ifstream infile;
	if (input)
	{
	    filename = *input;
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

	for (const auto& name: drop)
            logfile.drop(name);

	if (unnest)
	    logfile.unnest();

	if (atomize)
	    logfile.atomize();

        bool output_mcon = output == "mcon" || output == "MCON";
        if (!output_mcon && output != "tsv" && output != "TSV")
            throw std::runtime_error("Unrecognized output format '"+output+"'");

	if (split)
	{
	    std::filesystem::path split_filename = *split;
	    auto logs = logfile.split();
	    for(int i=0;i<logs.size();i++)
	    {
		if (time_field)
		    logs[i].add_time_field(*time_field);

		auto tmp_filename = split_filename;
		tmp_filename += "." + std::to_string(i+1);
                tmp_filename += output_mcon ? ".json" : ".tsv";
                std::ofstream outfile(tmp_filename);
                if (!outfile)
                    throw std::runtime_error("Cannot open split output '"+tmp_filename.string()+"'");
                if (output_mcon)
                    logs[i].dump_MCON(outfile);
                else
                    logs[i].dump_TSV(outfile);
                outfile.close();
                if (!outfile)
                    throw std::runtime_error("Error writing split output '"+tmp_filename.string()+"'");
	    }
	}

	if (time_field)
	    logfile.add_time_field(*time_field);
        if (output_mcon)
            logfile.dump_MCON(std::cout);
        else
            logfile.dump_TSV(std::cout);
    }
    catch (std::exception& e) {
	std::cerr<<"mcon-tool: Error! "<<e.what()<<std::endl;
	exit(1);
    }
    return 0;
}

