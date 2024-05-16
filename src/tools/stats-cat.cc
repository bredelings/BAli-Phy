#include <vector>
#include <fstream>

#include <boost/program_options.hpp>
#include <boost/scoped_ptr.hpp>

#include "util/io.H"
#include "stats-table.H"
#include "util/myexception.H"
#include "util/owned-ptr.H"
#include "util/string/join.H"

using namespace std;

namespace po = boost::program_options;
using po::variables_map;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
    using namespace po;

    // named options
    options_description invisible("Invisible options");
    invisible.add_options()
        ("filenames", value<vector<string> >()->composing(),"Filenames to analyze (empty for STDIN)")
        ;

    options_description visible("All options");
    visible.add_options()
        ("help,h", "Produce help message.")
	("verbose,V","Output more log messages on stderr.")

        ("skip,s",value<int>(),"Number of initial lines to skip.")
        ("subsample,x",value<int>()->default_value(1),"Factor by which to sub-sample.")
	("until,u",value<int>(),"Read up to this iteration.")

	("ignore,I", value<vector<string> >()->composing(),"Do not analyze these fields.")
	("select,S", value<vector<string> >()->composing(),"Analyze only these fields.")
        ("output,O", value<string>(), "Output format: json, tsv")
	("unnest", "Unnest JSON file.");

    options_description all("All options");
    all.add(invisible).add(visible);

    // positional options
    positional_options_description p;
    p.add("filenames", -1);

    variables_map args;     
    store(command_line_parser(argc, argv).
          options(all).positional(p).run(), args);
    notify(args);

    if (args.count("help")) {
        cout<<"Append tab-delimited files with the same field names.\n\n";
        cout<<"Usage: stats-cat [OPTIONS] file1 [file2 file3 ... ] \n\n";
        cout<<visible<<"\n";
        cout<<"Default: Report the median and 95% credible interval for each column.\n\n";
        exit(0);
    }

    return args;
}


int main(int argc,char* argv[]) 
{ 
    try 
    {
        variables_map args = parse_cmd_line(argc,argv);

        vector<string> filenames = args["filenames"].as< vector<string> >();

        int skip = 0;
        if (args.count("skip"))
            skip = args["skip"].as<int>();

        int subsample = 1;
        if (args.count("subsample"))
            subsample = args["subsample"].as<int>();

        int last = -1;
        if (args.count("until"))
            last = args["until"].as<int>();

	vector<string> ignore;
	if (args.count("ignore"))
	    ignore = args["ignore"].as<vector<string> >();

	vector<string> select;
	if (args.count("select"))
	    select = args["select"].as<vector<string> >();

        if (not args.count("filenames"))
            throw myexception()<<"No filenames specified.\n\nTry `"<<argv[0]<<" --help' for more information.";

        string out_format = "tsv";
        if (args.count("output"))
        {
            out_format = args["output"].as<string>();
            for(auto& c: out_format)
                c = std::tolower(c);
            if (out_format != "tsv" and out_format != "json")
                throw myexception()<<"I don't understand output format '"<<out_format<<"'";
        }
        else
        {
            if (args.count("unnest"))
                out_format = "json";
        }

        // it looks like currently we do not allow converting tsv to json, just json to tsv.
        if (out_format == "json")
        {
            if (not filenames.size())
                throw myexception()<<"--unnest: at least one file required.";

            auto file = shared_ptr<istream>(new istream_or_ifstream(std::cin, "-", filenames[0], "statistics file"));

            auto is_json = (file->peek() == '{');
            if (not is_json)
                throw myexception()<<"--unnest: file must be in JSON format";

	    std::cout<<json::serialize_options({.allow_infinity_and_nan=true});

            string line;
            if (portable_getline(*file,line))
            {
                auto h = json::parse(line, {},{.allow_infinity_and_nan=true}).as_object();
                if (not h.count("version"))
                    throw myexception()<<"JSON log file does not have a valid header line: no \"version\" field.";
                h["nested"] = false;
                std::cout<<h<<"\n";
            }

            bool do_unnest = args.count("unnest");
            while(portable_getline(*file,line))
            {
                auto j = json::parse(line, {}, {.allow_infinity_and_nan=true}).as_object();
                if (do_unnest)
                {
                    auto j2 = unnest_json(std::move(j));
                    std::swap(j, j2);
                }
                for(auto& field: ignore)
                    j.erase(field);
                if (select.size())
                {
		    json::object j2;
                    for(auto& field: select)
                    {
                        auto it = j.find(field);
                        if (it != j.end())
                            j2.insert(*it); //[field] = it->second;
                    }
                    std::swap(j,j2);
                }
                std::cout<<j<<"\n";
            }
            exit(0);
        }

        // Check that all files have the same field names
        vector<string> field_names;
        vector<shared_ptr<istream> > files(filenames.size());
        vector<TableReader> readers;

        for(int i=0;i<filenames.size();i++)
        {
            files[i] = shared_ptr<istream>(new istream_or_ifstream(std::cin, "-", filenames[i], "statistics file"));

            if (not *files[i])
                throw myexception()<<"Can't open file '"<<filenames[i]<<"'";

            readers.push_back( TableReader(*files[i], skip, subsample, last, ignore, select) );

            if (readers[0].names() != readers[i].names())
                throw myexception()<<filenames[i]<<": Column names differ from names in '"<<filenames[0]<<"'";
        }

        // Write all the files to cout, in the specified order, but with only one header
        write_header(std::cout,readers[0].names());
        for(auto reader: readers)
            while(auto row = reader.get_row())
                join(std::cout, *row,'\t')<<"\n";
    }
    catch (std::exception& e) {
        std::cerr<<"stats-cat: Error! "<<e.what()<<endl;
        exit(1);
    }

    return 0;
}


