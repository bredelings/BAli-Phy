#include <vector>
#include <fstream>

#include <CLI/CLI.hpp>

#include "mcon/mcon.hh"
#include "util/io.hh"
#include "stats-table.hh"
#include "util/myexception.hh"
#include "util/string/join.hh"

using namespace std;

int main(int argc,char* argv[]) 
{ 
    try 
    {
        CLI::App app{"Concatenate statistics tables or transform one MCON log.", "stats-cat"};
        app.usage("Usage: stats-cat [OPTIONS] FILE [FILE ...]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        vector<string> filenames, ignore, select;
        int skip = 0, subsample = 1, last = -1;
        string out_format = "tsv";
        bool do_unnest = false;
        auto* skip_option = app.add_option("-s,--skip", skip, "Number of initial data rows to skip")
            ->type_name("N");
        auto* subsample_option = app.add_option("-x,--subsample", subsample, "Keep every Nth data row")
            ->type_name("N")->capture_default_str();
        auto* until_option = app.add_option("-u,--until", last, "Read up to this data row")
            ->type_name("N");
        app.add_option("-I,--ignore", ignore, "Exclude fields")
            ->type_name("FIELD")->type_size(1)->expected(1)->allow_extra_args(false)->take_all();
        app.add_option("-S,--select", select, "Include only these fields")
            ->type_name("FIELD")->type_size(1)->expected(1)->allow_extra_args(false)->take_all();
        auto* output_option = app.add_option("-O,--output", out_format, "Output format: json or tsv")
            ->type_name("FORMAT")->capture_default_str();
        app.add_flag("--unnest", do_unnest, "Unnest MCON fields (implies JSON output)");
        app.add_option("FILE", filenames, "Input statistics files ('-' reads stdin)")
            ->required()->type_name("");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

        if (output_option->count())
        {
            for(auto& c: out_format)
                c = std::tolower(c);
            if (out_format != "tsv" and out_format != "json")
                throw myexception()<<"I don't understand output format '"<<out_format<<"'";
        }
        else if (do_unnest)
            out_format = "json";

        if (out_format == "tsv" and do_unnest)
            throw myexception()<<"--unnest cannot be combined with --output tsv.";

        // it looks like currently we do not allow converting tsv to json, just json to tsv.
        if (out_format == "json")
        {
            if (filenames.size() != 1)
                throw myexception()<<"JSON output requires exactly one input file.";
            if (skip_option->count() or until_option->count() or subsample_option->count())
                throw myexception()<<"--skip, --subsample, and --until are not supported with JSON output.";

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
                if (do_unnest)
                    h["nested"] = false;
                std::cout<<h<<"\n";
            }

            while(portable_getline(*file,line))
            {
                auto j = json::parse(line, {}, {.allow_infinity_and_nan=true}).as_object();
                if (do_unnest)
                {
                    auto j2 = MCON::unnest(j);
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
        for(auto& reader: readers)
            while(auto row = reader.get_row())
                join(std::cout, *row,'\t')<<"\n";
    }
    catch (std::exception& e) {
        std::cerr<<"stats-cat: Error! "<<e.what()<<endl;
        exit(1);
    }

    return 0;
}

