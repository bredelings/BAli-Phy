#include <iostream>
#include <string>
#include <vector>

#include <boost/program_options.hpp>

#include "alignment/character-property-summary.H"
#include "util/json.hh"
#include "util/myexception.H"

namespace po = boost::program_options;

using character_properties::summarize_options;
using std::string;
using std::vector;

/// Print the public command overview used by both --help and missing-command errors.
static void show_help()
{
    std::cout<<"Summarize and report sampled character properties.\n\n"
             <<"Usage: character-properties COMMAND [OPTIONS]\n\n"
             <<"Commands:\n"
             <<"  summarize   Compute posterior summaries from sampled character properties.\n";
}

/// Parse and validate options for the summarize subcommand.
static summarize_options parse_summarize_options(int argc, char* argv[])
{
    po::options_description positional("Positional options");
    positional.add_options()
        ("filenames", po::value<vector<string>>()->composing(), "property sample files");

    po::options_description visible("Allowed options");
    visible.add_options()
        ("help,h", "Produce help message.")
        ("skip", po::value<std::int64_t>(), "Discard samples at or before this iteration.")
        ("until", po::value<std::int64_t>(), "Discard samples after this iteration.")
        ("subsample", po::value<std::int64_t>()->default_value(1),
         "Retain every Nth eligible sample per chain.")
        ("median-memory", po::value<std::int64_t>()->default_value(256),
         "Target MiB of working memory for exact medians.");

    po::options_description all;
    all.add(positional).add(visible);
    po::positional_options_description positions;
    positions.add("filenames", -1);

    po::variables_map arguments;
    po::store(po::command_line_parser(argc, argv).options(all).positional(positions).run(), arguments);
    po::notify(arguments);

    if (arguments.count("help"))
    {
        std::cout<<"Compute posterior property summaries for each observed sequence character.\n\n"
                 <<"Usage: character-properties summarize [OPTIONS] SAMPLE-FILE [SAMPLE-FILE ...]\n\n"
                 <<visible<<"\n";
        std::exit(0);
    }

    summarize_options options;
    if (arguments.count("filenames"))
        for (const auto& filename: arguments["filenames"].as<vector<string>>())
            options.filenames.emplace_back(filename);
    if (arguments.count("skip"))
        options.selection.skip = arguments["skip"].as<std::int64_t>();
    if (arguments.count("until"))
        options.selection.until = arguments["until"].as<std::int64_t>();
    auto subsample = arguments["subsample"].as<std::int64_t>();
    auto median_memory_mib = arguments["median-memory"].as<std::int64_t>();
    if (subsample <= 0)
        throw myexception()<<"--subsample must be positive.";
    if (median_memory_mib <= 0)
        throw myexception()<<"--median-memory must be positive.";
    options.selection.subsample = static_cast<std::uint64_t>(subsample);
    options.median_memory_mib = static_cast<std::size_t>(median_memory_mib);
    return options;
}

/// Dispatch the requested character-property command and report concise errors.
int main(int argc, char* argv[])
{
    try
    {
        if (argc == 1 or string(argv[1]) == "--help" or string(argv[1]) == "-h")
        {
            show_help();
            return 0;
        }
        if (string(argv[1]) != "summarize")
            throw myexception()<<"Unknown command '"<<argv[1]<<"'.";

        auto options = parse_summarize_options(argc - 1, argv + 1);
        auto result = character_properties::summarize(options);
        std::cout<<json::serialize(character_properties::to_json(result))<<"\n";
        return 0;
    }
    catch (const std::exception& error)
    {
        std::cerr<<"character-properties: Error! "<<error.what()<<"\n";
        return 1;
    }
}
