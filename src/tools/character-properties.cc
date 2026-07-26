#include <iostream>
#include <string>
#include <vector>

#include <boost/program_options.hpp>

#include "alignment/character-property-alignment.H"
#include "alignment/character-property-report.H"
#include "alignment/character-property-summary.H"
#include "sequence/alphabet.H"
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
             <<"  summarize   Compute posterior summaries from sampled character properties.\n"
             <<"  report      Report one property by template-alignment column.\n";
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

struct report_arguments
{
    std::filesystem::path summary_filename;
    std::filesystem::path alignment_filename;
    std::string alphabet_name;
    character_properties::report_options options;
    std::string format;
};

/// Decode a report kind while keeping command-line spellings separate from the report implementation.
static character_properties::report_kind parse_report_kind(const std::string& name)
{
    if (name == "property")
        return character_properties::report_kind::property;
    if (name == "positive-selection")
        return character_properties::report_kind::positive_selection;
    throw myexception()<<"Unknown report kind '"<<name<<"'.";
}

/// Decode a report ordering while keeping command-line spellings separate from the report implementation.
static character_properties::report_sort parse_report_sort(const std::string& name)
{
    if (name == "column")
        return character_properties::report_sort::column;
    if (name == "mean-ascending")
        return character_properties::report_sort::mean_ascending;
    if (name == "mean-descending")
        return character_properties::report_sort::mean_descending;
    if (name == "sd-descending")
        return character_properties::report_sort::sd_descending;
    throw myexception()<<"Unknown report sort '"<<name<<"'.";
}

/// Parse and validate options for the report subcommand.
static report_arguments parse_report_options(int argc, char* argv[])
{
    po::options_description positional("Positional options");
    positional.add_options()
        ("summary", po::value<string>(), "character-property summary")
        ("alignment", po::value<string>(), "template alignment");

    po::options_description visible("Allowed options");
    visible.add_options()
        ("help,h", "Produce help message.")
        ("alphabet", po::value<string>(), "Alignment alphabet.")
        ("property", po::value<string>(), "Property to report.")
        ("kind", po::value<string>(), "Report kind: property or positive-selection.")
        ("format", po::value<string>()->default_value("text"), "Output format: text, tsv, or json.")
        ("sort", po::value<string>(), "Row order: column, mean-ascending, mean-descending, or sd-descending.")
        ("minimum-probability", po::value<double>(), "Minimum probability for positive-selection rows.");

    po::options_description all;
    all.add(positional).add(visible);
    po::positional_options_description positions;
    positions.add("summary", 1).add("alignment", 1);

    po::variables_map arguments;
    po::store(po::command_line_parser(argc, argv).options(all).positional(positions).run(), arguments);
    po::notify(arguments);

    if (arguments.count("help"))
    {
        std::cout<<"Report one posterior character property by template-alignment column.\n\n"
                 <<"Usage: character-properties report [OPTIONS] SUMMARY ALIGNMENT\n\n"
                 <<visible<<"\n";
        std::exit(0);
    }
    for (const auto* required: {"summary", "alignment", "alphabet", "property", "kind"})
        if (not arguments.count(required))
            throw myexception()<<"Option '"<<required<<"' is required by the report command.";

    report_arguments result;
    result.summary_filename = arguments["summary"].as<string>();
    result.alignment_filename = arguments["alignment"].as<string>();
    result.alphabet_name = arguments["alphabet"].as<string>();
    result.options.property = arguments["property"].as<string>();
    result.options.kind = parse_report_kind(arguments["kind"].as<string>());
    if (arguments.count("sort"))
        result.options.sort = parse_report_sort(arguments["sort"].as<string>());
    else if (result.options.kind == character_properties::report_kind::positive_selection)
        result.options.sort = character_properties::report_sort::mean_descending;
    else
        result.options.sort = character_properties::report_sort::column;
    if (arguments.count("minimum-probability"))
        result.options.minimum_probability = arguments["minimum-probability"].as<double>();
    result.format = arguments["format"].as<string>();
    if (result.format != "text" and result.format != "tsv" and result.format != "json")
        throw myexception()<<"Unknown report format '"<<result.format<<"'.";
    return result;
}

/// Load report inputs, project them onto template columns, and write the selected representation.
static void run_report(const report_arguments& arguments)
{
    auto properties = character_properties::read_summary(arguments.summary_filename);
    auto sequences = character_properties::load_template_alignment(arguments.alignment_filename);
    auto alph = get_alphabet(arguments.alphabet_name);
    auto tokens = character_properties::tokenize_alignment(sequences, alph.get());
    character_properties::validate_for_alignment(properties, sequences, tokens);
    auto projection = character_properties::project_alignment(sequences, tokens, *alph);
    auto report = character_properties::make_report(properties, projection, arguments.options);

    if (arguments.format == "json")
        std::cout<<json::serialize(character_properties::to_json(report))<<"\n";
    else if (arguments.format == "tsv")
        character_properties::write_tsv(std::cout, report);
    else
        character_properties::write_text(std::cout, report);
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
        string command = argv[1];
        if (command == "summarize")
        {
            auto options = parse_summarize_options(argc - 1, argv + 1);
            auto result = character_properties::summarize(options);
            std::cout<<json::serialize(character_properties::to_json(result))<<"\n";
        }
        else if (command == "report")
            run_report(parse_report_options(argc - 1, argv + 1));
        else
            throw myexception()<<"Unknown command '"<<command<<"'.";
        return 0;
    }
    catch (const std::exception& error)
    {
        std::cerr<<"character-properties: Error! "<<error.what()<<"\n";
        return 1;
    }
}
