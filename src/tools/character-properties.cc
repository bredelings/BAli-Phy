#include <cmath>
#include <iostream>
#include <string>
#include <vector>

#include <boost/program_options.hpp>

#include "alignment/character-property-alignment.H"
#include "alignment/character-property-report.H"
#include "alignment/character-property-summary.H"
#include "sequence/alphabet.H"
#include "sequence/sequence.H"
#include "util/json.hh"
#include "util/myexception.H"

namespace po = boost::program_options;

using character_properties::summarize_options;
using std::string;
using std::vector;

/// Print the public command overview, including one typical invocation of every command.
static void show_help()
{
    std::cout<<"Summarize and report sampled character properties.\n\n"
             <<"Usage: character-properties COMMAND [OPTIONS]\n\n"
             <<"Commands:\n"
             <<"  summarize           Compute posterior summaries from sampled character properties.\n"
             <<"  report              Report an ordinary property by alignment column.\n"
             <<"  positive-selection  Report columns containing positively selected letters.\n\n"
             <<"Examples:\n"
             <<"  character-properties summarize C1.properties1.json C1.properties2.json\n"
             <<"  character-properties report P1.character-properties.json P1.initial.fasta rate\n"
             <<"  character-properties positive-selection P1.character-properties.json P1.initial.fasta\n";
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
                 <<visible<<"\n"
                 <<"Examples:\n"
                 <<"  character-properties summarize C1.properties1.json C1.properties2.json\n"
                 <<"  character-properties summarize C1.properties1.json --skip 1000 --subsample 2\n";
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
    std::string property;
    character_properties::posterior_statistic statistic = character_properties::posterior_statistic::mean;
    character_properties::letter_selection selection;
    character_properties::report_order order = character_properties::report_order::column;
    std::optional<std::string> condition;
    bool default_positive_condition = false;
    bool positive_selection = false;
    std::string format;
};

/// Decode a report ordering while keeping command-line spellings separate from the report implementation.
static character_properties::report_order parse_report_order(const std::string& name)
{
    if (name == "column")
        return character_properties::report_order::column;
    if (name == "increasing")
        return character_properties::report_order::increasing;
    if (name == "decreasing")
        return character_properties::report_order::decreasing;
    throw myexception()<<"Unknown report sort '"<<name<<"'.";
}

/// Parse a required percent sign and return the fraction represented by the argument.
static double parse_percentage(const std::string& text, const std::string& option)
{
    if (not text.ends_with('%'))
        throw myexception()<<option<<" requires a percentage ending in '%'.";
    std::size_t used = 0;
    double percentage;
    try
    {
        percentage = std::stod(text.substr(0, text.size() - 1), &used);
    }
    catch (const std::exception&)
    {
        throw myexception()<<"Invalid percentage '"<<text<<"' for "<<option<<".";
    }
    if (used != text.size() - 1 or not std::isfinite(percentage) or percentage <= 0 or percentage > 100)
        throw myexception()<<option<<" percentage must be greater than 0% and at most 100%.";
    return percentage / 100;
}

/// Parse and validate the generic or positive-selection report command.
static report_arguments parse_report_options(int argc, char* argv[], bool positive_selection)
{
    po::options_description positional("Positional options");
    positional.add_options()
        ("summary", po::value<string>(), "character-property summary")
        ("alignment", po::value<string>(), "template alignment")
        ("property", po::value<string>(), "property to report");

    po::options_description visible("Allowed options");
    visible.add_options()
        ("help,h", "Produce help message.")
        ("alphabet", po::value<string>(), "Alignment alphabet or partial alphabet constraint.")
        ("format", po::value<string>()->default_value("text"), "Output format: text, tsv, or json.")
        ("sort", po::value<string>()->default_value("column"), "Row order: column, increasing, or decreasing.")
        ("condition", po::value<string>(), "Use only samples where this Boolean condition is true.")
        ("above", po::value<double>(), positive_selection ? "Select probabilities above this value."
                                                          : "Select letters whose score is above this value.")
        ("highest", po::value<string>()->implicit_value("1%"), "Select the highest-scoring percentage of letters.");
    if (positive_selection)
        visible.add_options()
            ("unconditional", "Use the model-averaged posterior instead of conditioning.");
    else
        visible.add_options()
            ("below", po::value<double>(), "Select letters whose score is below this value.")
            ("lowest", po::value<string>()->implicit_value("1%"), "Select the lowest-scoring percentage of letters.")
            ("by", po::value<string>()->default_value("mean"), "Letter score: mean or median.");

    po::options_description all;
    all.add(positional).add(visible);
    po::positional_options_description positions;
    positions.add("summary", 1).add("alignment", 1).add("property", 1);

    po::variables_map arguments;
    po::store(po::command_line_parser(argc, argv).options(all).positional(positions).run(), arguments);
    po::notify(arguments);

    if (arguments.count("help"))
    {
        if (positive_selection)
            std::cout<<"Report columns containing letters with posterior evidence of positive selection.\n\n"
                     <<"Usage: character-properties positive-selection [OPTIONS] SUMMARY ALIGNMENT [PROPERTY]\n\n";
        else
            std::cout<<"Report an ordinary posterior character property by template-alignment column.\n\n"
                     <<"Usage: character-properties report [OPTIONS] SUMMARY ALIGNMENT PROPERTY\n\n";
        std::cout<<visible<<"\nExamples:\n";
        if (positive_selection)
            std::cout<<"  character-properties positive-selection P1.character-properties.json P1.initial.fasta\n"
                     <<"  character-properties positive-selection P1.character-properties.json P1.initial.fasta --above 0.95\n"
                     <<"  character-properties positive-selection P1.character-properties.json P1.initial.fasta --highest\n"
                     <<"  character-properties positive-selection summary.json alignment.fasta --unconditional\n";
        else
            std::cout<<"  character-properties report P1.character-properties.json P1.initial.fasta rate\n"
                     <<"  character-properties report summary.json alignment.fasta rate --above 2 --by median\n"
                     <<"  character-properties report summary.json alignment.fasta rate --highest=5% --sort decreasing\n"
                     <<"  character-properties report summary.json codons.fasta rate --alphabet Codons\n";
        std::exit(0);
    }

    for (const auto* required: {"summary", "alignment"})
        if (not arguments.count(required))
            throw myexception()<<"Argument '"<<required<<"' is required by this report command.";
    if (not positive_selection and not arguments.count("property"))
        throw myexception()<<"Argument 'property' is required by the report command.";

    int selection_count = arguments.count("above") + arguments.count("highest");
    if (not positive_selection)
        selection_count += arguments.count("below") + arguments.count("lowest");
    if (selection_count > 1)
        throw myexception()<<"Specify only one of --above, --below, --highest, or --lowest.";
    if (positive_selection and arguments.count("condition") and arguments.count("unconditional"))
        throw myexception()<<"--condition and --unconditional cannot be used together.";

    report_arguments result;
    result.summary_filename = arguments["summary"].as<string>();
    result.alignment_filename = arguments["alignment"].as<string>();
    result.alphabet_name = arguments.count("alphabet") ? arguments["alphabet"].as<string>() : "";
    result.property = arguments.count("property") ? arguments["property"].as<string>() : "posSelection";
    result.positive_selection = positive_selection;
    result.order = parse_report_order(arguments["sort"].as<string>());
    if (arguments.count("above"))
        result.selection = {character_properties::letter_selection_kind::above, arguments["above"].as<double>()};
    else if (arguments.count("highest"))
        result.selection = {character_properties::letter_selection_kind::highest_fraction,
                            parse_percentage(arguments["highest"].as<string>(), "--highest")};
    else if (arguments.count("below"))
        result.selection = {character_properties::letter_selection_kind::below, arguments["below"].as<double>()};
    else if (arguments.count("lowest"))
        result.selection = {character_properties::letter_selection_kind::lowest_fraction,
                            parse_percentage(arguments["lowest"].as<string>(), "--lowest")};
    else if (positive_selection)
        result.selection = {character_properties::letter_selection_kind::above, 0.5};

    if (not positive_selection)
    {
        auto statistic = arguments["by"].as<string>();
        if (statistic == "mean")
            result.statistic = character_properties::posterior_statistic::mean;
        else if (statistic == "median")
            result.statistic = character_properties::posterior_statistic::median;
        else
            throw myexception()<<"Unknown report statistic '"<<statistic<<"'.";
    }
    if (arguments.count("condition"))
        result.condition = arguments["condition"].as<string>();
    else if (positive_selection and not arguments.count("unconditional"))
    {
        result.condition = "positiveSelectionInModel";
        result.default_positive_condition = true;
    }

    result.format = arguments["format"].as<string>();
    if (result.format != "text" and result.format != "tsv" and result.format != "json")
        throw myexception()<<"Unknown report format '"<<result.format<<"'.";
    return result;
}

/// Load report inputs, project them onto template columns, and write the requested representation.
static void run_report(const report_arguments& arguments)
{
    auto properties = character_properties::read_summary(arguments.summary_filename);
    if (arguments.default_positive_condition and not properties.conditioned.contains(*arguments.condition))
        throw myexception()<<"Character property condition 'positiveSelectionInModel' was not found; "
                           <<"use --unconditional to report the model-averaged posterior.";
    auto sequences = character_properties::load_template_alignment(arguments.alignment_filename);
    auto full_alphabet_name = guess_alphabet(arguments.alphabet_name, sequences);
    auto alph = get_alphabet(full_alphabet_name);
    auto tokens = character_properties::tokenize_alignment(sequences, alph.get());
    character_properties::validate_for_alignment(properties, sequences, tokens);
    auto projection = character_properties::project_alignment(sequences, tokens, *alph);
    auto view = character_properties::select_posterior_view(properties, arguments.condition);

    // Dispatch only after input resolution so both report types share the same validated posterior view.
    auto report = [&]() {
        if (arguments.positive_selection)
        {
            character_properties::positive_selection_report_options options;
            options.property = arguments.property;
            options.selection = arguments.selection;
            options.order = arguments.order;
            return character_properties::make_positive_selection_report(view, projection, options);
        }
        character_properties::property_report_options options;
        options.property = arguments.property;
        options.statistic = arguments.statistic;
        options.selection = arguments.selection;
        options.order = arguments.order;
        return character_properties::make_property_report(view, projection, options);
    }();

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
            run_report(parse_report_options(argc - 1, argv + 1, false));
        else if (command == "positive-selection")
            run_report(parse_report_options(argc - 1, argv + 1, true));
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
