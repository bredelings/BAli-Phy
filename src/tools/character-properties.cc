#include <algorithm>
#include <array>
#include <cassert>
#include <charconv>
#include <cmath>
#include <iostream>
#include <string>
#include <string_view>
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
    std::string statistic = "mean";
    std::string selection = "all";
    double selection_value = 0;
    std::string order = "column";
    std::optional<std::string> condition;
    bool default_positive_condition = false;
    bool positive_selection = false;
    std::string format;
};

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
    result.order = arguments["sort"].as<string>();
    if (result.order != "column" and result.order != "increasing" and result.order != "decreasing")
        throw myexception()<<"Unknown report sort '"<<result.order<<"'.";
    if (arguments.count("above"))
    {
        result.selection = "above";
        result.selection_value = arguments["above"].as<double>();
    }
    else if (arguments.count("highest"))
    {
        result.selection = "highest";
        result.selection_value = parse_percentage(arguments["highest"].as<string>(), "--highest");
    }
    else if (arguments.count("below"))
    {
        result.selection = "below";
        result.selection_value = arguments["below"].as<double>();
    }
    else if (arguments.count("lowest"))
    {
        result.selection = "lowest";
        result.selection_value = parse_percentage(arguments["lowest"].as<string>(), "--lowest");
    }
    else if (positive_selection)
    {
        result.selection = "above";
        result.selection_value = 0.5;
    }

    if (not positive_selection)
    {
        result.statistic = arguments["by"].as<string>();
        if (result.statistic != "mean" and result.statistic != "median")
            throw myexception()<<"Unknown report statistic '"<<result.statistic<<"'.";
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

/// Use enough significant digits for report values to round-trip through a double.
static std::string format_number(double value)
{
    std::array<char, 64> buffer;
    auto [end, error] = std::to_chars(buffer.data(), buffer.data() + buffer.size(), value);
    assert(error == std::errc());
    return {buffer.data(), end};
}

/// Encode one letter's posterior mean, standard deviation, and median.
static json::object summary_to_json(const character_properties::letter_posterior_summary& summary)
{
    return {{"mean", summary.mean}, {"sd", summary.sd}, {"median", summary.median}};
}

/// Encode the already validated CLI selection without another internal choice representation.
static json::object selection_to_json(const report_arguments& arguments)
{
    json::object result{{"kind", arguments.selection}};
    if (arguments.selection == "above" or arguments.selection == "below")
        result["threshold"] = arguments.selection_value;
    else if (arguments.selection == "highest" or arguments.selection == "lowest")
        result["fraction"] = arguments.selection_value;
    return result;
}

/// Start a standalone version-3 report document with metadata common to both row shapes.
static json::object report_json(const report_arguments& arguments, const char* kind,
                                std::uint64_t retained_samples, std::uint64_t total_retained_samples,
                                std::size_t candidate_letters, std::size_t selected_letters)
{
    return {
        {"format", "bali-phy-character-property-report"},
        {"version", 3},
        {"kind", kind},
        {"property", arguments.property},
        {"statistic", arguments.statistic},
        {"selection", selection_to_json(arguments)},
        {"sort", arguments.order},
        {"condition", arguments.condition ? json::value(*arguments.condition) : json::value(nullptr)},
        {"condition_value", arguments.condition ? json::value(true) : json::value(nullptr)},
        {"retained_samples", retained_samples},
        {"total_retained_samples", total_retained_samples},
        {"candidate_letters", candidate_letters},
        {"selected_letters", selected_letters}
    };
}

/// Write the human-readable report metadata shared by the two concrete table shapes.
static void write_text_header(const report_arguments& arguments, std::uint64_t retained_samples,
                              std::uint64_t total_retained_samples)
{
    std::cout<<(arguments.positive_selection ? "Positive-selection property: " : "Character property: ")
             <<arguments.property<<"\n";
    if (arguments.condition)
        std::cout<<"Posterior view: "<<*arguments.condition<<" = true\n"
                 <<"Matching samples: "<<retained_samples<<" of "<<total_retained_samples<<"\n";
    else
        std::cout<<"Posterior view: model-averaged\nRetained samples: "<<retained_samples<<"\n";
    std::cout<<"Selection: "<<arguments.selection;
    if (arguments.selection == "above" or arguments.selection == "below")
        std::cout<<" "<<format_number(arguments.selection_value);
    else if (arguments.selection == "highest" or arguments.selection == "lowest")
        std::cout<<" "<<format_number(arguments.selection_value * 100)<<"%";
    std::cout<<"\n\n";
}

/// Order ordinary all-column summaries by their requested middle value.
static void order_columns(std::vector<character_properties::column_property_summary>& rows,
                          const report_arguments& arguments)
{
    if (arguments.order == "column")
        return;
    // Compare the requested across-letter middle, using the displayed column as the stable tie-breaker.
    std::ranges::sort(rows, [&arguments](const auto& first, const auto& second) {
        double first_score = arguments.statistic == "mean" ? first.mean_middle : first.median_middle;
        double second_score = arguments.statistic == "mean" ? second.mean_middle : second.median_middle;
        if (first_score != second_score)
            return arguments.order == "increasing" ? first_score < second_score : first_score > second_score;
        return first.alignment_column < second.alignment_column;
    });
}

/// Order selected column representatives without changing which letter represents each column.
static void order_selected_columns(std::vector<character_properties::selected_column>& rows,
                                   const report_arguments& arguments)
{
    if (arguments.order == "column")
        return;
    // Compare the representative letter score without revisiting selection or grouping.
    std::ranges::sort(rows, [&arguments](const auto& first, const auto& second) {
        double first_score = arguments.statistic == "mean" ? first.property_summary.mean
                                                            : first.property_summary.median;
        double second_score = arguments.statistic == "mean" ? second.property_summary.mean
                                                             : second.property_summary.median;
        if (first_score != second_score)
            return arguments.order == "increasing" ? first_score < second_score : first_score > second_score;
        return first.alignment_column < second.alignment_column;
    });
}

/// Write an ordinary all-column report in the requested public format.
static void write_column_report(const report_arguments& arguments, std::uint64_t retained_samples,
                                std::uint64_t total_retained_samples, std::size_t letter_count,
                                const std::vector<character_properties::column_property_summary>& rows)
{
    if (arguments.format == "json")
    {
        json::array encoded_rows;
        for (const auto& row: rows)
            encoded_rows.push_back(json::object{
                {"column_index", row.alignment_column},
                {"letter_count", row.letter_count},
                {"posterior_means", json::object{{"minimum", row.mean_minimum},
                                                   {"middle", row.mean_middle},
                                                   {"maximum", row.mean_maximum}}},
                {"posterior_medians", json::object{{"minimum", row.median_minimum},
                                                     {"middle", row.median_middle},
                                                     {"maximum", row.median_maximum}}}
            });
        auto document = report_json(arguments, "property-columns", retained_samples, total_retained_samples,
                                    letter_count, letter_count);
        document["rows"] = std::move(encoded_rows);
        std::cout<<json::serialize(document)<<"\n";
        return;
    }

    if (arguments.format == "tsv")
        std::cout<<"column\tletters\tmean-min\tmean-middle\tmean-max\tmedian-min\tmedian-middle\tmedian-max\n";
    else
    {
        write_text_header(arguments, retained_samples, total_retained_samples);
        std::cout<<"Column  Letters  Mean min  Mean middle  Mean max  Median min  Median middle  Median max\n";
    }
    for (const auto& row: rows)
    {
        const char* separator = arguments.format == "tsv" ? "\t" : "  ";
        std::cout<<row.alignment_column + 1<<separator<<row.letter_count<<separator
                 <<format_number(row.mean_minimum)<<separator<<format_number(row.mean_middle)<<separator
                 <<format_number(row.mean_maximum)<<separator<<format_number(row.median_minimum)<<separator
                 <<format_number(row.median_middle)<<separator<<format_number(row.median_maximum)<<"\n";
    }
}

/// Encode one representative-letter row for the standalone JSON report.
static json::object selected_column_to_json(const character_properties::selected_column& row)
{
    json::object encoded{
        {"column_index", row.alignment_column},
        {"sequence_index", row.sequence_index},
        {"sequence", row.sequence_name},
        {"character_index", row.character_index},
        {"symbol", row.symbol},
        {"translation", row.translation ? json::value(*row.translation) : json::value(nullptr)},
        {"statistics", summary_to_json(row.property_summary)}
    };
    if (row.companion_summary)
        encoded["companion"] = json::object{{"property", *row.companion_property},
                                             {"statistics", summary_to_json(*row.companion_summary)}};
    else
        encoded["companion"] = nullptr;
    return encoded;
}

/// Write a selected-letter or positive-selection report in the requested public format.
static void write_selected_report(const report_arguments& arguments, std::uint64_t retained_samples,
                                  std::uint64_t total_retained_samples, std::size_t candidate_letters,
                                  std::size_t selected_letters,
                                  const std::vector<character_properties::selected_column>& rows)
{
    if (arguments.format == "json")
    {
        json::array encoded_rows;
        for (const auto& row: rows)
            encoded_rows.push_back(selected_column_to_json(row));
        auto kind = arguments.positive_selection ? "positive-selection" : "property-selection";
        auto document = report_json(arguments, kind, retained_samples, total_retained_samples,
                                    candidate_letters, selected_letters);
        document["rows"] = std::move(encoded_rows);
        std::cout<<json::serialize(document)<<"\n";
        return;
    }

    if (arguments.format == "tsv")
        std::cout<<"column\tsequence\tsequence-character\tsymbol\ttranslation\tmean\tsd\tmedian"
                 <<"\tcompanion-property\tcompanion-mean\tcompanion-sd\tcompanion-median\n";
    else
    {
        write_text_header(arguments, retained_samples, total_retained_samples);
        std::cout<<"Column  Sequence  Character  Symbol  Translation  "
                 <<(arguments.positive_selection ? "Probability mean +/- SD  Probability median"
                                                  : "Mean +/- SD  Median");
        if (not rows.empty() and rows.front().companion_property)
            std::cout<<"  Companion  Companion mean +/- SD  Companion median";
        std::cout<<"\n";
    }

    for (const auto& row: rows)
    {
        if (arguments.format == "tsv")
        {
            std::cout<<row.alignment_column + 1<<"\t"<<row.sequence_name<<"\t"<<row.character_index + 1<<"\t"
                     <<row.symbol<<"\t"<<row.translation.value_or("")<<"\t"<<format_number(row.property_summary.mean)
                     <<"\t"<<format_number(row.property_summary.sd)<<"\t"
                     <<format_number(row.property_summary.median)<<"\t";
            if (row.companion_summary)
                std::cout<<*row.companion_property<<"\t"<<format_number(row.companion_summary->mean)<<"\t"
                         <<format_number(row.companion_summary->sd)<<"\t"<<format_number(row.companion_summary->median);
            else
                std::cout<<"\t\t\t";
        }
        else
        {
            std::cout<<row.alignment_column + 1<<"  "<<row.sequence_name<<"  "<<row.character_index + 1<<"  "
                     <<row.symbol<<"  "<<row.translation.value_or("-")<<"  "<<format_number(row.property_summary.mean)
                     <<" +/- "<<format_number(row.property_summary.sd)<<"  "
                     <<format_number(row.property_summary.median);
            if (row.companion_summary)
                std::cout<<"  "<<*row.companion_property<<"  "<<format_number(row.companion_summary->mean)<<" +/- "
                         <<format_number(row.companion_summary->sd)<<"  "<<format_number(row.companion_summary->median);
        }
        std::cout<<"\n";
    }
}

/// Count projected non-gap letters without introducing report metadata into the computation library.
static std::size_t count_projected_letters(const character_properties::alignment_projection& projection)
{
    std::size_t count = 0;
    for (const auto& column: projection)
        count += column.characters.size();
    return count;
}

/// Load report inputs, resolve conditioning once, compute concrete rows, and write them.
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

    const auto* selected_properties = &properties.properties;
    std::uint64_t retained_samples = properties.retained_samples;
    if (arguments.condition)
    {
        auto found = properties.conditioned.find(*arguments.condition);
        if (found == properties.conditioned.end())
            throw myexception()<<"Character property condition '"<<*arguments.condition<<"' was not found.";
        if (found->second.retained_samples == 0)
            throw myexception()<<"Character property condition '"<<*arguments.condition<<"' has no True samples.";
        selected_properties = &found->second.properties;
        retained_samples = found->second.retained_samples;
    }
    auto property = selected_properties->find(arguments.property);
    if (property == selected_properties->end())
        throw myexception()<<"Character property '"<<arguments.property<<"' was not found.";

    std::size_t candidate_letters = count_projected_letters(projection);
    if (not arguments.positive_selection and arguments.selection == "all")
    {
        auto rows = character_properties::summarize_property_columns(property->second, projection);
        order_columns(rows, arguments);
        write_column_report(arguments, retained_samples, properties.retained_samples, candidate_letters, rows);
        return;
    }

    std::optional<double> threshold;
    std::optional<double> fraction;
    if (arguments.selection == "above" or arguments.selection == "below")
        threshold = arguments.selection_value;
    else
        fraction = arguments.selection_value;
    std::size_t selected_letters = 0;
    std::vector<character_properties::selected_column> rows;
    if (arguments.positive_selection)
    {
        std::string_view suffix = "posSelection";
        std::string companion_name = arguments.property.substr(0, arguments.property.size() - suffix.size())+"dNdS";
        const character_properties::property_summary* companion = nullptr;
        if (auto found = selected_properties->find(companion_name); found != selected_properties->end())
            companion = &found->second;
        rows = character_properties::select_positive_selection_columns(
            arguments.property, property->second, projection, threshold, fraction,
            companion_name, companion, selected_letters);
    }
    else
        rows = character_properties::select_property_columns(
            property->second, projection, arguments.statistic == "median", threshold, fraction,
            arguments.selection == "above" or arguments.selection == "highest", selected_letters);
    order_selected_columns(rows, arguments);
    write_selected_report(arguments, retained_samples, properties.retained_samples,
                          candidate_letters, selected_letters, rows);
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
