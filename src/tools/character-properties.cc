#include <algorithm>
#include <array>
#include <cassert>
#include <charconv>
#include <cmath>
#include <iostream>
#include <string>
#include <string_view>
#include <vector>

#include <CLI/CLI.hpp>
#include <fmt/format.h>

#include "alignment/character-property-alignment.hh"
#include "alignment/character-property-report.hh"
#include "alignment/character-property-summary.hh"
#include "sequence/alphabet.hh"
#include "sequence/sequence.hh"
#include "util/json.hh"
#include "util/myexception.hh"

using character_properties::summarize_options;
using std::string;
using std::vector;

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
    std::string format = "text";
};

/// Parse a required percent sign and return the fraction represented by the argument.
/// Reject non-finite percentages and values outside the command's (0%, 100%] domain.
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

/// Register the reporting inputs and options shared by the three report commands.
/// Defaults live in each command's arguments, and only that command binds to them.
static void add_report_options(CLI::App& command, report_arguments& arguments)
{
    command.add_option("SUMMARY", arguments.summary_filename, "Character-property summary file")
        ->required()->type_name("");
    command.add_option("ALIGNMENT", arguments.alignment_filename, "Template alignment file")
        ->required()->type_name("");
    auto* property = command.add_option("PROPERTY", arguments.property, "Property to report")->type_name("");
    if (arguments.positive_selection)
        property->capture_default_str();
    else
        property->required();
    command.add_option("--alphabet", arguments.alphabet_name, arguments.positive_selection
                       ? "Alignment alphabet (default: Codons; DNA/RNA guessed from alignment)"
                       : "Alignment alphabet (default: guessed from alignment)");
    command.add_option("--format", arguments.format, "Output format")
        ->check(CLI::IsMember({"text", "tsv"}))->capture_default_str();
    command.add_option("--sort", arguments.order, arguments.positive_selection
                       ? "Row order; increasing/decreasing use model-averaged probability"
                       : "Row order; increasing/decreasing use the statistic selected by --by")
        ->check(CLI::IsMember({"column", "increasing", "decreasing"}))->capture_default_str();
    command.add_option("--condition", arguments.condition, arguments.positive_selection
                       ? "Pair model-averaged results with this True-conditioned posterior"
                       : "Use the posterior conditioned on this Boolean being true");
    if (not arguments.positive_selection)
        command.add_option("--by", arguments.statistic, "Posterior statistic for selection and sorting")
            ->check(CLI::IsMember({"mean", "median"}))->capture_default_str();
}

/// Use enough significant digits for report values to round-trip through a double.
/// The general to_chars representation supplies the shortest such decimal without locale dependence.
static std::string format_number(double value)
{
    std::array<char, 64> buffer;
    auto [end, error] = std::to_chars(buffer.data(), buffer.data() + buffer.size(), value);
    assert(error == std::errc());
    return {buffer.data(), end};
}

/// Write the human-readable report metadata shared by the two concrete table shapes.
/// TSV output omits this prose so that every emitted line remains a tabular record.
static void write_text_header(const report_arguments& arguments, std::uint64_t retained_samples,
                              std::uint64_t total_retained_samples)
{
    std::cout<<"Character property: "<<arguments.property<<"\n";
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
/// Column order is retained on request; equal values have no secondary ordering.
static void order_columns(std::vector<character_properties::column_property_summary>& rows,
                          const report_arguments& arguments)
{
    if (arguments.order == "column")
        return;
    // Compare the requested across-letter middle rather than either range endpoint.
    // Equal values do not need a secondary ordering policy.
    std::ranges::sort(rows, [&arguments](const auto& first, const auto& second) {
        double first_score = arguments.statistic == "mean" ? first.mean_middle : first.median_middle;
        double second_score = arguments.statistic == "mean" ? second.mean_middle : second.median_middle;
        return arguments.order == "increasing" ? first_score < second_score : first_score > second_score;
    });
}

/// Order selected column representatives without changing which letter represents each column.
/// Sorting uses the requested posterior statistic and leaves equal values unordered.
static void order_selected_columns(std::vector<character_properties::selected_column>& rows,
                                   const report_arguments& arguments)
{
    if (arguments.order == "column")
        return;
    // Compare the representative letter score without revisiting selection or grouping.
    // Equal values do not need a secondary ordering policy.
    std::ranges::sort(rows, [&arguments](const auto& first, const auto& second) {
        double first_score = arguments.statistic == "mean" ? first.property_summary.mean
                                                            : first.property_summary.median;
        double second_score = arguments.statistic == "mean" ? second.property_summary.mean
                                                             : second.property_summary.median;
        return arguments.order == "increasing" ? first_score < second_score : first_score > second_score;
    });
}

/// Write an ordinary all-column report in the requested public format.
/// Text includes report metadata, while TSV emits the same column statistics without prose.
static void write_column_report(const report_arguments& arguments, std::uint64_t retained_samples,
                                std::uint64_t total_retained_samples,
                                const std::vector<character_properties::column_property_summary>& rows)
{
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

/// Write a selected-letter report in TSV or its command-specific text layout.
/// Positive-selection output labels its indicator mean as a probability and its associated values as dN/dS.
static void write_selected_report(const report_arguments& arguments, std::uint64_t retained_samples,
                                  std::uint64_t total_retained_samples,
                                  const std::vector<character_properties::selected_column>& rows)
{
    if (arguments.format == "text" and arguments.positive_selection)
    {
        auto model_averaged_count = std::ranges::count_if(rows, [&arguments](const auto& row) {
            return arguments.selection == "above" and row.property_summary.mean > arguments.selection_value;
        });
        auto conditioned_count = std::ranges::count_if(rows, [&arguments](const auto& row) {
            return arguments.selection == "above" and row.conditioned_property_summary
                   and row.conditioned_property_summary->mean > arguments.selection_value;
        });
        bool has_conditioned = arguments.condition and retained_samples > 0;
        bool has_model_averaged_dnds = std::ranges::any_of(
            rows, [](const auto& row) { return row.dnds_summary.has_value(); });
        bool has_conditioned_dnds = std::ranges::any_of(
            rows, [](const auto& row) { return row.conditioned_dnds_summary.has_value(); });
        auto column_subject = [](auto count) {
            return fmt::format("{} column{}", count, count == 1 ? "" : "s");
        };

        std::cout<<"Positive selection: "<<arguments.property<<"\n";
        if (arguments.selection == "above")
        {
            if (arguments.condition and retained_samples == total_retained_samples)
            {
                std::cout<<*arguments.condition<<" was true in all "<<total_retained_samples
                         <<" retained samples, so the conditioned and model-averaged estimates use the same samples.\n";
                std::cout<<column_subject(model_averaged_count)
                         <<(model_averaged_count == 1 ? " has" : " have")<<" Pr(dN/dS>1) above "
                         <<format_number(arguments.selection_value)<<".\n\n";
            }
            else
            {
                std::cout<<column_subject(model_averaged_count)
                         <<(model_averaged_count == 1 ? " has" : " have")
                         <<" model-averaged Pr(dN/dS>1) above "
                         <<format_number(arguments.selection_value)<<".\n";
                if (arguments.condition and retained_samples == 0)
                    std::cout<<*arguments.condition<<" was not true in any of the "<<total_retained_samples
                             <<" retained samples; conditioned site estimates are unavailable.\n\n";
                else if (arguments.condition)
                    std::cout<<"When conditioned on "<<*arguments.condition<<" = true ("<<retained_samples<<" of "
                             <<total_retained_samples<<" retained samples), "<<column_subject(conditioned_count)
                             <<(conditioned_count == 1 ? " exceeds " : " exceed ")
                             <<format_number(arguments.selection_value)<<".\n\n";
                else
                    std::cout<<"\n";
            }
        }
        else
        {
            if (arguments.condition and retained_samples == 0)
                std::cout<<*arguments.condition<<" was not true in any of the "<<total_retained_samples
                         <<" retained samples; selecting from the model-averaged posterior instead.\n";
            else if (arguments.condition)
                std::cout<<"Conditioned on "<<*arguments.condition<<" = true ("<<retained_samples<<" of "
                         <<total_retained_samples<<" retained samples).\n";
            else
                std::cout<<"Model-averaged posterior ("<<retained_samples<<" retained samples).\n";
            std::cout<<"Showing columns containing the highest "<<format_number(arguments.selection_value * 100)
                     <<"% of letters by Pr(dN/dS>1).\n\n";
        }

        if (has_conditioned)
            std::cout<<fmt::format("{:>49}  {:>38}\n", "Model-averaged posterior", "Conditioned posterior");
        std::cout<<fmt::format("{:>6}  {:<5}  {:<2}  {:>11}", "Column", "Codon", "AA", "Pr(dN/dS>1)");
        if (has_model_averaged_dnds)
            std::cout<<fmt::format("  {:>15}", "dN/dS mean ± SD");
        if (has_conditioned)
        {
            std::cout<<fmt::format("  {:>11}", "Pr(dN/dS>1)");
            if (has_conditioned_dnds)
                std::cout<<fmt::format("  {:>15}", "dN/dS mean ± SD");
        }
        std::cout<<"  Source letter\n";
        for (const auto& row: rows)
        {
            std::cout<<fmt::format("{:>6}  {:<5}  {:<2}  {:>11.3f}",
                                   row.alignment_column + 1, row.symbol, row.translation.value_or("-"),
                                   row.property_summary.mean);
            if (row.dnds_summary)
                std::cout<<fmt::format("    {:>5.3f} ± {:>5.3f}", row.dnds_summary->mean,
                                       row.dnds_summary->sd);
            if (has_conditioned)
            {
                if (row.conditioned_property_summary)
                    std::cout<<fmt::format("  {:>11.3f}", row.conditioned_property_summary->mean);
                else
                    std::cout<<fmt::format("  {:>11}", "-");
                if (row.conditioned_dnds_summary)
                    std::cout<<fmt::format("    {:>5.3f} ± {:>5.3f}", row.conditioned_dnds_summary->mean,
                                           row.conditioned_dnds_summary->sd);
            }
            std::cout<<fmt::format("  {}:{}\n", row.sequence_name, row.character_index + 1);
        }
        return;
    }

    if (arguments.format == "tsv" and arguments.positive_selection)
        std::cout<<"column\tsequence\tsequence-character\tsymbol\ttranslation\tmodel-averaged-probability"
                 <<"\tmodel-averaged-dNdS-mean\tmodel-averaged-dNdS-sd\tmodel-averaged-dNdS-median"
                 <<"\tconditioned-probability\tconditioned-dNdS-mean\tconditioned-dNdS-sd"
                 <<"\tconditioned-dNdS-median\n";
    else if (arguments.format == "tsv")
        std::cout<<"column\tsequence\tsequence-character\tsymbol\ttranslation\tmean\tsd\tmedian\n";
    else
    {
        write_text_header(arguments, retained_samples, total_retained_samples);
        std::cout<<"Column  Sequence  Character  Symbol  Translation  Mean +/- SD  Median\n";
    }

    for (const auto& row: rows)
    {
        if (arguments.format == "tsv")
        {
            std::cout<<row.alignment_column + 1<<"\t"<<row.sequence_name<<"\t"<<row.character_index + 1<<"\t"
                     <<row.symbol<<"\t"<<row.translation.value_or("")<<"\t"<<format_number(row.property_summary.mean);
            if (arguments.positive_selection)
            {
                std::cout<<"\t";
                if (row.dnds_summary)
                    std::cout<<format_number(row.dnds_summary->mean)<<"\t"<<format_number(row.dnds_summary->sd)
                             <<"\t"<<format_number(row.dnds_summary->median);
                else
                    std::cout<<"\t\t";
                std::cout<<"\t";
                if (row.conditioned_property_summary)
                    std::cout<<format_number(row.conditioned_property_summary->mean);
                std::cout<<"\t";
                if (row.conditioned_dnds_summary)
                    std::cout<<format_number(row.conditioned_dnds_summary->mean)<<"\t"
                             <<format_number(row.conditioned_dnds_summary->sd)<<"\t"
                             <<format_number(row.conditioned_dnds_summary->median);
                else
                    std::cout<<"\t\t";
            }
            else
                std::cout<<"\t"<<format_number(row.property_summary.sd)<<"\t"
                         <<format_number(row.property_summary.median);
        }
        else
        {
            std::cout<<row.alignment_column + 1<<"  "<<row.sequence_name<<"  "<<row.character_index + 1<<"  "
                     <<row.symbol<<"  "<<row.translation.value_or("-")<<"  "<<format_number(row.property_summary.mean)
                     <<" +/- "<<format_number(row.property_summary.sd)<<"  "
                     <<format_number(row.property_summary.median);
        }
        std::cout<<"\n";
    }
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
        {
            if (not arguments.positive_selection)
                throw myexception()<<"Character property condition '"<<*arguments.condition<<"' has no True samples.";
            selected_properties = nullptr;
        }
        else
            selected_properties = &found->second.properties;
        retained_samples = found->second.retained_samples;
    }

    if (not arguments.positive_selection and arguments.selection == "all")
    {
        auto property = selected_properties->find(arguments.property);
        if (property == selected_properties->end())
            throw myexception()<<"Character property '"<<arguments.property<<"' was not found.";
        auto rows = character_properties::summarize_property_columns(property->second, projection);
        order_columns(rows, arguments);
        write_column_report(arguments, retained_samples, properties.retained_samples, rows);
        return;
    }

    std::optional<double> threshold;
    std::optional<double> fraction;
    if (arguments.selection == "above" or arguments.selection == "below")
        threshold = arguments.selection_value;
    else
        fraction = arguments.selection_value;
    std::vector<character_properties::selected_column> rows;
    if (arguments.positive_selection)
    {
        auto model_averaged_property = properties.properties.find(arguments.property);
        if (model_averaged_property == properties.properties.end())
            throw myexception()<<"Character property '"<<arguments.property<<"' was not found.";
        const character_properties::property_summary* conditioned_property = nullptr;
        if (selected_properties and selected_properties != &properties.properties)
        {
            auto found = selected_properties->find(arguments.property);
            if (found == selected_properties->end())
                throw myexception()<<"Character property '"<<arguments.property
                                   <<"' was not found in conditioned posterior '"<<*arguments.condition<<"'.";
            conditioned_property = &found->second;
        }

        std::string_view suffix = "posSelection";
        std::string dnds_name = arguments.property.substr(0, arguments.property.size() - suffix.size())+"dNdS";
        const character_properties::property_summary* model_averaged_dnds = nullptr;
        if (auto found = properties.properties.find(dnds_name); found != properties.properties.end())
            model_averaged_dnds = &found->second;
        const character_properties::property_summary* conditioned_dnds = nullptr;
        if (selected_properties and selected_properties != &properties.properties)
            if (auto found = selected_properties->find(dnds_name); found != selected_properties->end())
                conditioned_dnds = &found->second;
        rows = character_properties::select_positive_selection_columns(
            arguments.property, model_averaged_property->second, projection, threshold, fraction,
            model_averaged_dnds, conditioned_property, conditioned_dnds);
    }
    else
    {
        auto property = selected_properties->find(arguments.property);
        if (property == selected_properties->end())
            throw myexception()<<"Character property '"<<arguments.property<<"' was not found.";
        rows = character_properties::select_property_columns(
            property->second, projection, arguments.statistic == "median", threshold, fraction,
            arguments.selection == "above" or arguments.selection == "highest");
    }
    order_selected_columns(rows, arguments);
    write_selected_report(arguments, retained_samples, properties.retained_samples, rows);
}

/// Parse one character-property subcommand before loading inputs or producing output.
int main(int argc, char* argv[])
{
    try
    {
        CLI::App app{"Summarize and report sampled character properties.", "character-properties"};
        app.require_subcommand(1, 1);
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        app.get_formatter()->enable_footer_formatting(false);
        app.footer("Use character-properties COMMAND --help for options and examples.");

        summarize_options summary_options;
        report_arguments column_arguments, letter_arguments, positive_arguments;
        positive_arguments.positive_selection = true;
        positive_arguments.property = "posSelection";
        positive_arguments.alphabet_name = "Codons";
        // Exactly one command runs, so the two selecting commands can share these parse values.
        std::optional<double> above, below;
        std::optional<string> highest, lowest;
        bool unconditional = false;

        auto* summarize = app.add_subcommand("summarize", "Compute posterior summaries for each sequence character");
        summarize->add_option("SAMPLE-FILE", summary_options.filenames, "Property sample files, one per chain")
            ->required()->type_name("");
        summarize->add_option("--skip", summary_options.selection.skip, "Discard records before this iteration")
            ->check(CLI::NonNegativeNumber);
        summarize->add_option("--until", summary_options.selection.until, "Discard records after this iteration")
            ->check(CLI::NonNegativeNumber);
        summarize->add_option("--subsample", summary_options.selection.subsample,
                              "Retain every Nth eligible sample per chain")
            ->check(CLI::PositiveNumber)->capture_default_str();
        summarize->add_option("--median-memory", summary_options.median_memory_mib,
                              "Target MiB of working memory for exact medians")
            ->check(CLI::PositiveNumber)->capture_default_str();
        summarize->footer("Example: character-properties summarize run-1/C1.P1.site-property-samples.jsonl "
                          "run-2/C1.P1.site-property-samples.jsonl --skip=1000");

        auto* columns = app.add_subcommand("columns", "Report ranges and middle values within each alignment column");
        auto* letters = app.add_subcommand("letters", "Report one selected representative letter per alignment column");
        auto* positive = app.add_subcommand("positive-selection", "Report paired posterior support for positive selection");
        add_report_options(*columns, column_arguments);
        add_report_options(*letters, letter_arguments);
        add_report_options(*positive, positive_arguments);
        positive->add_flag("--unconditional", unconditional, "Report only the model-averaged posterior")
            ->excludes(positive->get_option("--condition"));

        for (auto* command: {letters, positive})
        {
            auto* selection = command->add_option_group("Selection", command == letters
                                                        ? "Choose exactly one selection option"
                                                        : "Choose at most one; default: --above=0.5");
            selection->require_option(command == letters ? 1 : 0, 1);
            selection->add_option("--above", above, command == letters
                                  ? "Select letters with scores strictly above VALUE"
                                  : "Select representatives with either probability strictly above VALUE")
                ->type_name("VALUE");
            selection->add_option("--highest", highest, "Select the highest-scoring percentage of letters (e.g. 5%)")
                ->type_name("PERCENT");
            if (command == letters)
            {
                selection->add_option("--below", below, "Select letters with scores strictly below VALUE")
                    ->type_name("VALUE");
                selection->add_option("--lowest", lowest, "Select the lowest-scoring percentage of letters (e.g. 5%)")
                    ->type_name("PERCENT");
            }
        }
        columns->footer("Both mean and median ranges are reported; --by chooses the statistic for sorting.\n\n"
                        "Example: character-properties columns P1.site-property-summary.json P1.initial.fasta rate");
        letters->footer("Select letters globally, then report one score extreme per selected column.\n\n"
                        "Example: character-properties letters summary.json alignment.fasta rate --highest=5%");
        positive->footer("By default, pair model-averaged results with the positiveSelectionInModel=True posterior.\n"
                         "Use --condition to choose another condition, or --unconditional for model-averaged results only.\n\n"
                         "Example: character-properties positive-selection summary.json alignment.fasta --above=0.95");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

        if (*summarize)
        {
            auto result = character_properties::summarize(summary_options);
            std::cout<<json::serialize(character_properties::to_json(result))<<"\n";
            return 0;
        }

        auto& arguments = *columns ? column_arguments : (*letters ? letter_arguments : positive_arguments);
        if (above)
        {
            arguments.selection = "above";
            arguments.selection_value = *above;
        }
        else if (below)
        {
            arguments.selection = "below";
            arguments.selection_value = *below;
        }
        else if (highest)
        {
            arguments.selection = "highest";
            arguments.selection_value = parse_percentage(*highest, "--highest");
        }
        else if (lowest)
        {
            arguments.selection = "lowest";
            arguments.selection_value = parse_percentage(*lowest, "--lowest");
        }
        else if (*positive)
        {
            arguments.selection = "above";
            arguments.selection_value = 0.5;
        }
        if (*positive and not arguments.condition and not unconditional)
        {
            arguments.condition = "positiveSelectionInModel";
            arguments.default_positive_condition = true;
        }
        run_report(arguments);
        return 0;
    }
    catch (const std::exception& error)
    {
        std::cerr<<"character-properties: Error! "<<error.what()<<"\n";
        return 1;
    }
}
