#ifndef CHARACTER_PROPERTY_REPORT_H
#define CHARACTER_PROPERTY_REPORT_H

#include <iosfwd>
#include <map>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "character-properties.H"
#include "character-property-alignment.H"

namespace character_properties
{

enum class posterior_statistic
{
    mean,
    median
};

enum class report_order
{
    column,
    increasing,
    decreasing
};

enum class letter_selection_kind
{
    all,
    above,
    below,
    highest_fraction,
    lowest_fraction
};

struct letter_selection
{
    letter_selection_kind kind = letter_selection_kind::all;
    double value = 0;
};

struct posterior_view
{
    const std::map<std::string, property_summary>* properties;
    std::optional<std::string> condition;
    std::uint64_t retained_samples;
    std::uint64_t total_retained_samples;
};

struct property_report_options
{
    std::string property;
    posterior_statistic statistic = posterior_statistic::mean;
    letter_selection selection;
    report_order order = report_order::column;
};

struct positive_selection_report_options
{
    std::string property = "posSelection";
    letter_selection selection{letter_selection_kind::above, 0.5};
    report_order order = report_order::column;
};

struct summary_statistics
{
    double mean;
    double sd;
    double median;
};

struct value_range
{
    double minimum;
    double middle;
    double maximum;
};

struct column_summary_row
{
    std::size_t alignment_column;
    std::size_t letter_count;
    value_range posterior_means;
    value_range posterior_medians;
};

struct representative_row
{
    std::size_t alignment_column;
    std::size_t sequence_index;
    std::string sequence_name;
    std::size_t character_index;
    std::string symbol;
    std::optional<std::string> translation;
    summary_statistics statistics;
    std::optional<std::string> companion_property;
    std::optional<summary_statistics> companion_statistics;
};

using report_row = std::variant<column_summary_row, representative_row>;

enum class report_kind
{
    property_columns,
    property_selection,
    positive_selection
};

struct report
{
    report_kind kind;
    std::string property;
    posterior_statistic statistic;
    letter_selection selection;
    report_order order;
    std::optional<std::string> condition;
    std::uint64_t retained_samples;
    std::uint64_t total_retained_samples;
    std::size_t candidate_letters;
    std::size_t selected_letters;
    std::vector<report_row> rows;
};

/// Return the unconditional or named True-conditioned property view and its sample counts.
posterior_view select_posterior_view(const summary& properties, const std::optional<std::string>& condition);

/// Report whether a property name currently denotes a positive-selection probability.
bool is_positive_selection_property(const std::string& name);

/// Summarize all columns or selected letters for one ordinary character property.
report make_property_report(const posterior_view& view, const alignment_projection& projection,
                            const property_report_options& options);

/// Select positive-selection letters and report one coherent representative per resulting column.
report make_positive_selection_report(const posterior_view& view, const alignment_projection& projection,
                                      const positive_selection_report_options& options);

/// Encode report records with explicit zero-based coordinates for programmatic consumers.
json::value to_json(const report& value);

/// Write a compact human-readable character-property table.
void write_text(std::ostream& output, const report& value);

/// Write one tab-separated record per reported template column.
void write_tsv(std::ostream& output, const report& value);

}

#endif
