#ifndef CHARACTER_PROPERTY_REPORT_H
#define CHARACTER_PROPERTY_REPORT_H

#include <iosfwd>
#include <optional>
#include <string>
#include <vector>

#include "character-properties.H"
#include "character-property-alignment.H"

namespace character_properties
{

enum class report_kind
{
    property,
    positive_selection
};

enum class report_sort
{
    column,
    mean_ascending,
    mean_descending,
    sd_descending
};

struct report_options
{
    std::string property;
    report_kind kind = report_kind::property;
    report_sort sort = report_sort::column;
    std::optional<double> minimum_probability;
};

struct summary_statistics
{
    double mean;
    double sd;
    double median;
};

struct report_row
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

struct report
{
    report_options options;
    std::uint64_t retained_samples;
    std::vector<report_row> rows;
};

/// Report whether a property name currently denotes a positive-selection probability.
bool is_positive_selection_property(const std::string& name);

/// Select one representative observed character per template column and order the resulting rows.
report make_report(const summary& properties, const alignment_projection& projection, const report_options& options);

/// Encode report records with explicit zero-based coordinates for programmatic consumers.
json::value to_json(const report& value);

/// Write a compact human-readable character-property table.
void write_text(std::ostream& output, const report& value);

/// Write one tab-separated record per reported template column.
void write_tsv(std::ostream& output, const report& value);

}

#endif
