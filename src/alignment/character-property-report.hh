#ifndef CHARACTER_PROPERTY_REPORT_H
#define CHARACTER_PROPERTY_REPORT_H

#include <cstddef>
#include <optional>
#include <string>
#include <vector>

#include "character-properties.hh"
#include "character-property-alignment.hh"

namespace character_properties
{

/// Posterior summaries for one property of one observed sequence letter.
struct letter_posterior_summary
{
    double mean;
    double sd;
    double median;
};

/// Across-letter summaries for one nonempty column of the displayed alignment.
struct column_property_summary
{
    std::size_t alignment_column;
    std::size_t letter_count;
    double mean_minimum;
    double mean_middle;
    double mean_maximum;
    double median_minimum;
    double median_middle;
    double median_maximum;
};

/// One selected letter presented as the representative of its alignment column.
/// Primary summaries describe the caller's selected view. Paired positive-selection reports use
/// the model-averaged view as primary and optionally describe the same letter after conditioning.
struct selected_column
{
    std::size_t alignment_column;
    std::size_t sequence_index;
    std::string sequence_name;
    std::size_t character_index;
    std::string symbol;
    std::optional<std::string> translation;
    letter_posterior_summary property_summary;
    std::optional<letter_posterior_summary> dnds_summary;
    std::optional<letter_posterior_summary> conditioned_property_summary;
    std::optional<letter_posterior_summary> conditioned_dnds_summary;
};

/// Report whether a property name currently denotes a positive-selection probability.
bool is_positive_selection_property(const std::string& name);

/// Describe each nonempty alignment column without selecting a representative letter.
/// Report the observed mean and median range across the letters occupying each column.
std::vector<column_property_summary> summarize_property_columns(
    const property_summary& property, const alignment_projection& projection);

/// Select ordinary-property letters globally and retain one extreme representative per resulting column.
/// Apply the cutoff to letters first, then summarize only the winning letter in each selected column.
/// Percentage selection sorts letters and takes exactly the requested number before grouping them.
std::vector<selected_column> select_property_columns(
    const property_summary& property, const alignment_projection& projection, bool use_median,
    const std::optional<double>& threshold, const std::optional<double>& fraction, bool select_highest);

/// Select positive-selection columns using model-averaged and optional conditioned probabilities.
/// A threshold retains the union of both views at one conditioned representative per column;
/// percentage selection retains its existing single-view meaning. All summaries describe that letter.
std::vector<selected_column> select_positive_selection_columns(
    const std::string& property_name, const property_summary& primary_property,
    const alignment_projection& projection, const std::optional<double>& threshold,
    const std::optional<double>& fraction, const property_summary* primary_dnds,
    const property_summary* conditioned_property, const property_summary* conditioned_dnds);

}

#endif
