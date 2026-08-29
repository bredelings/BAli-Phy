#include "character-property-report.hh"

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>

#include "util/myexception.hh"

namespace character_properties
{

namespace
{

/*
 * Character properties belong to non-gap sequence letters, not alignment columns. Threshold and
 * percentage selection therefore operate globally over projected letters. A percentage sorts letters
 * by score and takes the requested prefix; a threshold scans each column directly. In either case the
 * first selected score extreme represents its column, with no secondary tie-breaking property.
 * Output ordering is deliberately left to callers, so it cannot change selection or representatives.
 *
 * An unselected ordinary report is a different calculation: it directly summarizes every nonempty
 * column and never constructs candidate or representative letters.
 */

/// Return all posterior summaries for one property of one projected letter.
/// The sequence name and ungapped character index address the three property matrices.
letter_posterior_summary summarize_letter(const property_summary& property, const projected_character& character)
{
    return {
        property.mean.at(character.sequence_name).at(character.character_index),
        property.sd.at(character.sequence_name).at(character.character_index),
        property.median.at(character.sequence_name).at(character.character_index)
    };
}

/// Return the mean or median by which one projected letter is selected.
/// Reading only that matrix avoids loading unused summaries for candidates that will be discarded.
double score_letter(const property_summary& property, const projected_character& character, bool use_median)
{
    const auto& values = use_median ? property.median : property.mean;
    return values.at(character.sequence_name).at(character.character_index);
}

/// One lightweight letter reference used only while sorting a percentage selection.
struct scored_character
{
    std::size_t alignment_column;
    const projected_character* character;
    double score;
};

/// Select one representative per column using a direct threshold scan or a sorted percentage prefix.
/// Stable sorting makes the first projected letter a pragmatic equal-score choice without another policy.
std::vector<const projected_character*> select_representatives(
    const property_summary& property, const alignment_projection& projection, bool use_median,
    const std::optional<double>& threshold, const std::optional<double>& fraction, bool select_highest)
{
    if (threshold.has_value() == fraction.has_value())
        throw myexception()<<"A letter selection must specify exactly one threshold or percentage.";

    std::vector<const projected_character*> representatives(projection.size(), nullptr);
    if (fraction)
    {
        if (not std::isfinite(*fraction) or *fraction <= 0 or *fraction > 1)
            throw myexception()<<"Character-property selection percentage must be greater than 0% and at most 100%.";

        std::vector<scored_character> candidates;
        for (const auto& column: projection)
            for (const auto& character: column.characters)
                candidates.push_back({column.alignment_column, &character,
                                      score_letter(property, character, use_median)});
        if (candidates.empty())
            throw myexception()<<"Cannot select a percentage from an alignment with no non-gap letters.";
        std::stable_sort(candidates.begin(), candidates.end(), [select_highest](const auto& first, const auto& second) {
            return select_highest ? first.score > second.score : first.score < second.score;
        });

        auto requested = std::max<std::size_t>(
            1, static_cast<std::size_t>(std::floor(static_cast<double>(candidates.size()) * *fraction)));
        for (std::size_t index = 0; index < requested; index++)
        {
            const auto& candidate = candidates[index];
            auto& representative = representatives[candidate.alignment_column];
            if (not representative)
                representative = candidate.character;
        }
        return representatives;
    }

    if (not std::isfinite(*threshold))
        throw myexception()<<"Character-property selection value must be finite.";
    for (const auto& column: projection)
    {
        const projected_character* representative = nullptr;
        double representative_score = 0;
        for (const auto& character: column.characters)
        {
            auto score = score_letter(property, character, use_median);
            bool selected = select_highest ? score > *threshold : score < *threshold;
            bool better = not representative or (select_highest ? score > representative_score
                                                                : score < representative_score);
            if (selected and better)
            {
                representative = &character;
                representative_score = score;
            }
        }
        representatives[column.alignment_column] = representative;
    }
    return representatives;
}

/// Summarize a selected letter as the public representative of its alignment column.
/// Complete posterior summaries are loaded here only after representative selection has finished.
selected_column make_selected_column(std::size_t alignment_column, const projected_character& character,
                                     const property_summary& property)
{
    return {
        alignment_column, character.sequence_index, character.sequence_name, character.character_index,
        character.symbol, character.translation, summarize_letter(property, character), {}
    };
}

/// Return the minimum, lower middle, and maximum observed values.
/// The lower middle is an observed order statistic, including when the number of letters is even.
std::array<double, 3> summarize_across_letters(std::vector<double> values)
{
    assert(not values.empty());
    std::ranges::sort(values);
    // BAli-Phy summaries consistently use the lower central observation for even sample counts;
    // using the same order statistic here avoids inventing an averaged value not seen for any letter.
    return {values.front(), values[(values.size() - 1) / 2], values.back()};
}

}

/// Report whether a property name currently denotes a positive-selection probability.
bool is_positive_selection_property(const std::string& name)
{
    // NOTE: Property names currently stand in for missing producer metadata. Remove this convention once summaries
    // carry explicit positive-selection probability and dN/dS roles.
    return name == "posSelection" or name.ends_with("-posSelection");
}

/// Describe each nonempty alignment column without selecting a representative letter.
/// Mean and median ranges are computed independently across the letters occupying that column.
std::vector<column_property_summary> summarize_property_columns(
    const property_summary& property, const alignment_projection& projection)
{
    std::vector<column_property_summary> result;
    for (const auto& column: projection)
    {
        if (column.characters.empty())
            continue;
        std::vector<double> means;
        std::vector<double> medians;
        means.reserve(column.characters.size());
        medians.reserve(column.characters.size());
        for (const auto& character: column.characters)
        {
            auto summary = summarize_letter(property, character);
            means.push_back(summary.mean);
            medians.push_back(summary.median);
        }
        auto [mean_minimum, mean_middle, mean_maximum] = summarize_across_letters(std::move(means));
        auto [median_minimum, median_middle, median_maximum] = summarize_across_letters(std::move(medians));
        result.push_back({column.alignment_column, column.characters.size(),
                          mean_minimum, mean_middle, mean_maximum,
                          median_minimum, median_middle, median_maximum});
    }
    return result;
}

/// Select ordinary-property letters globally and retain one extreme representative per resulting column.
/// Percentage selection sorts globally; threshold selection scans columns and summarizes only their winners.
/// Equal scores retain the first projected letter rather than invoking a secondary comparison policy.
std::vector<selected_column> select_property_columns(
    const property_summary& property, const alignment_projection& projection, bool use_median,
    const std::optional<double>& threshold, const std::optional<double>& fraction, bool select_highest)
{
    auto representatives = select_representatives(
        property, projection, use_median, threshold, fraction, select_highest);
    std::vector<selected_column> result;
    for (std::size_t column = 0; column < representatives.size(); column++)
    {
        const auto* representative = representatives[column];
        if (representative)
            result.push_back(make_selected_column(column, *representative, property));
    }
    return result;
}

/// Select positive-selection letters globally and retain one probability representative per column.
/// Equal probabilities retain the first projected letter rather than invoking a secondary comparison policy.
/// Complete probability and dN/dS summaries are loaded only for the retained representatives.
std::vector<selected_column> select_positive_selection_columns(
    const std::string& property_name, const property_summary& property, const alignment_projection& projection,
    const std::optional<double>& threshold, const std::optional<double>& fraction,
    const property_summary* dnds)
{
    if (not is_positive_selection_property(property_name))
        throw myexception()<<"Property '"<<property_name<<"' is not a positive-selection probability.";
    if (threshold and (*threshold < 0 or *threshold > 1))
        throw myexception()<<"Positive-selection probability thresholds must be between 0 and 1.";

    for (const auto& projected_column: projection)
        for (const auto& character: projected_column.characters)
        {
            auto probability = score_letter(property, character, false);
            if (probability < 0 or probability > 1)
                throw myexception()<<"Property '"<<property_name<<"' has probability "<<probability
                                   <<" outside [0,1] at sequence '"<<character.sequence_name
                                   <<"', character "<<character.character_index<<".";
        }

    auto representatives = select_representatives(property, projection, false, threshold, fraction, true);
    std::vector<selected_column> result;
    for (std::size_t column_index = 0; column_index < representatives.size(); column_index++)
    {
        const auto* representative = representatives[column_index];
        if (representative)
        {
            auto column = make_selected_column(column_index, *representative, property);
            if (dnds)
                column.dnds_summary = summarize_letter(*dnds, *representative);
            result.push_back(std::move(column));
        }
    }
    return result;
}

}
