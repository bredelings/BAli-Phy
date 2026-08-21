#include "character-property-report.H"

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>

#include "util/myexception.H"

namespace character_properties
{

namespace
{

/*
 * Character properties belong to non-gap sequence letters, not alignment columns. Threshold and
 * percentage selection therefore operate globally over projected letters. Once the global cutoff is
 * known, each displayed alignment column can independently retain its requested extreme as the
 * representative. Positive selection always scores letters by mean probability and uses mean dN/dS
 * only to break equal-probability ties. Output ordering is deliberately left to callers, so it cannot
 * change which letters were selected or which representative was chosen.
 *
 * An unselected ordinary report is a different calculation: it directly summarizes every nonempty
 * column and never constructs candidate or representative letters.
 */

/// Return all posterior summaries for one property of one projected letter.
letter_posterior_summary summarize_letter(const property_summary& property, const projected_character& character)
{
    return {
        property.mean.at(character.sequence_name).at(character.character_index),
        property.sd.at(character.sequence_name).at(character.character_index),
        property.median.at(character.sequence_name).at(character.character_index)
    };
}

/// Return the mean or median by which one projected letter is selected.
double score_letter(const property_summary& property, const projected_character& character, bool use_median)
{
    const auto& values = use_median ? property.median : property.mean;
    return values.at(character.sequence_name).at(character.character_index);
}

/// Validate the selection and return its threshold or globally calculated percentage cutoff.
double selection_boundary(const property_summary& property, const alignment_projection& projection, bool use_median,
                          const std::optional<double>& threshold, const std::optional<double>& fraction,
                          bool select_highest)
{
    if (threshold.has_value() == fraction.has_value())
        throw myexception()<<"A letter selection must specify exactly one threshold or percentage.";

    if (threshold)
    {
        if (not std::isfinite(*threshold))
            throw myexception()<<"Character-property selection value must be finite.";
        return *threshold;
    }

    if (not std::isfinite(*fraction) or *fraction <= 0 or *fraction > 1)
        throw myexception()<<"Character-property selection percentage must be greater than 0% and at most 100%.";

    std::vector<double> scores;
    for (const auto& column: projection)
        for (const auto& character: column.characters)
            scores.push_back(score_letter(property, character, use_median));
    if (scores.empty())
        throw myexception()<<"Cannot select a percentage from an alignment with no non-gap letters.";
    std::ranges::sort(scores);

    // floor(Np) is the requested tail size, except that a nonzero percentage must retain one
    // letter. Comparing with the boundary retains every exact tie at that requested tail.
    auto requested = std::max<std::size_t>(
        1, static_cast<std::size_t>(std::floor(static_cast<double>(scores.size()) * *fraction)));
    return select_highest ? scores[scores.size() - requested] : scores[requested - 1];
}

/// Apply strict threshold selection or tie-inclusive percentage selection.
bool score_is_selected(double score, double boundary, bool percentage, bool select_highest)
{
    return percentage ? (select_highest ? score >= boundary : score <= boundary)
                      : (select_highest ? score > boundary : score < boundary);
}

/// Break equal property values by choosing the earliest displayed sequence letter.
bool character_is_earlier(const projected_character& candidate, const projected_character& current)
{
    if (candidate.sequence_index != current.sequence_index)
        return candidate.sequence_index < current.sequence_index;
    return candidate.character_index < current.character_index;
}

/// Summarize a selected letter as the public representative of its alignment column.
selected_column make_selected_column(std::size_t alignment_column, const projected_character& character,
                                     const property_summary& property)
{
    return {
        alignment_column, character.sequence_index, character.sequence_name, character.character_index,
        character.symbol, character.translation, summarize_letter(property, character),
        {}, {}
    };
}

/// Return the minimum, lower middle, and maximum observed values.
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
    // carry explicit probability and companion-property roles.
    return name == "posSelection" or name.ends_with("-posSelection");
}

/// Describe each nonempty alignment column without selecting a representative letter.
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
std::vector<selected_column> select_property_columns(
    const property_summary& property, const alignment_projection& projection, bool use_median,
    const std::optional<double>& threshold, const std::optional<double>& fraction, bool select_highest)
{
    auto boundary = selection_boundary(property, projection, use_median, threshold, fraction, select_highest);
    std::vector<selected_column> result;
    for (const auto& column: projection)
    {
        const projected_character* representative = nullptr;
        double representative_score = 0;
        for (const auto& character: column.characters)
        {
            auto score = score_letter(property, character, use_median);
            if (not score_is_selected(score, boundary, fraction.has_value(), select_highest))
                continue;

            bool better = not representative or (select_highest ? score > representative_score
                                                                : score < representative_score);
            if (representative and score == representative_score)
                better = character_is_earlier(character, *representative);
            if (better)
            {
                representative = &character;
                representative_score = score;
            }
        }
        if (representative)
            result.push_back(make_selected_column(column.alignment_column, *representative, property));
    }
    return result;
}

/// Select positive-selection letters globally and use dN/dS to break equal-probability ties.
std::vector<selected_column> select_positive_selection_columns(
    const std::string& property_name, const property_summary& property, const alignment_projection& projection,
    const std::optional<double>& threshold, const std::optional<double>& fraction,
    const std::string& companion_name, const property_summary* companion)
{
    if (not is_positive_selection_property(property_name))
        throw myexception()<<"Property '"<<property_name<<"' is not a positive-selection probability.";
    if (threshold and (*threshold < 0 or *threshold > 1))
        throw myexception()<<"Positive-selection probability thresholds must be between 0 and 1.";

    auto boundary = selection_boundary(property, projection, false, threshold, fraction, true);
    std::vector<selected_column> result;
    for (const auto& projected_column: projection)
    {
        const projected_character* representative = nullptr;
        double representative_probability = 0;
        for (const auto& character: projected_column.characters)
        {
            auto probability = score_letter(property, character, false);
            if (probability < 0 or probability > 1)
                throw myexception()<<"Property '"<<property_name<<"' has probability "<<probability
                                   <<" outside [0,1] at sequence '"<<character.sequence_name
                                   <<"', character "<<character.character_index<<".";
            if (not score_is_selected(probability, boundary, fraction.has_value(), true))
                continue;

            // Probability is the scientific selection criterion. Only an exact probability tie reaches
            // dN/dS, and coordinate order makes the remaining tie deterministic without changing the score.
            bool better = not representative or probability > representative_probability;
            if (representative and probability == representative_probability and companion)
            {
                auto candidate_dnds = score_letter(*companion, character, false);
                auto representative_dnds = score_letter(*companion, *representative, false);
                better = candidate_dnds > representative_dnds;
                if (candidate_dnds == representative_dnds)
                    better = character_is_earlier(character, *representative);
            }
            else if (representative and probability == representative_probability)
                better = character_is_earlier(character, *representative);
            if (better)
            {
                representative = &character;
                representative_probability = probability;
            }
        }

        if (representative)
        {
            auto column = make_selected_column(projected_column.alignment_column, *representative, property);
            if (companion)
            {
                column.companion_property = companion_name;
                column.companion_summary = summarize_letter(*companion, *representative);
            }
            result.push_back(std::move(column));
        }
    }
    return result;
}

}
