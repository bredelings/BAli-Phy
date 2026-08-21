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
 * percentage selection therefore operate globally over projected letters. Only after selection do
 * we group letters by displayed alignment column and retain the requested extreme as that column's
 * representative. Positive selection always scores letters by mean probability and uses mean dN/dS
 * only to break equal-probability ties. Output ordering is deliberately left to callers, so it cannot
 * change which letters were selected or which representative was chosen.
 *
 * An unselected ordinary report is a different calculation: it directly summarizes every nonempty
 * column and never constructs candidate or representative letters.
 */

struct scored_character
{
    std::size_t alignment_column;
    const projected_character* character;
    letter_posterior_summary property_summary;
    double score;
};

/// Return all posterior summaries for one property of one projected letter.
letter_posterior_summary summarize_letter(const property_summary& property, const projected_character& character)
{
    return {
        property.mean.at(character.sequence_name).at(character.character_index),
        property.sd.at(character.sequence_name).at(character.character_index),
        property.median.at(character.sequence_name).at(character.character_index)
    };
}

/// Gather every projected letter and the mean or median by which it will be selected.
std::vector<scored_character> score_letters(const property_summary& property,
                                            const alignment_projection& projection, bool use_median)
{
    std::vector<scored_character> result;
    for (const auto& column: projection)
        for (const auto& character: column.characters)
        {
            auto summary = summarize_letter(property, character);
            result.push_back({column.alignment_column, &character, summary,
                              use_median ? summary.median : summary.mean});
        }
    return result;
}

/// Return selected letters in projection order and report their pre-grouping count.
std::vector<const scored_character*> select_letters(const std::vector<scored_character>& candidates,
                                                    const std::optional<double>& threshold,
                                                    const std::optional<double>& fraction, bool select_highest,
                                                    std::size_t& selected_letter_count)
{
    if (threshold.has_value() == fraction.has_value())
        throw myexception()<<"A letter selection must specify exactly one threshold or percentage.";

    std::optional<double> boundary = threshold;
    if (fraction)
    {
        if (not std::isfinite(*fraction) or *fraction <= 0 or *fraction > 1)
            throw myexception()<<"Character-property selection percentage must be greater than 0% and at most 100%.";
        if (candidates.empty())
            throw myexception()<<"Cannot select a percentage from an alignment with no non-gap letters.";

        std::vector<double> scores;
        scores.reserve(candidates.size());
        for (const auto& candidate: candidates)
            scores.push_back(candidate.score);
        std::ranges::sort(scores);

        // floor(Np) is the requested tail size, except that a nonzero percentage must retain one
        // letter. Comparing with the boundary retains every exact tie at that requested tail.
        auto requested = std::max<std::size_t>(
            1, static_cast<std::size_t>(std::floor(static_cast<double>(scores.size()) * *fraction)));
        boundary = select_highest ? scores[scores.size() - requested] : scores[requested - 1];
    }
    else if (not std::isfinite(*threshold))
        throw myexception()<<"Character-property selection value must be finite.";

    std::vector<const scored_character*> result;
    for (const auto& candidate: candidates)
    {
        bool selected = fraction ? (select_highest ? candidate.score >= *boundary : candidate.score <= *boundary)
                                 : (select_highest ? candidate.score > *boundary : candidate.score < *boundary);
        if (selected)
            result.push_back(&candidate);
    }
    selected_letter_count = result.size();
    return result;
}

/// Prefer the requested score extreme, then the earliest displayed sequence letter.
bool better_property_letter(const scored_character& candidate, const scored_character& current, bool select_highest)
{
    if (candidate.score != current.score)
        return select_highest ? candidate.score > current.score : candidate.score < current.score;
    if (candidate.character->sequence_index != current.character->sequence_index)
        return candidate.character->sequence_index < current.character->sequence_index;
    return candidate.character->character_index < current.character->character_index;
}

/// Convert a scored letter into the public column representative record.
selected_column make_selected_column(const scored_character& representative)
{
    const auto& character = *representative.character;
    return {
        representative.alignment_column, character.sequence_index, character.sequence_name,
        character.character_index, character.symbol, character.translation, representative.property_summary,
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
    const std::optional<double>& threshold, const std::optional<double>& fraction, bool select_highest,
    std::size_t& selected_letter_count)
{
    auto candidates = score_letters(property, projection, use_median);
    auto selected = select_letters(candidates, threshold, fraction, select_highest, selected_letter_count);
    std::vector<const scored_character*> representatives(projection.size(), nullptr);
    for (const auto* candidate: selected)
    {
        auto& representative = representatives[candidate->alignment_column];
        if (not representative or better_property_letter(*candidate, *representative, select_highest))
            representative = candidate;
    }

    std::vector<selected_column> result;
    for (const auto* representative: representatives)
        if (representative)
            result.push_back(make_selected_column(*representative));
    return result;
}

/// Select positive-selection letters globally and use dN/dS to break equal-probability ties.
std::vector<selected_column> select_positive_selection_columns(
    const std::string& property_name, const property_summary& property, const alignment_projection& projection,
    const std::optional<double>& threshold, const std::optional<double>& fraction,
    const std::string& companion_name, const property_summary* companion, std::size_t& selected_letter_count)
{
    if (not is_positive_selection_property(property_name))
        throw myexception()<<"Property '"<<property_name<<"' is not a positive-selection probability.";
    if (threshold and (*threshold < 0 or *threshold > 1))
        throw myexception()<<"Positive-selection probability thresholds must be between 0 and 1.";

    auto candidates = score_letters(property, projection, false);
    for (const auto& candidate: candidates)
        if (candidate.property_summary.mean < 0 or candidate.property_summary.mean > 1)
            throw myexception()<<"Property '"<<property_name<<"' has probability "<<candidate.property_summary.mean
                               <<" outside [0,1] at sequence '"<<candidate.character->sequence_name
                               <<"', character "<<candidate.character->character_index<<".";
    auto selected = select_letters(candidates, threshold, fraction, true, selected_letter_count);
    std::vector<const scored_character*> representatives(projection.size(), nullptr);
    for (const auto* candidate: selected)
    {
        auto& representative = representatives[candidate->alignment_column];
        if (not representative)
        {
            representative = candidate;
            continue;
        }

        // Probability is the scientific selection criterion. Only an exact probability tie reaches
        // dN/dS, and coordinate order makes the remaining tie deterministic without changing the score.
        bool better = candidate->score > representative->score;
        if (candidate->score == representative->score and companion)
        {
            auto candidate_dnds = summarize_letter(*companion, *candidate->character).mean;
            auto representative_dnds = summarize_letter(*companion, *representative->character).mean;
            better = candidate_dnds > representative_dnds;
            if (candidate_dnds == representative_dnds)
                better = better_property_letter(*candidate, *representative, true);
        }
        else if (candidate->score == representative->score)
            better = better_property_letter(*candidate, *representative, true);
        if (better)
            representative = candidate;
    }

    std::vector<selected_column> result;
    for (const auto* representative: representatives)
        if (representative)
        {
            auto column = make_selected_column(*representative);
            if (companion)
            {
                column.companion_property = companion_name;
                column.companion_summary = summarize_letter(*companion, *representative->character);
            }
            result.push_back(std::move(column));
        }
    return result;
}

}
