#include "character-property-report.H"

#include <algorithm>
#include <array>
#include <cassert>
#include <charconv>
#include <cmath>
#include <string_view>
#include <utility>

#include "util/myexception.H"

namespace character_properties
{

namespace
{

struct scored_character
{
    std::size_t alignment_column;
    const projected_character* character;
    summary_statistics statistics;
    double score;
};

/// Return the three posterior summaries for one observed sequence character.
summary_statistics statistics_for(const property_summary& property, const projected_character& character)
{
    return {
        property.mean.at(character.sequence_name).at(character.character_index),
        property.sd.at(character.sequence_name).at(character.character_index),
        property.median.at(character.sequence_name).at(character.character_index)
    };
}

/// Return the statistic used to select representatives and order completed rows.
double statistic_for(const summary_statistics& statistics, posterior_statistic statistic)
{
    return statistic == posterior_statistic::mean ? statistics.mean : statistics.median;
}

/// Require and return one property from a resolved posterior view.
const property_summary& find_property(const posterior_view& view, const std::string& name)
{
    auto found = view.properties->find(name);
    if (found == view.properties->end())
        throw myexception()<<"Character property '"<<name<<"' was not found.";
    return found->second;
}

/// Derive the optional dN/dS companion using the current producer naming convention.
std::string positive_selection_companion(const std::string& name)
{
    constexpr std::string_view suffix = "posSelection";
    return name.substr(0, name.size() - suffix.size())+"dNdS";
}

/// Gather every projected non-gap letter with its selected posterior statistic.
std::vector<scored_character> score_characters(const property_summary& property,
                                               const alignment_projection& projection,
                                               posterior_statistic statistic)
{
    std::vector<scored_character> result;
    for (const auto& column: projection)
        for (const auto& character: column.characters)
        {
            auto statistics = statistics_for(property, character);
            result.push_back({column.alignment_column, &character, statistics,
                              statistic_for(statistics, statistic)});
        }
    return result;
}

/// Validate a selection and return the globally selected letters, including cutoff ties.
std::vector<scored_character> select_letters(const std::vector<scored_character>& candidates,
                                             const letter_selection& selection)
{
    if (not std::isfinite(selection.value))
        throw myexception()<<"Character-property selection value must be finite.";

    std::optional<double> fraction_boundary;
    if (selection.kind == letter_selection_kind::highest_fraction
        or selection.kind == letter_selection_kind::lowest_fraction)
    {
        if (selection.value <= 0 or selection.value > 1)
            throw myexception()<<"Character-property selection percentage must be greater than 0% and at most 100%.";
        if (candidates.empty())
            throw myexception()<<"Cannot select a percentage from an alignment with no non-gap letters.";

        std::vector<double> scores;
        scores.reserve(candidates.size());
        for (const auto& candidate: candidates)
            scores.push_back(candidate.score);
        std::ranges::sort(scores);
        auto requested = std::max<std::size_t>(
            1, static_cast<std::size_t>(std::floor(static_cast<double>(scores.size()) * selection.value)));
        if (selection.kind == letter_selection_kind::highest_fraction)
            fraction_boundary = scores[scores.size() - requested];
        else
            fraction_boundary = scores[requested - 1];
    }

    std::vector<scored_character> result;
    for (const auto& candidate: candidates)
    {
        bool selected = false;
        switch (selection.kind)
        {
            case letter_selection_kind::all:
                selected = true;
                break;
            case letter_selection_kind::above:
                selected = candidate.score > selection.value;
                break;
            case letter_selection_kind::below:
                selected = candidate.score < selection.value;
                break;
            case letter_selection_kind::highest_fraction:
                selected = candidate.score >= *fraction_boundary;
                break;
            case letter_selection_kind::lowest_fraction:
                selected = candidate.score <= *fraction_boundary;
                break;
        }
        if (selected)
            result.push_back(candidate);
    }
    return result;
}

/// Return the observed minimum, lower middle, and maximum without averaging letters.
value_range summarize_values(std::vector<double> values)
{
    assert(not values.empty());
    std::ranges::sort(values);
    return {values.front(), values[(values.size() - 1) / 2], values.back()};
}

/// Prefer an extreme score and then the earliest displayed sequence character.
bool better_generic_representative(const scored_character& candidate, const scored_character& current,
                                   bool choose_highest)
{
    if (candidate.score != current.score)
        return choose_highest ? candidate.score > current.score : candidate.score < current.score;
    if (candidate.character->sequence_index != current.character->sequence_index)
        return candidate.character->sequence_index < current.character->sequence_index;
    return candidate.character->character_index < current.character->character_index;
}

/// Return a row's alignment column for deterministic ordering and tie-breaking.
std::size_t row_column(const report_row& row)
{
    return std::visit([](const auto& value) {return value.alignment_column;}, row);
}

/// Return the scalar by which a completed report row is ordered.
double row_score(const report& value, const report_row& row)
{
    if (const auto* summary = std::get_if<column_summary_row>(&row))
    {
        const auto& range = value.statistic == posterior_statistic::mean
                                ? summary->posterior_means : summary->posterior_medians;
        return range.middle;
    }
    return statistic_for(std::get<representative_row>(row).statistics, value.statistic);
}

/// Order completed rows without changing selection or representative identity.
void order_rows(report& value)
{
    // Compare only completed row scores, with column coordinates providing a stable scientific tie-breaker.
    std::ranges::sort(value.rows, [&value](const auto& first, const auto& second) {
        if (value.order == report_order::column)
            return row_column(first) < row_column(second);
        auto first_score = row_score(value, first);
        auto second_score = row_score(value, second);
        if (first_score != second_score)
            return value.order == report_order::increasing ? first_score < second_score : first_score > second_score;
        return row_column(first) < row_column(second);
    });
}

/// Use enough significant digits for report values to round-trip through a double.
std::string format_number(double value)
{
    std::array<char, 64> buffer;
    // The shortest representation of a double is much smaller than this buffer, so to_chars cannot run out of space.
    auto [end, error] = std::to_chars(buffer.data(), buffer.data() + buffer.size(), value);
    assert(error == std::errc());
    return {buffer.data(), end};
}

/// Encode one posterior summary triple for a representative letter.
json::object statistics_to_json(const summary_statistics& statistics)
{
    return {{"mean", statistics.mean}, {"sd", statistics.sd}, {"median", statistics.median}};
}

/// Encode a minimum, lower-middle, and maximum column summary.
json::object range_to_json(const value_range& range)
{
    return {{"minimum", range.minimum}, {"middle", range.middle}, {"maximum", range.maximum}};
}

/// Return the stable external spelling of a posterior statistic.
const char* statistic_name(posterior_statistic statistic)
{
    return statistic == posterior_statistic::mean ? "mean" : "median";
}

/// Return the stable external spelling of a completed-row ordering.
const char* order_name(report_order order)
{
    switch (order)
    {
        case report_order::column: return "column";
        case report_order::increasing: return "increasing";
        case report_order::decreasing: return "decreasing";
    }
    return "";
}

/// Return the stable external spelling of a letter-selection policy.
const char* selection_name(letter_selection_kind kind)
{
    switch (kind)
    {
        case letter_selection_kind::all: return "all";
        case letter_selection_kind::above: return "above";
        case letter_selection_kind::below: return "below";
        case letter_selection_kind::highest_fraction: return "highest";
        case letter_selection_kind::lowest_fraction: return "lowest";
    }
    return "";
}

/// Return the stable external spelling of a report layout.
const char* kind_name(report_kind kind)
{
    switch (kind)
    {
        case report_kind::property_columns: return "property-columns";
        case report_kind::property_selection: return "property-selection";
        case report_kind::positive_selection: return "positive-selection";
    }
    return "";
}

/// Encode the selection with units that distinguish raw thresholds from fractions.
json::object selection_to_json(const letter_selection& selection)
{
    json::object result{{"kind", selection_name(selection.kind)}};
    if (selection.kind == letter_selection_kind::above or selection.kind == letter_selection_kind::below)
        result["threshold"] = selection.value;
    if (selection.kind == letter_selection_kind::highest_fraction
        or selection.kind == letter_selection_kind::lowest_fraction)
        result["fraction"] = selection.value;
    return result;
}

}

/// Return the unconditional or named True-conditioned property view and its sample counts.
posterior_view select_posterior_view(const summary& properties, const std::optional<std::string>& condition)
{
    if (not condition)
        return {&properties.properties, {}, properties.retained_samples, properties.retained_samples};

    auto found = properties.conditioned.find(*condition);
    if (found == properties.conditioned.end())
        throw myexception()<<"Character property condition '"<<*condition<<"' was not found.";
    if (found->second.retained_samples == 0)
        throw myexception()<<"Character property condition '"<<*condition<<"' has no True samples.";
    return {&found->second.properties, condition, found->second.retained_samples, properties.retained_samples};
}

/// Report whether a property name currently denotes a positive-selection probability.
bool is_positive_selection_property(const std::string& name)
{
    // NOTE: Property names currently stand in for missing producer metadata. Remove this convention once summaries
    // carry explicit probability and companion-property roles.
    return name == "posSelection" or name.ends_with("-posSelection");
}

/// Summarize all columns or selected letters for one ordinary character property.
report make_property_report(const posterior_view& view, const alignment_projection& projection,
                            const property_report_options& options)
{
    const auto& property = find_property(view, options.property);
    auto candidates = score_characters(property, projection, options.statistic);
    auto selected = select_letters(candidates, options.selection);
    report result{
        options.selection.kind == letter_selection_kind::all ? report_kind::property_columns
                                                              : report_kind::property_selection,
        options.property, options.statistic, options.selection, options.order, view.condition,
        view.retained_samples, view.total_retained_samples, candidates.size(), selected.size(), {}
    };

    if (options.selection.kind == letter_selection_kind::all)
    {
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
                auto statistics = statistics_for(property, character);
                means.push_back(statistics.mean);
                medians.push_back(statistics.median);
            }
            result.rows.push_back(column_summary_row{
                column.alignment_column, column.characters.size(), summarize_values(std::move(means)),
                summarize_values(std::move(medians))});
        }
    }
    else
    {
        std::vector<std::vector<const scored_character*>> columns(projection.size());
        for (const auto& character: selected)
            columns[character.alignment_column].push_back(&character);
        bool choose_highest = options.selection.kind == letter_selection_kind::above
                              or options.selection.kind == letter_selection_kind::highest_fraction;
        for (std::size_t column = 0; column < columns.size(); column++)
        {
            if (columns[column].empty())
                continue;
            const auto* representative = columns[column].front();
            for (std::size_t index = 1; index < columns[column].size(); index++)
                if (better_generic_representative(*columns[column][index], *representative, choose_highest))
                    representative = columns[column][index];
            const auto& character = *representative->character;
            result.rows.push_back(representative_row{
                column, character.sequence_index, character.sequence_name, character.character_index,
                character.symbol, character.translation, representative->statistics, {}, {}});
        }
    }

    order_rows(result);
    return result;
}

/// Select positive-selection letters and report one coherent representative per resulting column.
report make_positive_selection_report(const posterior_view& view, const alignment_projection& projection,
                                      const positive_selection_report_options& options)
{
    if (not is_positive_selection_property(options.property))
        throw myexception()<<"Property '"<<options.property<<"' is not a positive-selection probability.";
    if (options.selection.kind == letter_selection_kind::below
        or options.selection.kind == letter_selection_kind::lowest_fraction)
        throw myexception()<<"Positive-selection reports support only all, above, or highest selection.";
    if (options.selection.kind == letter_selection_kind::above
        and (options.selection.value < 0 or options.selection.value > 1))
        throw myexception()<<"Positive-selection probability thresholds must be between 0 and 1.";

    const auto& property = find_property(view, options.property);
    auto candidates = score_characters(property, projection, posterior_statistic::mean);
    for (const auto& candidate: candidates)
        if (candidate.statistics.mean < 0 or candidate.statistics.mean > 1)
            throw myexception()<<"Property '"<<options.property<<"' has probability "<<candidate.statistics.mean
                               <<" outside [0,1] at sequence '"<<candidate.character->sequence_name
                               <<"', character "<<candidate.character->character_index<<".";
    auto selected = select_letters(candidates, options.selection);

    std::optional<std::string> companion_name = positive_selection_companion(options.property);
    const property_summary* companion = nullptr;
    if (auto found = view.properties->find(*companion_name); found != view.properties->end())
        companion = &found->second;

    report result{report_kind::positive_selection, options.property, posterior_statistic::mean, options.selection,
                  options.order, view.condition, view.retained_samples, view.total_retained_samples,
                  candidates.size(), selected.size(), {}};
    std::vector<std::vector<const scored_character*>> columns(projection.size());
    for (const auto& character: selected)
        columns[character.alignment_column].push_back(&character);

    for (std::size_t column = 0; column < columns.size(); column++)
    {
        if (columns[column].empty())
            continue;
        const auto* representative = columns[column].front();
        for (std::size_t index = 1; index < columns[column].size(); index++)
        {
            const auto* candidate = columns[column][index];
            bool better = candidate->score > representative->score;
            if (candidate->score == representative->score and companion)
            {
                auto candidate_companion = statistics_for(*companion, *candidate->character).mean;
                auto representative_companion = statistics_for(*companion, *representative->character).mean;
                better = candidate_companion > representative_companion;
            }
            if (candidate->score == representative->score
                and (not companion or statistics_for(*companion, *candidate->character).mean
                                          == statistics_for(*companion, *representative->character).mean))
                better = better_generic_representative(*candidate, *representative, true);
            if (better)
                representative = candidate;
        }

        const auto& character = *representative->character;
        representative_row row{
            column, character.sequence_index, character.sequence_name, character.character_index,
            character.symbol, character.translation, representative->statistics,
            companion ? companion_name : std::nullopt, {}};
        if (companion)
            row.companion_statistics = statistics_for(*companion, character);
        result.rows.push_back(std::move(row));
    }

    order_rows(result);
    return result;
}

/// Encode report records with explicit zero-based coordinates for programmatic consumers.
json::value to_json(const report& value)
{
    json::array rows;
    rows.reserve(value.rows.size());
    for (const auto& row: value.rows)
    {
        if (const auto* summary = std::get_if<column_summary_row>(&row))
        {
            rows.push_back(json::object{
                {"column_index", summary->alignment_column},
                {"letter_count", summary->letter_count},
                {"posterior_means", range_to_json(summary->posterior_means)},
                {"posterior_medians", range_to_json(summary->posterior_medians)}
            });
            continue;
        }

        const auto& representative = std::get<representative_row>(row);
        json::object encoded{
            {"column_index", representative.alignment_column},
            {"sequence_index", representative.sequence_index},
            {"sequence", representative.sequence_name},
            {"character_index", representative.character_index},
            {"symbol", representative.symbol},
            {"translation", representative.translation ? json::value(*representative.translation) : json::value(nullptr)},
            {"statistics", statistics_to_json(representative.statistics)}
        };
        if (representative.companion_statistics)
            encoded["companion"] = json::object{
                {"property", *representative.companion_property},
                {"statistics", statistics_to_json(*representative.companion_statistics)}
            };
        else
            encoded["companion"] = nullptr;
        rows.push_back(std::move(encoded));
    }

    return json::object{
        {"format", "bali-phy-character-property-report"},
        {"version", 3},
        {"kind", kind_name(value.kind)},
        {"property", value.property},
        {"statistic", statistic_name(value.statistic)},
        {"selection", selection_to_json(value.selection)},
        {"sort", order_name(value.order)},
        {"condition", value.condition ? json::value(*value.condition) : json::value(nullptr)},
        {"condition_value", value.condition ? json::value(true) : json::value(nullptr)},
        {"retained_samples", value.retained_samples},
        {"total_retained_samples", value.total_retained_samples},
        {"candidate_letters", value.candidate_letters},
        {"selected_letters", value.selected_letters},
        {"rows", std::move(rows)}
    };
}

/// Write a compact human-readable character-property table.
void write_text(std::ostream& output, const report& value)
{
    output<<(value.kind == report_kind::positive_selection ? "Positive-selection property: " : "Character property: ")
          <<value.property<<"\n";
    if (value.condition)
        output<<"Posterior view: "<<*value.condition<<" = true\n"
              <<"Matching samples: "<<value.retained_samples<<" of "<<value.total_retained_samples<<"\n";
    else
        output<<"Posterior view: model-averaged\nRetained samples: "<<value.retained_samples<<"\n";
    output<<"Selection: "<<selection_name(value.selection.kind);
    if (value.selection.kind == letter_selection_kind::above or value.selection.kind == letter_selection_kind::below)
        output<<" "<<format_number(value.selection.value);
    if (value.selection.kind == letter_selection_kind::highest_fraction
        or value.selection.kind == letter_selection_kind::lowest_fraction)
        output<<" "<<format_number(value.selection.value * 100)<<"%";
    output<<"\n\n";

    if (value.kind == report_kind::property_columns)
    {
        output<<"Column  Letters  Mean min  Mean middle  Mean max  Median min  Median middle  Median max\n";
        for (const auto& encoded: value.rows)
        {
            const auto& row = std::get<column_summary_row>(encoded);
            output<<row.alignment_column + 1<<"  "<<row.letter_count<<"  "
                  <<format_number(row.posterior_means.minimum)<<"  "<<format_number(row.posterior_means.middle)<<"  "
                  <<format_number(row.posterior_means.maximum)<<"  "<<format_number(row.posterior_medians.minimum)<<"  "
                  <<format_number(row.posterior_medians.middle)<<"  "<<format_number(row.posterior_medians.maximum)<<"\n";
        }
        return;
    }

    output<<"Column  Sequence  Character  Symbol  Translation  "
          <<(value.kind == report_kind::positive_selection ? "Probability mean +/- SD  Probability median"
                                                            : "Mean +/- SD  Median");
    bool has_companion = not value.rows.empty()
                         and std::get<representative_row>(value.rows.front()).companion_property.has_value();
    if (has_companion)
        output<<"  Companion  Companion mean +/- SD  Companion median";
    output<<"\n";
    for (const auto& encoded: value.rows)
    {
        const auto& row = std::get<representative_row>(encoded);
        output<<row.alignment_column + 1<<"  "<<row.sequence_name<<"  "<<row.character_index + 1<<"  "
              <<row.symbol<<"  "<<row.translation.value_or("-")<<"  "<<format_number(row.statistics.mean)
              <<" +/- "<<format_number(row.statistics.sd)<<"  "<<format_number(row.statistics.median);
        if (row.companion_statistics)
            output<<"  "<<*row.companion_property<<"  "<<format_number(row.companion_statistics->mean)<<" +/- "
                  <<format_number(row.companion_statistics->sd)<<"  "<<format_number(row.companion_statistics->median);
        output<<"\n";
    }
}

/// Write one tab-separated record per reported template column.
void write_tsv(std::ostream& output, const report& value)
{
    if (value.kind == report_kind::property_columns)
    {
        output<<"column\tletters\tmean-min\tmean-middle\tmean-max\tmedian-min\tmedian-middle\tmedian-max\n";
        for (const auto& encoded: value.rows)
        {
            const auto& row = std::get<column_summary_row>(encoded);
            output<<row.alignment_column + 1<<"\t"<<row.letter_count<<"\t"
                  <<format_number(row.posterior_means.minimum)<<"\t"<<format_number(row.posterior_means.middle)<<"\t"
                  <<format_number(row.posterior_means.maximum)<<"\t"<<format_number(row.posterior_medians.minimum)<<"\t"
                  <<format_number(row.posterior_medians.middle)<<"\t"<<format_number(row.posterior_medians.maximum)<<"\n";
        }
        return;
    }

    output<<"column\tsequence\tsequence-character\tsymbol\ttranslation\tmean\tsd\tmedian"
          <<"\tcompanion-property\tcompanion-mean\tcompanion-sd\tcompanion-median\n";
    for (const auto& encoded: value.rows)
    {
        const auto& row = std::get<representative_row>(encoded);
        output<<row.alignment_column + 1<<"\t"<<row.sequence_name<<"\t"<<row.character_index + 1<<"\t"
              <<row.symbol<<"\t"<<row.translation.value_or("")<<"\t"<<format_number(row.statistics.mean)<<"\t"
              <<format_number(row.statistics.sd)<<"\t"<<format_number(row.statistics.median)<<"\t";
        if (row.companion_statistics)
            output<<*row.companion_property<<"\t"<<format_number(row.companion_statistics->mean)<<"\t"
                  <<format_number(row.companion_statistics->sd)<<"\t"<<format_number(row.companion_statistics->median);
        else
            output<<"\t\t\t";
        output<<"\n";
    }
}

}
