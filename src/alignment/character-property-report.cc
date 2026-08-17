#include "character-property-report.H"

#include <algorithm>
#include <array>
#include <cassert>
#include <charconv>
#include <string_view>
#include <utility>

#include "util/myexception.H"

namespace character_properties
{

namespace
{

/// Return the three posterior summaries for one observed sequence character.
summary_statistics statistics_for(const property_summary& property, const projected_character& character)
{
    return {
        property.mean.at(character.sequence_name).at(character.character_index),
        property.sd.at(character.sequence_name).at(character.character_index),
        property.median.at(character.sequence_name).at(character.character_index)
    };
}

/// Derive the optional dN/dS companion using the same temporary naming convention.
std::string positive_selection_companion(const std::string& name)
{
    constexpr std::string_view suffix = "posSelection";
    return name.substr(0, name.size() - suffix.size())+"dNdS";
}

/// Compare candidate characters using the report's representative-selection statistic.
bool better_representative(const report_options& options, const summary_statistics& candidate,
                           const projected_character& candidate_character, const summary_statistics& current,
                           const projected_character& current_character)
{
    if (options.kind == report_kind::positive_selection and candidate.mean != current.mean)
        return candidate.mean > current.mean;
    if (options.sort == report_sort::mean_ascending and candidate.mean != current.mean)
        return candidate.mean < current.mean;
    if (candidate.mean != current.mean)
        return candidate.mean > current.mean;
    return candidate_character.sequence_index < current_character.sequence_index;
}

/// Order report rows by the requested statistic and use template order to break ties.
bool row_before(const report_options& options, const report_row& first, const report_row& second)
{
    if (options.sort == report_sort::column)
        return first.alignment_column < second.alignment_column;
    if (options.sort == report_sort::mean_ascending and first.statistics.mean != second.statistics.mean)
        return first.statistics.mean < second.statistics.mean;
    if (options.sort == report_sort::mean_descending and first.statistics.mean != second.statistics.mean)
        return first.statistics.mean > second.statistics.mean;
    return first.alignment_column < second.alignment_column;
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

/// Encode one posterior summary triple for a report row.
json::object statistics_to_json(const summary_statistics& statistics)
{
    return {
        {"mean", statistics.mean},
        {"sd", statistics.sd},
        {"median", statistics.median}
    };
}

/// Return the stable external spelling of a report kind.
const char* kind_name(report_kind kind)
{
    switch (kind)
    {
        case report_kind::property:
            return "property";
        case report_kind::positive_selection:
            return "positive-selection";
    }
    std::unreachable();
}

/// Return the stable external spelling of a report ordering.
const char* sort_name(report_sort sort)
{
    switch (sort)
    {
        case report_sort::column:
            return "column";
        case report_sort::mean_ascending:
            return "mean-ascending";
        case report_sort::mean_descending:
            return "mean-descending";
    }
    std::unreachable();
}

}

/// Report whether a property name currently denotes a positive-selection probability.
bool is_positive_selection_property(const std::string& name)
{
    // NOTE: Property names currently stand in for missing producer metadata. Remove this convention once summaries
    // carry explicit probability and companion-property roles.
    return name == "posSelection" or name.ends_with("-posSelection");
}

/// Select one representative observed character per template column and order the resulting rows.
report make_report(const summary& properties, const alignment_projection& projection, const report_options& options)
{
    report_options normalized_options = options;
    if (normalized_options.kind == report_kind::positive_selection and not normalized_options.minimum_probability)
        normalized_options.minimum_probability = 0.5;

    const auto* selected_properties = &properties.properties;
    std::uint64_t retained_samples = properties.retained_samples;
    if (normalized_options.condition)
    {
        auto view = properties.conditioned.find(*normalized_options.condition);
        if (view == properties.conditioned.end())
            throw myexception()<<"Character property condition '"<<*normalized_options.condition<<"' was not found.";
        if (view->second.retained_samples == 0)
            throw myexception()<<"Character property condition '"<<*normalized_options.condition
                               <<"' has no True samples.";
        selected_properties = &view->second.properties;
        retained_samples = view->second.retained_samples;
    }

    auto property = selected_properties->find(normalized_options.property);
    if (property == selected_properties->end())
        throw myexception()<<"Character property '"<<normalized_options.property<<"' was not found.";
    if (normalized_options.kind == report_kind::positive_selection
        and not is_positive_selection_property(normalized_options.property))
        throw myexception()<<"Property '"<<normalized_options.property<<"' is not a positive-selection probability.";
    if (normalized_options.kind == report_kind::property and normalized_options.minimum_probability)
        throw myexception()<<"--minimum-probability is available only for positive-selection reports.";
    if (normalized_options.minimum_probability
        and (*normalized_options.minimum_probability < 0 or *normalized_options.minimum_probability > 1))
        throw myexception()<<"--minimum-probability must be between 0 and 1.";

    const property_summary* companion = nullptr;
    std::optional<std::string> companion_name;
    if (normalized_options.kind == report_kind::positive_selection)
    {
        companion_name = positive_selection_companion(normalized_options.property);
        if (auto found = selected_properties->find(*companion_name); found != selected_properties->end())
            companion = &found->second;
    }

    report result{normalized_options, retained_samples, properties.retained_samples, {}};
    for (const auto& column: projection)
    {
        if (column.characters.empty())
            continue;

        const projected_character* representative = &column.characters.front();
        summary_statistics representative_statistics = statistics_for(property->second, *representative);
        for (std::size_t index = 1; index < column.characters.size(); index++)
        {
            const auto& character = column.characters[index];
            auto candidate_statistics = statistics_for(property->second, character);
            if (normalized_options.kind == report_kind::positive_selection
                and (candidate_statistics.mean < 0 or candidate_statistics.mean > 1))
                throw myexception()<<"Property '"<<normalized_options.property<<"' has probability "<<candidate_statistics.mean
                                   <<" outside [0,1] at sequence '"<<character.sequence_name<<"', character "
                                   <<character.character_index<<".";
            if (better_representative(normalized_options, candidate_statistics, character, representative_statistics,
                                      *representative))
            {
                representative = &character;
                representative_statistics = candidate_statistics;
            }
        }

        if (normalized_options.kind == report_kind::positive_selection
            and (representative_statistics.mean < 0 or representative_statistics.mean > 1))
            throw myexception()<<"Property '"<<normalized_options.property<<"' has probability "<<representative_statistics.mean
                               <<" outside [0,1] at sequence '"<<representative->sequence_name<<"', character "
                               <<representative->character_index<<".";
        if (normalized_options.minimum_probability
            and representative_statistics.mean < *normalized_options.minimum_probability)
            continue;

        report_row row{
            column.alignment_column,
            representative->sequence_index,
            representative->sequence_name,
            representative->character_index,
            representative->symbol,
            representative->translation,
            representative_statistics,
            companion ? companion_name : std::nullopt,
            {}
        };
        if (companion)
            row.companion_statistics = statistics_for(*companion, *representative);
        result.rows.push_back(std::move(row));
    }

    std::ranges::sort(result.rows, [&normalized_options](const auto& first, const auto& second) {
        return row_before(normalized_options, first, second);
    });
    return result;
}

/// Encode report records with explicit zero-based coordinates for programmatic consumers.
json::value to_json(const report& value)
{
    json::array rows;
    rows.reserve(value.rows.size());
    for (std::size_t index = 0; index < value.rows.size(); index++)
    {
        const auto& row = value.rows[index];
        json::object encoded{
            {"rank", index + 1},
            {"column_index", row.alignment_column},
            {"sequence_index", row.sequence_index},
            {"sequence", row.sequence_name},
            {"character_index", row.character_index},
            {"symbol", row.symbol},
            {"translation", row.translation ? json::value(*row.translation) : json::value(nullptr)},
            {"statistics", statistics_to_json(row.statistics)}
        };
        if (row.companion_statistics)
        {
            json::object companion{
                {"property", *row.companion_property},
                {"statistics", statistics_to_json(*row.companion_statistics)}
            };
            encoded["companion"] = std::move(companion);
        }
        else
            encoded["companion"] = nullptr;
        rows.push_back(std::move(encoded));
    }

    json::object result{
        {"format", "bali-phy-character-property-report"},
        {"version", 2},
        {"kind", kind_name(value.options.kind)},
        {"property", value.options.property},
        {"sort", sort_name(value.options.sort)},
        {"minimum_probability", value.options.minimum_probability ? json::value(*value.options.minimum_probability)
                                                                  : json::value(nullptr)},
        {"condition", value.options.condition ? json::value(*value.options.condition) : json::value(nullptr)},
        {"condition_value", value.options.condition ? json::value(true) : json::value(nullptr)},
        {"retained_samples", value.retained_samples},
        {"total_retained_samples", value.total_retained_samples},
        {"rows", std::move(rows)}
    };
    return result;
}

/// Write a compact human-readable character-property table.
void write_text(std::ostream& output, const report& value)
{
    output<<(value.options.kind == report_kind::positive_selection ? "Positive-selection property: " : "Character property: ")
          <<value.options.property<<"\n";
    if (value.options.condition)
        output<<"Posterior view: "<<*value.options.condition<<" = true\n"
              <<"Matching samples: "<<value.retained_samples<<" of "<<value.total_retained_samples<<"\n";
    else
        output<<"Posterior view: model-averaged\n"
              <<"Retained samples: "<<value.retained_samples<<"\n";
    if (value.options.minimum_probability)
        output<<"Minimum probability: "<<format_number(*value.options.minimum_probability)<<"\n";
    output<<"\nRank  Column  Sequence  Character  Symbol  Translation  Mean +/- SD  Median";
    if (not value.rows.empty() and value.rows.front().companion_property)
        output<<"  Companion  Companion mean +/- SD  Companion median";
    output<<"\n";

    for (std::size_t index = 0; index < value.rows.size(); index++)
    {
        const auto& row = value.rows[index];
        output<<index + 1<<"  "<<row.alignment_column + 1<<"  "<<row.sequence_name<<"  "<<row.character_index + 1
              <<"  "<<row.symbol<<"  "<<row.translation.value_or("-")<<"  "<<format_number(row.statistics.mean)
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
    output<<"rank\tcolumn\tsequence\tsequence-character\tsymbol\ttranslation\tmean\tsd\tmedian"
          <<"\tcompanion-property\tcompanion-mean\tcompanion-sd\tcompanion-median\n";
    for (std::size_t index = 0; index < value.rows.size(); index++)
    {
        const auto& row = value.rows[index];
        output<<index + 1<<"\t"<<row.alignment_column + 1<<"\t"<<row.sequence_name<<"\t"<<row.character_index + 1
              <<"\t"<<row.symbol<<"\t"<<row.translation.value_or("")<<"\t"<<format_number(row.statistics.mean)
              <<"\t"<<format_number(row.statistics.sd)<<"\t"<<format_number(row.statistics.median)<<"\t";
        if (row.companion_statistics)
            output<<*row.companion_property<<"\t"<<format_number(row.companion_statistics->mean)<<"\t"
                  <<format_number(row.companion_statistics->sd)<<"\t"<<format_number(row.companion_statistics->median);
        else
            output<<"\t\t\t";
        output<<"\n";
    }
}

}
