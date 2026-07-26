#include "character-property-summary.H"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <functional>
#include <limits>
#include <map>
#include <optional>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <unordered_set>
#include <utility>

#include "util/myexception.H"

namespace character_properties
{

namespace
{

constexpr std::size_t median_sample_size = 100;

// Each active median cell holds at most 100 sampled positions or pivots and 201 bucket counts.
// Four KiB is a conservative C++ allowance for those vectors and the cell itself.
constexpr std::size_t median_bytes_per_cell = 4 * 1024;

using sample_visitor = std::function<void(const json::object&, const std::string&)>;

/// Return a required sample field with a contextual error if it is absent.
const json::value& required_field(const json::object& object, const char* field, const std::string& context)
{
    if (const auto* value = object.if_contains(field))
        return *value;
    throw myexception()<<context<<": missing required field '"<<field<<"'.";
}

/// Read a non-negative JSON integer without accepting floating-point approximations.
std::uint64_t nonnegative_integer(const json::value& value, const std::string& context)
{
    if (value.is_uint64())
        return value.as_uint64();
    if (value.is_int64() and value.as_int64() >= 0)
        return static_cast<std::uint64_t>(value.as_int64());
    throw myexception()<<context<<" must be a non-negative integer.";
}

/// Convert a JSON number to a finite double for scientific summary calculations.
double finite_number(const json::value& value, const std::string& context)
{
    double result;
    if (value.is_double())
        result = value.as_double();
    else if (value.is_int64())
        result = static_cast<double>(value.as_int64());
    else if (value.is_uint64())
        result = static_cast<double>(value.as_uint64());
    else
        throw myexception()<<context<<" must be a number.";

    if (not std::isfinite(result))
        throw myexception()<<context<<" must be finite.";
    return result;
}

/// Parse one non-empty JSON Lines record and attach its file position to syntax errors.
json::object decode_sample(const std::string& line, const std::string& context)
{
    if (line.empty())
        throw myexception()<<context<<": empty JSON line.";

    json::value sample;
    try
    {
        sample = json::parse(line, {}, {.allow_infinity_and_nan = true});
    }
    catch (const std::exception& error)
    {
        throw myexception()<<context<<": invalid JSON: "<<error.what();
    }
    if (not sample.is_object())
        throw myexception()<<context<<": sample must be a JSON object.";
    return std::move(sample.as_object());
}

/// Stream selected samples from one chain using strict skip, inclusive until, and post-filter stride.
std::uint64_t visit_chain(const std::filesystem::path& filename,
                          const sample_selection& selection,
                          const sample_visitor& visitor)
{
    std::ifstream input(filename);
    if (not input)
        throw myexception()<<"Could not open property sample file '"<<filename.string()<<"'.";

    std::optional<std::uint64_t> previous_iteration;
    std::uint64_t eligible_samples = 0;
    std::uint64_t retained_samples = 0;
    std::string line;
    std::uint64_t line_number = 0;
    while (std::getline(input, line))
    {
        line_number++;
        std::string context = filename.string()+":"+std::to_string(line_number);
        auto sample = decode_sample(line, context);
        auto iteration =
            nonnegative_integer(required_field(sample, "iter", context), context+": field 'iter'");
        if (previous_iteration and iteration <= *previous_iteration)
            throw myexception()<<context<<": iterations must increase strictly; got "<<iteration
                               <<" after "<<*previous_iteration<<".";
        previous_iteration = iteration;

        if (selection.until and iteration > static_cast<std::uint64_t>(*selection.until))
            break;
        if (selection.skip and iteration <= static_cast<std::uint64_t>(*selection.skip))
            continue;
        bool retain = eligible_samples % selection.subsample == 0;
        eligible_samples++;
        if (not retain)
            continue;

        visitor(sample, context);
        retained_samples++;
    }
    if (input.bad())
        throw myexception()<<"Error while reading property sample file '"<<filename.string()<<"'.";
    if (retained_samples == 0)
        throw myexception()<<filename.string()<<": no samples remain after selection.";
    return retained_samples;
}

/// Replay all selected chains and return their retained sample counts.
std::vector<std::uint64_t> visit_samples(const summarize_options& options, const sample_visitor& visitor)
{
    std::vector<std::uint64_t> retained;
    retained.reserve(options.filenames.size());
    for (const auto& filename: options.filenames)
        retained.push_back(visit_chain(filename, options.selection, visitor));
    return retained;
}

/// Return object keys in sorted order so sample shapes can be compared deterministically.
std::vector<std::string> object_keys(const json::object& object)
{
    std::vector<std::string> names;
    names.reserve(object.size());
    for (const auto& item: object)
        names.emplace_back(item.key_c_str());
    std::ranges::sort(names);
    return names;
}

struct moment
{
    double mean = 0;
    double m2 = 0;
};

class moment_accumulator
{
    std::vector<std::string> property_names_;
    std::vector<std::string> sequence_names_;
    std::map<std::string, std::size_t> sequence_lengths_;
    std::map<std::string, std::map<std::string, std::vector<moment>>> moments_;
    std::uint64_t retained_samples_ = 0;

    /// Allocate one moment accumulator for every stable property and sequence character.
    void initialize(const json::object& cat_states, const json::object& properties)
    {
        property_names_ = object_keys(properties);
        sequence_names_ = object_keys(cat_states);
        for (const auto& name: sequence_names_)
        {
            const auto& states = cat_states.at(name);
            if (not states.is_array())
                throw myexception()<<"Sequence '"<<name<<"' category states must be an array.";
            sequence_lengths_[name] = states.as_array().size();
        }
        for (const auto& property_name: property_names_)
            for (const auto& sequence_name: sequence_names_)
                moments_[property_name][sequence_name].resize(sequence_lengths_.at(sequence_name));
    }

    /// Require every retained sample to describe the same properties and observed characters.
    void validate_shape(const json::object& cat_states,
                        const json::object& properties,
                        const std::string& context) const
    {
        if (object_keys(properties) != property_names_)
            throw myexception()<<context<<": property names changed.";
        if (object_keys(cat_states) != sequence_names_)
            throw myexception()<<context<<": sequence names changed.";
        for (const auto& name: sequence_names_)
        {
            const auto& states = cat_states.at(name);
            if (not states.is_array())
                throw myexception()<<context<<": sequence '"<<name<<"' category states must be an array.";
            if (states.as_array().size() != sequence_lengths_.at(name))
                throw myexception()<<context<<": sequence '"<<name<<"' length changed; expected "
                                   <<sequence_lengths_.at(name)<<" characters, got "
                                   <<states.as_array().size()<<".";
        }
    }

    /// Validate all component-state property tables before resolving character values.
    void validate_property_tables(const json::object& properties, const std::string& context) const
    {
        for (const auto& property_item: properties)
        {
            std::string property_name(property_item.key_c_str());
            if (not property_item.value().is_array())
                throw myexception()<<context<<": property '"<<property_name
                                   <<"' must contain an array of component tables.";
            const auto& components = property_item.value().as_array();
            for (std::size_t component = 0; component < components.size(); component++)
            {
                if (not components[component].is_array())
                    throw myexception()<<context<<": property '"<<property_name<<"' component "<<component
                                       <<" must contain an array of state values.";
                const auto& states = components[component].as_array();
                for (std::size_t state = 0; state < states.size(); state++)
                    finite_number(states[state], context+": property '"+property_name+"' component "
                                                 +std::to_string(component)+" state "+std::to_string(state));
            }
        }
    }

    /// Resolve a validated character state against one sampled property table.
    double property_value(const json::object& properties,
                          const std::string& property_name,
                          std::uint64_t component,
                          std::uint64_t state,
                          const std::string& context) const
    {
        const auto& components = properties.at(property_name).as_array();
        if (component >= components.size())
            throw myexception()<<context<<": property '"<<property_name
                               <<"' has no component "<<component<<".";
        const auto& states = components[component].as_array();
        if (state >= states.size())
            throw myexception()<<context<<": property '"<<property_name<<"' component "<<component
                               <<" has no state "<<state<<".";
        return finite_number(states[state], context+": property '"+property_name+"'");
    }

public:
    /// Validate and accumulate one complete property sample using Welford's stable recurrence.
    void add_sample(const json::object& sample, const std::string& context)
    {
        const auto& cat_states_value = required_field(sample, "catStates", context);
        const auto& properties_value = required_field(sample, "properties", context);
        if (not cat_states_value.is_object())
            throw myexception()<<context<<": 'catStates' must be an object.";
        if (cat_states_value.as_object().empty())
            throw myexception()<<context<<": 'catStates' must contain a sequence.";
        if (not properties_value.is_object())
            throw myexception()<<context<<": 'properties' must be an object.";
        const auto& cat_states = cat_states_value.as_object();
        const auto& properties = properties_value.as_object();

        validate_property_tables(properties, context);
        if (sequence_names_.empty())
            initialize(cat_states, properties);
        else
            validate_shape(cat_states, properties, context);

        const std::uint64_t count = retained_samples_ + 1;
        for (const auto& sequence_name: sequence_names_)
        {
            const auto& states = cat_states.at(sequence_name).as_array();
            for (std::size_t character = 0; character < states.size(); character++)
            {
                std::string character_context = context+": sequence '"+sequence_name
                                                +"' character "+std::to_string(character);
                if (not states[character].is_array() or states[character].as_array().size() != 2)
                    throw myexception()<<character_context
                                       <<": category-state value must be a two-element array.";
                const auto& pair = states[character].as_array();
                auto component = nonnegative_integer(pair[0], character_context+": category index");
                auto state = nonnegative_integer(pair[1], character_context+": state index");

                for (const auto& property_name: property_names_)
                {
                    double value =
                        property_value(properties, property_name, component, state, character_context);
                    auto& accumulator = moments_[property_name][sequence_name][character];

                    // Welford's recurrence accumulates the centered second moment without subtracting
                    // two potentially large squared values.
                    double delta = value - accumulator.mean;
                    accumulator.mean += delta / static_cast<double>(count);
                    double delta2 = value - accumulator.mean;
                    accumulator.m2 += delta * delta2;
                }
            }
        }
        retained_samples_ = count;
    }

    const std::vector<std::string>& property_names() const {return property_names_;}
    const std::vector<std::string>& sequence_names() const {return sequence_names_;}
    const std::map<std::string, std::size_t>& sequence_lengths() const {return sequence_lengths_;}
    std::uint64_t retained_samples() const {return retained_samples_;}

    /// Convert accumulated moments to posterior population means and standard deviations.
    std::map<std::string, property_summary> result() const
    {
        std::map<std::string, property_summary> result;
        for (const auto& property_name: property_names_)
        {
            auto& property = result[property_name];
            for (const auto& sequence_name: sequence_names_)
            {
                const auto& sequence_moments = moments_.at(property_name).at(sequence_name);
                auto& means = property.mean[sequence_name];
                auto& standard_deviations = property.sd[sequence_name];
                means.reserve(sequence_moments.size());
                standard_deviations.reserve(sequence_moments.size());
                for (const auto& value: sequence_moments)
                {
                    means.push_back(value.mean);
                    standard_deviations.push_back(
                        std::sqrt(std::max(value.m2 / static_cast<double>(retained_samples_), 0.0)));
                }
            }
        }
        return result;
    }
};

class median_cell
{
    std::optional<double> lower_;
    std::optional<double> upper_;
    std::size_t active_count_;
    std::size_t target_rank_;
    std::optional<double> result_;
    std::vector<std::size_t> positions_;
    std::size_t position_index_ = 0;
    std::vector<double> samples_;
    std::vector<double> pivots_;
    std::vector<std::size_t> bucket_counts_;
    std::size_t observed_count_ = 0;

    /// Report whether a value remains inside the current open search interval.
    bool active(double value) const
    {
        return (not lower_ or value > *lower_) and (not upper_ or value < *upper_);
    }

public:
    explicit median_cell(std::size_t count): active_count_(count), target_rank_((count - 1) / 2) {}

    bool done() const {return result_.has_value();}
    double result() const {return *result_;}

    /// Choose up to 100 distinct active-stream positions with Floyd's O(k) sampling algorithm.
    void start_sampling(std::mt19937_64& generator)
    {
        std::size_t sample_size = std::min(median_sample_size, active_count_);
        std::unordered_set<std::size_t> selected;
        selected.reserve(sample_size);
        for (std::size_t j = active_count_ - sample_size; j < active_count_; j++)
        {
            std::uniform_int_distribution<std::size_t> distribution(0, j);
            std::size_t candidate = distribution(generator);
            if (not selected.insert(candidate).second)
                selected.insert(j);
        }
        positions_.assign(selected.begin(), selected.end());
        std::ranges::sort(positions_);
        position_index_ = 0;
        samples_.clear();
        samples_.reserve(sample_size);
        observed_count_ = 0;
    }

    /// Retain an active value only when its stream position was selected.
    void observe_sample(double value)
    {
        if (not active(value))
            return;
        if (position_index_ < positions_.size() and observed_count_ == positions_[position_index_])
        {
            samples_.push_back(value);
            position_index_++;
        }
        observed_count_++;
    }

    /// Resolve a small cell directly or retain unique sampled values as pivots.
    void finish_sampling()
    {
        if (observed_count_ != active_count_ or samples_.size() != positions_.size())
            throw myexception()<<"Property samples changed while computing medians.";
        positions_.clear();
        if (active_count_ <= median_sample_size)
        {
            std::ranges::sort(samples_);
            result_ = samples_[target_rank_];
            samples_.clear();
            return;
        }
        std::ranges::sort(samples_);
        auto end = std::ranges::unique(samples_).begin();
        samples_.erase(end, samples_.end());
        pivots_ = std::move(samples_);
    }

    /// Allocate alternating open-interval and pivot-equality buckets.
    void start_counting()
    {
        bucket_counts_.assign(2 * pivots_.size() + 1, 0);
        observed_count_ = 0;
    }

    /// Count an active value in its ordered interval or pivot-equality bucket.
    void observe_count(double value)
    {
        if (not active(value))
            return;
        auto pivot = std::ranges::lower_bound(pivots_, value);
        std::size_t pivot_index = static_cast<std::size_t>(pivot - pivots_.begin());
        std::size_t bucket =
            pivot != pivots_.end() and *pivot == value ? 2 * pivot_index + 1 : 2 * pivot_index;
        bucket_counts_[bucket]++;
        observed_count_++;
    }

    /// Resolve a pivot rank or narrow the search while preserving its relative rank.
    void finish_counting()
    {
        if (observed_count_ != active_count_)
            throw myexception()<<"Property samples changed while computing medians.";

        // The ordered buckets partition the active values. The target bucket either identifies an
        // equal pivot or preserves the same order statistic in a smaller open interval.
        std::size_t preceding = 0;
        for (std::size_t bucket = 0; bucket < bucket_counts_.size(); bucket++)
        {
            std::size_t count = bucket_counts_[bucket];
            if (target_rank_ >= preceding + count)
            {
                preceding += count;
                continue;
            }

            if (bucket % 2)
                result_ = pivots_[bucket / 2];
            else
            {
                std::size_t gap = bucket / 2;
                if (gap)
                    lower_ = pivots_[gap - 1];
                if (gap < pivots_.size())
                    upper_ = pivots_[gap];
                active_count_ = count;
                target_rank_ -= preceding;
            }
            pivots_.clear();
            bucket_counts_.clear();
            return;
        }
        throw myexception()<<"Could not locate the posterior median.";
    }
};

struct coordinate
{
    std::string sequence;
    std::size_t character;
};

struct median_entry
{
    std::string property;
    coordinate location;
    median_cell cell;
};

/// Resolve one sampled property value for a character already validated in the moments pass.
double replay_value(const json::object& sample, const median_entry& entry)
{
    const auto& state = sample.at("catStates").as_object().at(entry.location.sequence)
                            .as_array()[entry.location.character].as_array();
    auto component = nonnegative_integer(state[0], "replayed category index");
    auto state_index = nonnegative_integer(state[1], "replayed state index");
    return finite_number(sample.at("properties").as_object().at(entry.property)
                             .as_array()[component].as_array()[state_index],
                         "replayed property value");
}

/// Replay each selected sample through the active median cells and detect changed chain lengths.
void replay_medians(const summarize_options& options,
                    const std::vector<std::uint64_t>& expected_counts,
                    std::vector<median_entry*>& entries,
                    bool sampling)
{
    auto observed_counts = visit_samples(
        options,
        [&](const json::object& sample, const std::string&)
        {
            for (auto* entry: entries)
            {
                double value = replay_value(sample, *entry);
                if (sampling)
                    entry->cell.observe_sample(value);
                else
                    entry->cell.observe_count(value);
            }
        });
    if (observed_counts != expected_counts)
        throw myexception()<<"Property sample files changed while computing medians.";
}

/// Compute exact lower medians using random pivots and bounded blocks of property cells.
void calculate_medians(const summarize_options& options,
                       const moment_accumulator& moments,
                       const std::vector<std::uint64_t>& retained_samples_by_chain,
                       std::map<std::string, property_summary>& properties)
{
    if (moments.property_names().empty())
        return;

    std::vector<coordinate> coordinates;
    for (const auto& sequence: moments.sequence_names())
        for (std::size_t character = 0; character < moments.sequence_lengths().at(sequence); character++)
            coordinates.push_back({sequence, character});

    std::size_t budget_bytes;
    if (options.median_memory_mib > std::numeric_limits<std::size_t>::max() / (1024 * 1024))
        budget_bytes = std::numeric_limits<std::size_t>::max();
    else
        budget_bytes = options.median_memory_mib * 1024 * 1024;
    std::size_t cells_per_block = std::max<std::size_t>(1, budget_bytes / median_bytes_per_cell);
    std::size_t characters_per_block =
        std::max<std::size_t>(1, cells_per_block / moments.property_names().size());

    // Random pivots affect only the number of passes, not the exact lower median.
    std::mt19937_64 generator(0);
    for (std::size_t start = 0; start < coordinates.size(); start += characters_per_block)
    {
        std::size_t end = std::min(coordinates.size(), start + characters_per_block);
        std::vector<median_entry> block;
        block.reserve((end - start) * moments.property_names().size());
        for (std::size_t index = start; index < end; index++)
            for (const auto& property: moments.property_names())
                block.push_back({property, coordinates[index], median_cell(moments.retained_samples())});

        while (std::ranges::any_of(block, [](const auto& entry) {return not entry.cell.done();}))
        {
            std::vector<median_entry*> unresolved;
            for (auto& entry: block)
            {
                if (entry.cell.done())
                    continue;
                entry.cell.start_sampling(generator);
                unresolved.push_back(&entry);
            }
            replay_medians(options, retained_samples_by_chain, unresolved, true);
            for (auto* entry: unresolved)
                entry->cell.finish_sampling();

            unresolved.clear();
            for (auto& entry: block)
            {
                if (entry.cell.done())
                    continue;
                entry.cell.start_counting();
                unresolved.push_back(&entry);
            }
            if (unresolved.empty())
                break;
            replay_medians(options, retained_samples_by_chain, unresolved, false);
            for (auto* entry: unresolved)
                entry->cell.finish_counting();
        }

        for (const auto& entry: block)
        {
            auto& values = properties.at(entry.property).median[entry.location.sequence];
            if (values.empty())
                values.resize(moments.sequence_lengths().at(entry.location.sequence));
            values[entry.location.character] = entry.cell.result();
        }
    }
}

}

/// Pool selected property samples and compute per-character mean, SD, and exact median.
summary summarize(const summarize_options& options)
{
    if (options.filenames.empty())
        throw myexception()<<"At least one property sample file is required.";
    if (options.selection.skip and *options.selection.skip < 0)
        throw myexception()<<"--skip must be non-negative.";
    if (options.selection.until and *options.selection.until < 0)
        throw myexception()<<"--until must be non-negative.";
    if (options.selection.subsample == 0)
        throw myexception()<<"--subsample must be positive.";
    if (options.median_memory_mib == 0)
        throw myexception()<<"--median-memory must be positive.";
    if (options.selection.skip and options.selection.until
        and *options.selection.until <= *options.selection.skip)
        throw myexception()<<"--until must be greater than --skip.";

    moment_accumulator moments;
    auto retained_samples_by_chain = visit_samples(
        options,
        [&](const json::object& sample, const std::string& context) {moments.add_sample(sample, context);});

    summary result;
    result.selection = options.selection;
    result.retained_samples = moments.retained_samples();
    result.retained_samples_by_chain = retained_samples_by_chain;
    result.properties = moments.result();
    calculate_medians(options, moments, retained_samples_by_chain, result.properties);
    return result;
}

}
