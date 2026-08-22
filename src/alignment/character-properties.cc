#include "character-properties.hh"

#include <cmath>
#include <fstream>
#include <limits>
#include <sstream>

#include "util/myexception.hh"

namespace character_properties
{

namespace
{

/// Return a required summary field with a contextual error if it is absent.
const json::value& required_field(const json::object& object, const char* field, const std::string& context)
{
    if (const auto* value = object.if_contains(field))
        return *value;
    throw myexception()<<context<<": missing required field '"<<field<<"'.";
}

/// Read a required JSON string field without accepting other scalar types.
std::string required_string(const json::object& object, const char* field, const std::string& context)
{
    const auto& value = required_field(object, field, context);
    if (not value.is_string())
        throw myexception()<<context<<": field '"<<field<<"' must be a string.";
    return std::string(value.as_string().c_str());
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

/// Read an optional non-negative iteration bound into the summary's signed representation.
std::optional<std::int64_t> parse_optional_iteration(const json::value& value, const std::string& context)
{
    if (value.is_null())
        return {};
    auto result = nonnegative_integer(value, context);
    if (result > static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()))
        throw myexception()<<context<<" is too large.";
    return static_cast<std::int64_t>(result);
}

/// Convert a JSON number to a finite double for a complete posterior summary.
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

/// Decode sequence-indexed arrays for one posterior summary statistic.
sequence_values parse_sequence_values(const json::value& value,
                                      const std::string& property,
                                      const std::string& statistic)
{
    if (not value.is_object())
        throw myexception()<<"Property '"<<property<<"': field '"<<statistic
                           <<"' must be an object keyed by sequence name.";

    sequence_values result;
    for (const auto& item: value.as_object())
    {
        std::string sequence(item.key_c_str());
        if (not item.value().is_array())
            throw myexception()<<"Property '"<<property<<"': "<<statistic<<" values for sequence '"
                               <<sequence<<"' must be an array.";
        auto& values = result[sequence];
        values.reserve(item.value().as_array().size());
        for (std::size_t character = 0; character < item.value().as_array().size(); character++)
            values.push_back(finite_number(item.value().as_array()[character],
                                           "Property '"+property+"', sequence '"+sequence+"', character "
                                               +std::to_string(character)+" "+statistic));
    }
    return result;
}

/// Require two statistics to cover the same sequences and character positions.
void require_same_shape(const sequence_values& reference, const sequence_values& values, const std::string& property,
                        const std::string& statistic)
{
    if (reference.size() != values.size())
        throw myexception()<<"Property '"<<property<<"': fields 'mean' and '"<<statistic
                           <<"' contain different sequence names.";
    for (const auto& [sequence, reference_values]: reference)
    {
        auto found = values.find(sequence);
        if (found == values.end())
            throw myexception()<<"Property '"<<property<<"': "<<statistic
                               <<" values are missing sequence '"<<sequence<<"'.";
        if (found->second.size() != reference_values.size())
            throw myexception()<<"Property '"<<property<<"': sequence '"<<sequence
                               <<"' has different numbers of mean and "<<statistic<<" values.";
    }
}

/// Decode one property's complete mean, SD, and median matrices.
property_summary parse_property(const std::string& name, const json::value& value)
{
    if (not value.is_object())
        throw myexception()<<"Property '"<<name<<"' must be an object.";
    const auto& object = value.as_object();

    property_summary result;
    result.mean = parse_sequence_values(required_field(object, "mean", "Property '"+name+"'"), name, "mean");
    result.sd = parse_sequence_values(required_field(object, "sd", "Property '"+name+"'"), name, "sd");
    result.median = parse_sequence_values(required_field(object, "median", "Property '"+name+"'"), name, "median");
    require_same_shape(result.mean, result.sd, name, "sd");
    require_same_shape(result.mean, result.median, name, "median");
    return result;
}

/// Decode a complete property map for either the unconditional or a conditioned view.
std::map<std::string, property_summary> parse_properties(const json::value& value, const std::string& context)
{
    if (not value.is_object())
        throw myexception()<<context<<" must be an object.";

    std::map<std::string, property_summary> result;
    for (const auto& item: value.as_object())
    {
        std::string name(item.key_c_str());
        result[name] = parse_property(name, item.value());
    }
    return result;
}

/// Decode per-chain counts and require that they sum to the supplied total.
std::vector<std::uint64_t> parse_chain_counts(const json::value& value, std::uint64_t total,
                                              const std::string& context)
{
    if (not value.is_array())
        throw myexception()<<context<<" must be an array.";

    std::vector<std::uint64_t> result;
    std::uint64_t sum = 0;
    for (const auto& entry: value.as_array())
    {
        auto count = nonnegative_integer(entry, context+" entry");
        sum += count;
        result.push_back(count);
    }
    if (sum != total)
        throw myexception()<<context<<" does not sum to retained_samples.";
    return result;
}

}

/// Encode an optional iteration bound as either an integer or JSON null.
static json::value encode_optional_iteration(const std::optional<std::int64_t>& value)
{
    if (value)
        return *value;
    return nullptr;
}

/// Encode sequence-indexed numeric arrays without changing their stable names.
static json::object sequence_values_to_json(const sequence_values& values)
{
    json::object result;
    for (const auto& [name, sequence]: values)
    {
        json::array array;
        array.reserve(sequence.size());
        for (double value: sequence)
            array.push_back(value);
        result[name] = std::move(array);
    }
    return result;
}

/// Encode per-chain retained-sample counts for either summary view.
static json::array chain_counts_to_json(const std::vector<std::uint64_t>& counts)
{
    json::array result;
    result.reserve(counts.size());
    for (auto count: counts)
        result.push_back(count);
    return result;
}

/// Encode all property summaries in one posterior view.
static json::object properties_to_json(const std::map<std::string, property_summary>& properties)
{
    json::object result;
    for (const auto& [name, property]: properties)
    {
        json::object encoded;
        encoded["mean"] = sequence_values_to_json(property.mean);
        encoded["sd"] = sequence_values_to_json(property.sd);
        encoded["median"] = sequence_values_to_json(property.median);
        result[name] = std::move(encoded);
    }
    return result;
}

/// Encode the versioned character-property summary shared by reporting tools.
json::value to_json(const summary& value)
{
    json::object selection;
    selection["skip"] = encode_optional_iteration(value.selection.skip);
    selection["until"] = encode_optional_iteration(value.selection.until);
    selection["subsample"] = value.selection.subsample;

    json::object conditioned;
    for (const auto& [name, view]: value.conditioned)
    {
        json::object encoded;
        encoded["condition_value"] = true;
        encoded["retained_samples"] = view.retained_samples;
        encoded["retained_samples_by_chain"] = chain_counts_to_json(view.retained_samples_by_chain);
        encoded["properties"] = properties_to_json(view.properties);
        conditioned[name] = std::move(encoded);
    }

    json::object coordinates;
    coordinates["kind"] = "ungapped-sequence-character";
    coordinates["index_base"] = 0;

    json::object result;
    result["format"] = "bali-phy-character-properties";
    result["version"] = 2;
    result["coordinates"] = std::move(coordinates);
    result["selection"] = std::move(selection);
    result["retained_samples"] = value.retained_samples;
    result["retained_samples_by_chain"] = chain_counts_to_json(value.retained_samples_by_chain);
    result["properties"] = properties_to_json(value.properties);
    result["conditioned"] = std::move(conditioned);
    return result;
}

/// Decode and validate a versioned character-property summary.
summary from_json(const json::value& document, const std::string& context)
{
    if (not document.is_object())
        throw myexception()<<context<<" must contain a JSON object.";
    const auto& root = document.as_object();
    if (required_string(root, "format", context) != "bali-phy-character-properties")
        throw myexception()<<context<<": unrecognized format.";
    auto version = nonnegative_integer(required_field(root, "version", context), context+" version");
    if (version != 1 and version != 2)
        throw myexception()<<context<<": only versions 1 and 2 are supported.";

    const auto& coordinates_value = required_field(root, "coordinates", context);
    if (not coordinates_value.is_object())
        throw myexception()<<context<<": field 'coordinates' must be an object.";
    const auto& coordinates = coordinates_value.as_object();
    if (required_string(coordinates, "kind", context+" coordinates") != "ungapped-sequence-character")
        throw myexception()<<context<<": unsupported coordinate kind.";
    if (nonnegative_integer(required_field(coordinates, "index_base", context+" coordinates"),
                            context+" coordinates index_base") != 0)
        throw myexception()<<context<<": coordinates index_base must be 0.";

    const auto& selection_value = required_field(root, "selection", context);
    if (not selection_value.is_object())
        throw myexception()<<context<<": field 'selection' must be an object.";
    const auto& selection = selection_value.as_object();
    std::string selection_context = context+" selection";

    summary result;
    result.selection.skip =
        parse_optional_iteration(
            required_field(selection, "skip", selection_context), selection_context+" skip");
    result.selection.until =
        parse_optional_iteration(
            required_field(selection, "until", selection_context), selection_context+" until");
    result.selection.subsample = nonnegative_integer(
        required_field(selection, "subsample", selection_context), selection_context+" subsample");
    if (result.selection.subsample == 0)
        throw myexception()<<context<<": selection subsample must be positive.";
    if (result.selection.skip and result.selection.until
        and *result.selection.until <= *result.selection.skip)
        throw myexception()<<context<<": selection until must be greater than skip.";

    result.retained_samples =
        nonnegative_integer(required_field(root, "retained_samples", context), context+" retained_samples");

    result.retained_samples_by_chain = parse_chain_counts(
        required_field(root, "retained_samples_by_chain", context), result.retained_samples,
        context+" retained_samples_by_chain");
    result.properties = parse_properties(required_field(root, "properties", context), context+" properties");

    if (version == 2)
    {
        const auto& conditioned = required_field(root, "conditioned", context);
        if (not conditioned.is_object())
            throw myexception()<<context<<": field 'conditioned' must be an object.";
        for (const auto& item: conditioned.as_object())
        {
            std::string name(item.key_c_str());
            std::string condition_context = context+" conditioned '"+name+"'";
            if (not item.value().is_object())
                throw myexception()<<condition_context<<" must be an object.";
            const auto& object = item.value().as_object();
            const auto& condition_value = required_field(object, "condition_value", condition_context);
            if (not condition_value.is_bool() or not condition_value.as_bool())
                throw myexception()<<condition_context<<": condition_value must be true.";

            auto& view = result.conditioned[name];
            view.retained_samples = nonnegative_integer(
                required_field(object, "retained_samples", condition_context), condition_context+" retained_samples");
            if (view.retained_samples > result.retained_samples)
                throw myexception()<<condition_context<<": retained_samples exceeds the unconditional count.";
            view.retained_samples_by_chain = parse_chain_counts(
                required_field(object, "retained_samples_by_chain", condition_context), view.retained_samples,
                condition_context+" retained_samples_by_chain");
            if (view.retained_samples_by_chain.size() != result.retained_samples_by_chain.size())
                throw myexception()<<condition_context<<": retained_samples_by_chain has the wrong number of chains.";
            for (std::size_t chain = 0; chain < view.retained_samples_by_chain.size(); chain++)
                if (view.retained_samples_by_chain[chain] > result.retained_samples_by_chain[chain])
                    throw myexception()<<condition_context
                                       <<": conditioned count exceeds the unconditional count for chain "<<chain<<".";
            view.properties = parse_properties(
                required_field(object, "properties", condition_context), condition_context+" properties");
            if (view.retained_samples == 0 and not view.properties.empty())
                throw myexception()<<condition_context<<": zero retained samples require an empty property map.";
            if (view.retained_samples > 0 and view.properties.size() != result.properties.size())
                throw myexception()<<condition_context<<": property names differ from the unconditional view.";
            for (const auto& [property_name, property]: result.properties)
                if (view.retained_samples > 0 and not view.properties.contains(property_name))
                    throw myexception()<<condition_context<<": property '"<<property_name<<"' is missing.";
        }
    }
    return result;
}

/// Read and validate a character-property summary from a JSON file.
summary read_summary(const std::filesystem::path& filename)
{
    std::ifstream input(filename);
    if (not input)
        throw myexception()<<"Could not open character property file '"<<filename.string()<<"'.";
    std::ostringstream contents;
    contents<<input.rdbuf();
    if (input.bad())
        throw myexception()<<"Error while reading character property file '"<<filename.string()<<"'.";

    std::string context = "Character property file '"+filename.string()+"'";
    try
    {
        return from_json(json::parse(contents.str()), context);
    }
    catch (myexception&)
    {
        throw;
    }
    catch (const std::exception& error)
    {
        throw myexception()<<context<<": invalid JSON: "<<error.what();
    }
}

}
