#include "character-properties.H"

namespace character_properties
{

/// Encode an optional iteration bound as either an integer or JSON null.
static json::value optional_iteration(const std::optional<std::int64_t>& value)
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

/// Encode the versioned character-property summary shared by reporting tools.
json::value to_json(const summary& value)
{
    json::object selection;
    selection["skip"] = optional_iteration(value.selection.skip);
    selection["until"] = optional_iteration(value.selection.until);
    selection["subsample"] = value.selection.subsample;

    json::array retained_samples_by_chain;
    retained_samples_by_chain.reserve(value.retained_samples_by_chain.size());
    for (auto count: value.retained_samples_by_chain)
        retained_samples_by_chain.push_back(count);

    json::object properties;
    for (const auto& [name, property]: value.properties)
    {
        json::object encoded;
        encoded["mean"] = sequence_values_to_json(property.mean);
        encoded["sd"] = sequence_values_to_json(property.sd);
        encoded["median"] = sequence_values_to_json(property.median);
        properties[name] = std::move(encoded);
    }

    json::object coordinates;
    coordinates["kind"] = "ungapped-sequence-character";
    coordinates["index_base"] = 0;

    json::object result;
    result["format"] = "bali-phy-character-properties";
    result["version"] = 1;
    result["coordinates"] = std::move(coordinates);
    result["selection"] = std::move(selection);
    result["retained_samples"] = value.retained_samples;
    result["retained_samples_by_chain"] = std::move(retained_samples_by_chain);
    result["properties"] = std::move(properties);
    return result;
}

}
