#ifndef CHARACTER_PROPERTIES_H
#define CHARACTER_PROPERTIES_H

#include <cstdint>
#include <filesystem>
#include <map>
#include <optional>
#include <string>
#include <vector>

#include "util/json.hh"

namespace character_properties
{

struct sample_selection
{
    std::optional<std::int64_t> skip;
    std::optional<std::int64_t> until;
    std::uint64_t subsample = 1;
};

using sequence_values = std::map<std::string, std::vector<double>>;

struct property_summary
{
    sequence_values mean;
    sequence_values sd;
    sequence_values median;
};

struct summary
{
    sample_selection selection;
    std::uint64_t retained_samples = 0;
    std::vector<std::uint64_t> retained_samples_by_chain;
    std::map<std::string, property_summary> properties;
};

/// Encode the versioned character-property summary shared by reporting tools.
json::value to_json(const summary& value);

/// Decode and validate a versioned character-property summary.
summary from_json(const json::value& document, const std::string& context);

/// Read and validate a character-property summary from a JSON file.
summary read_summary(const std::filesystem::path& filename);

}

#endif
