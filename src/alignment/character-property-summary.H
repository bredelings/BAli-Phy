#ifndef CHARACTER_PROPERTY_SUMMARY_H
#define CHARACTER_PROPERTY_SUMMARY_H

#include <cstddef>
#include <filesystem>
#include <vector>

#include "character-properties.H"

namespace character_properties
{

struct summarize_options
{
    std::vector<std::filesystem::path> filenames;
    sample_selection selection;
    std::size_t median_memory_mib = 256;
};

/// Pool selected property samples and compute per-character mean, SD, and exact median.
summary summarize(const summarize_options& options);

}

#endif
