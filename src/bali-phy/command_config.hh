#ifndef BALI_PHY_COMMAND_CONFIG_H
#define BALI_PHY_COMMAND_CONFIG_H

#include <cstddef>
#include <istream>
#include <map>
#include <string>
#include <vector>

struct CommandConfig
{
    std::map<std::string, std::vector<std::string>> options;
    std::map<std::string, std::size_t> option_lines;
    std::string model_source;
};

/// Read BAli-Phy's colon-prefixed options and collect all other source lines verbatim.
CommandConfig read_command_config(std::istream& file, const std::string& filename);

#endif
