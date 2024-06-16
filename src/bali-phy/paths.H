#ifndef PATHS_H
#define PATHS_H

#include <string>
#include <vector>
#include <filesystem>
#include <optional>
#include <boost/program_options.hpp>

std::optional<std::filesystem::path> get_system_lib_path();
std::optional<std::filesystem::path> get_user_lib_path();
std::optional<std::filesystem::path> get_cache_path();
std::vector<std::filesystem::path> get_package_paths(boost::program_options::variables_map& args);

#endif
