#ifndef PATHS_H
#define PATHS_H

#include <string>
#include <vector>
#include <filesystem>
#include <optional>

std::optional<std::filesystem::path> get_system_lib_path();
std::optional<std::filesystem::path> get_user_lib_path();
std::optional<std::filesystem::path> get_cache_path();
std::vector<std::filesystem::path> get_package_paths(const std::vector<std::string>& path_arguments);

#endif
