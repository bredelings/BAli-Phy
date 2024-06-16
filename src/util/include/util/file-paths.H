#ifndef UTIL_FILE_PATHS
#define UTIL_FILE_PATHS

#include <optional>
#include <string>
#include <vector>
#include <filesystem>

std::optional<std::filesystem::path> check_file_in_path(const std::vector<std::filesystem::path>& paths, const std::filesystem::path& file_path);
std::filesystem::path find_file_in_path(const std::vector<std::filesystem::path>& paths, const std::filesystem::path& file_path);

std::string show_path(const std::vector<std::filesystem::path>& paths);
std::filesystem::path find_exe_path();
std::vector<std::filesystem::path> clean_paths(const std::vector<std::filesystem::path>& paths);
std::optional<std::filesystem::path> get_home_dir();
std::optional<std::filesystem::path> base_user_data_path();
#endif
