#ifndef UTIL_FILE_PATHS
#define UTIL_FILE_PATHS

#include <optional>
#include <string>
#include <vector>
#include <filesystem>

// Path lists follow the host PATH convention so Windows drive-letter colons remain part of a path.
#ifdef _WIN32
inline constexpr char native_path_list_separator = ';';
#else
inline constexpr char native_path_list_separator = ':';
#endif

std::optional<std::filesystem::path> check_file_in_path(const std::vector<std::filesystem::path>& paths, const std::filesystem::path& file_path);
std::filesystem::path find_file_in_path(const std::vector<std::filesystem::path>& paths, const std::filesystem::path& file_path);

std::string show_path(const std::vector<std::filesystem::path>& paths);
std::filesystem::path find_exe_path();
std::vector<std::filesystem::path> clean_paths(const std::vector<std::filesystem::path>& paths);
std::optional<std::filesystem::path> get_home_dir();
std::optional<std::filesystem::path> base_user_data_path();
#endif
