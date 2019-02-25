#ifndef UTIL_FILE_PATHS
#define UTIL_FILE_PATHS

#include <string>
#include <vector>
#include <boost/filesystem/operations.hpp>

std::optional<boost::filesystem::path> check_file_in_path(const std::vector<std::string>& paths, const boost::filesystem::path& file_path);
boost::filesystem::path find_file_in_path(const std::vector<std::string>& paths, const boost::filesystem::path& file_path);
boost::filesystem::path find_exe_path(const boost::filesystem::path& argv0);
std::vector<boost::filesystem::path> clean_paths(const std::vector<boost::filesystem::path>& paths);
#endif
