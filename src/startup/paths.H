#ifndef PATHS_H
#define PATHS_H

#include <string>
#include <vector>
#include <boost/filesystem/operations.hpp>
#include <boost/program_options.hpp>
#include <boost/optional.hpp>

std::optional<boost::filesystem::path> check_file_in_path(const std::vector<std::string>& paths, const boost::filesystem::path& file_path);
boost::filesystem::path find_file_in_path(const std::vector<std::string>& paths, const boost::filesystem::path& file_path);
boost::filesystem::path find_exe_path(const boost::filesystem::path& argv0);
boost::filesystem::path get_system_lib_path(const std::string& exe_name);
boost::filesystem::path get_user_lib_path();
std::vector<boost::filesystem::path> clean_paths(const std::vector<boost::filesystem::path>& paths);
std::vector<boost::filesystem::path> get_package_paths(const std::string& argv0, boost::program_options::variables_map& args);

#endif
