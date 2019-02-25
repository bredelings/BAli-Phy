#ifndef PATHS_H
#define PATHS_H

#include <string>
#include <vector>
#include <boost/filesystem/operations.hpp>
#include <boost/program_options.hpp>

boost::filesystem::path get_system_lib_path(const std::string& exe_name);
boost::filesystem::path get_user_lib_path();
std::vector<boost::filesystem::path> get_package_paths(const std::string& argv0, boost::program_options::variables_map& args);

#endif
