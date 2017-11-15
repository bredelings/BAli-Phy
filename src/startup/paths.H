#ifndef PATHS_H
#define PATHS_H

#include <boost/filesystem/operations.hpp>
#include <string>

boost::filesystem::path find_exe_path(const boost::filesystem::path& argv0);
boost::filesystem::path get_system_lib_path(const std::string& exe_name);
boost::filesystem::path get_user_lib_path();

#endif
