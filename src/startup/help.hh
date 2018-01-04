#ifndef HELP_H
#define HELP_H

#include <string>
#include <vector>
#include <boost/filesystem.hpp>

void show_help(const std::string& topic, const std::vector<boost::filesystem::path>& package_paths);

#endif
