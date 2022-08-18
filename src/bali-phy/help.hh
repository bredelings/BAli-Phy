#ifndef HELP_H
#define HELP_H

#include <string>
#include <vector>
#include <filesystem>

void show_help(const std::string& topic, const std::vector<std::filesystem::path>& package_paths);
void help_topics(std::ostream& o, const std::vector<std::filesystem::path>& package_paths);

#endif
