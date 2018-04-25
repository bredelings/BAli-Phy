#ifndef HELP_H
#define HELP_H

#include <string>
#include <vector>
#include <boost/filesystem.hpp>

std::string pad(const std::string& s, int n);
int terminal_width();
std::string indent_and_wrap(int indent_first_line, int indent_other_lines, int width, const std::string& text);
std::string indent_and_wrap(int indent, int width, const std::string& text);
std::string indent(int indent, const std::string& text);

void show_help(const std::string& topic, const std::vector<boost::filesystem::path>& package_paths);

#endif
