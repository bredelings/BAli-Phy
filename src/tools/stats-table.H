#ifndef STATS_TABLE_H
#define STATS_TABLE_H

#include <vector>
#include <string>
#include <iostream>

std::vector<std::string> parse_header(const std::string&);

std::vector<std::string> read_header(std::istream&);

#endif
