#ifndef UTIL_STRING_SPLIT
#define UTIL_STRING_SPLIT

#include <vector>
#include <string>

/// Split the line into pieces at c
std::vector<std::string> split(const std::string& line,char c);
std::vector<std::string> split(const std::string& line,const std::string& s);
std::vector<std::string> resplit(const std::string& line,const std::string& s);
std::pair<std::string,std::string> split_on_last(char, const std::string&);
std::pair<std::string,std::string> split_on_first(char, const std::string&);
std::pair<std::string,std::string> split_on_last_regex(const std::string& sep_rgx, const std::string& final_rgx, const std::string& s);
#endif
