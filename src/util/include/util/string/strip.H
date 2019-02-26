#ifndef UTIL_STRING_STRIP
#define UTIL_STRING_STRIP
#include <string>

/// Remove all occurrences of char c from line
std::string strip(const std::string& line,char c);
/// Remove all occurrences of each char in chars from line
std::string strip(const std::string& line,const std::string& chars);

std::string rstrip(const std::string& line,const std::string& chars);
std::string lstrip(const std::string& line,const std::string& chars);

#endif

