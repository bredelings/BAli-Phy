#ifndef UTIL_STRING_PRED_H
#define UTIL_STRING_PRED_H

#include <string>

bool starts_with(const std::string& s, const std::string& prefix);
bool ends_with(const std::string& s, const std::string& prefix);
bool contains_char(const std::string& s,char c);

#endif
