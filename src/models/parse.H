#ifndef MODELS_PARSE_H
#define MODELS_PARSE_H

#include <string>
#include "util/ptree.H"

class Rules;

std::string unparse(const ptree& p);
std::string unparse_type(const ptree& p);
std::string show_model(const ptree p);
std::string show_model_abbrev(const ptree p, int length);

ptree parse_type(const std::string& s);
ptree parse(const Rules& R, const std::string& s);

bool is_constant(const ptree& p);
#endif
