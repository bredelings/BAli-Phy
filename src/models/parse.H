#ifndef MODELS_PARSE_H
#define MODELS_PARSE_H

#include <string>
#include "util/ptree.H"

class Rules;

std::string unparse(const ptree& p);
std::string unparse_annotated(const ptree&);
std::string unparse_type(const ptree& p);
std::string show_model(const ptree p);
std::string show_model_annotated(const ptree p);
std::string show_model_abbrev(const ptree p, int length);

ptree parse_type(const std::string& s);
ptree parse(const Rules& R, const std::string& s, const std::string& what);

ptree add_sample(const ptree& p);
ptree add_submodel(ptree term, const ptree& submodel);

// This assumes that any non-empty string is a variable, except the empty Tuple and empty List.
bool is_nontype_variable(const ptree&);
bool is_list(const ptree&);
bool is_constant(const ptree&);
bool is_tuple(const ptree&);
bool is_function(const ptree&);

#endif
