#ifndef MODELS_TRANSLATE_H
#define MODELS_TRANSLATE_H

#include <vector>
#include <string>
#include <utility>
#include "unification.H"
#include "util/ptree.H"

class Rules;

std::pair<ptree,equations>
typecheck_and_annotate_model(const Rules& R,
                             const ptree& required_type,
                             ptree model,
                             const std::map<std::string,term_t>& scope,
                             const std::map<std::string,term_t>& state = {});

std::set<std::string> get_used_args(const ptree& model);
term_t extract_value(const term_t& T);
term_t valueize(const term_t& T);
#endif
