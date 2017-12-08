#ifndef MODELS_TRANSLATE_H
#define MODELS_TRANSLATE_H

#include <vector>
#include <string>
#include <boost/property_tree/ptree.hpp>
#include <utility>
#include "unification.H"

class Rules;

std::pair<boost::property_tree::ptree,equations> translate_model(const Rules& R,
								 const boost::property_tree::ptree& required_type,
								 boost::property_tree::ptree model,
								 const std::vector<std::pair<std::string,term_t>>& scope);

#endif
