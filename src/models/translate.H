#ifndef MODELS_TRANSLATE_H
#define MODELS_TRANSLATE_H

#include <string>
#include <boost/property_tree/ptree.hpp>
#include <utility>
#include "unification.H"

std::pair<boost::property_tree::ptree,equations_t> translate_model(const std::string& required_type, const std::string& model);

#endif
