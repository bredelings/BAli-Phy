#ifndef MODELS_TRANSLATE_H
#define MODELS_TRANSLATE_H

#include <string>
#include <boost/property_tree/ptree.hpp>
#include <utility>
#include "unification.H"

std::pair<boost::property_tree::ptree,equations> translate_model(const boost::property_tree::ptree& required_type, boost::property_tree::ptree model);

#endif
