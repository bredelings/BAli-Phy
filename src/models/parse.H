#ifndef MODELS_PARSE_H
#define MODELS_PARSE_H

#include <string>
#include <boost/property_tree/ptree.hpp>

class Rules;

std::string unparse(const boost::property_tree::ptree& p);
std::string unparse_type(const boost::property_tree::ptree& p);
std::string show_model(const boost::property_tree::ptree p);

boost::property_tree::ptree parse_type(const std::string& s);
boost::property_tree::ptree parse(const Rules& R, const std::string& s);
#endif
