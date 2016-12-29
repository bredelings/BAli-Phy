#ifndef SMODEL_SETUP_H
#define SMODEL_SETUP_H
#include <vector>
#include <valarray>
#include <string>
#include <boost/program_options.hpp>
#include "object.H"
#include "alignment/alignment.H"
#include "sequence/alphabet.H"
#include "computation/expression.H"

expression_ref get_model(const std::string&);

std::string default_markov_model(const alphabet& a);

#endif
