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

struct model_t
{
    std::string description;
    std::string type;
    expression_ref expression;
};

model_t get_model(const std::string& type, const std::string& model);

std::string default_markov_model(const alphabet& a);

#endif
