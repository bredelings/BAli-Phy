#ifndef SMODEL_SETUP_H
#define SMODEL_SETUP_H
#include <vector>
#include <valarray>
#include <string>
#include <set>
#include <boost/program_options.hpp>
#include "util/ptree.H"
#include "object.H"
#include "alignment/alignment.H"
#include "sequence/alphabet.H"
#include "computation/expression/expression_ref.H"
#include "unification.H"

struct model_t
{
    ptree description;
    ptree type;
    std::set<term_t> constraints;
    expression_ref expression;
};

class Rules;

class pretty_model_t
{
public:
    ptree main;
    std::vector<std::string> term_names;
    std::vector<pretty_model_t> terms;

    std::string show(const Rules& R, bool = true) const;

    pretty_model_t(const ptree& m);
};

model_t get_model(const Rules& R, const std::string& type, const std::string& model, const std::vector<std::pair<std::string,std::string>>& = std::vector<std::pair<std::string,std::string>>());

std::string default_markov_model(const alphabet& a);
#endif
