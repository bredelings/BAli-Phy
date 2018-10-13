#ifndef DESUGAR_H
#define DESUGAR_H

#include <set>
#include <string>
#include <vector>
#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/module.H"

class Module;
class Model;

expression_ref desugar(const Module& p, const expression_ref&);
Module read_model(const std::string& filename);
void read_add_model(Model& M, const std::string& filename);
void add_model(Model& M, const std::string& filename);

struct desugar_state
{
    const Module& m;

    int var_index;

    var get_fresh_wildcard() { return var(-var_index++);}
    var get_fresh_var() { return var(var_index++);}
    var get_fresh_var(const std::string& name) {return var(name,var_index++);}

    std::vector<expression_ref> parse_fundecls(const std::vector<expression_ref>& v);
    expression_ref desugar(const expression_ref& E);

    desugar_state(const Module&);
};

#endif
