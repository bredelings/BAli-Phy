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

struct equation_info_t
{
    std::vector<expression_ref> patterns;
    expression_ref rhs;
};

class fresh_vars
{
    int var_index;
public:
    var get_fresh_wildcard() { return var(-var_index++);}
    var get_fresh_var() { return var(var_index++);}
    var get_fresh_var(const std::string& name) {return var(name,var_index++);}

    fresh_vars(int start):var_index(start) {}
};

struct desugar_state: public fresh_vars
{
    const Module& m;

    std::vector<expression_ref> parse_fundecls(const std::vector<expression_ref>& v);
    expression_ref def_function(const std::vector< std::vector<expression_ref> >& patterns, const std::vector<expression_ref>& bodies,const expression_ref& otherwise);

    expression_ref case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies, const expression_ref& otherwise={});
    expression_ref case_expression(const expression_ref& T, const expression_ref& pattern, const expression_ref& body, const expression_ref& otherwise={});
    expression_ref block_case(const std::vector<expression_ref>& x, const std::vector<std::vector<expression_ref>>& p, const std::vector<expression_ref>& b, const expression_ref&);
    expression_ref block_case(const std::vector<expression_ref>& x, const std::vector<equation_info_t>& equations, expression_ref={});

    expression_ref block_case_constant(const std::vector<expression_ref>& x, const std::vector<equation_info_t>& equations, expression_ref={});
    expression_ref block_case_var(const std::vector<expression_ref>& x, const std::vector<equation_info_t>& equations, expression_ref={});
    expression_ref block_case_empty(const std::vector<expression_ref>& x, const std::vector<equation_info_t>& equations, expression_ref={});

    expression_ref desugar(const expression_ref& E);

    desugar_state(const Module&);
};

#endif
