#ifndef DESUGAR_H
#define DESUGAR_H

#include <string>
#include <vector>
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"

class Module;

expression_ref desugar(const Module& p, const expression_ref&);
Haskell::Decls desugar(const Module& p, Haskell::Decls);

struct failable_expression: public Object
{
    failable_expression* clone() const {return new failable_expression(*this);}

    bool can_fail;

    std::function<expression_ref(const expression_ref&)> result;

    std::string print() const {return "("+std::to_string(can_fail)+","+convertToString(result(var("FAIL")))+")";}

    void add_binding(const CDecls& binds)
    {
	auto result2 = result;
	result(0);
	result2(0);
	result = [result2,binds](const expression_ref& o) {return let_expression(binds,result2(o));};
    }

    failable_expression(bool b, std::function<expression_ref(const expression_ref&)> f)
	:can_fail(b), result(f)
	{}

    explicit failable_expression(const expression_ref& E)
    {
       can_fail = false;
       result = [E](const expression_ref&){return E;};
    }
};

failable_expression fail_identity();

struct equation_info_t
{
    std::vector<expression_ref> patterns;
    failable_expression rhs;
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

CDecls translate_decls_to_cdecls(const Haskell::Decls&);

struct desugar_state: public fresh_vars
{
    const Module& m;

    Haskell::Decls parse_fundecls(Haskell::Decls);

    expression_ref def_function(const std::vector< equation_info_t >&, const expression_ref& otherwise);

    failable_expression case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<failable_expression>& bodies);

    failable_expression match(const std::vector<expression_ref>& x, const std::vector<equation_info_t>& equations);

    failable_expression match_constant(const std::vector<expression_ref>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_var(const std::vector<expression_ref>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_empty(const std::vector<expression_ref>& x, const std::vector<equation_info_t>& equations);

    failable_expression combine(const failable_expression& E1, const failable_expression& E2);
    failable_expression fold(const std::vector<failable_expression>& Es);
    failable_expression desugar_gdrh(const Haskell::GuardedRHS& ghrs);
    failable_expression desugar_rhs(const Hs::MultiGuardedRHS& R);

    void clean_up_pattern(const expression_ref& x, equation_info_t& eqn);

    expression_ref desugar_pattern(const expression_ref& E);
    expression_ref desugar(const expression_ref& E);
    Haskell::Decls desugar_decls(Haskell::Decls);
    CDecls desugar_decls_to_cdecls(const Haskell::Decls& D);

    desugar_state(const Module&);
};

#endif
