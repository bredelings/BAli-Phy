#ifndef DESUGAR_H
#define DESUGAR_H

#include <string>
#include <vector>
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"
#include "computation/fresh_vars.H"

class Module;

expression_ref desugar(const Module& p, FreshVarState&, const expression_ref&);
CDecls desugar(const Module& p, FreshVarState&, const Haskell::Decls&);
CDecls desugar(const Module& p, FreshVarState&, const Haskell::Binds&);

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

var make_var(const Hs::Var&);

CDecls translate_decls_to_cdecls(const Haskell::Decls&);

struct desugar_state: public FreshVarSource
{
    const Module& m;

    expression_ref def_function(const std::vector< equation_info_t >&, const expression_ref& otherwise);

    failable_expression case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<failable_expression>& bodies);

    failable_expression match(const std::vector<var>& x, const std::vector<equation_info_t>& equations);

    failable_expression match_constructor(const std::vector<var>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_literal(const std::vector<var>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_var(const std::vector<var>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_empty(const std::vector<var>& x, const std::vector<equation_info_t>& equations);

    failable_expression combine(const failable_expression& E1, const failable_expression& E2);
    failable_expression fold(const std::vector<failable_expression>& Es);
    failable_expression desugar_gdrh(const Haskell::GuardedRHS& ghrs);
    failable_expression desugar_rhs(const Hs::MultiGuardedRHS& R);

    void clean_up_pattern(const expression_ref& x, equation_info_t& eqn);

    equation_info_t desugar_match(const Hs::MRule& matches);
    std::vector<equation_info_t> desugar_matches(const Hs::Matches& matches);

    expression_ref desugar(const expression_ref& E);
    CDecls desugar_decls(const Haskell::Decls&);
    CDecls desugar_decls(const Haskell::Binds&);

    desugar_state(const Module&, FreshVarState&);
};

#endif
