#ifndef DESUGAR_H
#define DESUGAR_H

#include <string>
#include <vector>
#include "computation/haskell/haskell.H"
#include "computation/fresh_vars.H"

class Module;

Core2::Exp<> desugar(const Module& p, FreshVarState&, const expression_ref&);
Core2::Decls<> desugar(const Module& p, FreshVarState&, const Haskell::Decls&);
Core2::Decls<> desugar(const Module& p, FreshVarState&, const Haskell::Binds&);

struct failable_expression: public Object
{
    bool can_fail;

    std::function<Core2::Exp<>(const Core2::Exp<>&)> result;

    failable_expression* clone() const {return new failable_expression(*this);}

    std::string print() const {return "("+std::to_string(can_fail)+","+convertToString(result(Core2::Var<>("FAIL")))+")";}

    void add_binding(const Core2::Decls<>& binds);

    failable_expression(bool b, std::function<Core2::Exp<>(const Core2::Exp<>&)> f)
	:can_fail(b), result(f)
	{}

    explicit failable_expression(const Core2::Exp<>& E)
    {
       can_fail = false;
       result = [E](const Core2::Exp<>&){return E;};
    }
};

failable_expression fail_identity();

struct equation_info_t
{
    std::vector<Hs::Pattern> patterns;
    failable_expression rhs;
};

Hs::VarPattern make_VarPattern(const Core2::Var<>&);

struct desugar_state: public FreshVarSource
{
    const Module& m;

    Core2::Exp<> def_function(const std::vector< equation_info_t >&, const Core2::Exp<>& otherwise);

    failable_expression case_expression(const Core2::Exp<>& T, const std::vector<Hs::Pattern>& patterns, const std::vector<failable_expression>& bodies);

    failable_expression match(const std::vector<Core2::Var<>>& x, const std::vector<equation_info_t>& equations);

    failable_expression match_constructor(const std::vector<Core2::Var<>>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_literal(const std::vector<Core2::Var<>>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_var(const std::vector<Core2::Var<>>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_empty(const std::vector<Core2::Var<>>& x, const std::vector<equation_info_t>& equations);

    failable_expression combine(const failable_expression& E1, const failable_expression& E2);
    failable_expression fold(const std::vector<failable_expression>& Es);
    failable_expression desugar_gdrh(const Haskell::GuardedRHS& ghrs);
    failable_expression desugar_rhs(const Hs::MultiGuardedRHS& R);

    void clean_up_pattern(const Core2::Var<>& x, equation_info_t& eqn);

    equation_info_t desugar_match(const Hs::MRule& matches);
    std::vector<equation_info_t> desugar_matches(const Hs::Matches& matches);

    Core2::Exp<> desugar(const Hs::Exp& E);
    Core2::Exp<> desugar(const Hs::LExp& E);
    Core2::Decls<> desugar_decls(const Haskell::Decls&);
    Core2::Decls<> desugar_decls(const Haskell::Binds&);

    Core2::Exp<> safe_apply(const Core2::Exp<>& head, const std::vector<Core2::Exp<>>& args);

    desugar_state(const Module&, FreshVarState&);
};

#endif
