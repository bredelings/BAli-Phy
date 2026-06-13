#ifndef DESUGAR_H
#define DESUGAR_H

#include <string>
#include <vector>
#include "computation/haskell/haskell.H"
#include "computation/fresh_vars.H"

class Module;

Core::Exp<> desugar(const Module& p, FreshVarState&, const expression_ref&);
Core::Decls<> desugar(const Module& p, FreshVarState&, const Haskell::Decls&);
Core::Decls<> desugar(const Module& p, FreshVarState&, const Haskell::Binds&);

struct desugared_decls
{
    Core::Decls<> decls;
    std::vector<Core::Var<>> strict_vars;

    void append(const desugared_decls&);
    Core::Exp<> wrap_body(const Core::Exp<>&) const;
    std::string print() const {return decls.print();}
};

struct failable_expression: public Object
{
    bool can_fail;

    std::function<Core::Exp<>(const Core::Exp<>&)> result;

    failable_expression* clone() const {return new failable_expression(*this);}

    std::string print() const {return "("+std::to_string(can_fail)+","+convertToString(result(Core::Var<>("FAIL")))+")";}

    void add_binding(const Core::Decls<>& binds);
    void add_binding(const desugared_decls& binds);

    failable_expression(bool b, std::function<Core::Exp<>(const Core::Exp<>&)> f)
	:can_fail(b), result(f)
	{}

    explicit failable_expression(const Core::Exp<>& E)
    {
       can_fail = false;
       result = [E](const Core::Exp<>&){return E;};
    }
};

failable_expression fail_identity();

struct equation_info_t
{
    std::vector<Hs::Pattern> patterns;
    failable_expression rhs;
};

Hs::VarPattern make_VarPattern(const Core::Var<>&);

struct desugar_state: public FreshVarSource
{
    const Module& m;

    Core::Exp<> pattern_match_failure(const std::optional<yy::location>&, const std::string&) const;

    Core::Exp<> def_function(const std::vector< equation_info_t >&, const Core::Exp<>& otherwise);

    failable_expression case_expression(const Core::Exp<>& T, const std::vector<Hs::Pattern>& patterns, const std::vector<failable_expression>& bodies);

    failable_expression match(const std::vector<Core::Var<>>& x, const std::vector<equation_info_t>& equations);

    failable_expression match_constructor(const std::vector<Core::Var<>>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_literal(const std::vector<Core::Var<>>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_var(const std::vector<Core::Var<>>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_bang(const std::vector<Core::Var<>>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_wrap(const std::vector<Core::Var<>>& x, const std::vector<equation_info_t>& equations);
    failable_expression match_empty(const std::vector<Core::Var<>>& x, const std::vector<equation_info_t>& equations);

    failable_expression combine(const failable_expression& E1, const failable_expression& E2);
    failable_expression fold(const std::vector<failable_expression>& Es);
    failable_expression desugar_gdrh(const Haskell::GuardedRHS& ghrs);
    failable_expression desugar_rhs(const Hs::MultiGuardedRHS& R);

    void clean_up_pattern(const Core::Var<>& x, equation_info_t& eqn);

    equation_info_t desugar_match(const Hs::MRule& matches);
    std::vector<equation_info_t> desugar_matches(const Hs::Matches& matches);

    Core::Exp<> desugar(const Hs::Exp& E);
    Core::Exp<> desugar(const Hs::LExp& E);
    Core::Exp<> desugar_record_update(const Hs::CheckedRecordUpdate& update);
    desugared_decls desugar_decls(const Haskell::Decls&);
    desugared_decls desugar_decls(const Haskell::Binds&);

    Core::Exp<> safe_apply(const Core::Exp<>& head, const std::vector<Core::Exp<>>& args);

    desugar_state(const Module&, FreshVarState&);
};

#endif
