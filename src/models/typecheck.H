#ifndef MODELS_TRANSLATE_H
#define MODELS_TRANSLATE_H

#include <vector>
#include <string>
#include <utility>
#include <memory>
#include "unification.H"
#include "models/model-expr.H"
#include "util/ptree.H"

class Rules;

struct TypecheckingState
{
    const Rules& R;
    std::shared_ptr<const FVSource> fv_source;
    std::map<std::string,ptree> identifiers;
    std::map<std::string,ptree> state;
    std::optional<std::map<std::string,ptree>> args;
    mutable equations eqs;

    term_t get_fresh_type_var(const std::string& s) const { return fv_source->get_fresh_type_var(s);}

    TypecheckingState copy_no_equations() const;
    std::set<std::string> find_type_variables() const;
    std::optional<ptree> type_for_var(const std::string& name) const;
    std::optional<ptree> type_for_arg(const std::string& name) const;
    void extend_scope(const std::string& var, const ptree type);
    TypecheckingState extended_scope(const std::string& var, const ptree type) const;
    std::pair<ptree, std::map<std::string,ptree>> parse_pattern(const CM::UntypedPattern& pattern) const;

    void add_states(const std::map<std::string,std::pair<std::string,ptree>>&);

    std::optional<CM::UntypedExpr> unify_or_convert_model_expr(const CM::UntypedExpr& model, const ptree& type, const ptree& required_type) const;

    TypecheckingState(const Rules& r, const std::shared_ptr<const FVSource>& fv, const std::map<std::string,ptree>& id = {}, const std::map<std::string,ptree>& st = {})
	:R(r),fv_source(fv), identifiers(id), state(st)
    { }
};

void substitute_annotated(const equations&, CM::TypedExpr&);
void substitute_annotated(const equations&, CM::TypedPattern&);
void substitute_annotated(const equations&, CM::TypedDecls&);
CM::TypedExpr typecheck_model_expr(const TypecheckingState&, const ptree& required_type, const CM::UntypedExpr&);
CM::TypedDecls typecheck_model_decls(TypecheckingState&, const CM::Decls<CM::NoAnn>&);
#endif
