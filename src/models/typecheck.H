#ifndef MODELS_TRANSLATE_H
#define MODELS_TRANSLATE_H

#include <vector>
#include <string>
#include <utility>
#include <memory>
#include "unification.H"
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
    std::optional<ptree> typecheck_and_annotate_let(const ptree& required_type, const ptree& model) const;
    std::optional<ptree> typecheck_and_annotate_lambda(const ptree& required_type, const ptree& model) const;
    std::optional<ptree> typecheck_and_annotate_tuple(const ptree& required_type, const ptree& model) const;
    std::optional<ptree> typecheck_and_annotate_list(const ptree& required_type, const ptree& model) const;
    std::optional<ptree> typecheck_and_annotate_get_state(const ptree& required_type, const ptree& model) const;
    std::optional<ptree> typecheck_and_annotate_var(const ptree& required_type, const ptree& model) const;
    std::optional<ptree> typecheck_and_annotate_constant(const ptree& required_type, const ptree& model) const;
    ptree typecheck_and_annotate_function(const ptree& required_type, const ptree& model) const;
    ptree typecheck_and_annotate(const ptree& required_type, const ptree& model) const;
    std::pair<ptree, std::map<std::string,ptree>> parse_pattern(const ptree& pattern) const;

    ptree typecheck_and_annotate_decls(const ptree& decls);

    std::optional<ptree> unify_or_convert(const ptree& model, const ptree& type, const ptree& required_type) const;

    TypecheckingState(const Rules& r, const std::shared_ptr<const FVSource>& fv, const std::map<std::string,ptree>& id = {}, const std::map<std::string,ptree>& st = {})
	:R(r),fv_source(fv), identifiers(id), state(st)
    { }
};

std::set<std::string> get_used_args(const ptree& model);
term_t extract_value(const term_t& T);
term_t valueize(const term_t& T);
#endif
