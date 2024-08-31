#ifndef MODELS_TRANSLATE_H
#define MODELS_TRANSLATE_H

#include <vector>
#include <string>
#include <utility>
#include "unification.H"
#include "util/ptree.H"

class Rules;

struct tr_name_scope_t
{
    const Rules& R;
    std::map<std::string,ptree> identifiers;
    std::optional<std::map<std::string,ptree>> args;
    std::map<std::string,ptree> state;
    const FVSource& fv_source;
    mutable equations eqs;

    term_t get_fresh_type_var(const std::string& s) const { return fv_source.get_fresh_type_var(s);}

    tr_name_scope_t copy_no_equations() const;
    std::set<std::string> find_type_variables() const;
    std::optional<ptree> type_for_var(const std::string& name) const;
    std::optional<ptree> type_for_arg(const std::string& name) const;
    void extend_scope(const std::string& var, const ptree type);
    tr_name_scope_t extended_scope(const std::string& var, const ptree type) const;
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

    tr_name_scope_t(const Rules& r, const FVSource& fv)
	:R(r),fv_source(fv)
    { }
};

std::pair<ptree,equations>
typecheck_and_annotate_model(const Rules& R,
                             const ptree& required_type,
                             ptree model,
                             const std::map<std::string,term_t>& scope,
                             const std::map<std::string,term_t>& state = {});

std::set<std::string> get_used_args(const ptree& model);
term_t extract_value(const term_t& T);
term_t valueize(const term_t& T);
#endif
