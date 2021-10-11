#ifndef KINDCHECK_H
#define KINDCHECK_H

#include "parser/haskell.H"
#include "module.H"
#include "kind.H"

struct kindchecker_state
{
    const Module& mod;

    int next_kvar_index = 1;

    kind_var fresh_named_kind_var(const std::string& s) {return make_kind_var(s,next_kvar_index++);}
    kind_var fresh_kind_var() {return fresh_named_kind_var("k");}

    // These three together form the "type context"
    std::map<std::string,kind> type_con_to_kind;
    std::vector<std::map<Haskell::TypeVar,kind>> type_var_to_kind;
    k_substitution_t kind_var_to_kind;
    // We also need the kinds for type classes and type constructors of previous groups.

    void bind_type_var(const Haskell::TypeVar& tv, const kind& k);
    void push_type_var_scope();
    void pop_type_var_scope();
    bool type_var_in_scope(const Haskell::TypeVar& tv) const;

    void add_substitution(const KindVar& kv, const kind& k);
    void add_substitution(const k_substitution_t& s);

    kind apply_substitution(const kind& k) const;

    kind kind_for_type_con(const std::string&) const;
    kind kind_for_type_var(const Haskell::TypeVar&) const;
    void add_type_con_of_kind(const std::string& name, const kind& k);

    // Kind check a group.
    // Doesn't GHC first "guess" the kinds, and then "check" them?
    bool unify(const kind& k1, const kind& k2);

    void kind_check_type_of_kind(const Haskell::Type& t, const kind& k);
    kind kind_check_type_var(const Haskell::TypeVar& tv);
    kind kind_check_type_con(const std::string&);
    kind kind_check_type(const Haskell::Type& t);
    void kind_check_type_class(const Haskell::ClassDecl& class_decl);
    void kind_check_type_synonym(const Haskell::TypeSynonymDecl& type_syn_decl);
    void kind_check_data_type(const Haskell::DataOrNewtypeDecl& data_decl);
    void kind_check_constraint(const Haskell::Type& constraint);
    void kind_check_context(const Haskell::Context& context);
    void kind_check_constructor(const Haskell::Constructor& constructor, const Haskell::Type& data_type);
    std::map<std::string, std::pair<int,kind>> infer_kinds(const std::vector<expression_ref>& type_decl_group);

    kindchecker_state(const Module& m)
        :mod(m)
    {
        type_var_to_kind.push_back({});
    }
};

#endif
