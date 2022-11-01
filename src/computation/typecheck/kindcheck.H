#ifndef KINDCHECK_H
#define KINDCHECK_H

#include <map>
#include <string>

#include "types.H"
#include "module.H"
#include "kind.H"
#include "env.H"

struct kindchecker_state
{
    int next_kvar_index = 1;

    KindVar fresh_named_kind_var(const std::string& s) {return KindVar(s,next_kvar_index++);}
    KindVar fresh_kind_var() {return fresh_named_kind_var("k");}

    // These three together form the "type context"
    type_con_env type_con_to_kind;
    std::vector<std::map<Hs::TypeVar,Hs::Kind>> type_var_to_kind;
    k_substitution_t kind_var_to_kind;
    // We also need the kinds for type classes and type constructors of previous groups.

    void bind_type_var(const Haskell::TypeVar& tv, const Hs::Kind& k);
    void push_type_var_scope();
    void pop_type_var_scope();
    bool type_var_in_scope(const Haskell::TypeVar& tv) const;

    void add_substitution(const KindVar& kv, const Hs::Kind& k);
    void add_substitution(const k_substitution_t& s);

    Hs::Kind apply_substitution(const Hs::Kind& k) const;

    Hs::Kind kind_for_type_con(const std::string&) const;
    Hs::Kind kind_for_type_var(const Haskell::TypeVar&) const;
    void add_type_con_of_kind(const std::string& name, const Hs::Kind& k, int arity);

    // Kind check a group.
    // Doesn't GHC first "guess" the kinds, and then "check" them?
    bool unify(const Hs::Kind& k1, const Hs::Kind& k2);

    void kind_check_type_of_kind(Haskell::Type& t, const Hs::Kind& k);
    Hs::Kind kind_check_type_var(const Haskell::TypeVar& tv);
    Hs::Kind kind_check_type_con(const std::string&);
    Hs::Kind kind_check_type(Haskell::Type& t);
    void kind_check_constraint(Haskell::Type& constraint);
    void kind_check_context(Haskell::Context& context);

    Hs::Type zonk_kind_for_type(const Hs::Type& t);
    Hs::Kind kind_for_type(const Hs::Type& t);
    std::vector<Hs::TypeVar> unbound_free_type_variables(const Hs::Type& t);

    // class
    Hs::Type kind_and_type_check_type_(const Haskell::Type& type, const Hs::Kind& k);
    Hs::Type kind_and_type_check_type(const Haskell::Type& type);
    Hs::Type kind_and_type_check_constraint(const Haskell::Type& type);
    void kind_check_type_class(const Haskell::ClassDecl& class_decl);

    // data or newtype
    constr_env type_check_data_type(FreshVarSource& fresh_vars, const Haskell::DataOrNewtypeDecl& data_decl);
    void kind_check_data_type(Haskell::DataOrNewtypeDecl& data_decl);
    DataConInfo type_check_constructor(const Haskell::ConstructorDecl& constructor);
    void kind_check_constructor(const Haskell::ConstructorDecl& constructor, const Haskell::Type& data_type);

    // type synonym
    void kind_check_type_synonym(Haskell::TypeSynonymDecl& type_syn_decl);

    type_con_env infer_kinds(const std::vector<expression_ref>& type_decl_group);

    kindchecker_state(const type_con_env& tce)
        :type_con_to_kind(tce)
    {
        type_var_to_kind.push_back({});
    }
};

#endif
