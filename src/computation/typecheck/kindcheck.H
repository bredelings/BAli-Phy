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
    TypeConEnv type_con_to_kind;
    std::vector<std::map<TypeVar,Kind>> type_var_to_kind;
    k_substitution_t kind_var_to_kind;
    // We also need the kinds for type classes and type constructors of previous groups.

    void bind_type_var(const TypeVar& tv, const Hs::Kind& k);
    void push_type_var_scope();
    void pop_type_var_scope();
    bool type_var_in_scope(const TypeVar& tv) const;

    void add_substitution(const KindVar& kv, const Kind& k);
    void add_substitution(const k_substitution_t& s);

    Kind apply_substitution(const Kind& k) const;

    Kind kind_for_type_con(const std::string&) const;
    Kind kind_for_type_var(const TypeVar&) const;
    void add_type_con_of_kind(const std::string& name, const Kind& k, int arity);

    // Kind check a group.
    // Doesn't GHC first "guess" the kinds, and then "check" them?
    bool unify(const Kind& k1, const Kind& k2);

    Type kind_check_type_of_kind(const Type& t, const Kind& k);
    Kind kind_check_type_var(const TypeVar& tv);
    Kind kind_check_type_con(const std::string&);
    std::tuple<Type,Kind> kind_check_type(const Type& t);
    Type kind_check_constraint(const Type& constraint);
    Context kind_check_context(const Context& context);

    Type zonk_kind_for_type(const Type& t);
    Kind kind_for_type(const Type& t);
    std::vector<TypeVar> unbound_free_type_variables(const Type& t);

    // class
    Type kind_and_type_check_type_(const Type& type, const Hs::Kind& k);
    Type kind_and_type_check_type(const Type& type);
    Type kind_and_type_check_constraint(const Type& type);
    void kind_check_type_class(const Hs::ClassDecl& class_decl);

    // data or newtype
    DataConEnv type_check_data_type(FreshVarSource& fresh_vars, const Hs::DataOrNewtypeDecl& data_decl);
    void kind_check_data_type(Hs::DataOrNewtypeDecl& data_decl);
    DataConInfo type_check_constructor(const Hs::ConstructorDecl& constructor);
    void kind_check_constructor(const Hs::ConstructorDecl& constructor, const Type& data_type);

    // type synonym
    void kind_check_type_synonym(Hs::TypeSynonymDecl& type_syn_decl);

    TypeConEnv infer_kinds(const std::vector<expression_ref>& type_decl_group);

    kindchecker_state(const TypeConEnv& tce)
        :type_con_to_kind(tce)
    {
        type_var_to_kind.push_back({});
    }
};

#endif
