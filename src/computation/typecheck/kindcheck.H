#ifndef KINDCHECK_H
#define KINDCHECK_H

#include <map>
#include <string>

#include "types.H"
#include "computation/module.H"
#include "kind.H"
#include "env.H"

class TypeChecker;

struct kindchecker_state
{
    int next_kvar_index = 1;

    const Module& mod;
    TypeChecker& type_checker;

    KindVar fresh_named_kind_var(const std::string& s) {
	auto kv = TypeVar(s, kind_kind());
	kv.index = next_kvar_index++;
	return kv;
    }
    KindVar fresh_kind_var() {return fresh_named_kind_var("k");}

    // These three together form the "type context"
    TypeConEnv type_con_to_kind;
    std::vector<std::map<Hs::LTypeVar,Kind>> type_var_to_kind;
    k_substitution_t kind_var_to_kind;
    // We also need the kinds for type classes and type constructors of previous groups.

    TypeVar bind_type_var(const Hs::LTypeVar& tv, const Hs::Kind& k);
    void push_type_var_scope();
    void pop_type_var_scope();

    void add_substitution(const KindVar& kv, const Kind& k);
    void add_substitution(const k_substitution_t& s);

    Kind apply_substitution(const Kind& k) const;

    Kind kind_for_type_con(const std::string&) const;
    Kind kind_for_type_var(const Hs::LTypeVar&) const;
    void add_type_con_of_kind(const std::string& name, const Kind& k, int arity);

    // Kind check a group.
    // Doesn't GHC first "guess" the kinds, and then "check" them?
    bool unify(const Kind& k1, const Kind& k2);

    Type kind_check_type_of_kind(const Hs::LType& t, const Kind& k);
    Kind kind_check_type_con(const std::string&);
    std::tuple<Type,Kind> kind_check_type(const Hs::LType& t);
    Type kind_check_constraint(const Hs::LType& constraint);
    Context kind_check_context(const Hs::Context& context);

    Type zonk_kind_for_type(const Type& t);

    Type kind_and_type_check_type(const Hs::LType& type);
    Type kind_and_type_check_constraint(const Hs::LType& type);

    // class
    void kind_check_type_class(const Hs::ClassDecl& class_decl);

    // data or newtype
    void kind_check_data_type(Hs::DataOrNewtypeDecl& data_decl);
    void kind_check_constructor(const Hs::ConstructorDecl& constructor);

    // type synonym
    void kind_check_type_synonym(Hs::TypeSynonymDecl& type_syn_decl);

    TypeConEnv infer_kinds(const std::vector<expression_ref>& type_decl_group);

    kindchecker_state(TypeChecker&);
};

#endif
