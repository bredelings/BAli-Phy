#ifndef KIND_H
#define KIND_H

#include "computation/object.H"
#include <string>
#include <optional>
#include <map>
#include "computation/haskell/haskell.H"

struct KindStar: public Object
{
    virtual bool is_kstar() const {return true;}
    KindStar* clone()  const { return new KindStar(); }
    bool operator==(const KindStar&) const;
    bool operator==(const Object&) const;
    std::string print() const { return "*"; }
};

object_ptr<KindStar> kind_star();

struct KindConstraint: public Object
{
    virtual bool is_kconstraint() const {return true;}
    KindConstraint* clone()  const { return new KindConstraint(); }
    bool operator==(const KindConstraint&) const;
    bool operator==(const Object&) const;
    std::string print() const { return "Constraint"; }
};

object_ptr<KindConstraint> kind_constraint();

struct KindArrow: public Object
{
    Hs::Kind arg_kind;
    Hs::Kind result_kind;
    KindArrow* clone()  const { return new KindArrow(*this); }
    bool operator==(const KindArrow&) const;
    bool operator==(const Object&) const;
    std::string print() const;

    KindArrow(const Hs::Kind& k1, const Hs::Kind& k2):arg_kind(k1),result_kind(k2) {}
};

object_ptr<KindArrow> kind_arrow(const Hs::Kind& k1, const Hs::Kind& k2);

struct KindVar: public Object
{
    std::string name;
    std::optional<int> index;
    KindVar* clone()  const { return new KindVar(*this); }
    std::string print() const;
    bool operator==(const Object&) const;
    bool operator==(const KindVar&) const;
    bool operator<(const KindVar&) const;

    KindVar(const std::string& s, int i): name(s), index(i) {}
};

Hs::Kind function_kind(const std::vector<Hs::Kind>& arg_kinds, const Hs::Kind result_kind);

object_ptr<KindVar> kind_var(const std::string& s, int i);

typedef std::map<KindVar,Hs::Kind> k_substitution_t;

Hs::Kind apply_subst(const k_substitution_t& s, const Hs::Kind& k);

k_substitution_t compose(const k_substitution_t& s1, const k_substitution_t s2);

bool occurs_check(const KindVar& kv, const Hs::Kind& k);

std::optional<k_substitution_t> kunify(const Hs::Kind& k1, const Hs::Kind& k2);

Hs::Kind make_n_args_kind(int n);

Hs::Kind make_n_args_constraint_kind(int n);

Hs::Kind replace_kvar_with_star(const Hs::Kind& k);

#endif /*KIND_H*/
