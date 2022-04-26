#ifndef KIND_H
#define KIND_H

#include "computation/object.H"
#include <string>
#include <optional>
#include <map>

struct Kind: public Object
{
    virtual bool is_kstar() const {return false;}
    virtual bool is_kconstraint() const {return false;}
    virtual bool is_karrow() const {return false;}
    virtual bool is_kvar() const {return false;}
};

typedef object_ptr<Kind> kind;
typedef object_ptr<const Kind> const_kind;

struct KindStar: public Kind
{
    virtual bool is_kstar() const {return true;}
    KindStar* clone()  const { return new KindStar(); }
    std::string print() const { return "*"; }
};

typedef object_ptr<KindStar> kind_star;
kind_star make_kind_star();

struct KindConstraint: public Kind
{
    virtual bool is_kconstraint() const {return true;}
    KindConstraint* clone()  const { return new KindConstraint(); }
    std::string print() const { return "Constraint"; }
};

typedef object_ptr<KindConstraint> kind_constraint;
kind_constraint make_kind_constraint();

struct KindArrow: public Kind
{
    kind k1;
    kind k2;
    virtual bool is_karrow() const {return true;}
    KindArrow* clone()  const { return new KindArrow(*this); }
    KindArrow(const kind& _k1, const kind& _k2):k1(_k1),k2(_k2) {}
    std::string print() const;
};

typedef object_ptr<KindArrow> kind_arrow;
kind_arrow make_kind_arrow(const kind& k1, const kind& k2);

struct KindVar: public Kind
{
    std::string name;
    std::optional<int> index;
    virtual bool is_kvar() const {return true;}
    KindVar* clone()  const { return new KindVar(*this); }
    std::string print() const;
    bool operator==(const Object&) const;
    bool operator==(const KindVar&) const;
    bool operator<(const KindVar&) const;

    KindVar(const std::string& s, int i): name(s), index(i) {}
};

typedef object_ptr<KindVar> kind_var;
kind_var make_kind_var(const std::string& s, int i);

typedef std::map<KindVar,kind> k_substitution_t;

kind apply_subst(const k_substitution_t& s, const kind& k);

k_substitution_t compose(const k_substitution_t& s1, const k_substitution_t s2);

bool occurs_check(const KindVar& kv, const kind& k);

std::optional<k_substitution_t> unify(const kind& k1, const kind& k2);

kind make_n_args_kind(int n);

kind replace_kvar_with_star(const kind& k);

#endif /*KIND_H*/
