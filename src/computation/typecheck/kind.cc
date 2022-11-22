#include "kind.H"

#include <range/v3/all.hpp>

using std::string;

namespace views = ranges::views;

object_ptr<KindStar> kind_star() {return new KindStar();}

object_ptr<KindConstraint> kind_constraint() {return new KindConstraint();}

string KindArrow::print() const
{
    string s1 = arg_kind.print();
    string s2 = result_kind.print();
    if (arg_kind.is_a<KindArrow>())
        s1 = "("+s1+")";
    return s1+" -> "+s2;
}

object_ptr<KindArrow> kind_arrow(const Hs::Kind& k1, const Hs::Kind& k2) {return new KindArrow{k1,k2};}

std::string KindVar::print() const {
    string s = name;
    if (index)
        s += "_" + std::to_string(*index);
    return s;
}

object_ptr<KindVar> kind_var(const std::string& s, int i) {return new KindVar(s,i);}

bool KindStar::operator==(const KindStar&) const
{
    return true;
}

bool KindStar::operator==(const Object& o) const
{
    auto K = dynamic_cast<const KindStar*>(&o);
    if (not K)
        return false;

    return (*this) == *K;
}

bool KindConstraint::operator==(const KindConstraint&) const
{
    return true;
}

bool KindConstraint::operator==(const Object& o) const
{
    auto K = dynamic_cast<const KindConstraint*>(&o);
    if (not K)
        return false;

    return (*this) == *K;
}

bool KindArrow::operator==(const KindArrow& K2) const
{
    return arg_kind == K2.arg_kind and result_kind == K2.result_kind; 
}

bool KindArrow::operator==(const Object& o) const
{
    auto K = dynamic_cast<const KindArrow*>(&o);
    if (not K)
        return false;

    return (*this) == *K;
}

bool KindVar::operator==(const Object& o) const
{
    auto K = dynamic_cast<const KindVar*>(&o);
    if (not K)
        return false;

    return (*this) == *K;
}

bool KindVar::operator==(const KindVar& k) const
{
    return name == k.name and index == k.index;
}

bool KindVar::operator<(const KindVar& k) const
{
    if (index < k.index) return true;
    if (index > k.index) return false;

    int cmp = name.compare(k.name);

    return (cmp < 0);
}


Hs::Kind apply_subst(const k_substitution_t& s, const Hs::Kind& k)
{
    if (auto kv = k.to<KindVar>())
    {
        auto k2 = s.find( *kv );
        if (k2 != s.end())
            return apply_subst(s, k2->second);
        else
            return k;
    }
    else if (auto a = k.to<KindArrow>())
    {
        auto arg_kind    = apply_subst(s, a->arg_kind);
        auto result_kind = apply_subst(s, a->result_kind);
        return kind_arrow(arg_kind,result_kind);
    }
    else
        return k;
}

// This should yield a substitution that is equivalent to apply FIRST s1 and THEN s2,
k_substitution_t compose(const k_substitution_t& s1, const k_substitution_t s2)
{
    if (s2.empty()) return s1;

    auto s3 = s2;
    for(auto& [kv,k]: s1)
    {
        if (s3.count(kv))
            s3.erase(kv);
        s3.insert({kv,apply_subst(s2,k)});
    }

    return s3;
}

bool occurs_check(const KindVar& kv, const Hs::Kind& k)
{
    if (auto kv2 = k.to<KindVar>())
        return kv == *kv2;
    else if (auto a = k.to<KindArrow>())
    {
        return occurs_check(kv, a->arg_kind) or occurs_check(kv, a->result_kind);
    }
    else
        return false;
}

std::optional<k_substitution_t> kunify(const Hs::Kind& k1, const Hs::Kind& k2)
{
    if (k1.is_a<KindArrow>() and k2.is_a<KindArrow>())
    {
        auto A = k1.to<KindArrow>();
        auto B = k2.to<KindArrow>();
        auto s1 = kunify(A->arg_kind, B->arg_kind);
        if (not s1) return {};

        auto s2 = kunify(apply_subst(*s1, A->result_kind), apply_subst(*s1, B->result_kind));
        if (not s2) return {};

        return compose(*s1,*s2);
    }
    else if (auto kv1 = k1.to<KindVar>())
    {
        if (auto kv2 = k2.to<KindVar>(); kv2 and *kv1 == *kv2)
            return {{}};
        if (occurs_check(*kv1, k2)) return {};

        k_substitution_t s;
        s.insert({*kv1,k2});
        return s;
    }
    else if (auto kv2 = k2.to<KindVar>())
    {
        // k1 can't be a KindVar on this branch.
        if (occurs_check(*kv2, k1)) return {};

        k_substitution_t s;
        s.insert({*kv2,k1});
        return s;
    }
    else if (k1.is_a<KindStar>() and k2.is_a<KindStar>())
        return {{}};
    else if (k1.is_a<KindConstraint>() and k2.is_a<KindConstraint>())
        return {{}};
    else
        return {};
}

Hs::Kind function_kind(const std::vector<Hs::Kind>& arg_kinds, const Hs::Kind result_kind)
{
    auto kind = result_kind;
    for(auto& arg_kind: arg_kinds | views::reverse)
        kind = kind_arrow(arg_kind, kind);
    return kind;
}

Hs::Kind make_n_args_kind(int n)
{
    Hs::Kind star = kind_star();
    Hs::Kind k = star;
    for(int i=0;i<n;i++)
        k = kind_arrow(star,k);
    return k;
}

Hs::Kind make_n_args_constraint_kind(int n)
{
    Hs::Kind star = kind_star();
    Hs::Kind k = kind_constraint();
    for(int i=0;i<n;i++)
        k = kind_arrow(star,k);
    return k;
}

Hs::Kind replace_kvar_with_star(const Hs::Kind& k)
{
    if (k.is_a<KindVar>())
    {
        return kind_star();
    }
    else if (auto a = k.to<KindArrow>())
    {
        auto arg_kind    = replace_kvar_with_star( a->arg_kind );
        auto result_kind = replace_kvar_with_star( a->result_kind );
        return kind_arrow( arg_kind, result_kind );
    }
    else
        return k;

    std::abort();
}

