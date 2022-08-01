#include "kind.H"

using std::string;

object_ptr<KindStar> kind_star() {return new KindStar();}

object_ptr<KindConstraint> kind_constraint() {return new KindConstraint();}

string KindArrow::print() const
{
    string s1 = k1.print();
    string s2 = k2.print();
    if (k1.is_a<KindArrow>())
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
        auto k1 = apply_subst(s, a->k1);
        auto k2 = apply_subst(s, a->k2);
        return kind_arrow(k1,k2);
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
        return occurs_check(kv, a->k1) or occurs_check(kv, a->k2);
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
        auto s1 = kunify(A->k1, B->k1);
        if (not s1) return {};

        auto s2 = kunify(apply_subst(*s1, A->k2), apply_subst(*s1, B->k2));
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
        auto k1 = replace_kvar_with_star( a->k1 );
        auto k2 = replace_kvar_with_star( a->k2 );
        return kind_arrow(k1,k2);
    }
    else
        return k;

    std::abort();
}

