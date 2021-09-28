#include "kind.H"

using std::string;

kind_star make_kind_star() {return new KindStar();}

kind_constraint make_kind_constraint() {return new KindConstraint();}

string KindArrow::print() const {
    string s1 = k1->print();
    string s2 = k2->print();
    if (k1->is_karrow())
        s1 = "("+s1+")";
    return s1+" -> "+s2;
}

kind_arrow make_kind_arrow(const kind& k1, const kind& k2) {return new KindArrow{k1,k2};}

std::string KindVar::print() const {
    string s = name;
    if (index)
        s += "_" + std::to_string(*index);
    return s;
}

kind_var make_kind_var(const std::string& s, int i) {return new KindVar(s,i);}

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


kind apply_subst(const k_substitution_t& s, const kind& k)
{
    if (k->is_kvar())
    {
        auto& kv = dynamic_cast<KindVar&>(*k);
        auto k2 = s.find(kv);
        if (k2 != s.end())
            return apply_subst(s, k2->second);
        else
            return k;
    }
    else if (k->is_karrow())
    {
        auto& a = dynamic_cast<KindArrow&>(*k);
        auto k1 = apply_subst(s,a.k1);
        auto k2 = apply_subst(s,a.k2);
        if (k1 != a.k1 or k2 != a.k2)
            return make_kind_arrow(k1,k2);
        else
            return k;
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

bool occurs_check(const KindVar& kv, const kind& k)
{
    if (k->is_kvar())
        return kv == dynamic_cast<const KindVar&>(*k);
    else if (k->is_karrow())
    {
        auto& a = dynamic_cast<const KindArrow&>(*k);
        return occurs_check(kv, a.k1) or occurs_check(kv, a.k2);
    }
    else
        return false;
}

std::optional<k_substitution_t> unify(const kind& k1, const kind& k2)
{
    if (k1->is_karrow() and k2->is_karrow())
    {
        auto& A = dynamic_cast<const KindArrow&>(*k1);
        auto& B = dynamic_cast<const KindArrow&>(*k2);
        auto s1 = unify(A.k1, B.k1);
        if (not s1) return {};

        auto s2 = unify(apply_subst(*s1, A.k2), apply_subst(*s1, A.k2));
        if (not s2) return {};

        return compose(*s1,*s2);
    }
    else if (k1->is_kvar())
    {
        auto& kv1 = dynamic_cast<const KindVar&>(*k1);

        if (k2->is_kvar() and kv1 == dynamic_cast<const KindVar&>(*k2))
            return {{}};
        if (occurs_check(kv1,k2)) return {};

        k_substitution_t s;
        s.insert({kv1,k2});
        return s;
    }
    else if (k2->is_kvar())
    {
        auto& kv2 = dynamic_cast<const KindVar&>(*k2);

        if (k1->is_kvar() and kv2 == dynamic_cast<const KindVar&>(*k1))
            return {{}};
        if (occurs_check(kv2,k1)) return {};

        k_substitution_t s;
        s.insert({kv2,k1});
        return s;
    }
    else if (k1->is_kstar() and k2->is_kstar())
        return {{}};
    else if (k1->is_kconstraint() and k2->is_kconstraint())
        return {{}};
    else
        return {};
}

kind make_n_args_kind(int n)
{
    auto star = make_kind_star();
    kind k = star;
    for(int i=0;i<n;i++)
        k = make_kind_arrow(star,k);
    return k;
}

kind replace_kvar_with_star(const kind& k)
{
    if (k->is_kvar())
    {
        return make_kind_star();
    }
    else if (k->is_karrow())
    {
        auto& a = dynamic_cast<KindArrow&>(*k);
        auto k1 = replace_kvar_with_star( a.k1 );
        auto k2 = replace_kvar_with_star( a.k2 );
        if (k1 != a.k1 or k2 != a.k2)
            return make_kind_arrow(k1,k2);
        else
            return k;
    }
    else
        return k;

    std::abort();
}

