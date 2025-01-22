#include "kind.H"

#include <range/v3/all.hpp>

using std::string;
using std::optional;
using std::pair;
using std::vector;

namespace views = ranges::views;

// Note: Attempting to add kinds on TypeCon can cause loops!
// * make_arrow_type( ) <-> kind_arrow( )
// The arrow type has kind Type -> Type -> Type
// The arrow kind has sort Kind -> Kind -> Kind
// We could make all the sorts be empty.
// We could say that the Type kind has sort Type.
//   But how would we allocate this?  The object would have to be self-referential.

TypeCon kind_kind() {return TypeCon("Kind");}

TypeCon kind_type() {return TypeCon("Type");}

TypeCon kind_constraint() {return TypeCon("Constraint");}

Kind kind_arrow(const Hs::Kind& k1, const Hs::Kind& k2) {return make_arrow_type(k1,k2);}

bool is_kind_type(const Kind& k)
{
    if (auto tycon = k.to<TypeCon>())
	return *tycon == kind_type();
    else
	return false;
}

bool is_kind_constraint(const Kind& k)
{
    if (auto tycon = k.to<TypeCon>())
	return *tycon == kind_constraint();
    else
	return false;
}

TypeVar kind_var(const std::string& s, int i)
{
    TypeVar kv(s, kind_kind());
    kv.index = i;
    return kv;
}

// This should yield a substitution that is equivalent to apply FIRST s1 and THEN s2,
k_substitution_t compose(const k_substitution_t& s1, const k_substitution_t s2)
{
    if (s2.empty()) return s1;

    auto s3 = s2;
    for(auto& [kv,k]: s1)
    {
        if (s3.count(kv))
            s3 = s3.erase(kv);
        s3 = s3.insert({kv,apply_subst(s2,k)});
    }

    return s3;
}

bool occurs_check(const KindVar& kv, const Hs::Kind& k)
{
    if (auto kv2 = k.to<KindVar>())
        return kv == *kv2;
    else if (k.to<TypeCon>())
        return false;
    else if (auto app = k.to<TypeApp>())
	return occurs_check(kv, app->head) or occurs_check(kv, app->arg);
    else
	throw myexception()<<"Kind's should only have TypeVars, TypeCons, and TypeApps at the moment";
}

std::optional<k_substitution_t> kunify(const Hs::Kind& k1, const Hs::Kind& k2)
{
    auto k1_app = k1.to<TypeApp>();
    auto k2_app = k2.to<TypeApp>();

    if (k1_app and k2_app)
    {
        auto s1 = kunify(k1_app->head, k2_app->head);
        if (not s1) return {};

        auto s2 = kunify(apply_subst(*s1, k1_app->arg), apply_subst(*s1, k2_app->arg));
        if (not s2) return {};

        return compose(*s1,*s2);
    }
    else if (k1.is_a<TypeCon>() and k2.is_a<TypeCon>())
    {
	if (k1.as_<TypeCon>() == k2.as_<TypeCon>())
	    return {k_substitution_t{}};
	else
	    return {};
    }
    else if (auto kv1 = k1.to<KindVar>())
    {
        if (auto kv2 = k2.to<KindVar>(); kv2 and *kv1 == *kv2)
            return {k_substitution_t{}};
        if (occurs_check(*kv1, k2)) return {};

        k_substitution_t s;
        return s.insert({*kv1,k2});
    }
    else if (auto kv2 = k2.to<KindVar>())
    {
        // k1 can't be a KindVar on this branch.
        if (occurs_check(*kv2, k1)) return {};

        k_substitution_t s;
        return s.insert({*kv2,k1});
    }
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
    Hs::Kind star = kind_type();
    Hs::Kind k = star;
    for(int i=0;i<n;i++)
        k = kind_arrow(star,k);
    return k;
}

Hs::Kind make_n_args_constraint_kind(int n)
{
    Hs::Kind star = kind_type();
    Hs::Kind k = kind_constraint();
    for(int i=0;i<n;i++)
        k = kind_arrow(star,k);
    return k;
}

Hs::Kind replace_kvar_with_star(const Hs::Kind& k)
{
    if (k.is_a<KindVar>())
    {
        return kind_type();
    }
    else if (k.is_a<TypeCon>())
    {
        return k;
    }
    else if (auto a = k.to<TypeApp>())
    {
        auto arg_kind    = replace_kvar_with_star( a->head );
        auto result_kind = replace_kvar_with_star( a->arg );
        return TypeApp(arg_kind, result_kind);
    }
    else
	std::abort();
}

optional<pair<vector<Hs::Kind>,Hs::Kind>> arg_and_result_kinds(int n, const Hs::Kind& kind)
{
    vector<Hs::Kind> arg_kinds;
    auto k = kind;
    for(int i=0;i<n;i++)
    {
        auto a = is_function_type(k);
        if (not a) return {};
	auto [arg_kind, result_kind] = *a;

        arg_kinds.push_back(arg_kind);
        k = result_kind;
    }

    return {{arg_kinds,k}};
}

int num_args_for_kind(const Hs::Kind& kind)
{
    auto k = kind;
    int n = 0;
    while(auto a = is_function_type(k))
    {
        n++;
        k = a->second;
    }

    return n;
}
