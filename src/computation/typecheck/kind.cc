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

// Follow solved kind metavariables and default any remaining inference variables to Type.
// Rigid kind variables are preserved so that defaulting does not turn them into inference variables.
Hs::Kind default_kind(const Hs::Kind& k_)
{
    auto k = follow_meta_type_var(k_);

    if (auto mtv = k.to<MetaTypeVar>())
    {
        assert(mtv->kind == kind_kind());
        mtv->fill(kind_type());
        return kind_type();
    }
    else if (k.is_a<TypeVar>() or k.is_a<TypeCon>())
    {
        return k;
    }
    else if (auto a = k.to<TypeApp>())
    {
        auto arg_kind    = default_kind( a->head );
        auto result_kind = default_kind( a->arg );
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
