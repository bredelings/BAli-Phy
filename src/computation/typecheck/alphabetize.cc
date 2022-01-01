#include "alphabetize.H"

using std::string;
using std::map;

string alphabetized_type_var_name(int i)
{
    string s;
    while (true)
    {
        s.push_back( char('a'+(i%26)) );
        if (i < 26) break;
        i /= 26;
    }
    return s;
}

Haskell::TypeVar alphabetized_type_var(int i)
{
    auto s = alphabetized_type_var_name(i);
    auto v = Haskell::TypeVar({noloc,s});
    v.index = i;
    return v;
}

expression_ref alphabetize_type(const expression_ref& type, map<Haskell::TypeVar,Haskell::TypeVar>& s, int& index);

Hs::Context alphabetize(Hs::Context context, map<Haskell::TypeVar,Haskell::TypeVar>& s, int& index)
{
    for(auto& constraint: context.constraints)
        constraint = alphabetize_type(constraint, s, index);
    return context;
}

expression_ref alphabetize_type(const expression_ref& type, map<Haskell::TypeVar,Haskell::TypeVar>& s, int& index)
{
    // Lets just assume that there is no shadowing.

    if (auto tv = type.to<Haskell::TypeVar>())
    {
        auto rec = s.find(*tv);
        if (rec == s.end())
        {
            rec = s.insert({*tv, alphabetized_type_var(index++)}).first;
        }
        return expression_ref(rec->second);
    }
    else if (type.is_a<Haskell::TypeCon>())
        return type;
    else if (type.is_a<Haskell::ForallType>())
    {
        auto forall = type.as_<Haskell::ForallType>();

        // 2a. Ensure that we see each of the type var binders in the order they are used.
        for(auto& tv: forall.type_var_binders)
        {
            alphabetize_type(tv, s, index);
            tv = s.at(tv);
        }

        // 2b. Alphabetize the type body.
        forall.type = alphabetize_type(forall.type, s, index);

        // 2c. Return the type
        return forall;
    }
    else if (type.is_a<Haskell::TypeApp>())
    {
        auto app = type.as_<Haskell::TypeApp>();
        app.head = alphabetize_type(app.head, s, index);
        app.arg  = alphabetize_type(app.arg , s, index);
        return app;
    }
    else if (auto l = type.to<Hs::ListType>())
    {
        auto L = *l;
        L.element_type = alphabetize_type(L.element_type, s, index);
        return L;
    }
    else if (auto tup = type.to<Hs::TupleType>())
    {
        auto T = *tup;
        for(auto& type: T.element_types)
            type = alphabetize_type(type, s, index);
        return T;;
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        auto C = *c;
        C.type = alphabetize_type(C.type, s, index);
        C.context = alphabetize(C.context, s, index);
        return C;
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        auto SL = *sl;
        SL.type = alphabetize_type(SL.type, s, index);
        return SL;
    }
    else
        std::abort();
}

Hs::Type alphabetize_type(const Hs::Type& type)
{
    map<Haskell::TypeVar, Haskell::TypeVar> s;
    int index = 0;
    return alphabetize_type(type, s, index);
}

