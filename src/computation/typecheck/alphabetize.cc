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

TypeVar alphabetized_type_var(int i)
{
    auto s = alphabetized_type_var_name(i);
    auto v = TypeVar({noloc,s});
    return v;
}

Type alphabetize_type(const Type& type, map<TypeVar,TypeVar>& s, int& index);

Context alphabetize(Context context, map<TypeVar,TypeVar>& s, int& index)
{
    for(auto& constraint: context.constraints)
        constraint = alphabetize_type(constraint, s, index);
    return context;
}

Type alphabetize_type(const Type& type, map<TypeVar,TypeVar>& s, int& index)
{
    // Lets just assume that there is no shadowing.

    if (auto tv = type.to<TypeVar>())
    {
        auto rec = s.find(*tv);
        if (rec == s.end())
        {
            rec = s.insert({*tv, alphabetized_type_var(index++)}).first;
        }
        return rec->second;
    }
    else if (type.is_a<TypeCon>())
        return type;
    else if (type.is_a<ForallType>())
    {
        auto forall = type.as_<ForallType>();

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
    else if (type.is_a<TypeApp>())
    {
        auto app = type.as_<TypeApp>();
        app.head = alphabetize_type(app.head, s, index);
        app.arg  = alphabetize_type(app.arg , s, index);
        return app;
    }
    else if (auto c = type.to<ConstrainedType>())
    {
        auto C = *c;
        C.type = alphabetize_type(C.type, s, index);
        C.context = alphabetize(C.context, s, index);
        return C;
    }
    else if (auto sl = type.to<StrictLazyType>())
    {
        auto SL = *sl;
        SL.type = alphabetize_type(SL.type, s, index);
        return SL;
    }
    else
        std::abort();
}

Type alphabetize_type(const Type& type)
{
    map<TypeVar, TypeVar> s;
    int index = 0;
    return alphabetize_type(type, s, index);
}

