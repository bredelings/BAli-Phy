#include "tidy.H"
#include <tuple>
#include "util/string/join.H"
#include "haskell/ids.H"

using std::vector;
using std::string;
using std::tuple;

bool TidyState::is_taken(const string& name) const
{
    return taken.count(name);
}

tuple<string,int> split_suffix(const string& name)
{
    int i = name.size()-1;
    while(i >=0 and name[i] >= '0' and name[i] <= '9')
        i--;
    if (i > 0 and name[i] == '_')
    {
        return {name.substr(0,i), convertTo<int>(name.substr(i+1))};
    }
    else
        return {name,-1};
}

string TidyState::tidy_name(const string& name)
{
    // 1. If we've seen this name before, then we know the result
    auto iter = tidy_names.find(name);
    if (iter != tidy_names.end())
        return iter->second;

    // 2. If not, then split into prefix and suffix.
    auto [prefix,suffix] = split_suffix(name);
    assert(prefix.size());
    
    // 3. If we haven't seen this prefix before, then next suffix to try is 0 (which means no suffix)
    auto iter_index = next_index.find(prefix);
    if (iter_index == next_index.end())
    {
        next_index.insert({prefix,0});
        iter_index = next_index.find(prefix);
    }
    assert(iter_index != next_index.end());

    // 4. Create the next name to try
    string tname = prefix;
    if (iter_index->second > 0)
        tname += std::to_string(iter_index->second);

    while(is_taken(tname))
    {
        tname = prefix + std::to_string(iter_index->second);
        iter_index->second++;
    }
        
    taken.insert(tname);
    tidy_names.insert({name,tname});
    return tname;
}


std::string TidyState::print_paren(Type t, bool parenthesize_type_app)
{
    t = follow_meta_type_var(t);

    auto s = print(t);

    if (t.is_a<TypeCon>() or t.is_a<MetaTypeVar>() or t.is_a<TypeVar>() or is_tuple_type(t) or is_list_type(t))
        return s;
    else if (not parenthesize_type_app and t.is_a<TypeApp>() and not is_type_op(t))
        return s;
    else
        return "(" + s + ")";
}

// Alternatively, we could make something with options, and do
// ppr<<app.head<<" "<<app.arg.
// This would allow us to record state on the ppr object, like:
// struct PrettyPrinter
// {
//     bool debug = true;
// };

// However, GHC does have some functions like pprAsThing to print
// something in a particular fashion.
// We could handle this by constructing temprary objects like
// ppr<<"this is a "<<Thing(obj), but...

std::string TidyState::print(const MetaTypeVar& mtv)
{
    if (auto t = mtv.filled())
        return print(*t);

    auto name = unloc(mtv.name);
    if (mtv.index)
        name += "_"+std::to_string(*mtv.index);
    return tidy_name(name);
}

std::string TidyState::print(const TypeVar& tv)
{
    auto name = unloc(tv.name);
    if (tv.index)
        name += "_"+std::to_string(*tv.index);
    return tidy_name(name);
}

std::string TidyState::print(const TypeCon& tc)
{
    return get_unqualified_name(unloc(tc.name));
}

std::string TidyState::print(const TypeApp& app)
{
    ignore_top_foralls = false;

    if (auto type_op = is_type_op(app))
    {
        auto [tycon, arg1, arg2] = *type_op;

        if (is_function_type(app) and is_function_type(arg2))
            return print_paren(arg1, false) + " " + tycon.print() + " " + print(arg2);
        else
            return print_paren(arg1, false) + " " + tycon.print() + " " + print_paren(arg2, false);
    }
    else if (auto element_type = is_list_type(app))
        return "[" + print(*element_type) +"]";
    else if (auto element_types = is_tuple_type(app))
    {
        vector<string> parts;
        for(auto& element_type: *element_types)
            parts.push_back(print(element_type));
        return "(" + join(parts,", ") +")";
    }

    return print(app.head) + " " + print_paren(app.arg, true);
}

string TidyState::print(const ForallType& forall)
{
    vector<string> binders;
    for(auto& type_var_binder: forall.type_var_binders)
    {
        binders.push_back(print(type_var_binder));
//        binders.push_back(type_var_binder.print_with_kind());
    }
    if (ignore_top_foralls)
        return print(forall.type);
    else
        return "forall "+join(binders," ")+". "+print(forall.type);
}

string TidyState::print(const Context& c)
{
    bool prev_ignore = ignore_top_foralls;

    vector<string> cs;
    for(auto& constraint: c)
        cs.push_back(print(constraint));

    ignore_top_foralls = prev_ignore;

    string result = join(cs,", ");
    if (cs.size() == 1 and not is_type_op(c[0]))
        return result;
    else
        return "(" + result + ")";
}

string TidyState::print(const ConstrainedType& ct)
{
    return print(ct.context) + " => " + print(ct.type);
}

std::string TidyState::print(const Type& type)
{
    if (type.empty()) return "NOTYPE";

    if (auto mtv = type.to<MetaTypeVar>())
        return print(*mtv);
    else if (auto tv = type.to<TypeVar>())
        return print(*tv);
    else if (auto tc = type.to<TypeCon>())
        return print(*tc);
    else if (auto app = type.to<TypeApp>())
        return print(*app);
    else if (auto ct = type.to<ConstrainedType>())
        return print(*ct);
    else if (auto forall = type.to<ForallType>())
        return print(*forall);

    std::abort();
}
