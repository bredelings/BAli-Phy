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


std::string tidy_print_paren(TidyState& tidy_state, Type t)
{
    t = follow_meta_type_var(t);

    auto s = tidy_print(tidy_state, t);

    if (t.is_a<TypeCon>() or t.is_a<MetaTypeVar>() or t.is_a<TypeVar>() or is_tuple_type(t) or is_list_type(t))
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

std::string tidy_print(TidyState& tidy_state, const MetaTypeVar& mtv)
{
    if (auto t = mtv.filled())
        return tidy_print(tidy_state, *t);

    auto name = unloc(mtv.name);
    if (mtv.index)
        name += "_"+std::to_string(*mtv.index);
    return tidy_state.tidy_name(name);
}

std::string tidy_print(TidyState& tidy_state, const TypeVar& tv)
{
    auto name = unloc(tv.name);
    if (tv.index)
        name += "_"+std::to_string(*tv.index);
    return tidy_state.tidy_name(name);
}

std::string tidy_print(TidyState&, const TypeCon& tc)
{
    return get_unqualified_name(unloc(tc.name));
}

std::string tidy_print(TidyState& tidy_state, const TypeApp& app)
{
    if (auto type_op = is_type_op(app))
    {
        auto [tycon, arg1, arg2] = *type_op;

        string arg1s = tidy_print(tidy_state, arg1);
        if (is_type_op(arg1)) arg1s = "(" + arg1s + ")";
        string arg2s = tidy_print(tidy_state, arg2);
        if (is_type_op(arg2)) arg2s = "(" + arg2s + ")";

        return arg1s + " " + tidy_print(tidy_state, tycon) + " "+ arg2s;
    }
    else if (auto element_type = is_list_type(app))
        return "[" + tidy_print(tidy_state, *element_type) +"]";
    else if (auto element_types = is_tuple_type(app))
    {
        vector<string> parts;
        for(auto& element_type: *element_types)
            parts.push_back(tidy_print(tidy_state, element_type));
        return "(" + join(parts,", ") +")";
    }

    return tidy_print(tidy_state, app.head) + " " + tidy_print_paren(tidy_state, app.arg);
}

string tidy_print(TidyState& tidy_state, const ForallType& forall)
{
    vector<string> binders;
    for(auto& type_var_binder: forall.type_var_binders)
        binders.push_back(type_var_binder.print_with_kind());
    return "forall "+join(binders," ")+". "+tidy_print(tidy_state, forall.type);
}

string tidy_print(TidyState& tidy_state, const Context& c)
{
    vector<string> cs;
    for(auto& constraint: c.constraints)
        cs.push_back(tidy_print(tidy_state, constraint));

    string result = join(cs,", ");
    if (cs.size() == 1 and not is_type_op(c.constraints[0]))
        return result;
    else
        return "(" + result + ")";
}

string tidy_print(TidyState& tidy_state, const ConstrainedType& ct)
{
    return tidy_print(tidy_state, ct.context) + " => " + tidy_print(tidy_state, ct.type);
}

string tidy_print(TidyState& tidy_state, const StrictType& sl)
{
    return "!" + tidy_print(tidy_state, sl.type);
}

string tidy_print(TidyState& tidy_state, const LazyType& sl)
{
    return "!" + tidy_print(tidy_state, sl.type);
}


std::string tidy_print(TidyState& tidy_state, const Type& type)
{
    if (type.empty()) return "NOTYPE";

    if (auto mtv = type.to<MetaTypeVar>())
        return tidy_print(tidy_state, *mtv);
    else if (auto tv = type.to<TypeVar>())
        return tidy_print(tidy_state, *tv);
    else if (auto tc = type.to<TypeCon>())
        return tidy_print(tidy_state, *tc);
    else if (auto app = type.to<TypeApp>())
        return tidy_print(tidy_state, *app);
    else if (auto ct = type.to<ConstrainedType>())
        return tidy_print(tidy_state, *ct);
    else if (auto forall = type.to<ForallType>())
        return tidy_print(tidy_state, *forall);
    else if (auto s = type.to<StrictType>())
        return tidy_print(tidy_state, *s);
    else if (auto l = type.to<LazyType>())
        return tidy_print(tidy_state, *l);

    std::abort();
}
