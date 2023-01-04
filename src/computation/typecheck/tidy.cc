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

std::string print_unqualified(const MetaTypeVar& mtv)
{
    if (auto t = mtv.filled())
        return print_unqualified(*t);

    return unloc(mtv.name);
}

std::string print_unqualified(const TypeCon& tc)
{
    return get_unqualified_name(unloc(tc.name));
}

std::string print_unqualified(const TypeApp& app)
{
    if (auto type_op = is_type_op(app))
    {
        auto [tycon, arg1, arg2] = *type_op;

        string arg1s = print_unqualified(arg1);
        if (is_type_op(arg1)) arg1s = "(" + arg1s + ")";
        string arg2s = print_unqualified(arg2);
        if (is_type_op(arg2)) arg2s = "(" + arg2s + ")";

        return arg1s + " " + print_unqualified(tycon) + " "+ arg2s;
    }
    else if (auto element_type = is_list_type(app))
        return "[" + print_unqualified(*element_type) +"]";
    else if (auto element_types = is_tuple_type(app))
    {
        vector<string> parts;
        for(auto& element_type: *element_types)
            parts.push_back(print_unqualified(element_type));
        return "(" + join(parts,", ") +")";
    }

    return print_unqualified(app.head) + " " + print_unqualified(app.arg);
}

string print_unqualified(const ForallType& forall)
{
    vector<string> binders;
    for(auto& type_var_binder: forall.type_var_binders)
        binders.push_back(type_var_binder.print_with_kind());
    return "forall "+join(binders," ")+". "+print_unqualified(forall.type);
}

string print_unqualified(const Context& c)
{
    vector<string> cs;
    for(auto& constraint: c.constraints)
        cs.push_back(print_unqualified(constraint));

    string result = join(cs,", ");
    if (cs.size() == 1 and not is_type_op(c.constraints[0]))
        return result;
    else
        return "(" + result + ")";
}

string print_unqualified(const ConstrainedType& ct)
{
    return print_unqualified(ct.context) + " => " + print_unqualified(ct.type);
}

string print_unqualified(const StrictLazyType& sl)
{
    string mark = (sl.strict_lazy == StrictLazy::strict)?"!":"~";
    return mark + print_unqualified(sl.type);
}


std::string print_unqualified(const Type& type)
{
    if (type.empty()) return "NOTYPE";

    if (auto mtv = type.to<MetaTypeVar>())
        return print_unqualified(*mtv);
    else if (auto tv = type.to<TypeVar>())
        return tv->print();
    else if (auto tc = type.to<TypeCon>())
        return print_unqualified(*tc);
    else if (auto app = type.to<TypeApp>())
        return print_unqualified(*app);
    else if (auto ct = type.to<ConstrainedType>())
        return print_unqualified(*ct);
    else if (auto forall = type.to<ForallType>())
        return print_unqualified(*forall);
    else if (auto sl = type.to<StrictLazyType>())
        return print_unqualified(*sl);

    std::abort();
}
