#include "type.H"
#include "util/string/join.H"
#include "haskell/ids.H"       // for tuple_name
#include "util/set.H"          // for includes( , )
#include <range/v3/all.hpp>

using std::string;
using std::pair;
using std::vector;
using std::optional;

namespace views = ranges::views;

namespace Haskell
{

Type make_arrow_type(const Type& t1, const Type& t2)
{
    static TypeCon type_arrow(Located<string>({},"->"));
    return TypeApp(TypeApp(type_arrow,t1),t2);
}

Type function_type(const vector<Type>& arg_types, const Type& result_type)
{
    Type ftype = result_type;
    for(auto& arg_type: arg_types | views::reverse)
        ftype = make_arrow_type(arg_type, ftype);
    return ftype;
}

pair<Type,vector<Type>> decompose_type_apps(Type t)
{
    if (auto L = t.to<ListType>())
        return {TypeCon({noloc,"[]"}), {L->element_type}};

    if (auto T = t.to<TupleType>())
    {
        int n = T->element_types.size();
        return {TypeCon({noloc,tuple_name(n)}), T->element_types};
    }

    vector<Type> args;
    while(t.is_a<TypeApp>())
    {
        auto A = t.as_<TypeApp>();
        args.push_back(A.arg);
        t = A.head;
    }
    std::reverse(args.begin(), args.end());
    return {t,args};
}


optional<pair<Type,Type>> is_gen_function_type(const Type& t)
{
    return is_function_type( remove_top_gen(t) );
}

optional<pair<Type,Type>> is_function_type(const Type& t)
{
    auto [head,args] = decompose_type_apps(t);

    if (args.size() != 2) return {};

    auto tc = head.to<TypeCon>();
    if (not tc) return {};

    if (unloc(tc->name) == "->")
        return {{args[0],args[1]}};
    else
        return {};
}


Type remove_top_gen(Type type)
{
    if (auto f = type.to<ForallType>())
        type = f->type;

    if (auto c = type.to<ConstrainedType>())
        type = c->type;

    return type;
}

string parenthesize_type(const expression_ref& t)
{
    if (t.is_a<TypeCon>() or t.is_a<TypeVar>() or t.is_a<TupleType>() or t.is_a<ListType>())
        return t.print();
    else
        return "(" + t.print() + ")";
}

string TypeVar::print() const
{
    string uname = unloc(name);
    if (index)
        uname = uname +"#"+std::to_string(*index);

    return uname;
}

/*
int TypeVar::get_level() const
{
    if (info == typevar_info::other)
        return 0;
    else
        return *level;
}
*/

string TypeVar::print_with_kind() const
{
    string uname = print();

    if (kind)
        uname = "("+uname + " :: " + (*kind).print()+")";

    return uname;
}

bool TypeVar::operator==(const Object& o) const
{
    auto T = dynamic_cast<const TypeVar*>(&o);
    if (not T)
        return false;

    return (*this) == *T;
}

bool TypeVar::operator==(const TypeVar& tv) const
{
    return index == tv.index and unloc(name) == unloc(tv.name);
}

bool TypeVar::operator<(const TypeVar& tv) const
{
    if (not index and tv.index)
        return true;

    if (index and not tv.index)
        return false;

    if (*index < *tv.index)
        return true;

    if (*index > *tv.index)
        return false;

    int cmp = unloc(name).compare(unloc(tv.name));

    return (cmp < 0);
}

string TypeCon::print() const
{
    return unloc(name);
}

string TypeCon::print_with_kind() const
{
    if (kind)
        return "("+unloc(name) + " :: " + (*kind).print()+")";
    else
        return unloc(name);
}

bool TypeCon::operator==(const Object& o) const
{
    auto T = dynamic_cast<const TypeCon*>(&o);
    if (not T)
        return false;

    return (*this) == *T;
}

bool TypeCon::operator==(const TypeCon& tc) const
{
    return unloc(name) == unloc(tc.name);
}

bool TypeCon::operator<(const TypeCon& tc) const
{
    int cmp = unloc(name).compare(unloc(tc.name));

    return (cmp < 0);
}

string TypeApp::print() const
{
    if (auto ftype = is_function_type(*this))
    {
        auto& [source,dest] = *ftype;
        return parenthesize_type(source) + " -> " + arg.print();
    }

    return head.print() + " " + parenthesize_type(arg);
}

Type make_tyapps(const std::vector<Type>& tyapps)
{
    assert(not tyapps.empty());
    Type T = tyapps[0];
    for(int i=1;i<tyapps.size();i++)
	T = Haskell::TypeApp(T,tyapps[i]);
    return T;
}

Type make_tyapps(const Type& T0, const std::vector<Type>& args)
{
    Type T = T0;
    for(auto& arg: args)
	T = Haskell::TypeApp(T, arg);
    return T;
}

string ForallType::print() const
{
    vector<string> binders;
    for(auto& type_var_binder: type_var_binders)
        binders.push_back(type_var_binder.print_with_kind());
    return "forall "+join(binders," ")+". "+type.print();
}

Type add_forall_vars(const std::vector<TypeVar>& type_vars, const Type& type)
{
    if (type_vars.empty())
        return type;
    else if (auto FAT = type.to<ForallType>())
    {
        auto new_type_vars = type_vars;
        for(auto& type_var: FAT->type_var_binders)
        {
            assert(not includes(type_vars, type_var));
            new_type_vars.push_back(type_var);
        }
        return ForallType(new_type_vars, FAT->type);
    }
    else
        return ForallType(type_vars, type);
}

string ConstrainedType::print() const
{
    return context.print() + " => " + type.print();
}

Type add_constraints(const vector<Type>& constraints, const Type& type)
{
    if (constraints.empty())
        return type;
    else if (type.is_a<ConstrainedType>())
    {
        auto CT = type.as_<ConstrainedType>();
        for(auto& constraint: constraints)
            CT.context.constraints.push_back(constraint);
        return CT;
    }
    else
        return ConstrainedType(Context(constraints),type);
}

Type add_constraints(const Context& context, const Type& type)
{
    return add_constraints(context.constraints, type);
}

std::string Context::print() const
{
    vector<string> cs;
    for(auto& constraint: constraints)
        cs.push_back(constraint.print());

    string result = join(cs,", ");
    if (cs.size() == 1)
        return result;
    else
        return "(" + result + ")";
}

string StrictLazyType::print() const
{
    string mark = (strict_lazy == StrictLazy::strict)?"!":"~";
    return mark + type.print();
}

string TupleType::print() const
{
    vector<string> parts;
    for(auto& element_type: element_types)
        parts.push_back(element_type.print());
    return "(" + join(parts,", ") +")";
}

Type tuple_type(const std::vector<Type>& ts)
{
    if (ts.size() == 1)
        return ts[0];
    else
        return TupleType(ts);
}

string ListType::print() const
{
    return "[" + element_type.print() + "]";
}

}
