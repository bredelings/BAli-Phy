#include "type.H"
#include "haskell.H"
#include "typecheck/kind.H"
#include "util/string/join.H"
#include "haskell/ids.H"       // for tuple_name
#include "util/set.H"          // for includes( , )
#include <range/v3/all.hpp>

using std::string;
using std::pair;
using std::tuple;
using std::vector;
using std::optional;

namespace views = ranges::views;

namespace Haskell
{

bool Type::operator==(const Type& t) const
{
    if (type_ptr.index() != t.type_ptr.index()) return false;

    if (type_ptr.index() == 0) return true;

    if (is_a<TypeVar>())
        return as_<TypeVar>() == t.as_<TypeVar>();
    else if (is_a<TypeCon>())
        return as_<TypeCon>() == t.as_<TypeCon>();
    else if (is_a<TupleType>())
        return as_<TupleType>() == t.as_<TupleType>();
    else if (is_a<ListType>())
        return as_<ListType>() == t.as_<ListType>();
    else if (is_a<TypeApp>())
        return as_<TypeApp>() == t.as_<TypeApp>();
    else if (is_a<ConstrainedType>())
        return as_<ConstrainedType>() == t.as_<ConstrainedType>();
    else if (is_a<ForallType>())
        return as_<ForallType>() == t.as_<ForallType>();
    else if (is_a<StrictType>())
        return as_<StrictType>() == t.as_<StrictType>();
    else if (is_a<LazyType>())
        return as_<LazyType>() == t.as_<LazyType>();

    std::abort();
}


std::string Type::print() const
{
    if (type_ptr.index() == 0) return "NOTYPE";

    if (is_a<TypeVar>())
        return as_<TypeVar>().print();
    else if (is_a<TypeCon>())
        return as_<TypeCon>().print();
    else if (is_a<TupleType>())
        return as_<TupleType>().print();
    else if (is_a<ListType>())
        return as_<ListType>().print();
    else if (is_a<TypeApp>())
        return as_<TypeApp>().print();
    else if (is_a<ConstrainedType>())
        return as_<ConstrainedType>().print();
    else if (is_a<ForallType>())
        return as_<ForallType>().print();
    else if (is_a<StrictType>())
        return as_<StrictType>().print();
    else if (is_a<LazyType>())
        return as_<LazyType>().print();
    else if (is_a<FieldDecls>())
        return as_<FieldDecls>().print();

    std::abort();
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

Type remove_top_gen(Type type)
{
    if (auto f = type.to<ForallType>())
        type = f->type;

    if (auto c = type.to<ConstrainedType>())
        type = c->type;

    return type;
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

int gen_type_arity(Type t)
{
    int a = 0;
    while(auto x = is_gen_function_type(t))
    {
        a++;
        t = x->second;
    }
    return a;
}

int type_arity(Type t)
{
    int a = 0;
    while(auto x = is_function_type(t))
    {
        a++;
        t = x->second;
    }
    return a;
}

string parenthesize_type(Type t)
{
    if (t.is_a<TypeCon>() or t.is_a<TypeVar>() or t.is_a<ListType>() or t.is_a<TupleType>())
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

string TypeVar::print_with_kind() const
{
    string uname = print();

    if (kind)
        uname = "("+uname + " :: " + (*kind).print()+")";

    return uname;
}

bool TypeVar::operator==(const TypeVar& tv) const
{
    return index == tv.index and unloc(name) == unloc(tv.name);
}

bool TypeVar::operator<(const TypeVar& tv) const
{
    if (index < tv.index)
        return true;

    if (index > tv.index)
        return false;

    int cmp = unloc(name).compare(unloc(tv.name));

    return (cmp < 0);
}

TypeVar::TypeVar()
{}

TypeVar::TypeVar(const Located<std::string>& s)
    :name(s)
{}

TypeVar::TypeVar(const Located<std::string>& s, const Kind& k)
    :name(s), kind(k)
{}

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

bool TypeCon::operator==(const TypeCon& tc) const
{
    return unloc(name) == unloc(tc.name);
}

bool TypeCon::operator<(const TypeCon& tc) const
{
    int cmp = unloc(name).compare(unloc(tc.name));

    return (cmp < 0);
}

bool TypeApp::operator==(const TypeApp& t) const
{
    return (head == t.head) and (arg == t.arg);
}

optional< std::tuple<TypeCon, Type, Type> > is_type_op(const Type& t)
{
    auto [head,args] = decompose_type_apps(t);

    if (args.size() != 2) return {};

    auto tc = head.to<TypeCon>();

    if (tc and is_haskell_sym(unloc(tc->name)))
        return {{*tc, args[0], args[1]}};
    else
        return {};
}

string TypeApp::print() const
{
    if (auto type_op = is_type_op(*this))
    {
        auto& [tycon, arg1, arg2] = *type_op;
        if (is_type_op(arg1))
            return parenthesize_type(arg1) + " " + tycon.print() + " "+ arg2.print();
        else
            return arg1.print() + " " + tycon.print() + " "+ arg2.print();
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

bool ForallType::operator==(const ForallType&) const
{
    std::abort();
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

bool ConstrainedType::operator==(const ConstrainedType& t) const
{
    return context == t.context and type == t.type;
}

string ConstrainedType::print() const
{
    return context.print() + " => " + type.print();
}

bool Context::operator==(const Context& c) const
{
    if (constraints.size() != c.constraints.size()) return false;

    for(int i=0; i<constraints.size();i++)
        if (constraints[i] != c.constraints[i]) return false;

    return true;
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

bool StrictType::operator==(const StrictType& t) const
{
    return type == t.type;
}

string StrictType::print() const
{
    return "!" + type.print();
}

bool LazyType::operator==(const LazyType& t) const
{
    return type == t.type;
}

string LazyType::print() const
{
    return "~" + type.print();
}

bool TupleType::operator==(const TupleType& t) const
{
    if (element_types.size() != t.element_types.size()) return false;

    for(int i=0; i<element_types.size(); i++)
        if (element_types[i] != t.element_types[i]) return false;

    return true;
}

string TupleType::print() const
{
    vector<string> parts;
    for(auto& element_type: element_types)
        parts.push_back(element_type.print());
    return "(" + join(parts,", ") +")";
}

bool ListType::operator==(const ListType& t) const
{
    return element_type == t.element_type;
}

string ListType::print() const
{
    return "[" + element_type.print() + "]";
}

string TypeOfKind::print() const
{
    return type.print() + " :: " + kind.print();
}

}
