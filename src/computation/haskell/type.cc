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

pair<LType,vector<LType>> decompose_type_apps(LType lt)
{
    if (auto L = unloc(lt).to<ListType>())
        return {{noloc, TypeCon("[]")}, {L->element_type}};

    if (auto T = unloc(lt).to<TupleType>())
    {
        int n = T->element_types.size();
        return {{noloc, TypeCon(tuple_name(n))}, T->element_types};
    }

    vector<LType> args;
    while(unloc(lt).is_a<TypeApp>())
    {
        auto A = unloc(lt).as_<TypeApp>();
        args.push_back(A.arg);
        lt = A.head;
    }
    std::reverse(args.begin(), args.end());
    return {lt,args};
}

LType remove_top_gen(LType ltype)
{
    if (auto f = unloc(ltype).to<ForallType>())
        ltype = f->type;

    if (auto c = unloc(ltype).to<ConstrainedType>())
        ltype = c->type;

    return ltype;
}

optional<pair<LType,LType>> is_gen_function_type(const LType& lt)
{
    return is_function_type( remove_top_gen(lt) );
}

optional<pair<LType,LType>> is_function_type(const LType& lt)
{
    auto [head,args] = decompose_type_apps(lt);

    if (args.size() != 2) return {};

    auto tc = unloc(head).to<TypeCon>();
    if (not tc) return {};

    if (tc->name == "->")
        return {{args[0],args[1]}};
    else
        return {};
}

int gen_type_arity(LType lt)
{
    int a = 0;
    while(auto x = is_gen_function_type(lt))
    {
        a++;
        lt = x->second;
    }
    return a;
}

int type_arity(LType t)
{
    int a = 0;
    while(auto x = is_function_type(t))
    {
        a++;
        t = x->second;
    }
    return a;
}

optional< std::tuple<LTypeCon, LType, LType> > is_type_op(const Type& t)
{
    auto [head,args] = decompose_type_apps({noloc,t});

    if (args.size() != 2) return {};

    auto tc = unloc(head).to<TypeCon>();

    if (tc and is_haskell_sym(tc->name))
        return {{{head.loc,*tc}, args[0], args[1]}};
    else
        return {};
}

string parenthesize_type(const LType& lt, bool parenthesize_type_app)
{
    auto& t = unloc(lt);
    if (t.is_a<TypeCon>() or t.is_a<TypeVar>() or t.is_a<ListType>() or t.is_a<TupleType>())
        return t.print();
    else if (not parenthesize_type_app and t.is_a<TypeApp>() and not is_type_op(t))
        return t.print();
    else
        return "(" + t.print() + ")";
}

string TypeVar::print() const
{
    string uname = name;
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
    return index == tv.index and name == tv.name;
}

bool TypeVar::operator<(const TypeVar& tv) const
{
    if (index < tv.index)
        return true;

    if (index > tv.index)
        return false;

    int cmp = name.compare(tv.name);

    return (cmp < 0);
}

TypeVar::TypeVar()
{}

TypeVar::TypeVar(const std::string& s)
    :name(s)
{}

TypeVar::TypeVar(const std::string& s, const Kind& k)
    :name(s), kind(k)
{}

string TypeCon::print() const
{
    return name;
}

string TypeCon::print_with_kind() const
{
    if (kind)
        return "("+name + " :: " + (*kind).print()+")";
    else
        return name;
}

bool TypeCon::operator==(const TypeCon& tc) const
{
    return name == tc.name;
}

bool TypeCon::operator<(const TypeCon& tc) const
{
    int cmp = name.compare(tc.name);

    return (cmp < 0);
}

bool TypeApp::operator==(const TypeApp& t) const
{
    return (head == t.head) and (arg == t.arg);
}

string TypeApp::print() const
{
    if (auto type_op = is_type_op(*this))
    {
        auto& [tycon, arg1, arg2] = *type_op;

        if (is_function_type({noloc,*this}) and is_function_type(arg2))
            return parenthesize_type(arg1, false) + " " + tycon.print() + " "+ arg2.print();
        else
            return parenthesize_type(arg1, false) + " " + tycon.print() + " "+ parenthesize_type(arg2, false);
    }

    return head.print() + " " + parenthesize_type(arg,true);
}

LType make_tyapps(const std::vector<LType>& tyapps)
{
    assert(not tyapps.empty());
    LType T = tyapps[0];
    for(int i=1;i<tyapps.size();i++)
	T = {T.loc * tyapps[i].loc, Hs::TypeApp(T,tyapps[i])};
    return T;
}

LType make_tyapps(const LType& T0, const std::vector<LType>& args)
{
    LType T = T0;
    for(auto& arg: args)
	T = {T.loc * arg.loc, Hs::TypeApp(T, arg)};
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
        binders.push_back(unloc(type_var_binder).print_with_kind());
    return "forall "+join(binders," ")+". "+type.print();
}

LType add_forall_vars(const std::vector<LTypeVar>& type_vars, const LType& ltype)
{
    auto [loc,type] = ltype;
    for(auto& [tv_loc, tv]: type_vars)
        loc = loc * tv_loc;

    if (type_vars.empty())
        return {loc, type};
    else if (auto FAT = type.to<ForallType>())
    {
        auto new_type_vars = type_vars;
        for(auto& type_var: FAT->type_var_binders)
        {
            assert(not includes(type_vars, type_var));
            new_type_vars.push_back(type_var);
        }
        return {loc, ForallType(new_type_vars, FAT->type)};
    }
    else
        return {loc,ForallType(type_vars, ltype)};
}

LType add_constraints(const vector<LType>& constraints, const LType& ltype)
{
    auto [loc, type] = ltype;
    for(auto& constraint: constraints)
        loc = loc * constraint.loc;

    if (constraints.empty())
        return {loc, type};
    else if (type.is_a<ConstrainedType>())
    {
        auto CT = type.as_<ConstrainedType>();
        for(auto& constraint: constraints)
            CT.context.push_back(constraint);
        return {loc, CT};
    }
    else
        return {loc, ConstrainedType(Context(constraints), ltype)};
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
    if (size() != c.size()) return false;

    for(int i=0; i<size();i++)
        if ((*this)[i] != c[i]) return false;

    return true;
}

std::string Context::print() const
{
    vector<string> cs;
    for(auto& constraint: *this)
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
