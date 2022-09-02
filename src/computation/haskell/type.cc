#include "type.H"
#include "haskell.H"
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

std::optional<Type> filled_meta_type_var(const Type& t)
{
    if (auto mtv = t.to<MetaTypeVar>())
        return mtv->filled();
    else
        return {};
}

bool Type::operator==(const Type& t) const
{
    if (auto i1 = filled_meta_type_var(*this))
        return (*i1) == t;

    if (auto i2 = filled_meta_type_var(t))
        return operator==(*i2);

    if (type_ptr.index() != t.type_ptr.index()) return false;

    if (type_ptr.index() == 0) return true;

    if (is_a<MetaTypeVar>())
        return as_<MetaTypeVar>() == t.as_<MetaTypeVar>();
    else if (is_a<TypeVar>())
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
    else if (is_a<StrictLazyType>())
        return as_<StrictLazyType>() == t.as_<StrictLazyType>();

    std::abort();
}


std::string Type::print() const
{
    if (type_ptr.index() == 0) return "NOTYPE";

    if (is_a<MetaTypeVar>())
        return as_<MetaTypeVar>().print();
    else if (is_a<TypeVar>())
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
    else if (is_a<StrictLazyType>())
        return as_<StrictLazyType>().print();
    else if (is_a<FieldDecls>())
        return as_<FieldDecls>().print();

    std::abort();
}

Type make_arrow_type(const Type& t1, const Type& t2)
{
    static TypeCon type_arrow(Located<string>({},"->"));
    return TypeApp(TypeApp(type_arrow,t1),t2);
}

Type make_equality_constraint(const Type& t1, const Type& t2)
{
    static TypeCon type_arrow(Located<string>({},"~"));
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
    if (auto tt = filled_meta_type_var(t))
        return decompose_type_apps(*tt);

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


bool is_tau_type(const Type& type)
{
    if (auto mtv = type.to<MetaTypeVar>())
    {
        if (auto t = mtv->filled())
            return is_tau_type(*t);
        else
            return true;
    }
    else if (type.is_a<TypeVar>())
        return true;
    else if (auto l = type.to<ListType>())
        return is_tau_type(l->element_type);
    else if (auto tup = type.to<TupleType>())
    {
        for(auto& element_type: tup->element_types)
            if (not is_tau_type(element_type))
                return false;
        return true;
    }
    else if (type.is_a<ConstrainedType>())
        return false;
    else if (type.is_a<ForallType>())
        return false;
    else if (type.is_a<TypeCon>() or type.is_a<TypeApp>())
    {
        auto [head, args] = decompose_type_apps(type);

        for(auto& arg: args)
            if (not is_tau_type(arg))
                return false;

        return true;
    }
    throw myexception()<<"is_tau_type: I don't recognize type '"<<type<<"'";
}

bool is_rho_type(const Type& type)
{
    if (auto mtv = type.to<MetaTypeVar>())
    {
        if (auto t = mtv->filled())
            return is_rho_type(*t);
        else
            return true;
    }
    else if (type.is_a<TypeVar>())
        return true;
    else if (type.is_a<ListType>())
        return true;
    else if (type.is_a<TupleType>())
        return true;
    else if (type.is_a<ConstrainedType>())
        return false;
    else if (type.is_a<ForallType>())
        return false;
    else if (type.is_a<TypeCon>() or type.is_a<TypeApp>())
        return true;
    throw myexception()<<"is_rho_type: I don't recognize type '"<<type<<"'";
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

optional<pair<Type,Type>> is_equality_constraint(const Type& t)
{
    auto [head,args] = decompose_type_apps(t);

    if (args.size() != 2) return {};

    auto tc = head.to<TypeCon>();
    if (not tc) return {};

    if (unloc(tc->name) == "~")
        return {{args[0],args[1]}};
    else
        return {};
}

optional<Type> is_list_type(const Type& t)
{
    if (auto tt = filled_meta_type_var(t))
        return is_list_type(*tt);

    if (auto l = t.to<Hs::ListType>())
        return l->element_type;

    auto [head,args] = decompose_type_apps(t);

    if (args.size() != 1) return {};

    auto tc = head.to<TypeCon>();
    if (not tc) return {};

    if (unloc(tc->name) == "[]")
        return {args[0]};
    else
        return {};
}


optional<vector<Type>> is_tuple_type(const Type& t)
{
    if (auto tt = filled_meta_type_var(t))
        return is_tuple_type(*tt);

    if (auto tup = t.to<Hs::TupleType>())
        return tup->element_types;

    auto [head,args] = decompose_type_apps(t);

    auto tc = head.to<TypeCon>();
    if (not tc) return {};

    auto& head_name = unloc(tc->name);
    if (not is_tuple_name(head_name))
        return {};

    if (args.size() == tuple_arity(head_name))
        return args;
    else
        return {};
}


Type remove_top_gen(Type type)
{
    if (auto tt = filled_meta_type_var(type))
        type = *tt;

    if (auto f = type.to<ForallType>())
        type = f->type;

    if (auto c = type.to<ConstrainedType>())
        type = c->type;

    return type;
}

string parenthesize_type(const Hs::Type& t)
{
    if (auto t2 = filled_meta_type_var(t))
        return parenthesize_type(*t2);

    if (t.is_a<TypeCon>() or t.is_a<MetaTypeVar>() or t.is_a<TypeVar>() or is_tuple_type(t) or is_list_type(t))
        return t.print();
    else
        return "(" + t.print() + ")";
}

optional<Type> MetaTypeVar::filled() const
{
    if (indirect->empty())
        return {};
    else
        return *indirect;
}

void MetaTypeVar::fill(const Type& t) const
{
    assert(indirect->empty());
    assert(not t.empty());
    *indirect = t;
}

void MetaTypeVar::clear() const
{
    if (not indirect->empty())
    {
        Hs::Type t;
        *indirect = t;
    }
    assert(indirect->empty());
}

string MetaTypeVar::print() const
{
    if (auto t = filled())
        return t->print();

    string uname = unloc(name);
    if (index)
        uname = uname +"#"+std::to_string(*index);

    return uname;
}

string MetaTypeVar::print_with_kind() const
{
    assert(not filled());

    string uname = print();

    if (kind)
        uname = "("+uname + " :: " + (*kind).print()+")";

    return uname;
}

bool MetaTypeVar::operator==(const MetaTypeVar& tv) const
{
    return index == tv.index and unloc(name) == unloc(tv.name) and indirect == tv.indirect;
}

bool MetaTypeVar::operator<(const MetaTypeVar& tv) const
{
    if (index < tv.index)
        return true;

    if (index > tv.index)
        return false;

    int cmp = unloc(name).compare(unloc(tv.name));

    // Don't depend on the location of *indirect, if indirect is non-null.
    assert(cmp != 0 or indirect == tv.indirect);

    return (cmp < 0);
}


MetaTypeVar::MetaTypeVar(): MetaTypeVar({},{}) {}

MetaTypeVar::MetaTypeVar(const Located<std::string>& s): MetaTypeVar(s, {}) {}

MetaTypeVar::MetaTypeVar(const Located<std::string>& s, const Kind& k)
    :indirect(new Type),name(s),kind(k)
{}

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
    else if (auto etype = is_list_type(*this))
        return ListType(*etype).print();
    else if (auto ttype = is_tuple_type(*this))
        return TupleType(*ttype).print();

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

bool StrictLazyType::operator==(const StrictLazyType& t) const
{
    return strict_lazy == t.strict_lazy and type == t.type;
}

string StrictLazyType::print() const
{
    string mark = (strict_lazy == StrictLazy::strict)?"!":"~";
    return mark + type.print();
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

Type tuple_type(const std::vector<Type>& ts)
{
    if (ts.size() == 1)
        return ts[0];
    else
        return TupleType(ts);
}

bool ListType::operator==(const ListType& t) const
{
    return element_type == t.element_type;
}

string ListType::print() const
{
    return "[" + element_type.print() + "]";
}

bool Implication::operator==(const Implication& I) const
{
    return givens == I.givens and wanteds == I.wanteds;
}

std::string Implication::print() const
{
    return givens.print() + " => " + wanteds.print();
}

Implication::Implication(const Context& g, const Context& w)
    :givens(g), wanteds(w)
{
}

}
