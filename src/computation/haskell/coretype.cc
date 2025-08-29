#include "coretype.H"
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

std::optional<Type> filled_meta_type_var(const Type& t)
{
    if (auto mtv = t.to<MetaTypeVar>())
        return mtv->filled();
    else
        return {};
}

std::optional<int> unfilled_meta_type_var(const Type& t)
{
    if (auto mtv = t.to<MetaTypeVar>(); mtv and not mtv->filled())
        return mtv->level();
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
    else if (is_a<TypeApp>())
        return as_<TypeApp>() == t.as_<TypeApp>();
    else if (is_a<ConstrainedType>())
        return as_<ConstrainedType>() == t.as_<ConstrainedType>();
    else if (is_a<ForallType>())
        return as_<ForallType>() == t.as_<ForallType>();

    std::abort();
}


int max_level(const vector<Type>& ts)
{
    int l = 0;
    for(auto& t: ts)
        l = std::max(l, max_level(t));
    return l;
}

int max_level(Type t)
{
    t = follow_meta_type_var(t);

    if (auto mtv = t.to<MetaTypeVar>())
        return mtv->level();
    else if (auto tv = t.to<TypeVar>())
        return tv->level();
    else if (t.is_a<TypeCon>())
        return 0;
    else if (auto app = t.to<TypeApp>())
        return std::max( max_level(app->head), max_level(app->arg) );
    else if (auto ct = t.to<ConstrainedType>())
        return std::max( max_level(ct->context), max_level(ct->type) );
    else if (auto fa = t.to<ForallType>())
        return max_level( fa->type );

    std::abort();
    
}

int max_meta_level(const vector<Type>& ts)
{
    int l = 0;
    for(auto& t: ts)
        l = std::max(l, max_meta_level(t));
    return l;
}

int max_meta_level(Type t)
{
    t = follow_meta_type_var(t);

    if (auto mtv = t.to<MetaTypeVar>())
        return mtv->level();
    else if (t.is_a<TypeVar>())
        return 0;
    else if (t.is_a<TypeCon>())
        return 0;
    else if (auto app = t.to<TypeApp>())
        return std::max( max_meta_level(app->head), max_meta_level(app->arg) );
    else if (auto ct = t.to<ConstrainedType>())
        return std::max( max_meta_level(ct->context), max_meta_level(ct->type) );
    else if (auto fa = t.to<ForallType>())
        return max_meta_level( fa->type );

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
    else if (is_a<TypeApp>())
        return as_<TypeApp>().print();
    else if (is_a<ConstrainedType>())
        return as_<ConstrainedType>().print();
    else if (is_a<ForallType>())
        return as_<ForallType>().print();

    std::abort();
}

Type make_arrow_type(const Type& t1, const Type& t2)
{
    static TypeCon type_arrow("->");
    return TypeApp(TypeApp(type_arrow,t1),t2);
}

Type make_equality_pred(const Type& t1, const Type& t2)
{
    static TypeCon type_eq("~");
    return TypeApp(TypeApp(type_eq,t1),t2);
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
    vector<Type> args;
    while(t = follow_meta_type_var(t), t.is_a<TypeApp>())
    {
        auto A = t.as_<TypeApp>();
        args.push_back(A.arg);
        t = A.head;
    }
    std::reverse(args.begin(), args.end());
    return {t,args};
}


bool is_tau_type(Type type)
{
    type = follow_meta_type_var(type);

    if (type.is_a<MetaTypeVar>())
        return true;
    else if (type.is_a<TypeVar>())
        return true;
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

bool is_rho_type(Type type)
{
    type = follow_meta_type_var(type);

    if (type.is_a<MetaTypeVar>())
        return true;
    else if (type.is_a<TypeVar>())
        return true;
    else if (type.is_a<ConstrainedType>())
        return false;
    else if (type.is_a<ForallType>())
        return false;
    else if (type.is_a<TypeCon>() or type.is_a<TypeApp>())
        return true;
    throw myexception()<<"is_rho_type: I don't recognize type '"<<type<<"'";
}

Type type_apply(const std::vector<Type>& tyapps)
{
    assert(not tyapps.empty());
    Type T = tyapps[0];
    for(int i=1;i<tyapps.size();i++)
	T = TypeApp(T,tyapps[i]);
    return T;
}

Type type_apply(Type t, const std::vector<Type>& args)
{
    for(auto& arg: args)
        t = TypeApp(t,arg);
    return t;
}

Type type_apply(Type t, const std::vector<TypeVar>& args)
{
    for(auto& arg: args)
        t = TypeApp(t,arg);
    return t;
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

    if (tc->name == "->")
        return {{args[0],args[1]}};
    else
        return {};
}

std::pair<std::vector<Type>,Type> gen_arg_result_types(const Type& t)
{
    std::vector<Type> arg_types;
    Type result_type = t;
    while(auto x = is_gen_function_type(result_type))
    {
        arg_types.push_back(x->first);
        result_type = x->second;
    }
    return {arg_types,result_type};
}

std::pair<std::vector<Type>,Type> arg_result_types(const Type& t)
{
    std::vector<Type> arg_types;
    Type result_type = t;
    while(auto x = is_function_type(result_type))
    {
        arg_types.push_back(x->first);
        result_type = x->second;
    }

    return {arg_types,result_type};
}

std::tuple<std::vector<TypeVar>, std::vector<Type>, Type> peel_top_gen(Type t)
{
    std::vector<TypeVar> tvs;
    if (auto fa = t.to<ForallType>())
    {
        tvs = fa->type_var_binders;
        t = fa->type;
    }

    std::vector<Type> constraints;
    if (auto c = t.to<ConstrainedType>())
    {
        constraints = c->context;
        t = c->type;
    }

    return {tvs, constraints, t};
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

optional<pair<Type,Type>> is_equality_pred(const Type& t)
{
    auto [head,args] = decompose_type_apps(t);

    if (args.size() != 2) return {};

    auto tc = head.to<TypeCon>();
    if (not tc) return {};

    if (tc->name == "~")
        return {{args[0],args[1]}};
    else
        return {};
}

optional<tuple<Type,vector<Type>>> is_dictionary_pred(const Type& t)
{
    // This should be mutually exclusive with is_equality_pred
    // if the kind of the predicate is Constraint
    auto [head,args] = decompose_type_apps(t);

    if (auto tc = head.to<TypeCon>(); tc and tc->name == "~")
        return {};
    else
        return {{head, args}};
}

std::vector<Type> dictionary_preds(const std::vector<Type> constraints)
{
    vector<Type> constraints2;

    for(auto& constraint: constraints)
        if (is_dictionary_pred(constraint))
            constraints2.push_back(constraint);

    return constraints2;
}

std::vector<Type> equality_preds(const std::vector<Type> constraints)
{
    vector<Type> constraints2;

    for(auto& constraint: constraints)
        if (is_equality_pred(constraint))
            constraints2.push_back(constraint);

    return constraints2;
}

Type char_type()
{
    return TypeCon("Char");
}

Type int_type()
{
    return TypeCon("Int");
}

Type integer_type()
{
    return TypeCon("Integer");
}

Type double_type()
{
    return TypeCon("Double");
}

optional<Type> is_list_type(Type t)
{
    if (auto tcapp = is_type_con_app(t))
    {
        auto& [tc, args] = *tcapp;

        if (args.size() == 1 and tc.name == "[]")
            return args[0];
        else
            return {};
    }
    else
        return {};
}

std::optional<Type> is_IO_type(Type t)
{
    if (auto tcapp = is_type_con_app(t))
    {
        auto& [tc, args] = *tcapp;

        if (args.size() == 1 and tc.name == "Compiler.IO.IO")
            return args[0];
        else
            return {};
    }
    else
        return {};
}

optional<vector<Type>> is_tuple_type(Type t)
{
    if (auto tcapp = is_type_con_app(t))
    {
        auto& [tc, args] = *tcapp;

        auto& head_name = tc.name;
        if (is_tuple_name(head_name) and args.size() == tuple_arity(head_name))
            return args;
        else
            return {};
    }
    else
        return {};
}

std::optional<std::tuple<TypeCon,std::vector<Type>>> is_type_con_app(const Type& t)
{
    auto [head,args] = decompose_type_apps(t);

    if (auto tc = head.to<TypeCon>())
        return {{*tc, args}};
    else
        return {};
}


Type remove_top_gen(Type type)
{
    type = follow_meta_type_var(type);

    if (auto f = type.to<ForallType>())
        type = f->type;

    if (auto c = type.to<ConstrainedType>())
        type = c->type;

    return type;
}

string parenthesize_type(Type t, bool parenthesize_type_app)
{
    t = follow_meta_type_var(t);

    if (t.is_a<TypeCon>() or t.is_a<MetaTypeVar>() or t.is_a<TypeVar>() or is_tuple_type(t) or is_list_type(t))
        return t.print();
    else if (not parenthesize_type_app and t.is_a<TypeApp>() and not is_type_op(t))
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
        Type t;
        *indirect = t;
    }
    assert(indirect->empty());
}

string MetaTypeVar::print() const
{
    if (auto t = filled())
        return t->print();

    string uname = name;
    if (index)
        uname = uname +"#"+std::to_string(*index);

    uname = uname + "{"+std::to_string(level())+"}";

    return uname;
}

string MetaTypeVar::print_with_kind() const
{
    assert(not filled());

    string uname = print();

    uname = "("+uname + " :: " + kind.print()+")";

    return uname;
}

bool MetaTypeVar::operator==(const MetaTypeVar& tv) const
{
    return index == tv.index and name == tv.name and indirect == tv.indirect;
}

bool MetaTypeVar::operator<(const MetaTypeVar& tv) const
{
    if (index < tv.index)
        return true;

    if (index > tv.index)
        return false;

    int cmp = name.compare(tv.name);

    // Don't depend on the location of *indirect, if indirect is non-null.
    assert(cmp != 0 or indirect == tv.indirect);

    return (cmp < 0);
}

int MetaTypeVar::level() const
{
    if (not filled())
        return level_;
    else
        throw myexception()<<"Trying to get level for filled meta-typevar";
}

MetaTypeVar::MetaTypeVar(int l, const std::string& s, const Kind& k)
    :level_(l), indirect(new Type),name(s),kind(k)
{}

bool TypeVar::is_skolem_constant() const
{
    return level_.has_value();
}

int TypeVar::level() const
{
    if (not is_skolem_constant())
        return 0;

    return *level_;
}
    
string TypeVar::print() const
{
    string uname = name;
    if (index)
        uname = uname +"#"+std::to_string(*index);

    if (is_skolem_constant())
        uname = uname + "{{"+std::to_string(level())+"}}";

    return uname;
}

string TypeVar::print_with_kind() const
{
    string uname = print();

    uname = "("+uname + " :: " + kind.print()+")";

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

TypeVar::TypeVar(const std::string& s, const Kind& k)
    :name(s), kind(k)
{}

TypeVar::TypeVar(int l, const std::string& s, const Kind& k)
    :level_(l), name(s),kind(k)
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

optional< std::tuple<TypeCon, Type, Type> > is_type_op(const Type& t)
{
    auto [head,args] = decompose_type_apps(t);

    if (args.size() != 2) return {};

    auto tc = head.to<TypeCon>();

    if (tc and is_haskell_sym(tc->name))
        return {{*tc, args[0], args[1]}};
    else
        return {};
}

// This is guaranteed not to return a filled meta-typevar.
Type follow_meta_type_var(Type t)
{
    while(auto uv = t.to<MetaTypeVar>())
    {
        if (uv->filled())
            t = *uv->filled();
        else
            return t;
    }
    return t;
}

string TypeApp::print() const
{
    if (auto type_op = is_type_op(*this))
    {
        auto [tycon, arg1, arg2] = *type_op;

	if (is_function_type(*this) and is_function_type(arg2))
	    return parenthesize_type(arg1, false) + " " + tycon.print() + " "+ arg2.print();
	else
	    return parenthesize_type(arg1, false) + " " + tycon.print() + " "+ parenthesize_type(arg2, false);
    }
    else if (auto element_type = is_list_type(*this))
        return "[" + element_type->print() +"]";
    else if (auto element_types = is_tuple_type(*this))
    {
        vector<string> parts;
        for(auto& element_type: *element_types)
            parts.push_back(element_type.print());
        return "(" + join(parts,", ") +")";
    }

    return head.print() + " " + parenthesize_type(arg, true);
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
            CT.context.push_back(constraint);
        return CT;
    }
    else
        return ConstrainedType(Context(constraints),type);
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
    if (cs.size() == 1 and not is_type_op((*this)[0]))
        return result;
    else
        return "(" + result + ")";
}

Type tuple_type(const std::vector<Type>& ts)
{
    int n = ts.size();
    if (n == 1)
        return ts[0];
    else
        return type_apply(tuple_tycon(n), ts);
}

Type list_type(const Type& t)
{
    return type_apply(list_tycon(), {t});
}

TypeCon tuple_tycon(int n)
{
    auto kind = make_n_args_kind(n);
    return TypeCon( tuple_name(n), kind );
}

TypeCon list_tycon()
{
    auto kind = make_n_args_kind(1);
    return TypeCon( "[]", kind );
}

Type quantify(const std::vector<TypeVar>& tvs, const std::vector<Type>& constraints, const Type& type)
{
    return add_forall_vars(tvs, add_constraints(constraints, type));
}
