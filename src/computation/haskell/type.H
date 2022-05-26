#ifndef HASKELL_TYPE
#define HASKELL_TYPE

#include "computation/object.H"
#include "computation/parser/located.H"

// Can we eliminate this?
#include "computation/expression/expression_ref.H"

namespace Haskell
{

    typedef expression_ref Kind;
    typedef expression_ref Type;

    std::string parenthesize_type(const expression_ref& t);

enum class typevar_info {meta, rigid, other};

struct TypeVar: public Object
{
    Located<std::string> name;
    std::optional<Kind> kind;
    typevar_info info = typevar_info::other;
//    std::optional<int> level;              // typechecking scope depth

    bool operator==(const Object&) const;
    bool operator==(const TypeVar&) const;
    bool operator<(const TypeVar&) const;

//    int get_level() const;

    TypeVar* clone() const { return new TypeVar(*this);}
    std::string print() const;
    std::string print_with_kind() const;

    TypeVar() = default;
    TypeVar(const Located<std::string>& s):name(s) {}
    TypeVar(const Located<std::string>& s, const Kind& k):name(s),kind(k) {}
};

    
struct TypeCon: public Object
{
    Located<std::string> name;
    std::optional<Kind> kind;

    bool operator==(const Object&) const;
    bool operator==(const TypeCon&) const;
    bool operator<(const TypeCon&) const;

    TypeCon* clone() const { return new TypeCon(*this);}
    std::string print() const;
    std::string print_with_kind() const;

    TypeCon() = default;
    TypeCon(const Located<std::string>& s):name(s) {}
    TypeCon(const Located<std::string>& s, const Kind& k):name(s),kind(k) {}
};

struct Context
{
    std::vector<Type> constraints;
    Context* clone() const { return new Context(*this);}

    std::string print() const;
    Context() = default;
    Context(const Context&) = default;
    Context(const std::vector<Type>& cs): constraints(cs) {}
};


struct TupleType: public Object
{
    std::vector<Type> element_types;

    TupleType* clone() const { return new TupleType(*this);}
    std::string print() const;
    TupleType(const std::vector<Type>& v):element_types(v) {
        assert(v.size() != 1);
    }
};

Type tuple_type(const std::vector<Type>& v);

struct ListType: public Object
{
    Type element_type;

    ListType* clone() const { return new ListType(*this);}
    std::string print() const;
    ListType(const Type& t):element_type(t) {};
};

struct TypeApp: public Object
{
    Type head;
    Type arg;

    TypeApp* clone() const { return new TypeApp(*this);}
    std::string print() const;
    TypeApp(const Type& t1, const Type& t2):head(t1), arg(t2) {}
};

Type make_tyapps(const std::vector<Type>& tyapps);
Type make_tyapps(const Type& T0, const std::vector<Type>& args);

template <typename T>
Type make_tyapps(const Type& T0, const std::vector<T>& args)
{
    std::vector<Type> type_args;
    for(auto& arg: args)
        type_args.push_back(arg);

    return make_tyapps(T0, type_args);
}

struct ForallType: public Object
{
    std::vector<TypeVar> type_var_binders;
    Type type;

    ForallType* clone() const { return new ForallType(*this);}
    std::string print() const;
    ForallType(const std::vector<TypeVar>& tvb, const Type& t):type_var_binders(tvb), type(t) {}
};

Type add_forall_vars(const std::vector<TypeVar>& type_vars, const Type& type);

struct ConstrainedType: public Object
{
    Context context;
    Type type;

    ConstrainedType* clone() const { return new ConstrainedType(*this);}
    std::string print() const;
    ConstrainedType(const Context& c, const Type& t):context(c), type(t) {}
};

Type add_constraints(const std::vector<Type>& constraints, const Type& type);
Type add_constraints(const Context& constraints, const Type& type);

enum class StrictLazy {strict,lazy};

struct StrictLazyType: public Object
{
    StrictLazy strict_lazy;
    Type type;

    StrictLazyType* clone() const { return new StrictLazyType(*this);}
    std::string print() const;
    StrictLazyType(const StrictLazy& sl, const Type& t):strict_lazy(sl), type(t) {}
};

}

#endif 
