#ifndef HASKELL_TYPE
#define HASKELL_TYPE

#include <variant>
#include <iostream>
#include <set>

#include "util/cow-ptr.H"
#include "computation/parser/located.H"

// Can we eliminate this?
#include "computation/expression/expression_ref.H"
#include "core.H"

namespace Haskell
{
    class MetaTypeVar;
    struct TypeVar;
    struct TypeCon;
    struct TupleType;
    struct ListType;
    struct TypeApp;
    struct ConstrainedType;
    struct ForallType;
    struct StrictLazyType;
    struct FieldDecls;
    struct Implication;

    struct Type
    {
        std::variant
        <
            std::monostate,
            cow_ptr<MetaTypeVar>,
            cow_ptr<TypeVar>,
            cow_ptr<TypeCon>,
            cow_ptr<TupleType>,
            cow_ptr<ListType>,
            cow_ptr<TypeApp>,
            cow_ptr<ConstrainedType>,
            cow_ptr<ForallType>,
            cow_ptr<StrictLazyType>,
            cow_ptr<FieldDecls>,
            cow_ptr<Implication>
        >
        type_ptr;

        std::string print() const;

        bool empty() const
        {
            return std::holds_alternative<std::monostate>(type_ptr);
        }

        template <typename T>
        const T* to() const
        {
            if (std::holds_alternative<cow_ptr<T>>(type_ptr))
                return std::get<cow_ptr<T>>(type_ptr).get();
            else
                return nullptr;
        }


        template <typename T>
        T* to_modifiable()
        {
            if (std::holds_alternative<cow_ptr<T>>(type_ptr))
                return std::get<cow_ptr<T>>(type_ptr).modify_ptr().get();
            else
                return nullptr;
        }


        bool operator==(const Type& t) const;
        bool operator!=(const Type& t) const {return not (*this == t);}

        template <typename T>
        bool is_a() const {return to<T>();}

        template <typename T>
        const T& as_() const {return *std::get<cow_ptr<T>>(type_ptr).get_ptr();}

        template <typename T>
        T& as_modifiable() {return *std::get<cow_ptr<T>>(type_ptr).modify_ptr();}

        Type& operator=(const Type& t)
        {
            auto tmp = type_ptr;
            type_ptr = t.type_ptr;

            return *this;
        }

        template <typename T>
        Type& operator=(const T& t)
        {
            auto tmp = type_ptr;
            type_ptr = std::make_shared<const T>(t);
            return *this;
        }

        Type() = default;
        Type(const Type& t) = default;

        template <typename T>
        Type(const T& t):type_ptr( cow_ptr<T>(t) ) {}

        ~Type() = default;
    };

    typedef expression_ref Kind;

    std::string parenthesize_type(const Type& t);

class MetaTypeVar
{
    std::shared_ptr<Type> indirect;

public:
    Located<std::string> name;
    std::optional<int> index;

    std::optional<Kind> kind;

    std::optional<Type> filled() const;
    void fill(const Type& t) const;
    void clear() const;

    bool operator==(const MetaTypeVar&) const;
    bool operator<(const MetaTypeVar&) const;

    std::string print() const;
    std::string print_with_kind() const;

    MetaTypeVar();
    MetaTypeVar(const Located<std::string>& s);
    MetaTypeVar(const Located<std::string>& s, const Kind& k);
};

std::optional<Type> filled_meta_type_var(const Type& t);

inline std::ostream& operator<<(std::ostream& o,const Type& t)
{
    return o<<t.print();
}

enum class typevar_info {other, rigid};

struct TypeVar
{
    Located<std::string> name;
    std::optional<int> index;

    std::optional<Kind> kind;

    typevar_info info = typevar_info::other;

    bool operator==(const TypeVar&) const;
    bool operator<(const TypeVar&) const;

    std::string print() const;
    std::string print_with_kind() const;

    TypeVar() = default;
    TypeVar(const Located<std::string>& s):name(s) {}
    TypeVar(const Located<std::string>& s, const Kind& k):name(s),kind(k) {}
};

    
struct TypeCon
{
    Located<std::string> name;
    std::optional<Kind> kind;

    bool operator==(const TypeCon&) const;
    bool operator<(const TypeCon&) const;

    std::string print() const;
    std::string print_with_kind() const;

    TypeCon() = default;
    TypeCon(const Located<std::string>& s):name(s) {}
    TypeCon(const Located<std::string>& s, const Kind& k):name(s),kind(k) {}
};

struct Context
{
    std::vector<Type> constraints;

    bool operator==(const Context& t) const;

    std::string print() const;
    Context() = default;
    Context(const std::vector<Type>& cs): constraints(cs) {}
};


struct TupleType
{
    std::vector<Type> element_types;

    bool operator==(const TupleType& t) const;

    std::string print() const;
    TupleType(const std::vector<Type>& v):element_types(v) {
        assert(v.size() != 1);
    }
};

Type tuple_type(const std::vector<Type>& v);

struct ListType
{
    Type element_type;

    bool operator==(const ListType& t) const;

    std::string print() const;
    ListType(const Type& t):element_type(t) {};
};

struct TypeApp
{
    Type head;
    Type arg;

    bool operator==(const TypeApp& t) const;

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

struct ForallType
{
    std::vector<TypeVar> type_var_binders;
    Type type;

    bool operator==(const ForallType& t) const;
    std::string print() const;
    ForallType(const std::vector<TypeVar>& tvb, const Type& t):type_var_binders(tvb), type(t) {}
};

Type add_forall_vars(const std::vector<TypeVar>& type_vars, const Type& type);

struct ConstrainedType
{
    Context context;
    Type type;

    bool operator==(const ConstrainedType& t) const;
    std::string print() const;
    ConstrainedType(const Context& c, const Type& t):context(c), type(t) {}
};

Type add_constraints(const std::vector<Type>& constraints, const Type& type);
Type add_constraints(const Context& constraints, const Type& type);

enum class StrictLazy {strict,lazy};

struct StrictLazyType
{
    StrictLazy strict_lazy;
    Type type;

    bool operator==(const StrictLazyType& t) const;
    std::string print() const;
    StrictLazyType(const StrictLazy& sl, const Type& t):strict_lazy(sl), type(t) {}
};

struct Implication
{
    // Add a level number l.
    // The givens  can have type vars with level <  l.
    // The wanteds can have type vars with level <= l.

    // Variables that we are allowed to unify.
    std::set<TypeVar> touchable_vars;

    // Pointer to a set of evidence bindings.
    // We can append new bindings to this object.
    std::shared_ptr<Core::Decls> evidence_binds;

    Context givens;
    Context wanteds;

    bool operator==(const Implication& t) const;

    std::string print() const;
    Implication(const Context& g, const Context& w);
};


Type function_type(const std::vector<Type>& arg_types, const Type& result_type);

Type make_arrow_type(const Type& t1, const Type& t2);

Type make_equality_constraint(const Type& t1, const Type& t2);

std::pair<Type,std::vector<Type>> decompose_type_apps(Type t);

std::optional<std::pair<Type,Type>> is_gen_function_type(const Type& t);

std::optional<std::pair<Type,Type>> is_function_type(const Type& t);

std::optional<std::pair<Type,Type>> is_equality_constraint(const Type& t);

std::optional<Type> is_list_type(const Type& t);

std::optional<std::vector<Type>> is_tuple_type(const Type& t);

Type remove_top_gen(Type type);

// no foralls
bool is_tau_type(const Type& type);

// no foralls at top level
bool is_rho_type(const Type& type);

typedef Type TauType;
typedef Type RhoType;
typedef Type SigmaType;
}

namespace Hs = Haskell;

#endif 
