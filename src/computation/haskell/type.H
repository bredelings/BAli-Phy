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
#include "coretype.H" // for Kind

namespace Haskell
{
    class TypeVar;
    struct TypeCon;
    struct TupleType;
    struct ListType;
    struct TypeApp;
    struct ConstrainedType;
    struct ForallType;
    struct StrictType;
    struct LazyType;
    struct FieldDecls;
    struct TypeOfKind;

    struct Type
    {
        std::variant
        <
            std::monostate,
            cow_ptr<TypeVar>,
            cow_ptr<TypeCon>,
            cow_ptr<TupleType>,
            cow_ptr<ListType>,
            cow_ptr<TypeApp>,
            cow_ptr<ConstrainedType>,
            cow_ptr<ForallType>,
            cow_ptr<StrictType>,
            cow_ptr<LazyType>,
            cow_ptr<FieldDecls>,
            cow_ptr<TypeOfKind>
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
        Type(const T& t):type_ptr( make_cow_ptr<T>(t) ) {}

        ~Type() = default;
    };

    typedef Located<Type> LType;

inline std::ostream& operator<<(std::ostream& o,const Type& t)
{
    return o<<t.print();
}

class TypeVar
{
public:
    std::string name;
    std::optional<int> index;

    std::optional<Kind> kind;

    bool operator==(const TypeVar&) const;
    bool operator<(const TypeVar&) const;

    std::string print() const;
    std::string print_with_kind() const;

    TypeVar();
    TypeVar(const std::string& s);
    TypeVar(int l, const std::string& s);
    TypeVar(const std::string& s, const Kind& k);
    TypeVar(int l, const std::string& s, const Kind& k);
};

typedef Located<TypeVar> LTypeVar;

struct TypeCon
{
    std::string name;
    std::optional<Kind> kind;

    bool operator==(const TypeCon&) const;
    bool operator<(const TypeCon&) const;

    std::string print() const;
    std::string print_with_kind() const;

    TypeCon() = default;
    TypeCon(const std::string& s):name(s) {}
    TypeCon(const std::string& s, const Kind& k):name(s),kind(k) {}
};

typedef Located<TypeCon> LTypeCon;

struct Context: public std::vector<LType>
{
    bool operator==(const Context& t) const;

    std::string print() const;
    Context() = default;
    Context(const std::vector<LType>& cs): std::vector<LType>(cs) {}
};


struct TupleType
{
    std::vector<LType> element_types;

    bool operator==(const TupleType& t) const;

    std::string print() const;
    TupleType(const std::vector<LType>& v):element_types(v) {
        assert(v.size() != 1);
    }
};

Type tuple_type(const std::vector<LType>& v);

struct ListType
{
    LType element_type;

    bool operator==(const ListType& t) const;

    std::string print() const;
    ListType(const LType& t):element_type(t) {};
};

struct TypeApp
{
    LType head;
    LType arg;

    bool operator==(const TypeApp& t) const;

    std::string print() const;
    TypeApp(const LType& t1, const LType& t2):head(t1), arg(t2) {}
};

LType type_apply(const std::vector<LType>& tyapps);
LType type_apply(const LType& T0, const std::vector<LType>& args);

template <typename T>
LType type_apply(const LType& T0, const std::vector<T>& args)
{
    std::vector<LType> type_args;
    for(auto& arg: args)
        type_args.push_back(arg);

    return type_apply(T0, type_args);
}

struct ForallType
{
    std::vector<LTypeVar> type_var_binders;
    LType type;

    bool operator==(const ForallType& t) const;
    std::string print() const;
    ForallType(const std::vector<LTypeVar>& tvb, const LType& t):type_var_binders(tvb), type(t) {}
};

LType add_forall_vars(const std::vector<LTypeVar>& type_vars, const LType& type);

struct ConstrainedType
{
    Context context;
    LType type;

    bool operator==(const ConstrainedType& t) const;
    std::string print() const;
    ConstrainedType(const Context& c, const LType& t):context(c), type(t) {}
};

LType add_constraints(const std::vector<LType>& constraints, const LType& type);

struct StrictType
{
    LType type;

    bool operator==(const StrictType& t) const;
    std::string print() const;
    StrictType(const LType& t):type(t) {}
};

struct LazyType
{
    LType type;

    bool operator==(const LazyType& t) const;
    std::string print() const;
    LazyType(const LType& t):type(t) {}
};

struct TypeOfKind
{
    LType type;
    Kind kind;

    bool operator==(const TypeOfKind& t) const;
    std::string print() const;
    TypeOfKind(const LType& t, const Kind& k):type(t), kind(k) {}
};

std::string parenthesize_type(const LType& t, bool parenthesize_type_apps);

std::pair<LType,std::vector<LType>> decompose_type_apps(LType t);

std::optional<std::pair<LType,LType>> is_function_type(const LType& t);

std::optional<std::pair<LType,LType>> is_gen_function_type(const LType& t);

int gen_type_arity(LType t);

LType make_arrow_type(const LType& t1, const LType& t2);
LType function_type(const std::vector<LType>& arg_types, const LType& result_type);
}

namespace Hs = Haskell;

namespace std
{
    template <>
    class hash < Hs::TypeVar >{
    public :
        size_t operator()(const Hs::TypeVar &x) const
        {
            return std::hash<std::string>()(x.name);
        }
    };

    template <>
    class hash < Hs::LTypeVar >{
    public :
        size_t operator()(const Hs::LTypeVar &x) const
        {
            return std::hash<std::string>()(unloc(x).name);
        }
    };
}

#endif 
