#ifndef HASKELL_CORETYPE
#define HASKELL_CORETYPE

#include <variant>
#include <iostream>
#include <set>

#include "util/cow-ptr.H"

// Can we eliminate this?
#include "computation/expression/expression_ref.H"
#include "core.H"

#include <cereal/types/memory.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/variant.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/types/optional.hpp>

class MetaTypeVar;
class TypeVar;
struct TypeCon;
struct TypeApp;
struct ConstrainedType;
struct ForallType;

struct Type
{
    std::variant
    <
        std::monostate,
        cow_ptr<MetaTypeVar>,
        cow_ptr<TypeVar>,
        cow_ptr<TypeCon>,
        cow_ptr<TypeApp>,
        cow_ptr<ConstrainedType>,
        cow_ptr<ForallType>
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

    template <class Archive>
    void serialize(Archive& ar);

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

typedef Type Kind;

class MetaTypeVar
{
    int level_;

    std::shared_ptr<Type> indirect;
public:

    std::string name;
    std::optional<int> index;
    bool cycle_breaker = false;

    Kind kind;

    std::optional<Type> filled() const;
    void fill(const Type& t) const;
    void clear() const;

    bool operator==(const MetaTypeVar&) const;
    bool operator<(const MetaTypeVar&) const;

    std::string print() const;
    std::string print_with_kind() const;

    int level() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(level_, indirect, name, index, cycle_breaker, kind);
    }

    MetaTypeVar(int level, const std::string& s, const Kind& k);

private:
    friend cereal::access;
    MetaTypeVar() = default;
};

std::optional<Type> filled_meta_type_var(const Type& t);

std::optional<int> unfilled_meta_type_var(const Type& t);

inline std::ostream& operator<<(std::ostream& o,const Type& t)
{
    return o<<t.print();
}

class TypeVar
{
    std::optional<int> level_;

public:
    std::string name;
    std::optional<int> index;

    Kind kind;

    bool is_skolem_constant() const;

    bool operator==(const TypeVar&) const;
    bool operator<(const TypeVar&) const;

    std::string print() const;
    std::string print_with_kind() const;

    int level() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(level_, name, index, kind);
    }

    TypeVar();
    TypeVar(const std::string& s, const Kind& k);
    TypeVar(int l, const std::string& s, const Kind& k);
};

typedef TypeVar KindVar;

struct TypeCon
{
    std::string name;
    std::optional<Kind> kind;

    bool operator==(const TypeCon&) const;
    bool operator<(const TypeCon&) const;

    std::string print() const;
    std::string print_with_kind() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(name, kind);
    }

    TypeCon() = default;
    TypeCon(const std::string& s):name(s) {}
    TypeCon(const std::string& s, const Kind& k):name(s),kind(k) {}
};

struct Context: public std::vector<Type>
{
    bool operator==(const Context& t) const;

    std::string print() const;

    Context() = default;
    Context(const std::vector<Type>& cs): std::vector<Type>(cs) {}
};


struct TypeApp
{
    Type head;
    Type arg;

    bool operator==(const TypeApp& t) const;

    std::string print() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(head,arg);
    }

    TypeApp(const Type& t1, const Type& t2):head(t1), arg(t2) {}
private:
    friend cereal::access;

    TypeApp() = default;
};

struct ForallType
{
    std::vector<TypeVar> type_var_binders;
    Type type;

    bool operator==(const ForallType& t) const;
    std::string print() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(type_var_binders, type);
    }

    ForallType(const std::vector<TypeVar>& tvb, const Type& t):type_var_binders(tvb), type(t) {}

private:
    friend cereal::access;

    ForallType() = default;
};

Type add_forall_vars(const std::vector<TypeVar>& type_vars, const Type& type);

struct ConstrainedType
{
    Context context;
    Type type;

    bool operator==(const ConstrainedType& t) const;
    std::string print() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(context, type);
    }

    ConstrainedType(const Context& c, const Type& t):context(c), type(t) {}

private:
    friend cereal::access;

    ConstrainedType() = default;
};

Type add_constraints(const std::vector<Type>& constraints, const Type& type);

template <class Archive>
void Type::serialize(Archive& ar)
{
    ar(type_ptr);
}

struct TypeFamilyInstanceDecl
{
    TypeCon con;
    std::vector<Type> args;
    Type rhs;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(con, args, rhs);
    }
};

Type function_type(const std::vector<Type>& arg_types, const Type& result_type);

Type make_arrow_type(const Type& t1, const Type& t2);

Type make_equality_pred(const Type& t1, const Type& t2);

std::string parenthesize_type(Type t, bool parenthesize_type_app);

std::pair<Type,std::vector<Type>> decompose_type_apps(Type t);

Type type_apply(const std::vector<Type>& args);

Type type_apply(Type t, const std::vector<Type>& args);

Type type_apply(Type t, const std::vector<TypeVar>& args);

template <typename T>
Type type_apply(const Type& T0, const std::vector<T>& args)
{
    std::vector<Type> type_args;
    for(auto& arg: args)
        type_args.push_back(arg);

    return type_apply(T0, type_args);
}

Type quantify(const std::vector<TypeVar>& type_vars, const std::vector<Type>& constraints, const Type& type);

std::optional<std::pair<Type,Type>> is_gen_function_type(const Type& t);

std::optional<std::pair<Type,Type>> is_function_type(const Type& t);

int gen_type_arity(Type t);

int type_arity(Type t);

std::pair<std::vector<Type>,Type> gen_arg_result_types(const Type&);

std::pair<std::vector<Type>,Type> arg_result_types(const Type&);

std::optional<std::pair<Type,Type>> is_equality_pred(const Type& t);

std::optional<std::tuple<Type,std::vector<Type>>> is_dictionary_pred(const Type& t);

std::vector<Type> equality_preds(const std::vector<Type> constraints);

std::vector<Type> dictionary_preds(const std::vector<Type> constraints);

std::tuple<std::vector<TypeVar>, std::vector<Type>, Type> peel_top_gen(Type t);

std::optional<Type> is_list_type(Type t);

std::optional<std::vector<Type>> is_tuple_type(Type t);

std::optional<std::tuple<TypeCon,std::vector<Type>>> is_type_con_app(const Type& t);

Type char_type();

Type int_type();

Type integer_type();

Type double_type();

Type list_type(const Type& t);

Type tuple_type(const std::vector<Type>& v);

Type follow_meta_type_var(Type t);

Type remove_top_gen(Type type);

// no foralls
bool is_tau_type(Type type);

// no foralls at top level
bool is_rho_type(Type type);

std::optional<Type> is_IO_type(Type t);

int max_level(Type t);
int max_level(const std::vector<Type>& t);

int max_meta_level(Type t);
int max_meta_level(const std::vector<Type>& t);

typedef Type TauType;
typedef Type RhoType;
typedef Type SigmaType;

TypeCon tuple_tycon(int n);
TypeCon list_tycon();

namespace std
{
    template <>
    class hash < TypeVar >{
    public :
        size_t operator()(const TypeVar &x) const
        {
            return std::hash<std::string>()(x.name);
        }
    };

    template <>
    class hash < MetaTypeVar >{
    public :
        size_t operator()(const MetaTypeVar &x) const
        {
            return std::hash<std::string>()(x.name);
        }
    };
}

std::optional< std::tuple<TypeCon, Type, Type> > is_type_op(const Type& t);
#endif 
