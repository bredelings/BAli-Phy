#ifndef CLOSURE_H
#define CLOSURE_H

#include <vector>
#include <string>
#include <utility>
#include <boost/container/small_vector.hpp>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include "computation/expression/bool.H"
#include "computation/runtime/ast.H"

struct closure
{
private:
    expression_ref exp_cache;

    /// The runtime expression
    Runtime::Exp code;

public:
    typedef boost::container::small_vector<int,10> Env_t;
  
    /// Legacy expression_ref view derived from code, or set through set_legacy_exp().
    const expression_ref& exp;
    /// The environment (regs bound to free variables of E).
    Env_t Env;

    /// Is the closure not empty
    operator bool() const {return exp and exp.head();}

    bool has_code() const { return not code.empty(); }

    const Runtime::Exp& get_code() const {return code; }

    void set_code(Runtime::Exp e);

    void set_legacy_exp(expression_ref e);

    bool operator==(const closure& C2) const {return exp == C2.exp and Env == C2.Env;}

    // Alternatively I could return std::variant<int,expression_ref>
    expression_ref arg_for_slot(int i) const
    {
        assert(i < exp.sub().size());
        auto& E = exp.sub()[i];
        if (E.is_index_var())
        {
            int index = E.as_index_var();
            int r = lookup_in_env(index);
            return expression_ref(reg_var(r));
        }
        else
            return E;
    }

    int reg_for_slot(int i) const
    {
        assert(i < exp.sub().size());
        auto& E = exp.sub()[i];
        if (E.is_index_var())
            return lookup_in_env(E.as_index_var());
        else if (E.is_reg_var())
            return E.as_reg_var();
        else
            std::abort();
    }

    bool is_reg_ref() const
    {
        return exp.is_index_var() or exp.is_reg_var();
    }

    int reg_for_ref() const
    {
        if (exp.is_index_var())
            return lookup_in_env(exp.as_index_var());
        else if (exp.is_reg_var())
            return exp.as_reg_var();
        else
            std::abort();
    }

    int lookup_in_env(int i) const;

    /// Clear the closure.
    void clear();

    std::string print() const;

    closure& operator=(const closure& C)
    {
        exp_cache = C.exp_cache;
        code = C.code;
        Env = C.Env;
        return *this;
    }

    closure& operator=(closure&& C) noexcept
    {
        exp_cache = std::move(C.exp_cache);
        code = std::move(C.code);
        Env = std::move(C.Env);
        return *this;
    }

    // Default
    closure():exp(exp_cache) {}

    closure(const closure& C)
        :exp_cache(C.exp_cache),
         code(C.code),
         exp(exp_cache),
         Env(C.Env)
    { }

    closure(closure&& C) noexcept
        :exp_cache(std::move(C.exp_cache)),
         code(std::move(C.code)),
         exp(exp_cache),
         Env(std::move(C.Env))
    { }

    // Constructing from an expression_ref
    closure(const expression_ref& e)
	:closure()
    {
        set_legacy_exp(e);
    }

    closure(const expression_ref& e, const Env_t& E)
	:closure(e)
    {
        Env = E;
    }

    closure(const expression_ref& e, std::initializer_list<int> E)
	:closure(e, Env_t(E))
    { }

    // Constructing from an expression_ref
    closure(Runtime::Exp e)
        :closure()
    {
        set_code(std::move(e));
    }

    closure(Runtime::Exp e, const Env_t& E)
	:closure(std::move(e))
    {
        Env = E;
    }

    closure(Runtime::Exp e, std::initializer_list<int> E)
	:closure(std::move(e), Env_t(E))
    { }

    // Builtin-type arguments
    template <typename T>
    requires std::same_as<std::remove_cvref_t<T>, int>
    closure(T x):closure(Runtime::Int(x)) {}

    template <typename T>
    requires std::same_as<std::remove_cvref_t<T>, char>
    closure(T x):closure(Runtime::Char(x)) {}

    template <typename T>
    requires std::same_as<std::remove_cvref_t<T>, double>
    closure(T x):closure(Runtime::Double(x)) {}

    template <typename T>
    requires std::same_as<std::remove_cvref_t<T>, log_double_t>
    closure(T x):closure(Runtime::LogDouble(x)) {}

    template <typename T>
    requires std::same_as<std::remove_cvref_t<T>, bool>
    closure(T x):closure(Runtime::Constructor(constructor(x?bool_true_name:bool_false_name,0))) {}

    closure(std::string x):closure(Runtime::String(std::move(x))) {}

    closure(integer x):closure(Runtime::Integer(std::move(x))) {}

    closure(const Box<std::string>& x):closure(Runtime::String(x.value())) {}

    closure(Box<std::string>&& x):closure(Runtime::String(x.value())) {}

    closure(const Box<integer>& x):closure(Runtime::Integer(x)) {}

    closure(Box<integer>&& x):closure(Runtime::Integer(std::move(x))) {}

    closure(constructor x):closure(Runtime::Constructor(std::move(x))) {}

    closure(const index_var& iv):closure(Runtime::IndexVar(iv.index)) {}

    closure(const index_var& iv, const Env_t& E):closure(Runtime::IndexVar(iv.index), E) {}

    closure(const index_var& iv, std::initializer_list<int> E):closure(Runtime::IndexVar(iv.index), E) {}

    closure(const reg_var& rv):closure(Runtime::RegRef(rv.target)) {}

    // Constructing w/o an environment
    closure(Object* o)
        :closure()
    {
        set_code(Runtime::ObjectValue(o));
    }

    closure(const object_ptr<const Object>& o)
        :closure()
    {
        set_code(Runtime::ObjectValue(o));
    }

    template <typename T>
    closure(const object_ptr<T>& o)
        :closure()
    {
        set_code(Runtime::ObjectValue(o));
    }

    closure(const Object& O)
        :closure()
    {
        set_code(Runtime::ObjectValue(O.clone()));
    }
};

inline int lookup_in_env(const closure::Env_t& Env, int i) {return Env[Env.size() - 1 - i];}

inline int closure::lookup_in_env(int i) const {return ::lookup_in_env(Env,i);}

closure get_trimmed(const closure& C);

closure get_trimmed(closure&& C);

void do_trim(closure& C);

closure trim_unnormalize(const closure& C);

expression_ref deindexify(const closure& C);

#endif
