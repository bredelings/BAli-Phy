#ifndef CLOSURE_H
#define CLOSURE_H

#include <string>
#include <utility>
#include <boost/container/small_vector.hpp>
#include "computation/object.H"
#include "computation/haskell/ids.H"  // for bool_true_name, bool_false_name
#include "computation/runtime/ast.H"

struct closure
{
private:
    /// The runtime expression
    Runtime::Exp code;

public:
    typedef boost::container::small_vector<int,10> Env_t;
    /// The environment (regs bound to free variables of E).
    Env_t Env;

    /// Is the closure not empty
    operator bool() const {return bool(code);}

    bool has_code() const { return not code.empty(); }

    const Runtime::Exp& get_code() const {return code; }

    void set_code(Runtime::Exp e);

    bool operator==(const closure& C2) const {return code == C2.code and Env == C2.Env;}

    const Runtime::Exp& function_head_ref() const { return code.function_head(); }
    const Runtime::Exp& function_arg_ref(int i) const { return code.function_arg(i); }
    const Runtime::Exp& constructor_slot_ref(int i) const { return code.constructor_arg(i); }
    const Runtime::Exp& operation_slot_ref(int i) const { return code.operation_arg(i); }

    Runtime::Exp constructor_slot(int i) const;
    Runtime::Exp operation_slot(int i) const;

    int reg_for_function_arg(int i) const;
    int reg_for_constructor_slot(int i) const;
    int reg_for_operation_slot(int i) const;

    int n_function_args() const { return code.num_function_args(); }
    int n_constructor_slots() const { return code.num_constructor_args(); }
    int n_operation_slots() const { return code.num_operation_args(); }

    bool is_reg_ref() const
    {
        return code.to<Runtime::IndexVar>() or code.to<Runtime::RegRef>();
    }

    Runtime::Exp slot_for_code(const R::Exp& E) const
    {
        if (auto index_var = E.to<Runtime::IndexVar>())
            return Runtime::RegRef(lookup_in_env(index_var->index));
        else
            return E;
    }

    std::optional<int> reg_for_code(const R::Exp& E) const
    {
        if (auto index_var = E.to<Runtime::IndexVar>())
        {
            assert(index_var->index < Env.size());

            return lookup_in_env(index_var->index);
        }
        else if (auto reg_ref = E.to<Runtime::RegRef>())
            return reg_ref->target;
        else
            return {};
    }

    std::optional<int> reg_for_ref_maybe() const
    {
        return reg_for_code(code);
    }

    int reg_for_ref() const
    {
        if (auto r = reg_for_ref_maybe())
            return *r;

        std::abort();
    }

    int lookup_in_env(int i) const;

    /// Clear the closure.
    void clear();

    std::string print() const;

    closure& operator=(const closure& C)
    {
        code = C.code;
        Env = C.Env;
        return *this;
    }

    closure& operator=(closure&& C) noexcept
    {
        code = std::move(C.code);
        Env = std::move(C.Env);
        return *this;
    }

    // Default
    closure() = default;

    closure(const closure& C)
        :code(C.code),
         Env(C.Env)
    { }

    closure(closure&& C) noexcept
        :code(std::move(C.code)),
         Env(std::move(C.Env))
    { }

    // Constructing from runtime code.
    closure(Runtime::Exp e)
        :code(std::move(e))
    {
    }

    closure(Runtime::Exp e, const Env_t& E)
        :code(std::move(e)),
         Env(E)
    {
    }

    closure(Runtime::Exp e, std::initializer_list<int> E)
        :code(std::move(e)),
         Env(E)
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
    closure(T x):closure(Runtime::ConstructorApp(x ? bool_true_name : bool_false_name, 0, {})) {}

    closure(std::string x):closure(Runtime::String(std::move(x))) {}

    closure(integer x):closure(Runtime::Integer(std::move(x))) {}

    closure(const Box<std::string>& x) = delete;

    closure(const Box<integer>& x) = delete;

    // Constructing w/o an environment
    closure(Object* o)
        :closure()
    {
        set_code(Runtime::Exp(o));
    }

    closure(const object_ptr<const Object>& o)
        :closure()
    {
        set_code(Runtime::Exp(o));
    }

    template <typename T>
    closure(const object_ptr<T>& o)
        :closure()
    {
        set_code(Runtime::Exp(o));
    }

    closure(const Object& O)
        :closure()
    {
        set_code(Runtime::Exp(O));
    }
};

inline int lookup_in_env(const closure::Env_t& Env, int i) {return Env[Env.size() - 1 - i];}

inline int closure::lookup_in_env(int i) const {return ::lookup_in_env(Env,i);}

closure get_trimmed(const Runtime::Exp& code, const closure::Env_t& Env);

closure trim_unnormalize(const closure& C);

#endif
