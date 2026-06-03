#ifndef CLOSURE_H
#define CLOSURE_H

#include <vector>
#include <string>
#include <memory>
#include <concepts>
#include <type_traits>
#include <boost/container/small_vector.hpp>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include "computation/expression/bool.H"
#include "computation/runtime/ast.H"
#include "computation/haskell/Integer.H"

struct closure
{
private:
    expression_ref exp_cache_;
    /// Runtime AST representation for this closure.
    Runtime::Exp runtime_exp;

public:
    typedef boost::container::small_vector<int,10> Env_t;
  
    /// Legacy expression_ref view derived from runtime_exp.
    ///
    /// The expression_ref form is a compatibility cache.  Code outside closure
    /// may inspect it, but semantic updates should go through set_expression().
    const expression_ref& exp;
    /// The environment (regs bound to free variables of E).
    Env_t Env;

    /// Is the closure not empty
    operator bool() const {return exp and exp.head();}

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

    int runtime_n_args() const;

    Runtime::Exp runtime_arg_for_slot(int i) const;

    int runtime_reg_for_slot(int i) const;

    const Runtime::Exp& runtime_expression() const {return runtime_exp;}

    void set_expression(Runtime::Exp E);

    void check_runtime_expression() const;

    bool has_structured_runtime_expression() const
    {
        return runtime_exp and not runtime_exp.is<Runtime::ObjectValue>();
    }

    void peel_lambdas(int n);

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

    closure& operator=(closure&& C) noexcept
	{
	    exp_cache_ = std::move(C.exp_cache_);
	    runtime_exp = std::move(C.runtime_exp);
	    Env = std::move(C.Env);
	    return *this;
	}

    closure& operator=(const closure& C)
	{
	    exp_cache_ = C.exp_cache_;
	    runtime_exp = C.runtime_exp;
	    Env = C.Env;
	    return *this;
	}

    // Default
    closure():exp(exp_cache_) {}

    closure(const closure& C)
        :exp_cache_(C.exp_cache_),
         runtime_exp(C.runtime_exp),
         exp(exp_cache_),
	 Env(C.Env)
        { }

    closure(closure&& C) noexcept
	:exp_cache_(std::move(C.exp_cache_)),
         runtime_exp(std::move(C.runtime_exp)),
         exp(exp_cache_),
	 Env( std::move(C.Env) )
	{ }

    // Constructing from a runtime expression
    closure(Runtime::Exp e)
	:closure()
	{ set_expression(std::move(e)); }

    closure(Runtime::Exp e, const Env_t& E)
	:closure(std::move(e))
	{ Env = E; }

    closure(Runtime::Exp e, std::initializer_list<int> E)
	:closure(std::move(e), Env_t(E))
	{ }

    template <typename T>
        requires std::same_as<std::remove_cvref_t<T>, int>
    closure(T x):closure(Runtime::Int(x)) {}

    closure(double x):closure(Runtime::Double(x)) {}

    closure(log_double_t x):closure(Runtime::LogDouble(x)) {}

    closure(char x):closure(Runtime::Char(x)) {}

    template <typename T>
        requires std::same_as<std::remove_cvref_t<T>, bool>
    closure(T x):closure(Runtime::ObjectValue(x ? bool_true : bool_false)) {}

    closure(std::string x):closure(Runtime::String(std::move(x))) {}

    closure(const String& x):closure(Runtime::String(x.value())) {}

    closure(String&& x):closure(Runtime::String(x.value())) {}

    closure(integer x):closure(Runtime::Integer(std::move(x))) {}

    closure(const Integer& x):closure(Runtime::Integer(x.value())) {}

    closure(Integer&& x):closure(Runtime::Integer(x.value())) {}

    closure(constructor x):closure(Runtime::Constructor(std::move(x))) {}

    static closure legacy_expression(expression_ref e)
    {
        return closure(std::move(e));
    }

    static closure legacy_expression(expression_ref e, const Env_t& E)
    {
        return closure(std::move(e), E);
    }

    static closure object_value(expression_ref e)
    {
        return legacy_expression(std::move(e));
    }

    static closure object_value(expression_ref e, const Env_t& E)
    {
        return legacy_expression(std::move(e), E);
    }

    static closure object_value(Object* o)
    {
        return object_value(expression_ref(o));
    }

    static closure object_value(Object* o, const Env_t& E)
    {
        return object_value(expression_ref(o), E);
    }

    static closure object_value(const object_ptr<const Object>& o)
    {
        return object_value(expression_ref(o));
    }

    static closure object_value(const object_ptr<const Object>& o, const Env_t& E)
    {
        return object_value(expression_ref(o), E);
    }

    template <typename T>
    static closure object_value(const object_ptr<T>& o)
    {
        return object_value(expression_ref(o));
    }

    template <typename T>
    static closure object_value(const object_ptr<T>& o, const Env_t& E)
    {
        return object_value(expression_ref(o), E);
    }

    static closure object_value(const Object& O)
    {
        return object_value(expression_ref(O));
    }

    static closure object_value(const Object& O, const Env_t& E)
    {
        return object_value(expression_ref(O), E);
    }

    static closure atomic_value(expression_ref e)
    {
        assert(not e.is_expression());
        return object_value(std::move(e));
    }

    // Constructing from an expression_ref
    explicit closure(const expression_ref& e)
	:closure()
	{
            if (e)
                set_expression(Runtime::ObjectValue(e));
        }

    explicit closure(const expression_ref& e, const Env_t& E)
	:closure(e)
	{ Env = E; }

    explicit closure(const expression_ref& e, std::initializer_list<int> E)
	:closure(e, Env_t(E))
	{ }

    // Constructing w/ an environment
    explicit closure(Object* o, const Env_t& E)
	:closure(expression_ref(o),E)
	{ }

    explicit closure(const object_ptr<const Object>& o, const Env_t& E)
	:closure(expression_ref(o),E)
	{ }

    template <typename T>
    explicit closure(const object_ptr<T>& o, const Env_t& E)
	:closure(expression_ref(o),E)
	{ }

    explicit closure(const Object& O, const Env_t& E):closure(expression_ref(O),E) { }

    explicit closure(const Object& O, std::initializer_list<int> E):closure(O,Env_t(E)) { }

    // Constructing w/o an environment
    explicit closure(Object* o)
	:closure(expression_ref(o))
	{ }

    explicit closure(const object_ptr<const Object>& o)
	:closure(expression_ref(o))
	{ }

    template <typename T>
    explicit closure(const object_ptr<T>& o)
	:closure(expression_ref(o))
	{ }

    explicit closure(const Object& O)
	:closure(expression_ref(O))
	{ }
};

inline int lookup_in_env(const closure::Env_t& Env, int i) {return Env[Env.size() - 1 - i];}

inline int closure::lookup_in_env(int i) const {return ::lookup_in_env(Env,i);}

closure get_trimmed(const closure& C);

closure get_trimmed(closure&& C);

void do_trim(closure& C);

closure trim_unnormalize(const closure& C);

expression_ref deindexify(const closure& C);

#endif
