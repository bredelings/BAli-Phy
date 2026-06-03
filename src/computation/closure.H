#ifndef CLOSURE_H
#define CLOSURE_H

#include <vector>
#include <string>
#include <memory>
#include <boost/container/small_vector.hpp>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include "computation/runtime/ast.H"

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

    // Constructing from an expression_ref
    closure(const expression_ref& e)
	:closure()
	{
            if (e)
                set_expression(Runtime::ObjectValue(e));
        }

    closure(const expression_ref& e, const Env_t& E)
	:closure(e)
	{ Env = E; }

    closure(const expression_ref& e, std::initializer_list<int> E)
	:closure(e, Env_t(E))
	{ }

    // Constructing w/ an environment
    closure(Object* o, const Env_t& E)
	:closure(expression_ref(o),E)
	{ }

    closure(const object_ptr<const Object>& o, const Env_t& E)
	:closure(expression_ref(o),E)
	{ }

    template <typename T>
    closure(const object_ptr<T>& o, const Env_t& E)
	:closure(expression_ref(o),E)
	{ }

    closure(const Object& O, const Env_t& E):closure(expression_ref(O),E) { }

    closure(const Object& O, std::initializer_list<int> E):closure(O,Env_t(E)) { }

    // Constructing w/o an environment
    closure(Object* o)
	:closure(expression_ref(o))
	{ }

    closure(const object_ptr<const Object>& o)
	:closure(expression_ref(o))
	{ }

    template <typename T>
    closure(const object_ptr<T>& o)
	:closure(expression_ref(o))
	{ }

    closure(const Object& O)
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
