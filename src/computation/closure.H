#ifndef CLOSURE_H
#define CLOSURE_H

#include <vector>
#include <string>
#include <boost/container/small_vector.hpp>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"

struct closure
{
    typedef boost::container::small_vector<int,10> Env_t;
  
    /// The expression
    expression_ref exp;
    /// The environment (regs bound to free variables of E).
    Env_t Env;

    /// Is the closure not empty
    operator bool() const {return exp and exp.head();}

    bool operator==(const closure& C2) const {return exp == C2.exp and Env == C2.Env;}

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
	int index = exp.sub()[i].as_index_var();
	return lookup_in_env(index);
    }

    int reg_for_index_var() const
    {
	int index = exp.as_index_var();
	return lookup_in_env(index);
    }

    int lookup_in_env(int i) const;

    /// Clear the closure.
    void clear();

    std::string print() const;

    // Assignment operators
    closure& operator=(const closure&) = default;

    closure& operator=(closure&& C) noexcept
	{
	    exp = C.exp;
	    Env = std::move(C.Env);
	    return *this;
	}

    // Default
    closure() = default;

    closure(const closure&) = default;

    closure(closure&& C) noexcept
	:exp(C.exp),
	 Env( std::move(C.Env) )
	{ }

    // Constructing from an expression_ref
    closure(const expression_ref& e)
	:exp(e)
	{ }

    closure(const expression_ref& e, const Env_t& E)
	:exp(e), Env(E)
	{ }

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
