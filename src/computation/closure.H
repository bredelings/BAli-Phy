#ifndef CLOSURE_H
#define CLOSURE_H

#include <vector>
#include <string>
#include "object.H"
#include "computation/expression.H"

inline int lookup_in_env(const std::vector<int>& Env, int i) {return Env[Env.size() - 1 - i];}

struct closure
{
  /// The expression
  expression_ref exp;
  /// The environment (regs bound to free variables of E).
  std::vector<int> Env;

  /// Is the closure not empty
  operator bool() const {return exp;}

  bool operator==(const closure& C2) const {return exp->maybe_equals(*C2.exp) and Env == C2.Env;}

  int lookup_in_env(int i) const {return ::lookup_in_env(Env,i);}

  /// Clear the closure.
  void clear();

  std::string print() const;

  // Default
  closure() {}

  // Constructing from an expression_ref
  closure(const expression_ref& e)
    :exp(e)
  { }

  closure(const expression_ref& e, const std::vector<int>& E)
    :exp(e), Env(E)
  { }

  // Constructing w/ an environment
  closure(Object* o, const std::vector<int>& E)
    :closure(expression_ref(o),E)
  { }

  closure(const object_ptr<const Object>& o, const std::vector<int>& E)
    :closure(expression_ref(o),E)
  { }

  template <typename T>
  closure(const object_ptr<T>& o, const std::vector<int>& E)
    :closure(expression_ref(o),E)
  { }

  closure(const Object& O, const std::vector<int>& E):closure(expression_ref(O),E) { }

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

  closure(const closure&) = default;

  closure(closure&& C)
  :exp(C.exp),
   Env( std::move(C.Env) )
  { }

  closure& operator=(const closure&) = default;

  closure& operator=(closure&& C)
  {
    exp = C.exp;
    Env = std::move(C.Env);
    return *this;
  }
};

closure get_trimmed(const closure& C);

closure trim_unnormalize(closure C);

expression_ref deindexify(const closure& C);

#endif
