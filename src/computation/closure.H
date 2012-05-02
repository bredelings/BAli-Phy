#ifndef CLOSURE_H
#define CLOSURE_H

#include <vector>
#include <string>
#include "object.H"

inline int lookup_in_env(const std::vector<int>& Env, int i) {return Env[Env.size() - 1 - i];}

struct closure
{
  /// The expression
  object_ptr<const Object> exp;
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

  // Constructing w/ an environment
  closure(Object* o, const std::vector<int>& E)
    :exp(o), Env(E)
  { }

  closure(const object_ptr<const Object>& o, const std::vector<int>& E)
    :exp(o), Env(E)
  { }

  template <typename T>
  closure(const object_ptr<T>& o, const std::vector<int>& E)
    :exp(boost::static_pointer_cast<const Object>(o)), Env(E)
  { }

  closure(const Object& O, const std::vector<int>& E):exp(O.clone()), Env(E) { }

  // Constructing w/ an environment
  closure(Object* o)
    :closure(o,{})
  { }

  closure(const object_ptr<const Object>& o)
    :closure(o,{})
  { }

  template <typename T>
  closure(const object_ptr<T>& o)
    :closure(o,{})
  { }

  closure(const Object& O)
    :closure(O,{})
  { }


};

closure get_trimmed(const closure& C);

#endif
