#ifndef KIND_H
#define KIND_H

#include "computation/object.H"
#include <string>
#include <optional>
#include <map>
#include "computation/haskell/haskell.H"
#include "computation/haskell/coretype.H"
#include "computation/typecheck/substitution.H"

namespace Haskell
{
    typedef ::Kind Kind;
}

namespace Hs = Haskell;

TypeCon kind_type();
TypeCon kind_constraint();
Hs::Kind kind_arrow(const Hs::Kind& k1, const Hs::Kind& k2);
Hs::Kind function_kind(const std::vector<Hs::Kind>& arg_kinds, const Hs::Kind result_kind);
TypeVar kind_var(const std::string& s, int i);

bool is_kind_type(const Kind& k);
bool is_kind_constraint(const Hs::Kind& k);

typedef bsubstitution_t k_substitution_t;

// Hs::Kind apply_subst(const k_substitution_t& s, const Hs::Kind& k);

k_substitution_t compose(const k_substitution_t& s1, const k_substitution_t s2);

bool occurs_check(const TypeVar& kv, const Hs::Kind& k);

std::optional<k_substitution_t> kunify(const Hs::Kind& k1, const Hs::Kind& k2);

Hs::Kind make_n_args_kind(int n);

Hs::Kind make_n_args_constraint_kind(int n);

Hs::Kind replace_kvar_with_star(const Hs::Kind& k);

std::optional<std::pair<std::vector<Hs::Kind>,Hs::Kind>> arg_and_result_kinds(int n, const Hs::Kind& kind);

int num_args_for_kind(const Hs::Kind& k);

#endif /*KIND_H*/
