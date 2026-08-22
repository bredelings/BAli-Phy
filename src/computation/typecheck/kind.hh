#ifndef KIND_H
#define KIND_H

#include "computation/object.hh"
#include <string>
#include <optional>
#include <map>
#include "computation/haskell/haskell.hh"
#include "computation/core/type.hh"

namespace Haskell
{
    typedef ::Kind Kind;
}

namespace Hs = Haskell;

TypeCon kind_kind();
TypeCon kind_type();
TypeCon kind_constraint();
Hs::Kind kind_arrow(const Hs::Kind& k1, const Hs::Kind& k2);
Hs::Kind function_kind(const std::vector<Hs::Kind>& arg_kinds, const Hs::Kind result_kind);

bool is_kind_type(const Kind& k);
bool is_kind_constraint(const Hs::Kind& k);

Hs::Kind make_n_args_kind(int n);

Hs::Kind make_n_args_constraint_kind(int n);

Hs::Kind default_kind(const Hs::Kind& k);

std::optional<std::pair<std::vector<Hs::Kind>,Hs::Kind>> arg_and_result_kinds(int n, const Hs::Kind& kind);

int num_args_for_kind(const Hs::Kind& k);

#endif /*KIND_H*/
