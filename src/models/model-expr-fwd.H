#ifndef MODELS_MODEL_EXPR_FWD_H
#define MODELS_MODEL_EXPR_FWD_H

#include <utility>
#include <string>
#include <variant>
#include <vector>

namespace CmdModel
{

using NoAnn = std::monostate;

template<class A>
struct Expr;

template<class A>
struct Arg;

template<class A>
using Decls = std::vector<std::pair<std::string, Expr<A>>>;

using UntypedExpr = Expr<NoAnn>;

struct Ann;

using TypedExpr = Expr<Ann>;
using TypedDecls = Decls<Ann>;

}

namespace CM = CmdModel;

#endif
