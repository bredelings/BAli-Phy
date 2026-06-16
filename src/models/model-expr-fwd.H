#ifndef MODELS_MODEL_EXPR_FWD_H
#define MODELS_MODEL_EXPR_FWD_H

#include <utility>
#include <string>
#include <variant>
#include <vector>

using UntypedAnn = std::monostate;

template<class A>
struct ModelExpr;

template<class A>
struct ModelArg;

template<class A>
using ModelDecls = std::vector<std::pair<std::string, ModelExpr<A>>>;

using UntypedModelExpr = ModelExpr<UntypedAnn>;

struct ModelAnn;

using TypedModelExpr = ModelExpr<ModelAnn>;
using TypedModelDecls = ModelDecls<ModelAnn>;

#endif
