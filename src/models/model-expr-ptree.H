#ifndef MODELS_MODEL_EXPR_PTREE_H
#define MODELS_MODEL_EXPR_PTREE_H

#include "models/model-expr.H"
#include "util/ptree.H"

namespace CmdModel
{

UntypedExpr model_expr_from_ptree(const ptree&);
ptree ptree_from_model_expr(const UntypedExpr&);

Decls<NoAnn> model_decls_from_ptree(const ptree&);
ptree ptree_from_model_decls(const Decls<NoAnn>&);

TypedExpr typed_model_expr_from_annotated_ptree(const ptree&);
ptree annotated_ptree_from_typed_model_expr(const TypedExpr&);

TypedDecls typed_model_decls_from_annotated_ptree(const ptree&);
ptree annotated_ptree_from_typed_model_decls(const TypedDecls&);

}

#endif
