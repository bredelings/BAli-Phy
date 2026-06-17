#ifndef MODELS_PARSE_H
#define MODELS_PARSE_H

#include <string>
#include "models/model-expr.H"
#include "models/model-type.H"

class Rules;

std::string unparse(const CM::UntypedExpr&);
std::string unparse(const CM::UntypedPattern&);
std::string unparse(const CM::Decls<CM::NoAnn>&);
std::string unparse_annotated(const CM::TypedExpr&);
std::string unparse_annotated(const CM::TypedPattern&);
std::string show_model(const CM::UntypedExpr&);
std::string show_model_annotated(const CM::TypedExpr&);

CM::Type parse_type(const std::string& s);

CM::UntypedExpr parse_model_expr(const Rules& R, const std::string& s, const std::string& what);
CM::Decls<CM::NoAnn> parse_model_decls(const Rules& R, const std::string& s);

#endif
