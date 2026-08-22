#ifndef MODELS_PARSE_H
#define MODELS_PARSE_H

#include <set>
#include <string>
#include "models/model-expr.hh"
#include "models/model-type.hh"

class Rules;

std::string unparse(const CM::UntypedExpr&);
std::string unparse(const CM::UntypedPattern&);
std::string unparse(const CM::Decls<CM::NoAnn>&);
std::string unparse_annotated(const CM::TypedExpr&);
std::string unparse_annotated(const CM::TypedPattern&);
std::string show_model(const CM::UntypedExpr&);
std::string show_model_annotated(const CM::TypedExpr&);

CM::Type parse_type(const std::string& s);

void resolve_model_fixities(CM::UntypedExpr&, const Rules&, const std::string& source, const std::string& what,
                            const std::set<std::string>& bound_names = {});
CM::UntypedExpr parse_model_expr(const Rules& R, const std::string& s, const std::string& what,
                                 const std::set<std::string>& bound_names = {});
CM::Decls<CM::NoAnn> parse_model_decls(const Rules& R, const std::string& s,
                                       const std::set<std::string>& bound_names = {});

#endif
