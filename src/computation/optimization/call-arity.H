#ifndef CALL_ARITY_H
#define CALL_ARITY_H

#include "computation/core/ast.H"

class Module;

// Infer transient call arities for non-recursive Core bindings.
Core::Binds<> call_arity_analyze(const Module& module, const Core::Binds<>& binds);

#endif
