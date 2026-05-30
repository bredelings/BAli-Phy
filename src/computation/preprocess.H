#ifndef PREPROCESS_H
#define PREPROCESS_H

#include "computation/core/ast.H"
#include "computation/fresh_vars.H"
#include "computation/runtime/ast.H"

Runtime::ExpPtr runtime_prepare_for_translation(FreshVarSource&, const Core2::Exp<>&);

Runtime::ExpPtr runtime_prepare_for_translation(FreshVarState&, const Core2::Exp<>&);

#endif
