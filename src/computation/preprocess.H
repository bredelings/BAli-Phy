#ifndef PREPROCESS_H
#define PREPROCESS_H

#include <map>
#include <string>
#include <vector>
#include "computation/core/ast.H"
#include "computation/fresh_vars.H"
#include "computation/runtime/ast.H"

Runtime::Exp runtime_prepare_for_translation(FreshVarSource&, const Core::Exp<>&);

Runtime::Exp runtime_prepare_for_translation(FreshVarState&, const Core::Exp<>&);

namespace Runtime
{
    Exp untranslate_vars(const Exp&, const std::map<int, std::string>&);
    Exp untranslate_vars(const Exp&, const std::map<std::string, int>&);
}

struct closure;

Core::Exp<> runtime_deindexify(const Runtime::Exp&, const std::vector<Core::Var<>>&);

Core::Exp<> runtime_deindexify(const Runtime::Exp&);

Core::Exp<> runtime_deindexify(const closure&);

Core::Exp<> runtime_unprepare_for_translation(const Runtime::Exp&, const std::map<int, std::string>& = {});

Core::Exp<> runtime_unprepare_for_translation(const Runtime::Exp&, const std::map<std::string, int>&);

#endif
