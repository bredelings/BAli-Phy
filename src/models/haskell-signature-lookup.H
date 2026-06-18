#ifndef HASKELL_SIGNATURE_LOOKUP_H
#define HASKELL_SIGNATURE_LOOKUP_H

#include <string>
#include <vector>

#include "computation/haskell/coretype.H"
#include "models/haskell-binding-contexts.H"

struct HaskellValueSignature
{
    std::string resolved_name;
    Type type;
    std::vector<TypeVar> quantified_vars;
    std::vector<Type> constraints;
};

HaskellValueSignature lookup_value_signature(const HaskellBindingContexts&, const BindingImportSet&, const std::string&);

#endif
