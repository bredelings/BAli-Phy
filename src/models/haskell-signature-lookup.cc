#include "models/haskell-signature-lookup.H"

#include "computation/module.H"

// Looks up a value in a compiled binding import context and returns the
// semantic signature pieces needed by later binding inference/audit code.
HaskellValueSignature lookup_value_signature(const HaskellBindingContexts& contexts, const BindingImportSet& imports, const std::string& name)
{
    auto context = contexts.context_for(imports);
    auto symbol = context->lookup_symbol(name);
    auto [type_vars, constraints, _] = peel_top_gen(symbol->type);
    return {symbol->name, symbol->type, std::move(type_vars), std::move(constraints)};
}
