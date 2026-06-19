#ifndef HASKELL_TYPE_TO_MODEL_TYPE_H
#define HASKELL_TYPE_TO_MODEL_TYPE_H

#include <map>
#include <string>
#include <vector>

#include "computation/haskell/coretype.H"
#include "models/model-type.H"

struct HaskellTypeBridgeState
{
    std::map<TypeVar, std::string> type_vars;
    int next_type_var = 0;
};

void seed_haskell_type_bridge_vars(HaskellTypeBridgeState&, const std::vector<TypeVar>&);

CM::Type bridge_haskell_type_to_model_type(const Type&, HaskellTypeBridgeState&);

CM::Type bridge_haskell_type_to_model_type(const Type&);

CM::Type bridge_haskell_constraint_to_model_constraint(const Type&, HaskellTypeBridgeState&);

#endif
