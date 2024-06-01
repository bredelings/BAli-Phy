#ifndef INSTANCE_INFO_H
#define INSTANCE_INFO_H

#include "immer/map.hpp" // for immer::map
#include <vector>
#include "haskell/core.H"
#include "haskell/coretype.H"

struct DataConInfo
{
    std::vector<TypeVar> uni_tvs;
    std::vector<TypeVar> exi_tvs;
    std::vector<Type> top_constraints; // The "stupid theta"
    std::vector<Type> written_constraints;
    std::vector<Type> gadt_eq_constraints;
    std::vector<Type> field_types;
    std::vector<Type> all_constraints() const;
    std::vector<Type> dictionary_preds() const;
    std::vector<Type> equality_preds() const;
    TypeCon data_type;
    int dict_arity() const;
    int arity() const;
    Type result_type() const;

    Type constructor_type() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(uni_tvs, exi_tvs, top_constraints, written_constraints, gadt_eq_constraints, field_types);
    }
};

typedef immer::map<std::string, DataConInfo> DataConEnv;

#endif
