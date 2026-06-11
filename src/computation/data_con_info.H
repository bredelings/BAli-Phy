#ifndef DATA_CON_INFO_H
#define DATA_CON_INFO_H

#include <optional>
#include <string>
#include <vector>
#include "haskell/coretype.H"

struct DataConInfo
{
    std::string name;
    std::vector<TypeVar> uni_tvs;
    std::vector<TypeVar> exi_tvs;
    std::vector<Type> top_constraints; // The "stupid theta"
    std::vector<Type> written_constraints;
    std::vector<Type> gadt_eq_constraints;
    std::vector<Type> field_types;
    std::optional<std::vector<std::string>> field_names;
    std::vector<bool> field_strictness;
    TypeCon data_type;
    std::optional<Type> constructor_result_type;
    bool is_newtype_constructor = false;

    std::vector<Type> all_constraints() const;
    std::vector<Type> dictionary_preds() const;
    std::vector<Type> equality_preds() const;
    int dict_arity() const;
    int arity() const;
    bool is_record_constructor() const { return field_names.has_value(); }
    bool is_infix_constructor() const;
    Type result_type() const;

    Type constructor_type() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(name, uni_tvs, exi_tvs, top_constraints, written_constraints, gadt_eq_constraints, field_types, field_names, field_strictness, data_type, constructor_result_type, is_newtype_constructor);
    }
};

#endif
