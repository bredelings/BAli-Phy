#ifndef RECORD_UTILS_H
#define RECORD_UTILS_H

#include <cassert>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "computation/haskell/haskell.H"
#include "computation/haskell/ids.H"

namespace record_utils
{
    std::vector<std::string> field_decl_names(const Hs::FieldDecls& field_decls);
    std::vector<Hs::LType> field_decl_types(const Hs::FieldDecls& field_decls);
    std::optional<std::vector<std::string>> constructor_field_names(const Hs::ConstructorDecl& constructor);
    std::optional<std::vector<std::string>> gadt_constructor_field_names(Hs::LType type);

    // Map declared record fields to constructor argument positions.
    std::map<std::string,int> record_field_positions(const std::vector<std::string>& field_names);
    // Test whether a source field spelling denotes a declared field.
    bool record_field_name_matches(const std::string& declared_field, const std::string& requested_field);
    // Return the declared field name matched by a source field spelling, if any.
    std::optional<std::string> resolve_record_field_name(const std::vector<std::string>& field_names, const std::string& requested_field);

    // Place fields validated by renaming into constructor order, reporting only absent binding values.
    template<class Slot, class LocatedField, class GetSlot, class OnMissingSlot>
    std::vector<std::optional<Slot>> order_record_field_bindings(
        const std::vector<std::string>& field_names,
        const std::vector<LocatedField>& fields,
        GetSlot get_slot,
        OnMissingSlot on_missing_slot)
    {
        auto positions = record_field_positions(field_names);
        std::vector<std::optional<Slot>> ordered_fields(field_names.size());
        std::set<int> used_fields;

        for(auto& field: fields)
        {
            auto& f = unloc(field);
            auto source_field_name = unloc(f.field).name;
            assert(f.resolved_field);
            auto pos = positions.find(*f.resolved_field);
            assert(pos != positions.end());
            assert(used_fields.insert(pos->second).second);

            if (auto slot = get_slot(field))
                ordered_fields[pos->second] = *slot;
            else
                on_missing_slot(field, source_field_name);
        }

        return ordered_fields;
    }

    Hs::LPat record_field_pun_pattern(const Hs::LVar& field);
    Hs::LExp record_field_pun_exp(const Hs::LVar& field);
}

#endif
