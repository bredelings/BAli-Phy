#ifndef RECORD_UTILS_H
#define RECORD_UTILS_H

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

    // Place source field bindings into constructor-field order and report invalid fields through callbacks.
    template<class Slot, class LocatedField, class GetSlot, class OnUnknown, class OnDuplicate, class OnMissingSlot>
    std::vector<std::optional<Slot>> order_record_field_bindings(
        const std::vector<std::string>& field_names,
        const std::vector<LocatedField>& fields,
        GetSlot get_slot,
        OnUnknown on_unknown,
        OnDuplicate on_duplicate,
        OnMissingSlot on_missing_slot)
    {
        auto positions = record_field_positions(field_names);
        std::vector<std::optional<Slot>> ordered_fields(field_names.size());
        std::set<int> used_fields;

        for(auto& field: fields)
        {
            auto& f = unloc(field);
            auto source_field_name = unloc(f.field).name;
            auto field_name = f.resolved_field.value_or(source_field_name);
            auto pos = positions.find(field_name);
            if (pos == positions.end())
                pos = positions.find(get_unqualified_name(field_name));

            if (pos == positions.end())
                on_unknown(field, source_field_name);
            else if (used_fields.count(pos->second))
                on_duplicate(field, source_field_name);
            else if (auto slot = get_slot(field))
            {
                used_fields.insert(pos->second);
                ordered_fields[pos->second] = *slot;
            }
            else
                on_missing_slot(field, source_field_name);
        }

        return ordered_fields;
    }

    Hs::LPat record_field_pun_pattern(const Hs::LVar& field);
    Hs::LExp record_field_pun_exp(const Hs::LVar& field);
}

#endif
