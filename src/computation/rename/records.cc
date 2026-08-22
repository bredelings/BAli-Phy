#include "records.hh"

#include "rename.hh"
#include "computation/module.hh"
#include "computation/record_utils.hh"
#include "computation/haskell/ids.hh"

namespace
{
    // Resolve fields against one constructor and report source errors that do not require inferred types.
    template<class FieldBindings>
    void resolve_constructor_fields(
        const renamer_state& rn,
        const std::string& constructor_name,
        const std::string& duplicate_context,
        FieldBindings& fields)
    {
        auto field_names = rn.m.record_field_names_for_constructor(constructor_name);
        if (not field_names)
            return;

        std::set<std::string> used_fields;
        for(auto& field: fields.fields)
        {
            auto& binding = unloc(field);
            auto source_name = unloc(binding.field).name;
            binding.resolved_field = record_utils::resolve_record_field_name(*field_names, source_name);
            if (not binding.resolved_field)
                rn.error(field.loc, Note()<<"Constructor '"<<get_unqualified_name(constructor_name)
                         <<"' does not have field '"<<source_name<<"'.");
            else if (not used_fields.insert(*binding.resolved_field).second)
                rn.error(field.loc, Note()<<"Field '"<<source_name
                         <<"' appears more than once in "<<duplicate_context<<".");
        }
    }
}

namespace record_rename
{
    void require_record_extension(const renamer_state& rn, const std::optional<yy::location>& loc, LangExt extension, const std::string& extension_name, const std::string& syntax)
    {
        if (not rn.m.language_extensions.has_extension(extension))
            rn.error(loc, Note()<<syntax<<" requires the "<<extension_name<<" extension.");
    }

    void reject_record_update_wildcard(const renamer_state& rn, const Located<Hs::FieldBindings>& fbinds)
    {
        if (unloc(fbinds).dotdot)
            rn.error(*unloc(fbinds).dotdot, Note()<<"Record wildcard '..' is not allowed in record updates.");
    }

    void check_duplicate_field(const renamer_state& rn, std::set<std::string>& used_field_names, const std::optional<yy::location>& loc, const std::string& field_name, const std::string& context)
    {
        auto field_key = get_unqualified_name(field_name);
        if (used_field_names.count(field_key))
            rn.error(loc, Note()<<"Field '"<<field_name<<"' appears more than once in "<<context<<".");
        used_field_names.insert(field_key);
    }

    void expand_expression_pun(const renamer_state& rn, Located<Hs::FieldBinding>& field)
    {
        auto& binding = unloc(field);
        if (binding.value)
            return;

        require_record_extension(rn, field.loc, LangExt::NamedFieldPuns, "NamedFieldPuns", "Record field pun");
        binding.value = record_utils::record_field_pun_exp(binding.field);
    }

    void resolve_record_update_candidates(const renamer_state& rn, Located<Hs::FieldBinding>& field)
    {
        auto& binding = unloc(field);
        auto field_name = unloc(binding.field).name;
        try
        {
            binding.record_update_candidates = rn.m.lookup_record_field_candidates(field_name);
        }
        catch (myexception& e)
        {
            auto message = std::string(e.what());
            if (message.find("ambiguous") != std::string::npos)
                rn.error(field.loc, Note()<<message);
            else
                rn.error(field.loc, Note()<<"Record field '"<<field_name<<"' not in scope for update.");
            return;
        }

        if (binding.record_update_candidates.empty())
            rn.error(field.loc, Note()<<"Record field '"<<field_name<<"' not in scope for update.");
    }

    void resolve_constructor_field_identities(
        const renamer_state& rn,
        const std::string& constructor_name,
        Hs::FieldBindings& fields)
    {
        resolve_constructor_fields(rn, constructor_name, "record construction", fields);
    }

    void resolve_constructor_field_identities(
        const renamer_state& rn,
        const std::string& constructor_name,
        Hs::PatternFieldBindings& fields)
    {
        auto pattern_text = get_unqualified_name(constructor_name) + " {" + fields.print() + "}";
        resolve_constructor_fields(rn, constructor_name, "pattern '" + pattern_text + "'", fields);
    }

    void check_pattern_pun(const renamer_state& rn, const Located<Hs::PatternFieldBinding>& field)
    {
        if (unloc(field).pun)
            require_record_extension(rn, field.loc, LangExt::NamedFieldPuns, "NamedFieldPuns", "Record field pun");
    }

    std::vector<std::string> missing_pattern_wildcard_fields(const renamer_state& rn, const Hs::LCon& head, const Hs::PatternFieldBindings& fields)
    {
        if (not fields.dotdot)
            return {};

        require_record_extension(rn, *fields.dotdot, LangExt::RecordWildCards, "RecordWildCards", "Record wildcard '..'");

        std::set<std::string> explicit_fields;
        for(const auto& field: fields.fields)
            explicit_fields.insert(get_unqualified_name(unloc(unloc(field).field).name));

        std::vector<std::string> missing_fields;
        try
        {
            auto S = rn.m.lookup_symbol(unloc(head).name);
            if (S->symbol_type == symbol_type_t::constructor)
            {
                if (auto field_names = rn.m.record_field_names_for_constructor(S->name))
                {
                    for(const auto& field_name: *field_names)
                    {
                        auto unqualified = get_unqualified_name(field_name);
                        if (not explicit_fields.count(unqualified))
                            missing_fields.push_back(field_name);
                    }
                }
            }
        }
        catch (myexception&)
        {
            // Normal constructor lookup below will report the underlying name error.
        }

        return missing_fields;
    }
}
