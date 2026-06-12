#include "records.H"

#include "rename.H"
#include "computation/module.H"
#include "computation/record_utils.H"
#include "computation/haskell/ids.H"

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

    void resolve_constructor_field_identities(const renamer_state& rn, const std::string& constructor_name, Hs::FieldBindings& fields)
    {
        auto field_names = rn.m.record_field_names_for_constructor(constructor_name);
        if (not field_names)
            return;

        for(auto& field: fields.fields)
        {
            auto& binding = unloc(field);
            binding.resolved_field = record_utils::resolve_record_field_name(*field_names, unloc(binding.field).name);
        }
    }
}
