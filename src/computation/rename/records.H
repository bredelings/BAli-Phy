#ifndef RENAME_RECORDS_H
#define RENAME_RECORDS_H

#include <optional>
#include <set>
#include <string>

#include "computation/haskell/haskell.H"
#include "computation/haskell/extensions.H"

struct renamer_state;

namespace record_rename
{
    // Report that a record source form requires a disabled extension.
    void require_record_extension(const renamer_state& rn, const std::optional<yy::location>& loc, LangExt extension, const std::string& extension_name, const std::string& syntax);

    // Reject record-update wildcards, which GHC does not allow.
    void reject_record_update_wildcard(const renamer_state& rn, const Located<Hs::FieldBindings>& fbinds);

    // Report repeated source field labels in a record field list.
    void check_duplicate_field(const renamer_state& rn, std::set<std::string>& used_field_names, const std::optional<yy::location>& loc, const std::string& field_name, const std::string& context);

    // Expand an expression-side record pun after checking NamedFieldPuns.
    void expand_expression_pun(const renamer_state& rn, Located<Hs::FieldBinding>& field);

    // Resolve constructor-directed fields to their declared names when available.
    void resolve_constructor_field_identities(const renamer_state& rn, const std::string& constructor_name, Hs::FieldBindings& fields);
}

#endif
