#ifndef RECORD_UTILS_H
#define RECORD_UTILS_H

#include <map>
#include <optional>
#include <string>
#include <vector>

#include "computation/haskell/haskell.H"

namespace record_utils
{
    std::vector<std::string> field_decl_names(const Hs::FieldDecls& field_decls);
    std::vector<Hs::LType> field_decl_types(const Hs::FieldDecls& field_decls);
    std::optional<std::vector<std::string>> constructor_field_names(const Hs::ConstructorDecl& constructor);
    std::optional<std::vector<std::string>> gadt_constructor_field_names(Hs::LType type);

    std::map<std::string,int> record_field_positions(const std::vector<std::string>& field_names);
    bool record_field_name_matches(const std::string& declared_field, const std::string& requested_field);

    Hs::LPat record_field_pun_pattern(const Hs::LVar& field);
    Hs::LExp record_field_pun_exp(const Hs::LVar& field);
}

#endif
