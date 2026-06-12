#include "record_utils.H"

#include "haskell/ids.H"

using std::optional;
using std::string;
using std::vector;

namespace record_utils
{
    std::vector<std::string> field_decl_names(const Hs::FieldDecls& field_decls)
    {
        vector<string> names;
        for(const auto& field_decl: field_decls.field_decls)
            for(const auto& field_name: field_decl.field_names)
                names.push_back(unloc(field_name).name);
        return names;
    }

    std::vector<Hs::LType> field_decl_types(const Hs::FieldDecls& field_decls)
    {
        vector<Hs::LType> types;
        for(const auto& field_decl: field_decls.field_decls)
            for(int i=0; i<field_decl.field_names.size(); i++)
                types.push_back(field_decl.type);
        return types;
    }

    std::optional<std::vector<std::string>> constructor_field_names(const Hs::ConstructorDecl& constructor)
    {
        if (not constructor.is_record_constructor())
            return {};

        return field_decl_names(std::get<1>(constructor.fields));
    }

    std::optional<std::vector<std::string>> gadt_constructor_field_names(Hs::LType type)
    {
        auto [tvs, context, rho_type] = Hs::peel_top_gen(type);

        optional<vector<string>> names;
        while(auto function_type = Hs::is_function_type(rho_type))
        {
            auto arg_type = function_type->first;
            if (auto field_decls = unloc(arg_type).to<Hs::FieldDecls>())
            {
                if (not names)
                    names = vector<string>{};
                auto arg_names = field_decl_names(*field_decls);
                names->insert(names->end(), arg_names.begin(), arg_names.end());
            }
            else if (names)
                return {};

            rho_type = function_type->second;
        }

        return names;
    }

    std::map<std::string,int> record_field_positions(const std::vector<std::string>& field_names)
    {
        std::map<std::string,int> positions;
        for(int i=0; i<field_names.size(); i++)
        {
            positions[field_names[i]] = i;
            positions[get_unqualified_name(field_names[i])] = i;
        }
        return positions;
    }

    bool record_field_name_matches(const std::string& declared_field, const std::string& requested_field)
    {
        if (is_qualified_symbol(requested_field))
            return declared_field == requested_field;
        return declared_field == requested_field or get_unqualified_name(declared_field) == get_unqualified_name(requested_field);
    }

    Hs::LPat record_field_pun_pattern(const Hs::LVar& field)
    {
        auto name = get_unqualified_name(unloc(field).name);
        return {field.loc, Hs::VarPattern({field.loc, Hs::Var(name)})};
    }

    Hs::LExp record_field_pun_exp(const Hs::LVar& field)
    {
        auto name = get_unqualified_name(unloc(field).name);
        return {field.loc, Hs::Var(name)};
    }
}
