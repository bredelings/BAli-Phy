#include "typecheck.H"
#include "haskell/ids.H"
#include "tidy.H"

#include <map>
#include <set>

using std::string;
using std::vector;
using std::map;
using std::pair;
using std::set;
using std::optional;

namespace
{
    struct DerivingDataInfo
    {
        TypeCon type_con;
        const_type_ptr type_info;
        const type_info::data_info& data;
    };

    struct DerivingTarget
    {
        DerivingDataInfo data;
        optional<Hs::LType> explicit_polytype;
        const Hs::DataOrNewtypeDecl* source_decl = nullptr;
    };

    using StockDeriver = Hs::InstanceDecl (*)(TypeChecker&, const DerivingDataInfo&, const std::optional<yy::location>&);
    using StockValidator = bool (*)(TypeChecker&, const DerivingDataInfo&);

    struct StockDerivingSpec
    {
        string class_name;
        bool needs_field_constraints;
        StockDeriver derive;
        StockValidator validate;
        string unsupported_message;
    };

    struct StockDerivingInfo
    {
        const StockDerivingSpec* spec;
        TypeCon type_con;
    };

    struct GeneralizedNewtypeDerivingClass
    {
        const type_info::class_info* info;
        vector<Hs::LType> fixed_args;
    };

    struct GeneralizedNewtypeDerivingResult
    {
        bool handled = false;
        optional<Hs::InstanceDecl> instance;
    };

    // Look up the semantic data metadata for a source declaration being derived.
    optional<DerivingDataInfo> deriving_data_info_for_decl(TypeChecker& tc, const Hs::DataOrNewtypeDecl& data_decl)
    {
        auto T = tc.this_mod().lookup_resolved_type(unloc(data_decl.con).name);
        auto data_info = T ? T->is_data() : nullptr;
        if (not T or not data_info)
            return {};

        return DerivingDataInfo{TypeCon(T->name, T->kind), T, *data_info};
    }

    optional<DerivingTarget> deriving_target_for_decl(TypeChecker& tc, const Hs::DataOrNewtypeDecl& data_decl, const optional<Hs::LType>& explicit_polytype = {})
    {
        auto data_info = deriving_data_info_for_decl(tc, data_decl);
        if (not data_info)
            return {};

        return DerivingTarget{*data_info, explicit_polytype, &data_decl};
    }

    // Find a local source declaration when a derivation still needs source syntax.
    const Hs::DataOrNewtypeDecl* local_data_decl_for_type(const Hs::Decls& decls, const string& type_name)
    {
        for(auto& [_, decl]: decls)
        {
            auto data_decl = decl.to<Hs::DataOrNewtypeDecl>();
            if (data_decl and unloc(data_decl->con).name == type_name)
                return data_decl;
        }

        return nullptr;
    }

    // Build a deriving target from resolved semantic type metadata.
    optional<DerivingTarget> deriving_target_for_type(TypeChecker& tc, const Hs::Decls& decls, const Hs::TypeCon& target_con, const optional<Hs::LType>& explicit_polytype = {})
    {
        auto T = tc.this_mod().lookup_resolved_type(target_con.name);
        auto data_info = T ? T->is_data() : nullptr;
        if (not T or not data_info)
            return {};

        auto source_decl = local_data_decl_for_type(decls, target_con.name);
        return DerivingTarget{DerivingDataInfo{TypeCon(T->name, T->kind), T, *data_info}, explicit_polytype, source_decl};
    }

    optional<DataConInfo> deriving_constructor_info(TypeChecker& tc, const string& con_name)
    {
        return tc.this_mod().constructor_info(con_name);
    }

    bool has_ordered_constructors(const DerivingDataInfo& data_info)
    {
        return data_info.data.info
            ? not data_info.data.info->constructors.empty()
            : not data_info.data.constructors.empty();
    }

    bool has_semantic_data_info(const DerivingDataInfo& data_info)
    {
        return bool(data_info.data.info);
    }

    const vector<string>& deriving_constructors(const DerivingDataInfo& data_info)
    {
        return data_info.data.info ? data_info.data.info->constructors : data_info.data.constructors;
    }

    // Check that every ordered constructor has registered constructor metadata.
    bool has_constructor_infos(TypeChecker& tc, const DerivingDataInfo& data_info)
    {
        if (not has_ordered_constructors(data_info))
            return false;

        for(const auto& con_name: deriving_constructors(data_info))
            if (not deriving_constructor_info(tc, con_name))
                return false;
        return true;
    }

    // Check the semantic constructor list for Enum/Ix-style nullary-only deriving.
    bool has_only_nullary_constructors(TypeChecker& tc, const DerivingDataInfo& data_info)
    {
        if (not has_ordered_constructors(data_info))
            return false;

        for(const auto& con_name: deriving_constructors(data_info))
        {
            auto con_info = deriving_constructor_info(tc, con_name);
            if (not con_info or con_info->arity() != 0)
                return false;
        }
        return true;
    }

    vector<string> record_field_names(const Hs::ConstructorDecl& constructor)
    {
        vector<string> names;
        if (not constructor.is_record_constructor())
            return names;

        for(const auto& field_group: std::get<1>(constructor.fields).field_decls)
            for(const auto& field_name: field_group.field_names)
                names.push_back(unloc(field_name).name);

        return names;
    }

    bool is_prefix_constructor(const Hs::ConstructorDecl& constructor)
    {
        return is_haskell_conid(get_unqualified_name(unloc(*constructor.con).name));
    }

    bool is_basic_infix_constructor(const Hs::ConstructorDecl& constructor)
    {
        return not constructor.is_record_constructor()
            and constructor.arity() == 2
            and is_haskell_consym(get_unqualified_name(unloc(*constructor.con).name));
    }

    // Accept the constructor forms for which derived Read currently emits parser code.
    bool is_regular_data_decl_with_readable_constructors(const Hs::DataOrNewtypeDecl& data_decl)
    {
        if (not data_decl.is_regular_decl())
            return false;

        for(const auto& constructor: data_decl.get_constructors())
            if (not is_prefix_constructor(constructor) and not is_basic_infix_constructor(constructor))
                return false;

        return true;
    }

    Type class_constraint(const TypeCon& class_con, const Type& type)
    {
        return type_apply(class_con, vector<Type>{type});
    }

    bool is_allowed_derived_context_pred(const Type& pred, const set<TypeVar>& data_tvs)
    {
        auto [head, args] = decompose_type_apps(pred);
        if (args.empty())
            return false;

        auto field_type = follow_meta_type_var(args.back());
        auto tv = field_type.to<TypeVar>();
        return tv and data_tvs.contains(*tv);
    }

}

namespace
{
    Hs::LType type_var_type(const Hs::LTypeVar& tv)
    {
        return {tv.loc, Hs::TypeVar(unloc(tv))};
    }

    Hs::LTypeVar hs_type_var(const TypeVar& tv)
    {
        Hs::TypeVar hs_tv(tv.name, tv.kind);
        hs_tv.index = tv.index;
        return {noloc, hs_tv};
    }

    Hs::LType type_var_type(const TypeVar& tv)
    {
        auto hs_tv = hs_type_var(tv);
        return type_var_type(hs_tv);
    }

    optional<Hs::LType> semantic_type_to_hs_type(const Type& type);

    // Convert semantic constraints back into parsed constraint syntax for synthetic instances.
    optional<Hs::Context> semantic_context_to_hs_context(const Context& context)
    {
        Hs::Context hs_context;
        for(auto& pred: context)
        {
            auto hs_pred = semantic_type_to_hs_type(pred);
            if (not hs_pred)
                return {};
            hs_context.push_back(*hs_pred);
        }
        return hs_context;
    }

    // Convert semantic type variables back into parsed type-variable binders for synthetic instances.
    vector<Hs::LTypeVar> hs_type_vars(const vector<TypeVar>& tvs, int n)
    {
        vector<Hs::LTypeVar> hs_tvs;
        for(int i=0; i<n; i++)
            hs_tvs.push_back(hs_type_var(tvs[i]));
        return hs_tvs;
    }

    Hs::LType type_con_type(const Hs::LTypeCon& con, const vector<Hs::LType>& args)
    {
        return Hs::type_apply({con.loc, Hs::TypeCon(unloc(con).name)}, args);
    }

    Hs::LType class_constraint(const string& class_name, const Hs::LType& type)
    {
        return Hs::type_apply({{noloc,Hs::TypeCon(class_name)}, type});
    }

    Hs::LType class_constraint(const string& class_name, const vector<Hs::LType>& args)
    {
        return Hs::type_apply({noloc,Hs::TypeCon(class_name)}, args);
    }

    // Convert semantic type variables back into parsed type arguments for synthetic instances.
    vector<Hs::LType> type_var_types(const vector<TypeVar>& tvs, int n)
    {
        vector<Hs::LType> types;
        for(int i=0; i<n; i++)
            types.push_back(type_var_type(tvs[i]));
        return types;
    }

    // Build the Haskell syntax for applying a derived data type to its first arity parameters.
    Hs::LType derived_data_type(const DerivingDataInfo& data_info, int arity)
    {
        assert(data_info.data.info);
        Hs::LTypeCon type_con{noloc, Hs::TypeCon(data_info.data.info->name)};
        return type_con_type(type_con, type_var_types(data_info.data.info->type_vars, arity));
    }

    Hs::Context source_context_for_target(const DerivingTarget& target)
    {
        return target.source_decl ? target.source_decl->context : Hs::Context{};
    }

    // Convert the subset of semantic types used in generated deriving code to parsed syntax.
    optional<Hs::LType> semantic_type_to_hs_type(const Type& type)
    {
        auto type1 = follow_meta_type_var(type);
        if (auto tv = type1.to<TypeVar>())
            return type_var_type(*tv);
        else if (auto tc = type1.to<TypeCon>())
        {
            Hs::TypeCon hs_tc(tc->name);
            hs_tc.kind = tc->kind;
            return Hs::LType{noloc, hs_tc};
        }
        else if (type1.is_a<TypeApp>())
        {
            auto [head, args] = decompose_type_apps(type1);
            auto hs_head = semantic_type_to_hs_type(head);
            if (not hs_head)
                return {};

            vector<Hs::LType> hs_args;
            for(auto& arg: args)
            {
                auto hs_arg = semantic_type_to_hs_type(arg);
                if (not hs_arg)
                    return {};
                hs_args.push_back(*hs_arg);
            }

            return Hs::type_apply(*hs_head, hs_args);
        }
        else if (auto forall_type = type1.to<ForallType>())
        {
            auto hs_body = semantic_type_to_hs_type(forall_type->type);
            if (not hs_body)
                return {};
            return Hs::add_forall_vars(hs_type_vars(forall_type->type_var_binders, forall_type->type_var_binders.size()), *hs_body);
        }
        else if (auto constrained_type = type1.to<ConstrainedType>())
        {
            auto hs_context = semantic_context_to_hs_context(constrained_type->context);
            auto hs_body = semantic_type_to_hs_type(constrained_type->type);
            if (not hs_context or not hs_body)
                return {};
            return Hs::LType{noloc, Hs::ConstrainedType(*hs_context, *hs_body)};
        }

        return {};
    }

    // Convert the stored data/newtype context for generated instance heads.
    optional<Hs::Context> semantic_data_context(const DerivingDataInfo& data_info)
    {
        assert(data_info.data.info);
        return semantic_context_to_hs_context(data_info.data.info->context);
    }

    Hs::LExp local_var_exp(const string& name)
    {
        return {noloc, Hs::Var(name)};
    }

    Hs::LExp wired_var_exp(const string& name)
    {
        return {noloc, Hs::Var(name)};
    }

    Hs::LExp wired_con_exp(const string& name)
    {
        return {noloc, Hs::Con(name)};
    }

    Hs::LExp bool_exp(bool b)
    {
        return wired_con_exp(b ? bool_true_name : bool_false_name);
    }

    Hs::LExp string_exp(const string& s)
    {
        return {noloc, Hs::Literal(Hs::String(s))};
    }

    Hs::LExp int_exp(int i)
    {
        return {noloc, Hs::Literal(Hs::BoxedInteger{integer(i)})};
    }

    Hs::LExp eq_exp(const Hs::LExp& x, const Hs::LExp& y)
    {
        return Hs::apply(wired_var_exp(eq_method_name), {x, y});
    }

    Hs::LExp and_exp(const Hs::LExp& x, const Hs::LExp& y)
    {
        return Hs::apply(wired_var_exp(bool_and_name), {x, y});
    }

    Hs::LExp eq_all_exp(const vector<pair<Hs::LExp,Hs::LExp>>& fields)
    {
        if (fields.empty())
            return bool_exp(true);

        auto result = eq_exp(fields[0].first, fields[0].second);
        for(int i=1; i<fields.size(); i++)
            result = and_exp(result, eq_exp(fields[i].first, fields[i].second));
        return result;
    }

    Hs::LPat var_pat(const string& name)
    {
        return {noloc, Hs::VarPattern({noloc, Hs::Var(name)})};
    }

    Hs::LPat wildcard_pat()
    {
        return {noloc, Hs::WildcardPattern()};
    }

    Hs::LPat con_pat(const string& name, int arity, const Hs::LPats& args)
    {
        return {noloc, Hs::ConPattern({noloc, Hs::Con(name, arity)}, args)};
    }

    Hs::LPat constructor_pattern(const Hs::ConstructorDecl& constructor, const Hs::LPats& args)
    {
        return con_pat(unloc(*constructor.con).name, constructor.arity(), args);
    }

    Hs::LPat constructor_pattern(const string& con_name, int arity, const Hs::LPats& args)
    {
        return con_pat(con_name, arity, args);
    }

    Hs::LPat constructor_var_pattern(const Hs::ConstructorDecl& constructor, const string& prefix)
    {
        Hs::LPats args;
        for(int i=0; i<constructor.arity(); i++)
            args.push_back(var_pat(prefix + std::to_string(i)));

        return constructor_pattern(constructor, args);
    }

    Hs::LPat constructor_var_pattern(const string& con_name, int arity, const string& prefix)
    {
        Hs::LPats args;
        for(int i=0; i<arity; i++)
            args.push_back(var_pat(prefix + std::to_string(i)));

        return constructor_pattern(con_name, arity, args);
    }

    Hs::LPat constructor_wildcard_pattern(const string& con_name, int arity)
    {
        Hs::LPats args(arity, wildcard_pat());
        return constructor_pattern(con_name, arity, args);
    }

    Hs::MRule binary_method_rule(const Hs::LPat& x, const Hs::LPat& y, const Hs::LExp& rhs)
    {
        return Hs::MRule({x, y}, Hs::SimpleRHS(rhs));
    }

    Hs::MRule unary_method_rule(const Hs::LPat& x, const Hs::LExp& rhs)
    {
        return Hs::MRule({x}, Hs::SimpleRHS(rhs));
    }

    Hs::LDecl derived_method_decl(const std::optional<yy::location>& loc, const string& method_name, const Hs::Matches& matches)
    {
        return {loc, Hs::FunDecl({loc,Hs::Var(get_unqualified_name(method_name))}, matches)};
    }

    Hs::LExp case_exp(const Hs::LExp& object, const Hs::Alts& alts)
    {
        return {noloc, Hs::CaseExp(object, alts)};
    }

    Located<Hs::Alt> simple_alt(const Hs::LPat& pattern, const Hs::LExp& rhs)
    {
        return {noloc, Hs::Alt(pattern, Hs::SimpleRHS(rhs))};
    }

    Hs::LType stock_instance_type(const DerivingDataInfo& data_info, const string& class_name)
    {
        assert(data_info.data.info);

        Hs::Context context;
        for(auto& tv: data_info.data.info->type_vars)
        {
            auto tv_type = type_var_type(tv);
            context.push_back(class_constraint(class_name, tv_type));
        }

        auto instance_head = class_constraint(class_name, derived_data_type(data_info, data_info.data.info->type_vars.size()));
        Hs::LType polytype = context.empty() ? instance_head : Hs::LType{noloc, Hs::ConstrainedType(context, instance_head)};
        return Hs::add_forall_vars(hs_type_vars(data_info.data.info->type_vars, data_info.data.info->type_vars.size()), polytype);
    }

    Hs::LExp constructor_tag_exp(TypeChecker& tc, const DerivingDataInfo& data_info, const Hs::LExp& value)
    {
        Hs::Alts alts;
        const auto& constructors = deriving_constructors(data_info);

        for(int i=0; i<constructors.size(); i++)
        {
            auto con_info = deriving_constructor_info(tc, constructors[i]);
            assert(con_info);
            alts.push_back(simple_alt(constructor_wildcard_pattern(constructors[i], con_info->arity()), int_exp(i)));
        }

        return case_exp(value, alts);
    }

    Hs::InstanceDecl derive_eq_instance(TypeChecker& tc, const DerivingDataInfo& data_info, const std::optional<yy::location>& deriving_loc)
    {
        Hs::Matches eq_matches;
        for(const auto& constructor: deriving_constructors(data_info))
        {
            auto con_info = deriving_constructor_info(tc, constructor);
            assert(con_info);

            vector<pair<Hs::LExp,Hs::LExp>> fields;
            for(int i=0; i<con_info->arity(); i++)
                fields.push_back({local_var_exp("x$" + std::to_string(i)), local_var_exp("y$" + std::to_string(i))});

            eq_matches.push_back(binary_method_rule(constructor_var_pattern(constructor, con_info->arity(), "x$"),
                                                    constructor_var_pattern(constructor, con_info->arity(), "y$"),
                                                    eq_all_exp(fields)));
        }

        auto wildcard = wildcard_pat();
        eq_matches.push_back(binary_method_rule(wildcard, wildcard, bool_exp(false)));

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, eq_method_name, eq_matches));

        return Hs::InstanceDecl({}, stock_instance_type(data_info, eq_class_name), {}, {}, methods);
    }

    Hs::LExp compare_exp(const Hs::LExp& x, const Hs::LExp& y)
    {
        return Hs::apply(wired_var_exp(ord_compare_name), {x, y});
    }

    Hs::LExp error_exp(const string& message)
    {
        return Hs::apply(wired_var_exp(error_name), {string_exp(message)});
    }

    Hs::LExp if_exp(const Hs::LExp& condition, const Hs::LExp& true_branch, const Hs::LExp& false_branch)
    {
        return {noloc, Hs::IfExp(condition, true_branch, false_branch)};
    }

    Hs::LExp greater_than_exp(const Hs::LExp& x, const Hs::LExp& y)
    {
        return Hs::apply(wired_var_exp(ord_greater_than_name), {x, y});
    }

    Hs::LExp subtract_exp(const Hs::LExp& x, const Hs::LExp& y)
    {
        return Hs::apply(wired_var_exp("Compiler.Num.-"), {x, y});
    }

    Hs::LExp add_exp(const Hs::LExp& x, const Hs::LExp& y)
    {
        return Hs::apply(wired_var_exp("Compiler.Num.+"), {x, y});
    }

    Hs::LExp ordering_exp(const string& name)
    {
        return wired_con_exp(name);
    }

    Hs::LExp constructor_exp(const Hs::ConstructorDecl& constructor, const vector<Hs::LExp>& args)
    {
        return Hs::apply({constructor.con->loc, Hs::Con(unloc(*constructor.con).name, constructor.arity())}, args);
    }

    Hs::LExp constructor_exp(const string& con_name, int arity, const vector<Hs::LExp>& args)
    {
        return Hs::apply({noloc, Hs::Con(con_name, arity)}, args);
    }

    Hs::LPat ordering_pat(const string& name)
    {
        return con_pat(name, 0, {});
    }

    Hs::LExp compare_all_exp(const vector<pair<Hs::LExp,Hs::LExp>>& fields)
    {
        auto result = ordering_exp(ordering_eq_name);

        for(int i=fields.size()-1; i>=0; i--)
        {
            auto ord_var = "ord$" + std::to_string(i);
            Hs::Alts alts;
            alts.push_back(simple_alt(ordering_pat(ordering_eq_name), result));
            alts.push_back(simple_alt(var_pat(ord_var), local_var_exp(ord_var)));
            result = case_exp(compare_exp(fields[i].first, fields[i].second), alts);
        }

        return result;
    }

    Hs::LExp compare_constructor_tags_exp(TypeChecker& tc, const DerivingDataInfo& data_info, const Hs::LExp& x, const Hs::LExp& y)
    {
        // Preliminary source-level form of GHC's dataToTag#/tag layout idea:
        // synthesize a case over constructors now, but keep this isolated so it
        // can later become a shared helper or Core primitive known to the optimizer.
        return compare_exp(constructor_tag_exp(tc, data_info, x), constructor_tag_exp(tc, data_info, y));
    }

    Hs::InstanceDecl derive_ord_instance(TypeChecker& tc, const DerivingDataInfo& data_info, const std::optional<yy::location>& deriving_loc)
    {
        Hs::Matches compare_matches;
        const auto& constructors = deriving_constructors(data_info);

        for(const auto& constructor: constructors)
        {
            auto con_info = deriving_constructor_info(tc, constructor);
            assert(con_info);

            vector<pair<Hs::LExp,Hs::LExp>> fields;
            for(int i=0; i<con_info->arity(); i++)
                fields.push_back({local_var_exp("x$" + std::to_string(i)), local_var_exp("y$" + std::to_string(i))});

            compare_matches.push_back(binary_method_rule(constructor_var_pattern(constructor, con_info->arity(), "x$"),
                                                         constructor_var_pattern(constructor, con_info->arity(), "y$"),
                                                         compare_all_exp(fields)));
        }

        if (not constructors.empty())
        {
            compare_matches.push_back(binary_method_rule(var_pat("x$tag"),
                                                         var_pat("y$tag"),
                                                         compare_constructor_tags_exp(tc, data_info, local_var_exp("x$tag"), local_var_exp("y$tag"))));
        }

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, ord_compare_name, compare_matches));

        return Hs::InstanceDecl({}, stock_instance_type(data_info, ord_class_name), {}, {}, methods);
    }

    Hs::MRule nullary_method_rule(const Hs::LExp& rhs)
    {
        return Hs::MRule({}, Hs::SimpleRHS(rhs));
    }

    Hs::LDecl nullary_method_decl(const std::optional<yy::location>& loc, const string& method_name, const Hs::LExp& rhs)
    {
        return derived_method_decl(loc, method_name, {nullary_method_rule(rhs)});
    }

    Hs::LExp bounded_constructor_exp(TypeChecker& tc, const string& con_name, const string& bound_method_name)
    {
        auto con_info = deriving_constructor_info(tc, con_name);
        assert(con_info);
        vector<Hs::LExp> args(con_info->arity(), wired_var_exp(bound_method_name));
        return constructor_exp(con_name, con_info->arity(), args);
    }

    // Derive Bounded from ordered constructor metadata rather than source constructor order.
    Hs::InstanceDecl derive_bounded_instance(TypeChecker& tc, const DerivingDataInfo& data_info, const std::optional<yy::location>& deriving_loc)
    {
        const auto& constructors = deriving_constructors(data_info);

        Hs::Decls methods;
        methods.push_back(nullary_method_decl(deriving_loc, bounded_min_bound_name, bounded_constructor_exp(tc, constructors.front(), bounded_min_bound_name)));
        methods.push_back(nullary_method_decl(deriving_loc, bounded_max_bound_name, bounded_constructor_exp(tc, constructors.back(), bounded_max_bound_name)));

        return Hs::InstanceDecl({}, stock_instance_type(data_info, bounded_class_name), {}, {}, methods);
    }

    Hs::LPat int_pat(int i)
    {
        return {noloc, Hs::LiteralPattern(Hs::Literal(Hs::BoxedInteger{integer(i)}))};
    }

    // Derive Enum by assigning tags according to semantic constructor order.
    Hs::InstanceDecl derive_enum_instance(TypeChecker& tc, const DerivingDataInfo& data_info, const std::optional<yy::location>& deriving_loc)
    {
        const auto& constructors = deriving_constructors(data_info);
        int max_tag = constructors.size() - 1;

        Hs::Matches from_enum_matches;
        Hs::Matches to_enum_matches;
        for(int i=0; i<constructors.size(); i++)
        {
            auto con_info = deriving_constructor_info(tc, constructors[i]);
            assert(con_info);
            from_enum_matches.push_back(unary_method_rule(constructor_pattern(constructors[i], con_info->arity(), {}), int_exp(i)));
            to_enum_matches.push_back(unary_method_rule(int_pat(i), constructor_exp(constructors[i], con_info->arity(), {})));
        }
        to_enum_matches.push_back(unary_method_rule(wildcard_pat(), error_exp("toEnum: tag out of range")));

        auto from_enum = [](const string& var_name) {
            return Hs::apply(wired_var_exp(enum_from_enum_name), {local_var_exp(var_name)});
        };

        auto int_range_from_to = [&](const Hs::LExp& from, const Hs::LExp& to) {
            return Hs::apply(wired_var_exp(enum_from_to_name), {from, to});
        };

        auto int_range_from_then_to = [&](const Hs::LExp& from, const Hs::LExp& next, const Hs::LExp& to) {
            return Hs::apply(wired_var_exp(enum_from_then_to_name), {from, next, to});
        };

        auto map_to_enum = [](const Hs::LExp& tags) {
            return Hs::apply(wired_var_exp(list_map_name), {wired_var_exp(enum_to_enum_name), tags});
        };

        Hs::Matches enum_from_matches;
        enum_from_matches.push_back(unary_method_rule(var_pat("x$"), map_to_enum(int_range_from_to(from_enum("x$"), int_exp(max_tag)))));

        Hs::Matches enum_from_to_matches;
        enum_from_to_matches.push_back(binary_method_rule(var_pat("x$"), var_pat("y$"),
                                                          map_to_enum(int_range_from_to(from_enum("x$"), from_enum("y$")))));

        Hs::Matches enum_from_then_matches;
        auto limit = if_exp(greater_than_exp(from_enum("y$"), from_enum("x$")), int_exp(max_tag), int_exp(0));
        enum_from_then_matches.push_back(binary_method_rule(var_pat("x$"), var_pat("y$"),
                                                            map_to_enum(int_range_from_then_to(from_enum("x$"), from_enum("y$"), limit))));

        Hs::Matches enum_from_then_to_matches;
        enum_from_then_to_matches.push_back(Hs::MRule({var_pat("x$"), var_pat("y$"), var_pat("z$")},
                                                       Hs::SimpleRHS(map_to_enum(int_range_from_then_to(from_enum("x$"), from_enum("y$"), from_enum("z$"))))));

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, enum_from_enum_name, from_enum_matches));
        methods.push_back(derived_method_decl(deriving_loc, enum_to_enum_name, to_enum_matches));
        methods.push_back(derived_method_decl(deriving_loc, enum_from_name, enum_from_matches));
        methods.push_back(derived_method_decl(deriving_loc, enum_from_to_name, enum_from_to_matches));
        methods.push_back(derived_method_decl(deriving_loc, enum_from_then_name, enum_from_then_matches));
        methods.push_back(derived_method_decl(deriving_loc, enum_from_then_to_name, enum_from_then_to_matches));

        return Hs::InstanceDecl({}, stock_instance_type(data_info, enum_class_name), {}, {}, methods);
    }

    Hs::LPat pair_pat(const Hs::LPat& x, const Hs::LPat& y)
    {
        return {noloc, Hs::TuplePattern({x, y})};
    }

    Hs::LExp pair_exp(const Hs::LExp& x, const Hs::LExp& y)
    {
        return {noloc, Hs::Tuple({x, y})};
    }

    Hs::LExp list_exp(const vector<Hs::LExp>& elements)
    {
        return {noloc, Hs::List(elements)};
    }

    Hs::LExp ix_tag_exp(TypeChecker& tc, const DerivingDataInfo& data_info, const string& var_name)
    {
        return constructor_tag_exp(tc, data_info, local_var_exp(var_name));
    }

    // Compare Ix bounds by semantic constructor tags.
    Hs::LExp ix_in_range_exp(TypeChecker& tc, const DerivingDataInfo& data_info, const string& lo, const string& hi, const string& x)
    {
        auto lo_tag = ix_tag_exp(tc, data_info, lo);
        auto hi_tag = ix_tag_exp(tc, data_info, hi);
        auto x_tag = ix_tag_exp(tc, data_info, x);

        return if_exp(greater_than_exp(lo_tag, x_tag),
                      bool_exp(false),
                      if_exp(greater_than_exp(x_tag, hi_tag), bool_exp(false), bool_exp(true)));
    }

    // Derive Ix for enumeration-like types using semantic constructor tags.
    Hs::InstanceDecl derive_ix_instance(TypeChecker& tc, const DerivingDataInfo& data_info, const std::optional<yy::location>& deriving_loc)
    {
        const auto& constructors = deriving_constructors(data_info);
        Hs::LPat bounds_pat = pair_pat(var_pat("lo$"), var_pat("hi$"));
        Hs::LExp bounds_exp = pair_exp(local_var_exp("lo$"), local_var_exp("hi$"));

        vector<Hs::LExp> constructor_exps;
        for(const auto& constructor: constructors)
        {
            auto con_info = deriving_constructor_info(tc, constructor);
            assert(con_info);
            constructor_exps.push_back(constructor_exp(constructor, con_info->arity(), {}));
        }
        Hs::LExp all_constructors = {noloc, Hs::List(constructor_exps)};

        Hs::Matches range_matches;
        auto in_range_bounds = Hs::apply(wired_var_exp(ix_in_range_name), {bounds_exp});
        range_matches.push_back(unary_method_rule(bounds_pat, Hs::apply(wired_var_exp(list_filter_name), {in_range_bounds, all_constructors})));

        Hs::Matches index_matches;
        index_matches.push_back(binary_method_rule(bounds_pat, var_pat("x$"),
                                                   subtract_exp(ix_tag_exp(tc, data_info, "x$"), ix_tag_exp(tc, data_info, "lo$"))));

        Hs::Matches in_range_matches;
        in_range_matches.push_back(binary_method_rule(bounds_pat, var_pat("x$"), ix_in_range_exp(tc, data_info, "lo$", "hi$", "x$")));

        Hs::Matches range_size_matches;
        auto size = add_exp(subtract_exp(ix_tag_exp(tc, data_info, "hi$"), ix_tag_exp(tc, data_info, "lo$")), int_exp(1));
        range_size_matches.push_back(unary_method_rule(bounds_pat,
                                                       if_exp(greater_than_exp(ix_tag_exp(tc, data_info, "lo$"), ix_tag_exp(tc, data_info, "hi$")),
                                                              int_exp(0),
                                                              size)));

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, ix_range_name, range_matches));
        methods.push_back(derived_method_decl(deriving_loc, ix_index_name, index_matches));
        methods.push_back(derived_method_decl(deriving_loc, ix_in_range_name, in_range_matches));
        methods.push_back(derived_method_decl(deriving_loc, ix_range_size_name, range_size_matches));

        return Hs::InstanceDecl({}, stock_instance_type(data_info, ix_class_name), {}, {}, methods);
    }

    Hs::LExp compose_exp(const Hs::LExp& f, const Hs::LExp& g)
    {
        return Hs::apply(wired_var_exp(function_compose_name), {f, g});
    }

    Hs::LExp compose_all_exp(const vector<Hs::LExp>& parts)
    {
        assert(not parts.empty());

        auto result = parts.back();
        for(int i=parts.size()-2; i>=0; i--)
            result = compose_exp(parts[i], result);
        return result;
    }

    Hs::LExp show_string_exp(const string& s)
    {
        return Hs::apply(wired_var_exp(show_show_string_name), {string_exp(s)});
    }

    Hs::LExp shows_prec_exp(int precedence, const Hs::LExp& x)
    {
        return Hs::apply(wired_var_exp(show_shows_prec_name), {int_exp(precedence), x});
    }

    Hs::LExp show_paren_exp(const Hs::LExp& condition, const Hs::LExp& body)
    {
        return Hs::apply(wired_var_exp(show_show_paren_name), {condition, body});
    }

    Hs::LExp show_record_constructor_exp(const Hs::ConstructorDecl& constructor)
    {
        auto con_name = get_unqualified_name(unloc(*constructor.con).name);
        auto field_names = record_field_names(constructor);

        vector<Hs::LExp> parts;
        parts.push_back(show_string_exp(con_name + " {"));
        for(int i=0; i<field_names.size(); i++)
        {
            parts.push_back(show_string_exp((i == 0 ? "" : ", ") + get_unqualified_name(field_names[i]) + " = "));
            parts.push_back(shows_prec_exp(0, local_var_exp("x$" + std::to_string(i))));
        }
        parts.push_back(show_string_exp("}"));

        return compose_all_exp(parts);
    }

    Hs::LExp show_infix_constructor_exp(const Hs::ConstructorDecl& constructor)
    {
        auto con_name = get_unqualified_name(unloc(*constructor.con).name);
        constexpr int infix_con_prec = 5;
        constexpr int infix_con_operand_prec = infix_con_prec + 1;

        // FIXME: This intentionally ignores fixity declarations.  We parse operands
        // at the next higher precedence so nested infix constructors need parens
        // until derived Read/Show can share a real fixity-aware expression parser.
        auto body = compose_all_exp({shows_prec_exp(infix_con_operand_prec, local_var_exp("x$0")),
                                     show_string_exp(" " + con_name + " "),
                                     shows_prec_exp(infix_con_operand_prec, local_var_exp("x$1"))});
        return show_paren_exp(greater_than_exp(local_var_exp("d$"), int_exp(infix_con_prec)), body);
    }

    Hs::LExp shows_prec_constructor_exp(const Hs::ConstructorDecl& constructor)
    {
        auto con_name = get_unqualified_name(unloc(*constructor.con).name);
        if (constructor.is_record_constructor())
            return show_record_constructor_exp(constructor);
        if (is_basic_infix_constructor(constructor))
            return show_infix_constructor_exp(constructor);

        if (constructor.arity() == 0)
            return show_string_exp(con_name);

        vector<Hs::LExp> parts;
        for(int i=0; i<constructor.arity(); i++)
        {
            parts.push_back(show_string_exp(i == 0 ? con_name + " " : " "));
            parts.push_back(shows_prec_exp(11, local_var_exp("x$" + std::to_string(i))));
        }

        return show_paren_exp(greater_than_exp(local_var_exp("d$"), int_exp(10)), compose_all_exp(parts));
    }

    Hs::InstanceDecl derive_show_instance(TypeChecker&, const Hs::DataOrNewtypeDecl& data_decl, const DerivingDataInfo& data_info, const std::optional<yy::location>& deriving_loc)
    {
        Hs::Matches shows_prec_matches;
        for(const auto& constructor: data_decl.get_constructors())
        {
            shows_prec_matches.push_back(Hs::MRule({var_pat("d$"), constructor_var_pattern(constructor, "x$")},
                                                   Hs::SimpleRHS(shows_prec_constructor_exp(constructor))));
        }

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, show_shows_prec_name, shows_prec_matches));

        return Hs::InstanceDecl({}, stock_instance_type(data_info, show_class_name), {}, {}, methods);
    }

    Hs::LExp lambda_exp(const Hs::LPats& pats, const Hs::LExp& body)
    {
        return {noloc, Hs::LambdaExp(pats, body)};
    }

    Hs::LExp append_exp(const Hs::LExp& x, const Hs::LExp& y)
    {
        return Hs::apply(wired_var_exp(list_append_name), {x, y});
    }

    // Concatenate generated parser result lists without requiring a shared helper in Text.Read.
    Hs::LExp append_all_exp(const vector<Hs::LExp>& lists)
    {
        if (lists.empty())
            return list_exp({});

        auto result = lists.back();
        for(int i=lists.size()-2; i>=0; i--)
            result = append_exp(lists[i], result);
        return result;
    }

    Hs::LQual pat_qual(const Hs::LPat& pattern, const Hs::LExp& exp)
    {
        return {noloc, Hs::PatQual(pattern, exp)};
    }

    Hs::LExp read_paren_exp(const Hs::LExp& condition, const Hs::LExp& parser, const Hs::LExp& input)
    {
        return Hs::apply(wired_var_exp(read_read_paren_name), {condition, parser, input});
    }

    Hs::LExp read_constructor_exp(const string& constructor_name, const Hs::LExp& input)
    {
        return Hs::apply(wired_var_exp(read_read_constructor_name), {string_exp(get_unqualified_name(constructor_name)), input});
    }

    Hs::LExp read_field_name_exp(const string& field_name, const Hs::LExp& input)
    {
        return Hs::apply(wired_var_exp(read_read_field_name), {string_exp(get_unqualified_name(field_name)), input});
    }

    Hs::LExp read_infix_constructor_exp(const string& constructor_name, const Hs::LExp& input)
    {
        return Hs::apply(wired_var_exp(read_read_infix_constructor_name), {string_exp(get_unqualified_name(constructor_name)), input});
    }

    Hs::LExp read_punctuation_exp(const string& punctuation, const Hs::LExp& input)
    {
        return Hs::apply(wired_var_exp(read_read_punctuation_name), {string_exp(punctuation), input});
    }

    Hs::LExp reads_prec_exp(int precedence, const Hs::LExp& input)
    {
        return Hs::apply(wired_var_exp(read_reads_prec_name), {int_exp(precedence), input});
    }

    // Parse one prefix constructor and its fields using the syntax emitted by derived Show.
    Hs::LExp read_constructor_parser_exp(const Hs::ConstructorDecl& constructor)
    {
        vector<Hs::LQual> quals;
        quals.push_back(pat_qual(pair_pat(wildcard_pat(), var_pat("r$0")),
                                 read_constructor_exp(unloc(*constructor.con).name, local_var_exp("r$"))));

        vector<Hs::LExp> constructor_args;
        for(int i=0; i<constructor.arity(); i++)
        {
            auto field_var = "x$" + std::to_string(i);
            auto next_rest_var = "r$" + std::to_string(i + 1);
            constructor_args.push_back(local_var_exp(field_var));
            quals.push_back(pat_qual(pair_pat(var_pat(field_var), var_pat(next_rest_var)),
                                     reads_prec_exp(11, local_var_exp("r$" + std::to_string(i)))));
        }

        auto body = pair_exp(constructor_exp(constructor, constructor_args),
                             local_var_exp("r$" + std::to_string(constructor.arity())));
        return {noloc, Hs::ListComprehension(body, quals)};
    }

    // Parse record fields in declaration order.  Later support should allow
    // arbitrary order, reject duplicates, and report missing fields directly.
    Hs::LExp read_record_constructor_parser_exp(const Hs::ConstructorDecl& constructor)
    {
        auto field_names = record_field_names(constructor);
        vector<Hs::LQual> quals;
        quals.push_back(pat_qual(pair_pat(wildcard_pat(), var_pat("r$0")),
                                 read_constructor_exp(unloc(*constructor.con).name, local_var_exp("r$"))));
        quals.push_back(pat_qual(pair_pat(wildcard_pat(), var_pat("r$1")),
                                 read_punctuation_exp("{", local_var_exp("r$0"))));

        vector<Hs::LExp> constructor_args;
        int rest_index = 1;
        for(int i=0; i<field_names.size(); i++)
        {
            auto field_var = "x$" + std::to_string(i);
            constructor_args.push_back(local_var_exp(field_var));

            int previous_rest = rest_index++;
            quals.push_back(pat_qual(pair_pat(wildcard_pat(), var_pat("r$" + std::to_string(rest_index))),
                                     read_field_name_exp(field_names[i], local_var_exp("r$" + std::to_string(previous_rest)))));

            previous_rest = rest_index++;
            quals.push_back(pat_qual(pair_pat(wildcard_pat(), var_pat("r$" + std::to_string(rest_index))),
                                     read_punctuation_exp("=", local_var_exp("r$" + std::to_string(previous_rest)))));

            previous_rest = rest_index++;
            quals.push_back(pat_qual(pair_pat(var_pat(field_var), var_pat("r$" + std::to_string(rest_index))),
                                     reads_prec_exp(0, local_var_exp("r$" + std::to_string(previous_rest)))));

            if (i + 1 < field_names.size())
            {
                previous_rest = rest_index++;
                quals.push_back(pat_qual(pair_pat(wildcard_pat(), var_pat("r$" + std::to_string(rest_index))),
                                         read_punctuation_exp(",", local_var_exp("r$" + std::to_string(previous_rest)))));
            }
        }

        int previous_rest = rest_index++;
        quals.push_back(pat_qual(pair_pat(wildcard_pat(), var_pat("r$" + std::to_string(rest_index))),
                                 read_punctuation_exp("}", local_var_exp("r$" + std::to_string(previous_rest)))));

        auto body = pair_exp(constructor_exp(constructor, constructor_args),
                             local_var_exp("r$" + std::to_string(rest_index)));
        return {noloc, Hs::ListComprehension(body, quals)};
    }

    Hs::LExp read_infix_constructor_parser_exp(const Hs::ConstructorDecl& constructor)
    {
        constexpr int infix_con_prec = 5;
        constexpr int infix_con_operand_prec = infix_con_prec + 1;

        // FIXME: This mirrors the fixed-precedence derived Show form above.  A
        // complete implementation should consult the actual constructor fixity.
        vector<Hs::LQual> quals;
        quals.push_back(pat_qual(pair_pat(var_pat("x$0"), var_pat("r$0")),
                                 reads_prec_exp(infix_con_operand_prec, local_var_exp("r$"))));
        quals.push_back(pat_qual(pair_pat(wildcard_pat(), var_pat("r$1")),
                                 read_infix_constructor_exp(unloc(*constructor.con).name, local_var_exp("r$0"))));
        quals.push_back(pat_qual(pair_pat(var_pat("x$1"), var_pat("r$2")),
                                 reads_prec_exp(infix_con_operand_prec, local_var_exp("r$1"))));

        auto body = pair_exp(constructor_exp(constructor, {local_var_exp("x$0"), local_var_exp("x$1")}),
                             local_var_exp("r$2"));
        return {noloc, Hs::ListComprehension(body, quals)};
    }

    // Synthesize readsPrec and let read use the class default.
    Hs::InstanceDecl derive_read_instance(TypeChecker&, const Hs::DataOrNewtypeDecl& data_decl, const DerivingDataInfo& data_info, const std::optional<yy::location>& deriving_loc)
    {
        vector<Hs::LExp> constructor_parsers;
        for(const auto& constructor: data_decl.get_constructors())
        {
            Hs::LExp constructor_parser = constructor.is_record_constructor()
                ? read_record_constructor_parser_exp(constructor)
                : is_basic_infix_constructor(constructor)
                    ? read_infix_constructor_parser_exp(constructor)
                    : read_constructor_parser_exp(constructor);
            auto parser = lambda_exp({var_pat("r$")}, constructor_parser);
            auto needs_parens = is_basic_infix_constructor(constructor)
                ? greater_than_exp(local_var_exp("d$"), int_exp(5))
                : constructor.is_record_constructor() or constructor.arity() == 0
                    ? bool_exp(false)
                    : greater_than_exp(local_var_exp("d$"), int_exp(10));
            constructor_parsers.push_back(read_paren_exp(needs_parens, parser, local_var_exp("s$")));
        }

        auto body = append_all_exp(constructor_parsers);

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, read_reads_prec_name, {Hs::MRule({var_pat("d$"), var_pat("s$")}, Hs::SimpleRHS(body))}));

        return Hs::InstanceDecl({}, stock_instance_type(data_info, read_class_name), {}, {}, methods);
    }

    const vector<StockDerivingSpec>& stock_deriving_specs()
    {
        static const vector<StockDerivingSpec> specs = {
            {eq_class_name, true, derive_eq_instance, has_constructor_infos,
             "deriving Eq is only supported for data/newtype declarations with constructor metadata"},
            {ord_class_name, true, derive_ord_instance, has_constructor_infos,
             "deriving Ord is only supported for data/newtype declarations with constructor metadata"},
            {bounded_class_name, true, nullptr, has_constructor_infos,
             "deriving Bounded is only supported for data/newtype declarations with constructors"},
            {enum_class_name, false, nullptr, has_only_nullary_constructors,
             "deriving Enum is only supported for regular data declarations with only nullary constructors"},
            {ix_class_name, false, nullptr, has_only_nullary_constructors,
             "deriving Ix is only supported for regular data declarations with only nullary constructors"},
            {show_class_name, true, nullptr, has_constructor_infos,
             "deriving Show is only supported for data/newtype declarations with constructor metadata"},
            {read_class_name, true, nullptr, has_constructor_infos,
             "deriving Read is only supported for data/newtype declarations with prefix, record, or binary infix constructors"},
        };

        return specs;
    }

    optional<StockDerivingInfo> stock_deriving_class(const Module& mod, const Hs::LType& deriving)
    {
        auto [head, args] = Hs::decompose_type_apps(deriving);
        auto con = unloc(head).to<Hs::TypeCon>();
        if (not con or not args.empty())
            return {};

        auto type_info = mod.lookup_resolved_type(con->name);
        if (not type_info)
            return {};

        for(const auto& spec: stock_deriving_specs())
        {
            if (type_info->name == spec.class_name)
                return StockDerivingInfo{&spec, TypeCon(type_info->name, type_info->kind)};
        }

        return {};
    }

    // Resolve a deriving class head to class metadata and any fixed leading arguments.
    optional<GeneralizedNewtypeDerivingClass> resolved_deriving_class(const Module& mod, const Hs::LType& deriving)
    {
        auto [head, args] = Hs::decompose_type_apps(deriving);
        auto con = unloc(head).to<Hs::TypeCon>();
        if (not con)
            return {};

        auto type_info = mod.lookup_resolved_type(con->name);
        if (not type_info)
            return {};

        auto class_info = type_info->is_class();
        if (not class_info)
            return {};

        return GeneralizedNewtypeDerivingClass{class_info, args};
    }

    // Return the single semantic representation field type for one-constructor newtypes.
    optional<Type> newtype_representation_type(TypeChecker& tc, const DerivingDataInfo& data_info)
    {
        if (not data_info.data.info
            or data_info.data.info->data_or_newtype != Hs::DataOrNewtype::newtype
            or deriving_constructors(data_info).size() != 1)
            return {};

        auto con_info = deriving_constructor_info(tc, deriving_constructors(data_info)[0]);
        if (not con_info or con_info->field_types.size() != 1)
            return {};

        return con_info->field_types[0];
    }

    // Drop trailing data variables from the representation type when they match exactly.
    optional<Type> drop_eta_args(Type rep_type, const vector<TypeVar>& data_tvs, int eta_arity)
    {
        if (eta_arity == 0)
            return rep_type;

        auto [rep_head, rep_args] = decompose_type_apps(rep_type);
        if (data_tvs.size() < eta_arity or rep_args.size() < eta_arity)
            return {};

        int first_dropped_data_tv = data_tvs.size() - eta_arity;
        int first_dropped_rep_arg = rep_args.size() - eta_arity;
        for(int i=0; i<eta_arity; i++)
        {
            auto rep_tv = follow_meta_type_var(rep_args[first_dropped_rep_arg + i]).to<TypeVar>();
            if (not rep_tv or *rep_tv != data_tvs[first_dropped_data_tv + i])
                return {};
        }

        rep_args.resize(first_dropped_rep_arg);
        return type_apply(rep_head, rep_args);
    }

    // Build the synthetic instance head C fixed... (T prefix...) with a C fixed... rep-prefix context.
    optional<Hs::LType> generalized_newtype_instance_type(const DerivingDataInfo& data_info, const string& class_name, const vector<Hs::LType>& fixed_args, const Type& rep_type, int eta_arity, const optional<yy::location>& loc)
    {
        assert(data_info.data.info);
        int prefix_arity = data_info.data.info->type_vars.size() - eta_arity;

        auto hs_rep_type = semantic_type_to_hs_type(rep_type);
        auto context = semantic_data_context(data_info);
        if (not hs_rep_type or not context)
            return {};

        auto rep_constraint_args = fixed_args;
        rep_constraint_args.push_back(*hs_rep_type);
        context->push_back(class_constraint(class_name, rep_constraint_args));

        auto data_type = derived_data_type(data_info, prefix_arity);
        auto instance_args = fixed_args;
        instance_args.push_back(data_type);
        auto instance_head = class_constraint(class_name, instance_args);
        Hs::LType polytype = Hs::LType{loc, Hs::ConstrainedType(*context, instance_head)};
        auto quantified_tvs = hs_type_vars(data_info.data.info->type_vars, prefix_arity);
        return Hs::add_forall_vars(quantified_tvs, polytype);
    }

    // Build an empty DeriveAnyClass instance head C fixed... (T args...).
    Hs::LType anyclass_instance_type(const DerivingTarget& target, const string& class_name, const vector<Hs::LType>& fixed_args)
    {
        auto data_type = derived_data_type(target.data, target.data.data.info->type_vars.size());
        auto instance_args = fixed_args;
        instance_args.push_back(data_type);
        auto instance_head = class_constraint(class_name, instance_args);
        auto context = source_context_for_target(target);
        Hs::LType polytype = context.empty() ? instance_head : Hs::LType{noloc, Hs::ConstrainedType(context, instance_head)};
        return Hs::add_forall_vars(hs_type_vars(target.data.data.info->type_vars, target.data.data.info->type_vars.size()), polytype);
    }

    // Build the synthetic DerivingVia instance head C fixed... (T args...) with a C fixed... via context.
    Hs::LType deriving_via_instance_type(const DerivingTarget& target, const string& class_name, const vector<Hs::LType>& fixed_args, const Hs::LType& via_type)
    {
        auto data_type = derived_data_type(target.data, target.data.data.info->type_vars.size());

        Hs::Context context = source_context_for_target(target);
        auto via_constraint_args = fixed_args;
        via_constraint_args.push_back(via_type);
        context.push_back(class_constraint(class_name, via_constraint_args));

        auto instance_args = fixed_args;
        instance_args.push_back(data_type);
        auto instance_head = class_constraint(class_name, instance_args);
        Hs::LType polytype = Hs::LType{noloc, Hs::ConstrainedType(context, instance_head)};
        return Hs::add_forall_vars(hs_type_vars(target.data.data.info->type_vars, target.data.data.info->type_vars.size()), polytype);
    }

    // Synthesize an empty anyclass instance and let ordinary instance checking handle defaults.
    optional<Hs::InstanceDecl> derive_anyclass_instance(TypeChecker& tc, const DerivingTarget& target, const Hs::LType& deriving)
    {
        auto deriving_class = resolved_deriving_class(tc.this_mod(), deriving);
        if (not deriving_class)
            return {};

        auto class_info = deriving_class->info->info;
        if (not class_info)
            return {};

        if (deriving_class->fixed_args.size() + 1 != class_info->type_vars.size())
        {
            tc.record_error(deriving.loc, Note()<<"DeriveAnyClass expects the deriving clause to leave exactly one class parameter for the data/newtype");
            return {};
        }

        return Hs::InstanceDecl({}, anyclass_instance_type(target, class_info->name, deriving_class->fixed_args), {}, {}, {});
    }

    // Build the class-parameter substitution used by generated GND associated type instances.
    optional<Hs::LType> gnd_associated_family_arg(const vector<TypeVar>& class_tvs, const vector<Hs::LType>& fixed_args, const Hs::LType& data_type, const Hs::LType& rep_type, const TypeVar& tv, bool use_rep_type)
    {
        for(int i=0; i<class_tvs.size(); i++)
        {
            if (class_tvs[i] != tv)
                continue;

            if (i < fixed_args.size())
                return fixed_args[i];

            return use_rep_type ? rep_type : data_type;
        }

        return {};
    }

    // Synthesize associated type equations replacing the derived type argument with another type.
    vector<Hs::TypeFamilyInstanceDecl> associated_type_instances_for_replacement(TypeChecker& tc, const ClassInfo& class_info, const vector<Hs::LType>& fixed_args, const Hs::LType& data_type, const Hs::LType& rep_type, const optional<yy::location>& loc)
    {
        vector<Hs::TypeFamilyInstanceDecl> type_instances;
        for(auto& [tf_con, maybe_default]: class_info.associated_type_families)
        {
            auto tf_info = tc.info_for_type_fam(tf_con.name);
            assert(tf_info);

            vector<Hs::LType> lhs_args;
            vector<Hs::LType> rhs_args;
            for(auto& family_arg: tf_info->args)
            {
                auto lhs_arg = gnd_associated_family_arg(class_info.type_vars, fixed_args, data_type, rep_type, family_arg, false);
                auto rhs_arg = gnd_associated_family_arg(class_info.type_vars, fixed_args, data_type, rep_type, family_arg, true);

                if (lhs_arg and rhs_arg)
                {
                    lhs_args.push_back(*lhs_arg);
                    rhs_args.push_back(*rhs_arg);
                }
                else
                {
                    auto arg = Hs::LType{noloc, Hs::TypeVar(family_arg.name)};
                    lhs_args.push_back(arg);
                    rhs_args.push_back(arg);
                }
            }

            Hs::LTypeCon hs_tf_con{loc, Hs::TypeCon(tf_con.name)};
            auto rhs = Hs::type_apply(hs_tf_con, rhs_args);
            rhs.loc = loc;
            type_instances.push_back(Hs::TypeFamilyInstanceDecl(hs_tf_con, lhs_args, rhs));
        }

        return type_instances;
    }

    // Synthesize associated type equations mapping the newtype argument to its representation.
    optional<vector<Hs::TypeFamilyInstanceDecl>> generalized_newtype_associated_type_instances(TypeChecker& tc, const DerivingDataInfo& data_info, const ClassInfo& class_info, const vector<Hs::LType>& fixed_args, const Type& rep_type, int eta_arity, const optional<yy::location>& loc)
    {
        assert(data_info.data.info);
        int prefix_arity = data_info.data.info->type_vars.size() - eta_arity;
        auto hs_rep_type = semantic_type_to_hs_type(rep_type);
        if (not hs_rep_type)
            return {};

        auto data_type = derived_data_type(data_info, prefix_arity);
        return associated_type_instances_for_replacement(tc, class_info, fixed_args, data_type, *hs_rep_type, loc);
    }

    // Synthesize associated type equations mapping the derived type argument to the via type.
    vector<Hs::TypeFamilyInstanceDecl> deriving_via_associated_type_instances(TypeChecker& tc, const DerivingDataInfo& data_info, const ClassInfo& class_info, const vector<Hs::LType>& fixed_args, const Hs::LType& via_type, const optional<yy::location>& loc)
    {
        auto data_type = derived_data_type(data_info, data_info.data.info->type_vars.size());
        return associated_type_instances_for_replacement(tc, class_info, fixed_args, data_type, via_type, loc);
    }

    // Synthesize a GND instance header and leave method bodies for instance pass 2.
    GeneralizedNewtypeDerivingResult derive_generalized_newtype_instance(TypeChecker& tc, const DerivingTarget& target, const Hs::LType& deriving, bool explicit_newtype_strategy = false)
    {
        auto deriving_class = resolved_deriving_class(tc.this_mod(), deriving);
        if (not deriving_class)
            return {};

        auto class_info = deriving_class->info->info;
        if (not class_info)
            return {};

        const auto& deriving_data = target.data;
        bool is_newtype = deriving_data.data.info and deriving_data.data.info->data_or_newtype == Hs::DataOrNewtype::newtype;
        if (not is_newtype)
        {
            if (explicit_newtype_strategy)
            {
                tc.record_error(deriving.loc, Note()<<"deriving newtype is only supported for newtype declarations");
                return {true, {}};
            }
            return {};
        }

        if (deriving_class->fixed_args.size() + 1 != class_info->type_vars.size())
        {
            tc.record_error(deriving.loc, Note()<<"GeneralizedNewtypeDeriving expects the deriving clause to leave exactly one class parameter for the newtype");
            return {true, {}};
        }

        if (not class_info->associated_data_families.empty())
        {
            tc.record_error(deriving.loc, Note()<<"GeneralizedNewtypeDeriving for classes with associated data families is not supported yet");
            return {true, {}};
        }

        auto rep_type = newtype_representation_type(tc, deriving_data);
        if (not rep_type)
        {
            tc.record_error(deriving.loc, Note()<<"GeneralizedNewtypeDeriving is only supported for newtype declarations with one field");
            return {true, {}};
        }

        int newtype_param_index = deriving_class->fixed_args.size();
        int eta_arity = num_args_for_kind(class_info->type_vars[newtype_param_index].kind);
        auto eta_reduced_rep_type = drop_eta_args(*rep_type, deriving_data.data.info->type_vars, eta_arity);
        if (not eta_reduced_rep_type)
        {
            tc.record_error(deriving.loc, Note()<<"GeneralizedNewtypeDeriving cannot eta-reduce the representation type to match the class argument kind");
            return {true, {}};
        }

        auto associated_type_instances = generalized_newtype_associated_type_instances(tc, deriving_data, *class_info, deriving_class->fixed_args, *eta_reduced_rep_type, eta_arity, deriving.loc);
        auto instance_type = generalized_newtype_instance_type(deriving_data, class_info->name, deriving_class->fixed_args, *eta_reduced_rep_type, eta_arity, deriving.loc);
        if (not associated_type_instances or not instance_type)
        {
            tc.record_error(deriving.loc, Note()<<"GeneralizedNewtypeDeriving could not convert the semantic representation type into a generated instance");
            return {true, {}};
        }

        Hs::InstanceDecl instance({}, *instance_type, *associated_type_instances, {}, {});
        instance.generalized_newtype_deriving = true;
        return {true, instance};
    }

    // Synthesize a DerivingVia instance whose methods are coerced from the via dictionary.
    optional<Hs::InstanceDecl> derive_via_instance(TypeChecker& tc, const DerivingTarget& target, const Hs::Deriving& deriving)
    {
        if (not deriving.via_type)
        {
            tc.record_error(deriving.type.loc, Note()<<"DerivingVia requires a via type");
            return {};
        }

        auto deriving_class = resolved_deriving_class(tc.this_mod(), deriving.type);
        if (not deriving_class)
            return {};

        auto class_info = deriving_class->info->info;
        if (not class_info)
            return {};

        if (deriving_class->fixed_args.size() + 1 != class_info->type_vars.size())
        {
            tc.record_error(deriving.type.loc, Note()<<"DerivingVia expects the deriving clause to leave exactly one class parameter for the data/newtype");
            return {};
        }

        if (not class_info->associated_data_families.empty())
        {
            tc.record_error(deriving.type.loc, Note()<<"DerivingVia for classes with associated data families is not supported yet");
            return {};
        }

        auto associated_type_instances = deriving_via_associated_type_instances(tc, target.data, *class_info, deriving_class->fixed_args, *deriving.via_type, deriving.type.loc);
        Hs::InstanceDecl instance({}, deriving_via_instance_type(target, class_info->name, deriving_class->fixed_args, *deriving.via_type), associated_type_instances, {}, {});
        instance.generalized_newtype_deriving = true;
        return instance;
    }

    // Override standalone stock heads, but keep inferred contexts needed by coercive deriving.
    void add_derived_instance(Hs::Decls& instances, const optional<yy::location>& loc, Hs::InstanceDecl instance, const DerivingTarget& target)
    {
        if (target.explicit_polytype and not instance.generalized_newtype_deriving)
            instance.polytype = *target.explicit_polytype;
        instances.push_back({loc, instance});
    }

    // Dispatch one deriving clause through stock deriving or GND according to its strategy.
    void synthesize_deriving_clause(TypeChecker& tc, Hs::Decls& instances, const DerivingTarget& target, const Hs::Deriving& deriving)
    {
        if (deriving.strategy == Hs::DerivingStrategy::via)
        {
            auto instance = derive_via_instance(tc, target, deriving);
            if (instance)
                add_derived_instance(instances, deriving.type.loc, *instance, target);
            else
                tc.record_error(deriving.type.loc, Note()<<"DerivingVia deriving "<<deriving.type.print()<<" is not supported yet");
            return;
        }

        if (deriving.strategy == Hs::DerivingStrategy::anyclass)
        {
            auto instance = derive_anyclass_instance(tc, target, deriving.type);
            if (instance)
                add_derived_instance(instances, deriving.type.loc, *instance, target);
            else
                tc.record_error(deriving.type.loc, Note()<<"DeriveAnyClass deriving "<<deriving.type.print()<<" is not supported yet");
            return;
        }

        auto derive_stock = [&]() -> bool
        {
            auto derived_class = stock_deriving_class(tc.this_mod(), deriving.type);
            if (not derived_class)
                return false;

            const auto& spec = *derived_class->spec;

            auto& semantic_data = target.data;
            if (not has_semantic_data_info(semantic_data))
            {
                tc.record_error(deriving.type.loc, Note()<<spec.unsupported_message);
                return true;
            }

            if (spec.class_name == bounded_class_name)
            {
                if (not spec.validate(tc, semantic_data))
                {
                    tc.record_error(deriving.type.loc, Note()<<spec.unsupported_message);
                    return true;
                }

                add_derived_instance(instances, deriving.type.loc, derive_bounded_instance(tc, semantic_data, deriving.type.loc), target);
                return true;
            }

            if (spec.class_name == enum_class_name)
            {
                if (not spec.validate(tc, semantic_data))
                {
                    tc.record_error(deriving.type.loc, Note()<<spec.unsupported_message);
                    return true;
                }

                add_derived_instance(instances, deriving.type.loc, derive_enum_instance(tc, semantic_data, deriving.type.loc), target);
                return true;
            }

            if (spec.class_name == ix_class_name)
            {
                if (not spec.validate(tc, semantic_data))
                {
                    tc.record_error(deriving.type.loc, Note()<<spec.unsupported_message);
                    return true;
                }

                add_derived_instance(instances, deriving.type.loc, derive_ix_instance(tc, semantic_data, deriving.type.loc), target);
                return true;
            }

            if (spec.class_name == show_class_name)
            {
                if (not target.source_decl or not spec.validate(tc, semantic_data) or not target.source_decl->is_regular_decl())
                {
                    tc.record_error(deriving.type.loc, Note()<<spec.unsupported_message);
                    return true;
                }

                add_derived_instance(instances, deriving.type.loc, derive_show_instance(tc, *target.source_decl, semantic_data, deriving.type.loc), target);
                return true;
            }

            if (spec.class_name == read_class_name)
            {
                if (not target.source_decl or not spec.validate(tc, semantic_data) or not is_regular_data_decl_with_readable_constructors(*target.source_decl))
                {
                    tc.record_error(deriving.type.loc, Note()<<spec.unsupported_message);
                    return true;
                }

                add_derived_instance(instances, deriving.type.loc, derive_read_instance(tc, *target.source_decl, semantic_data, deriving.type.loc), target);
                return true;
            }

            if (not spec.validate(tc, semantic_data))
            {
                tc.record_error(deriving.type.loc, Note()<<spec.unsupported_message);
                return true;
            }

            assert(spec.derive);
            add_derived_instance(instances, deriving.type.loc, spec.derive(tc, semantic_data, deriving.type.loc), target);
            return true;
        };

        if (deriving.strategy == Hs::DerivingStrategy::stock)
        {
            if (not derive_stock())
                tc.record_error(deriving.type.loc, Note()<<"stock deriving "<<deriving.type.print()<<" is not supported yet");
            return;
        }

        if (deriving.strategy == Hs::DerivingStrategy::newtype)
        {
            auto gnd = derive_generalized_newtype_instance(tc, target, deriving.type, true);
            if (gnd.instance)
                add_derived_instance(instances, deriving.type.loc, *gnd.instance, target);
            else if (not gnd.handled)
                tc.record_error(deriving.type.loc, Note()<<"newtype deriving "<<deriving.type.print()<<" is not supported yet");
            return;
        }

        if (not derive_stock())
        {
            auto gnd = derive_generalized_newtype_instance(tc, target, deriving.type);
            if (gnd.instance)
                add_derived_instance(instances, deriving.type.loc, *gnd.instance, target);
            else if (not gnd.handled)
                tc.record_error(deriving.type.loc, Note()<<"deriving "<<deriving.type.print()<<" is not supported yet");
        }
    }

    // Convert a standalone deriving instance head into a deriving clause and its target type.
    optional<pair<DerivingTarget, Hs::Deriving>> standalone_deriving_target(TypeChecker& tc, const Hs::Decls& decls, const Hs::StandaloneDerivingDecl& standalone)
    {
        auto peeled = Hs::peel_top_gen(standalone.polytype);
        auto instance_head = std::get<2>(peeled);
        auto [class_head, class_args] = Hs::decompose_type_apps(instance_head);
        if (class_args.empty())
        {
            tc.record_error(instance_head.loc, Note()<<"Standalone deriving instance head must apply a class to a data/newtype type");
            return {};
        }

        auto target_type = class_args.back();
        class_args.pop_back();

        auto [target_head, target_args] = Hs::decompose_type_apps(target_type);
        auto target_con = unloc(target_head).to<Hs::TypeCon>();
        if (not target_con)
        {
            tc.record_error(target_type.loc, Note()<<"Standalone deriving target must be a data/newtype type constructor application");
            return {};
        }

        auto target = deriving_target_for_type(tc, decls, *target_con, standalone.polytype);
        if (target)
        {
            Hs::Deriving deriving(standalone.strategy, Hs::type_apply(class_head, class_args), standalone.via_type);
            return pair<DerivingTarget, Hs::Deriving>{*target, deriving};
        }

        tc.record_error(target_head.loc, Note()<<"Standalone deriving target `"<<target_type.print()<<"` is not a data/newtype type");
        return {};
    }
}

Hs::Decls TypeChecker::synthesize_derived_instances(const Hs::Decls& decls)
{
    Hs::Decls instances;

    for(auto& [_,decl]: decls)
    {
        if (auto data_decl = decl.to<Hs::DataOrNewtypeDecl>())
        {
            for(auto& deriving: data_decl->derivings)
            {
                if (auto target = deriving_target_for_decl(*this, *data_decl))
                    synthesize_deriving_clause(*this, instances, *target, deriving);
            }
        }
        else if (auto standalone = decl.to<Hs::StandaloneDerivingDecl>())
        {
            if (auto target = standalone_deriving_target(*this, decls, *standalone))
            {
                synthesize_deriving_clause(*this, instances, target->first, target->second);
            }
        }
        else if (auto data_inst = decl.to<Hs::DataFamilyInstanceDecl>())
        {
            if (not data_inst->rhs.derivings.empty())
                record_error(data_inst->rhs.derivings.front().type.loc, Note()<<"deriving clauses on data family instances are not supported yet");
        }
    }

    return instances;
}

// Return the first derived constraint that cannot be solved or floated into the instance context.
optional<Type> TypeChecker::find_missing_derived_constraint(const Type& pred, const set<TypeVar>& data_tvs, vector<Type>& active)
{
    if (is_allowed_derived_context_pred(pred, data_tvs))
        return {};

    for(auto& active_pred: active)
        if (active_pred == pred)
            return {};

    active.push_back(pred);

    auto inst = lookup_instance(pred);
    if (not inst)
    {
        active.pop_back();
        return pred;
    }

    auto& [_, wanteds] = *inst;
    for(auto& wanted: wanteds)
    {
        if (auto missing = find_missing_derived_constraint(wanted.pred, data_tvs, active))
        {
            active.pop_back();
            return missing;
        }
    }

    active.pop_back();
    return {};
}

// Check stock-derived instances after pass 1 has registered local instance headers.
void TypeChecker::check_derived_instances(const Hs::Decls& decls)
{
    for(auto& [_, decl]: decls)
    {
        auto data_decl = decl.to<Hs::DataOrNewtypeDecl>();
        if (not data_decl)
            continue;

        auto type_info = this_mod().lookup_resolved_type(unloc(data_decl->con).name);
        auto data_info = type_info ? type_info->is_data() : nullptr;
        if (not data_info or not data_info->info)
            continue;

        for(auto& deriving: data_decl->derivings)
        {
            if (deriving.strategy and deriving.strategy != Hs::DerivingStrategy::stock)
                continue;

            auto derived_class = stock_deriving_class(this_mod(), deriving.type);
            if (not derived_class or not derived_class->spec->needs_field_constraints)
                continue;

            for(const auto& con_name: data_info->info->constructors)
            {
                auto con_info = this_mod().constructor_info(con_name);
                if (not con_info)
                    continue;

                set<TypeVar> data_tvs(con_info->uni_tvs.begin(), con_info->uni_tvs.end());
                for(int i=0; i<con_info->field_types.size(); i++)
                {
                    auto pred = class_constraint(derived_class->type_con, con_info->field_types[i]);
                    vector<Type> active;
                    auto missing = find_missing_derived_constraint(pred, data_tvs, active);
                    if (not missing)
                        continue;

                    auto span = source_span_scope(deriving.type.loc);
                    TidyState tidy_state;
                    auto data_name = data_info->info ? data_info->info->name : unloc(data_decl->con).name;
                    auto instance_pred = class_constraint(derived_class->type_con, type_apply(TypeCon(data_name), con_info->uni_tvs));
                    auto note = note_scope(Note()<<"When deriving the instance for "<<show_type_plain(tidy_state, instance_pred));
                    record_error(Note()<<"Could not deduce '"<<show_type_plain(tidy_state, *missing)<<"' arising from field "<<(i+1)<<" of constructor '"<<get_unqualified_name(con_name)<<"' (type '"<<show_type_plain(tidy_state, con_info->field_types[i])<<"')");
                }
            }
        }
    }
}
