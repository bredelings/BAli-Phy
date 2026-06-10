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
    using StockDeriver = Hs::InstanceDecl (*)(const Hs::DataOrNewtypeDecl&, const std::optional<yy::location>&);
    using StockValidator = bool (*)(const Hs::DataOrNewtypeDecl&);

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

    bool is_regular_data_decl(const Hs::DataOrNewtypeDecl& data_decl)
    {
        return data_decl.is_regular_decl();
    }

    bool is_regular_data_decl_with_constructors(const Hs::DataOrNewtypeDecl& data_decl)
    {
        return data_decl.is_regular_decl() and not data_decl.get_constructors().empty();
    }

    bool has_only_nullary_constructors(const Hs::DataOrNewtypeDecl& data_decl)
    {
        if (not data_decl.is_regular_decl() or data_decl.get_constructors().empty())
            return false;

        for(const auto& constructor: data_decl.get_constructors())
            if (constructor.arity() != 0)
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

    map<string, vector<optional<yy::location>>> source_field_locs_by_constructor(const Hs::DataOrNewtypeDecl& data_decl)
    {
        map<string, vector<optional<yy::location>>> field_locs;
        if (not data_decl.is_regular_decl())
            return field_locs;

        for(const auto& constructor: data_decl.get_constructors())
        {
            auto con_name = unloc(*constructor.con).name;
            for(const auto& field_type: constructor.get_field_types())
                field_locs[con_name].push_back(field_type.loc);
        }

        return field_locs;
    }
}

namespace
{
    Hs::LType type_var_type(const Hs::LTypeVar& tv)
    {
        return {tv.loc, Hs::TypeVar(unloc(tv))};
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

    bool is_type_var(const Hs::LType& type, const Hs::LTypeVar& tv)
    {
        auto type_var = unloc(type).to<Hs::TypeVar>();
        return type_var and *type_var == unloc(tv);
    }

    vector<Hs::LType> type_var_types(const vector<Hs::LTypeVar>& tvs, int n)
    {
        vector<Hs::LType> types;
        for(int i=0; i<n; i++)
            types.push_back(type_var_type(tvs[i]));
        return types;
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

    Hs::LPat constructor_var_pattern(const Hs::ConstructorDecl& constructor, const string& prefix)
    {
        Hs::LPats args;
        for(int i=0; i<constructor.arity(); i++)
            args.push_back(var_pat(prefix + std::to_string(i)));

        return constructor_pattern(constructor, args);
    }

    Hs::LPat constructor_wildcard_pattern(const Hs::ConstructorDecl& constructor)
    {
        Hs::LPats args(constructor.arity(), wildcard_pat());
        return constructor_pattern(constructor, args);
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

    Hs::LType stock_instance_type(const Hs::DataOrNewtypeDecl& data_decl, const string& class_name)
    {
        vector<Hs::LType> data_args;
        Hs::Context context = data_decl.context;
        for(auto& tv: data_decl.type_vars)
        {
            auto tv_type = type_var_type(tv);
            data_args.push_back(tv_type);
            context.push_back(class_constraint(class_name, tv_type));
        }

        auto data_type = type_con_type(data_decl.con, data_args);
        auto instance_head = class_constraint(class_name, data_type);
        Hs::LType polytype = context.empty() ? instance_head : Hs::LType{data_decl.con.loc, Hs::ConstrainedType(context, instance_head)};
        return Hs::add_forall_vars(data_decl.type_vars, polytype);
    }

    Hs::InstanceDecl derive_eq_instance(const Hs::DataOrNewtypeDecl& data_decl, const std::optional<yy::location>& deriving_loc)
    {
        Hs::Matches eq_matches;
        for(const auto& constructor: data_decl.get_constructors())
        {
            vector<pair<Hs::LExp,Hs::LExp>> fields;
            for(int i=0; i<constructor.arity(); i++)
                fields.push_back({local_var_exp("x$" + std::to_string(i)), local_var_exp("y$" + std::to_string(i))});

            eq_matches.push_back(binary_method_rule(constructor_var_pattern(constructor, "x$"),
                                                    constructor_var_pattern(constructor, "y$"),
                                                    eq_all_exp(fields)));
        }

        auto wildcard = wildcard_pat();
        eq_matches.push_back(binary_method_rule(wildcard, wildcard, bool_exp(false)));

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, eq_method_name, eq_matches));

        return Hs::InstanceDecl({}, stock_instance_type(data_decl, eq_class_name), {}, {}, methods);
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

    Hs::LExp constructor_tag_exp(const Hs::DataOrNewtypeDecl& data_decl, const Hs::LExp& value)
    {
        Hs::Alts alts;
        const auto& constructors = data_decl.get_constructors();

        for(int i=0; i<constructors.size(); i++)
            alts.push_back(simple_alt(constructor_wildcard_pattern(constructors[i]), int_exp(i)));

        return case_exp(value, alts);
    }

    Hs::LExp compare_constructor_tags_exp(const Hs::DataOrNewtypeDecl& data_decl, const Hs::LExp& x, const Hs::LExp& y)
    {
        // Preliminary source-level form of GHC's dataToTag#/tag layout idea:
        // synthesize a case over constructors now, but keep this isolated so it
        // can later become a shared helper or Core primitive known to the optimizer.
        return compare_exp(constructor_tag_exp(data_decl, x), constructor_tag_exp(data_decl, y));
    }

    Hs::InstanceDecl derive_ord_instance(const Hs::DataOrNewtypeDecl& data_decl, const std::optional<yy::location>& deriving_loc)
    {
        Hs::Matches compare_matches;
        const auto& constructors = data_decl.get_constructors();

        for(const auto& constructor: constructors)
        {
            vector<pair<Hs::LExp,Hs::LExp>> fields;
            for(int i=0; i<constructor.arity(); i++)
                fields.push_back({local_var_exp("x$" + std::to_string(i)), local_var_exp("y$" + std::to_string(i))});

            compare_matches.push_back(binary_method_rule(constructor_var_pattern(constructor, "x$"),
                                                         constructor_var_pattern(constructor, "y$"),
                                                         compare_all_exp(fields)));
        }

        if (not constructors.empty())
        {
            compare_matches.push_back(binary_method_rule(var_pat("x$tag"),
                                                         var_pat("y$tag"),
                                                         compare_constructor_tags_exp(data_decl, local_var_exp("x$tag"), local_var_exp("y$tag"))));
        }

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, ord_compare_name, compare_matches));

        return Hs::InstanceDecl({}, stock_instance_type(data_decl, ord_class_name), {}, {}, methods);
    }

    Hs::MRule nullary_method_rule(const Hs::LExp& rhs)
    {
        return Hs::MRule({}, Hs::SimpleRHS(rhs));
    }

    Hs::LDecl nullary_method_decl(const std::optional<yy::location>& loc, const string& method_name, const Hs::LExp& rhs)
    {
        return derived_method_decl(loc, method_name, {nullary_method_rule(rhs)});
    }

    Hs::LExp bounded_constructor_exp(const Hs::ConstructorDecl& constructor, const string& bound_method_name)
    {
        vector<Hs::LExp> args(constructor.arity(), wired_var_exp(bound_method_name));
        return constructor_exp(constructor, args);
    }

    Hs::InstanceDecl derive_bounded_instance(const Hs::DataOrNewtypeDecl& data_decl, const std::optional<yy::location>& deriving_loc)
    {
        const auto& constructors = data_decl.get_constructors();

        Hs::Decls methods;
        methods.push_back(nullary_method_decl(deriving_loc, bounded_min_bound_name, bounded_constructor_exp(constructors.front(), bounded_min_bound_name)));
        methods.push_back(nullary_method_decl(deriving_loc, bounded_max_bound_name, bounded_constructor_exp(constructors.back(), bounded_max_bound_name)));

        return Hs::InstanceDecl({}, stock_instance_type(data_decl, bounded_class_name), {}, {}, methods);
    }

    Hs::LPat int_pat(int i)
    {
        return {noloc, Hs::LiteralPattern(Hs::Literal(Hs::BoxedInteger{integer(i)}))};
    }

    Hs::InstanceDecl derive_enum_instance(const Hs::DataOrNewtypeDecl& data_decl, const std::optional<yy::location>& deriving_loc)
    {
        const auto& constructors = data_decl.get_constructors();
        int max_tag = constructors.size() - 1;

        Hs::Matches from_enum_matches;
        Hs::Matches to_enum_matches;
        for(int i=0; i<constructors.size(); i++)
        {
            from_enum_matches.push_back(unary_method_rule(constructor_pattern(constructors[i], {}), int_exp(i)));
            to_enum_matches.push_back(unary_method_rule(int_pat(i), constructor_exp(constructors[i], {})));
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

        return Hs::InstanceDecl({}, stock_instance_type(data_decl, enum_class_name), {}, {}, methods);
    }

    Hs::LPat pair_pat(const Hs::LPat& x, const Hs::LPat& y)
    {
        return {noloc, Hs::TuplePattern({x, y})};
    }

    Hs::LExp pair_exp(const Hs::LExp& x, const Hs::LExp& y)
    {
        return {noloc, Hs::Tuple({x, y})};
    }

    Hs::LExp ix_tag_exp(const Hs::DataOrNewtypeDecl& data_decl, const string& var_name)
    {
        return constructor_tag_exp(data_decl, local_var_exp(var_name));
    }

    Hs::LExp ix_in_range_exp(const Hs::DataOrNewtypeDecl& data_decl, const string& lo, const string& hi, const string& x)
    {
        auto lo_tag = ix_tag_exp(data_decl, lo);
        auto hi_tag = ix_tag_exp(data_decl, hi);
        auto x_tag = ix_tag_exp(data_decl, x);

        return if_exp(greater_than_exp(lo_tag, x_tag),
                      bool_exp(false),
                      if_exp(greater_than_exp(x_tag, hi_tag), bool_exp(false), bool_exp(true)));
    }

    Hs::InstanceDecl derive_ix_instance(const Hs::DataOrNewtypeDecl& data_decl, const std::optional<yy::location>& deriving_loc)
    {
        const auto& constructors = data_decl.get_constructors();
        Hs::LPat bounds_pat = pair_pat(var_pat("lo$"), var_pat("hi$"));
        Hs::LExp bounds_exp = pair_exp(local_var_exp("lo$"), local_var_exp("hi$"));

        vector<Hs::LExp> constructor_exps;
        for(const auto& constructor: constructors)
            constructor_exps.push_back(constructor_exp(constructor, {}));
        Hs::LExp all_constructors = {noloc, Hs::List(constructor_exps)};

        Hs::Matches range_matches;
        auto in_range_bounds = Hs::apply(wired_var_exp(ix_in_range_name), {bounds_exp});
        range_matches.push_back(unary_method_rule(bounds_pat, Hs::apply(wired_var_exp(list_filter_name), {in_range_bounds, all_constructors})));

        Hs::Matches index_matches;
        index_matches.push_back(binary_method_rule(bounds_pat, var_pat("x$"),
                                                   subtract_exp(ix_tag_exp(data_decl, "x$"), ix_tag_exp(data_decl, "lo$"))));

        Hs::Matches in_range_matches;
        in_range_matches.push_back(binary_method_rule(bounds_pat, var_pat("x$"), ix_in_range_exp(data_decl, "lo$", "hi$", "x$")));

        Hs::Matches range_size_matches;
        auto size = add_exp(subtract_exp(ix_tag_exp(data_decl, "hi$"), ix_tag_exp(data_decl, "lo$")), int_exp(1));
        range_size_matches.push_back(unary_method_rule(bounds_pat,
                                                       if_exp(greater_than_exp(ix_tag_exp(data_decl, "lo$"), ix_tag_exp(data_decl, "hi$")),
                                                              int_exp(0),
                                                              size)));

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, ix_range_name, range_matches));
        methods.push_back(derived_method_decl(deriving_loc, ix_index_name, index_matches));
        methods.push_back(derived_method_decl(deriving_loc, ix_in_range_name, in_range_matches));
        methods.push_back(derived_method_decl(deriving_loc, ix_range_size_name, range_size_matches));

        return Hs::InstanceDecl({}, stock_instance_type(data_decl, ix_class_name), {}, {}, methods);
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

    Hs::LExp shows_prec_constructor_exp(const Hs::ConstructorDecl& constructor)
    {
        auto con_name = get_unqualified_name(unloc(*constructor.con).name);
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

    Hs::InstanceDecl derive_show_instance(const Hs::DataOrNewtypeDecl& data_decl, const std::optional<yy::location>& deriving_loc)
    {
        Hs::Matches shows_prec_matches;
        for(const auto& constructor: data_decl.get_constructors())
        {
            shows_prec_matches.push_back(Hs::MRule({var_pat("d$"), constructor_var_pattern(constructor, "x$")},
                                                   Hs::SimpleRHS(shows_prec_constructor_exp(constructor))));
        }

        Hs::Decls methods;
        methods.push_back(derived_method_decl(deriving_loc, show_shows_prec_name, shows_prec_matches));

        return Hs::InstanceDecl({}, stock_instance_type(data_decl, show_class_name), {}, {}, methods);
    }

    const vector<StockDerivingSpec>& stock_deriving_specs()
    {
        static const vector<StockDerivingSpec> specs = {
            {eq_class_name, true, derive_eq_instance, is_regular_data_decl,
             "deriving Eq is only supported for regular data/newtype declarations"},
            {ord_class_name, true, derive_ord_instance, is_regular_data_decl,
             "deriving Ord is only supported for regular data/newtype declarations"},
            {bounded_class_name, true, derive_bounded_instance, is_regular_data_decl_with_constructors,
             "deriving Bounded is only supported for regular data/newtype declarations with constructors"},
            {enum_class_name, false, derive_enum_instance, has_only_nullary_constructors,
             "deriving Enum is only supported for regular data declarations with only nullary constructors"},
            {ix_class_name, false, derive_ix_instance, has_only_nullary_constructors,
             "deriving Ix is only supported for regular data declarations with only nullary constructors"},
            {show_class_name, true, derive_show_instance, is_regular_data_decl,
             "deriving Show is only supported for regular data/newtype declarations"},
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

    // Return the single representation field type for regular one-field newtypes.
    optional<Hs::LType> newtype_representation_type(const Hs::DataOrNewtypeDecl& data_decl)
    {
        if (data_decl.data_or_newtype != Hs::DataOrNewtype::newtype
            or not data_decl.is_regular_decl()
            or data_decl.get_constructors().size() != 1
            or data_decl.get_constructors()[0].arity() != 1)
            return {};

        return data_decl.get_constructors()[0].get_field_types()[0];
    }

    // Drop trailing data variables from the representation type when they match exactly.
    optional<Hs::LType> drop_eta_args(const Hs::LType& rep_type, const vector<Hs::LTypeVar>& data_tvs, int eta_arity)
    {
        if (eta_arity == 0)
            return rep_type;

        auto [rep_head, rep_args] = Hs::decompose_type_apps(rep_type);
        if (data_tvs.size() < eta_arity or rep_args.size() < eta_arity)
            return {};

        int first_dropped_data_tv = data_tvs.size() - eta_arity;
        int first_dropped_rep_arg = rep_args.size() - eta_arity;
        for(int i=0; i<eta_arity; i++)
            if (not is_type_var(rep_args[first_dropped_rep_arg + i], data_tvs[first_dropped_data_tv + i]))
                return {};

        rep_args.resize(first_dropped_rep_arg);
        return Hs::type_apply(rep_head, rep_args);
    }

    // Build the synthetic instance head C fixed... (T prefix...) with a C fixed... rep-prefix context.
    Hs::LType generalized_newtype_instance_type(const Hs::DataOrNewtypeDecl& data_decl, const string& class_name, const vector<Hs::LType>& fixed_args, const Hs::LType& rep_type, int eta_arity)
    {
        int prefix_arity = data_decl.type_vars.size() - eta_arity;
        auto data_args = type_var_types(data_decl.type_vars, prefix_arity);

        Hs::Context context = data_decl.context;
        auto rep_constraint_args = fixed_args;
        rep_constraint_args.push_back(rep_type);
        context.push_back(class_constraint(class_name, rep_constraint_args));

        auto data_type = type_con_type(data_decl.con, data_args);
        auto instance_args = fixed_args;
        instance_args.push_back(data_type);
        auto instance_head = class_constraint(class_name, instance_args);
        Hs::LType polytype = Hs::LType{data_decl.con.loc, Hs::ConstrainedType(context, instance_head)};
        vector<Hs::LTypeVar> quantified_tvs(data_decl.type_vars.begin(), data_decl.type_vars.begin() + prefix_arity);
        return Hs::add_forall_vars(quantified_tvs, polytype);
    }

    // Synthesize a GND instance header and leave method bodies for instance pass 2.
    GeneralizedNewtypeDerivingResult derive_generalized_newtype_instance(TypeChecker& tc, const Hs::DataOrNewtypeDecl& data_decl, const Hs::LType& deriving)
    {
        auto deriving_class = resolved_deriving_class(tc.this_mod(), deriving);
        if (not deriving_class)
            return {};

        auto class_info = deriving_class->info->info;
        if (not class_info)
            return {};

        if (data_decl.data_or_newtype != Hs::DataOrNewtype::newtype)
            return {};

        if (deriving_class->fixed_args.size() + 1 != class_info->type_vars.size())
        {
            tc.record_error(deriving.loc, Note()<<"GeneralizedNewtypeDeriving expects the deriving clause to leave exactly one class parameter for the newtype");
            return {true, {}};
        }

        if (not class_info->associated_type_families.empty() or not class_info->associated_data_families.empty())
        {
            tc.record_error(deriving.loc, Note()<<"GeneralizedNewtypeDeriving for classes with associated families is not supported yet");
            return {true, {}};
        }

        auto rep_type = newtype_representation_type(data_decl);
        if (not rep_type)
        {
            tc.record_error(deriving.loc, Note()<<"GeneralizedNewtypeDeriving is only supported for newtype declarations with one field");
            return {true, {}};
        }

        int newtype_param_index = deriving_class->fixed_args.size();
        int eta_arity = num_args_for_kind(class_info->type_vars[newtype_param_index].kind);
        auto eta_reduced_rep_type = drop_eta_args(*rep_type, data_decl.type_vars, eta_arity);
        if (not eta_reduced_rep_type)
        {
            tc.record_error(deriving.loc, Note()<<"GeneralizedNewtypeDeriving cannot eta-reduce the representation type to match the class argument kind");
            return {true, {}};
        }

        Hs::InstanceDecl instance({}, generalized_newtype_instance_type(data_decl, class_info->name, deriving_class->fixed_args, *eta_reduced_rep_type, eta_arity), {}, {}, {});
        instance.generalized_newtype_deriving = true;
        return {true, instance};
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
                auto derived_class = stock_deriving_class(this_mod(), deriving);
                if (not derived_class)
                {
                    auto gnd = derive_generalized_newtype_instance(*this, *data_decl, deriving);
                    if (gnd.instance)
                        instances.push_back({deriving.loc, *gnd.instance});
                    else if (not gnd.handled)
                        record_error(deriving.loc, Note()<<"deriving "<<deriving.print()<<" is not supported yet");
                    continue;
                }

                const auto& spec = *derived_class->spec;
                if (not spec.validate(*data_decl))
                {
                    record_error(deriving.loc, Note()<<spec.unsupported_message);
                    continue;
                }

                instances.push_back({deriving.loc, spec.derive(*data_decl, deriving.loc)});
            }
        }
        else if (auto data_inst = decl.to<Hs::DataFamilyInstanceDecl>())
        {
            if (not data_inst->rhs.derivings.empty())
                record_error(data_inst->rhs.derivings.front().loc, Note()<<"deriving clauses on data family instances are not supported yet");
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
        if (not data_decl or not data_decl->is_regular_decl())
            continue;

        auto type_info = this_mod().lookup_resolved_type(unloc(data_decl->con).name);
        auto data_info = type_info ? type_info->is_data() : nullptr;
        if (not data_info)
            continue;

        auto source_field_locs = source_field_locs_by_constructor(*data_decl);

        for(auto& deriving: data_decl->derivings)
        {
            auto derived_class = stock_deriving_class(this_mod(), deriving);
            if (not derived_class or not derived_class->spec->needs_field_constraints)
                continue;

            for(const auto& con_name: data_info->constructors)
            {
                auto con_info = this_mod().constructor_info(con_name);
                if (not con_info)
                    continue;

                set<TypeVar> data_tvs(con_info->uni_tvs.begin(), con_info->uni_tvs.end());
                auto locs = source_field_locs.find(con_name);

                for(int i=0; i<con_info->field_types.size(); i++)
                {
                    auto pred = class_constraint(derived_class->type_con, con_info->field_types[i]);
                    vector<Type> active;
                    auto missing = find_missing_derived_constraint(pred, data_tvs, active);
                    if (not missing)
                        continue;

                    auto field_loc = (locs != source_field_locs.end() and i < locs->second.size()) ? locs->second[i] : deriving.loc;
                    auto span = source_span_scope(field_loc);
                    TidyState tidy_state;
                    auto instance_pred = class_constraint(derived_class->type_con, type_apply(TypeCon(unloc(data_decl->con).name), con_info->uni_tvs));
                    auto note = note_scope(Note()<<"When deriving the instance for "<<show_type_plain(tidy_state, instance_pred));
                    record_error(Note()<<"Could not deduce '"<<show_type_plain(tidy_state, *missing)<<"' arising from field "<<(i+1)<<" of constructor '"<<get_unqualified_name(con_name)<<"' (type '"<<show_type_plain(tidy_state, con_info->field_types[i])<<"')");
                }
            }
        }
    }
}
