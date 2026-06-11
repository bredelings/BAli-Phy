#include <string>
#include <vector>
#include <set>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::optional;
using std::pair;
using std::set;

namespace
{
    Hs::LPat record_field_pun_pattern(const Hs::LVar& field)
    {
        auto name = get_unqualified_name(unloc(field).name);
        return {field.loc, Hs::VarPattern({field.loc, Hs::Var(name)})};
    }

    void add_field_layout_name(record_constructor_layout& layout, const string& field_name, int index)
    {
        layout.fields[field_name] = index;
        layout.fields[get_unqualified_name(field_name)] = index;
    }

    void add_field_constructor(std::map<string,set<string>>& constructors, const string& field_name, const string& con_name)
    {
        constructors[field_name].insert(con_name);
        constructors[get_unqualified_name(field_name)].insert(con_name);
    }

    // Flatten grouped record labels into constructor field order.
    vector<string> field_decl_names(const Hs::FieldDecls& field_decls)
    {
        vector<string> names;
        for(const auto& field_decl: field_decls.field_decls)
            for(const auto& field_name: field_decl.field_names)
                names.push_back(unloc(field_name).name);
        return names;
    }

    // Extract labels from GADT record signatures like C :: { x :: A } -> T.
    optional<vector<string>> gadt_constructor_field_names(Hs::LType type)
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
}

// The issue here is to rewrite @ f x y -> f x y
// so that f is actually the head.
Hs::LPat unapply(Hs::LExp LE)
{
    auto& E = unloc(LE);

    Hs::LPat LP;
    LP.loc = LE.loc;
    auto& P = unloc(LP);

    if (auto l = E.to<Hs::List>())
    {
        Hs::ListPattern LP;
        for(auto& pattern: l->elements)
            LP.elements.push_back( unapply(pattern) );
        P = LP;
    }
    else if (auto t = E.to<Hs::Tuple>())
    {
        Hs::TuplePattern TP;
        for(auto& pattern: t->elements)
            TP.elements.push_back( unapply(pattern) );
        P = TP;
    }
    else if (auto ap = E.to<Hs::AsPattern>())
    {
        P = Hs::AsPattern(ap->var, unapply(ap->pattern));
    }
    else if (auto lp = E.to<Hs::LazyPattern>())
    {
        P = Hs::LazyPattern(unapply(lp->pattern));
    }
    else if (E.is_a<Hs::StrictPattern>())
    {
        auto SP = E.as_<Hs::StrictPattern>();
        SP.pattern = unapply(SP.pattern);
        P = SP;
    }
    else if (E.is_a<Hs::ParsedApp>())
    {
        auto [head, args] = Hs::decompose_apps(LE);

        auto con = unloc(head).to<Hs::Con>();
        if (not con)
            throw myexception()<<"In pattern `"<<E<<"`:\n    `"<<head<<"` is not a data constructor.";

        Hs::LPats pat_args;
        for(auto& arg: args)
            pat_args.push_back(unapply(arg));

        P = Hs::ConPattern({head.loc,*con}, pat_args);
    }
    else if (E.is_a<Hs::ApplyExp>())
    {
        auto [head, args] = Hs::decompose_apps(LE);

        // We shouldn't have e.g. (@ (@ f x) y) -- this should already be dealt with by rename_infix
        auto con = unloc(head).to<Hs::Con>();
        if (not con)
            throw myexception()<<"In pattern `"<<E<<"`:\n    `"<<head<<"` is not a data constructor.";

        Hs::LPats pat_args;
        for(auto& arg: args)
            pat_args.push_back(unapply(arg));

        P = Hs::ConPattern({head.loc,*con}, pat_args);
    }
    else if (auto r = E.to<Hs::RecordSyntax>())
    {
        auto con = unloc(r->head).to<Hs::Con>();
        if (not con)
            throw myexception()<<"In pattern `"<<E<<"`:\n    `"<<r->head<<"` is not a data constructor.";

        Hs::PatternFieldBindings fbinds;
        fbinds.dotdot = unloc(r->fbinds).dotdot;

        for(auto& lfield: unloc(r->fbinds))
        {
            auto& field = unloc(lfield);
            auto pattern = field.value ? unapply(*field.value) : record_field_pun_pattern(field.field);
            fbinds.push_back({lfield.loc, Hs::PatternFieldBinding(field.field, pattern)});
        }

        P = Hs::RecordPattern({r->head.loc, *con}, {r->fbinds.loc, fbinds});
    }
    else if (auto r = E.to<Hs::RecordCon>())
    {
        Hs::PatternFieldBindings fbinds;
        fbinds.dotdot = unloc(r->fbinds).dotdot;

        for(auto& lfield: unloc(r->fbinds))
        {
            auto& field = unloc(lfield);
            auto pattern = field.value ? unapply(*field.value) : record_field_pun_pattern(field.field);
            fbinds.push_back({lfield.loc, Hs::PatternFieldBinding(field.field, pattern)});
        }

        P = Hs::RecordPattern(r->con, {r->fbinds.loc, fbinds});
    }
    else if (E.is_a<Hs::RecordUpdate>())
    {
        throw myexception()<<"In pattern `"<<E<<"`:\n    record update syntax is not valid in a pattern.";
    }
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        Hs::TypedPattern TP;
        TP.pat = unapply(texp->exp);
        TP.type = texp->type;
        P = TP;
    }
    else if (auto l = E.to<Hs::Literal>())
        P = Hs::LiteralPattern(*l);
    else if (auto c = E.to<Hs::Con>())
        P = Hs::ConPattern({LE.loc,*c}, {});
    else if (auto v = E.to<Hs::Var>())
        P = Hs::VarPattern({LE.loc,*v});
    else if (E.is_a<Hs::WildcardPattern>())
        P = Hs::WildcardPattern();
    else {
        // Can we return a real error message?
        throw myexception()<<"unapply: '"<<unloc(LE).print()<<"' does not seem to be a pattern.";
        P = Hs::WildcardPattern();
    }

    return LP;
}

void renamer_state::record_record_layouts(const Hs::Decls& decls)
{
    auto record_constructor_layouts_for = [&](const Hs::ConstructorsDecl& constructors)
    {
        for(const auto& constructor: constructors)
        {
            if (not constructor.is_record_constructor())
                continue;

            record_constructor_layout layout;
            layout.arity = constructor.arity();
            auto con_name = unloc(*constructor.con).name;

            int i = 0;
            for(auto& field_group: std::get<1>(constructor.fields).field_decls)
                for(auto& field_name: field_group.field_names)
                {
                    add_field_layout_name(layout, unloc(field_name).name, i++);
                    add_field_constructor(record_field_constructors, unloc(field_name).name, con_name);
                }

            record_constructor_layouts[con_name] = layout;
            record_constructor_layouts[get_unqualified_name(con_name)] = layout;
        }
    };

    auto gadt_record_constructor_layouts_for = [&](const Hs::GADTConstructorsDecl& constructors)
    {
        for(const auto& constructor: constructors)
        {
            auto field_names = gadt_constructor_field_names(constructor.type);
            if (not field_names)
                continue;

            record_constructor_layout layout;
            layout.arity = field_names->size();
            for(int i=0; i<field_names->size(); i++)
                add_field_layout_name(layout, (*field_names)[i], i);

            for(const auto& con_name: constructor.con_names)
            {
                auto name = unloc(con_name);
                for(const auto& field_name: *field_names)
                    add_field_constructor(record_field_constructors, field_name, name);

                record_constructor_layouts[name] = layout;
                record_constructor_layouts[get_unqualified_name(name)] = layout;
            }
        }
    };

    for(const auto& [_,decl]: decls)
    {
        if (auto D = decl.to<Hs::DataOrNewtypeDecl>(); D and D->is_regular_decl())
            record_constructor_layouts_for(D->get_constructors());
        else if (auto D = decl.to<Hs::DataOrNewtypeDecl>(); D and D->is_gadt_decl())
            gadt_record_constructor_layouts_for(D->get_gadt_constructors());
        else if (auto D = decl.to<Hs::DataFamilyInstanceDecl>(); D and D->rhs.is_regular_decl())
            record_constructor_layouts_for(D->rhs.get_constructors());
        else if (auto D = decl.to<Hs::DataFamilyInstanceDecl>(); D and D->rhs.is_gadt_decl())
            gadt_record_constructor_layouts_for(D->rhs.get_gadt_constructors());
    }
}

std::optional<record_constructor_layout> renamer_state::record_layout_for_constructor(const string& con_name)
{
    auto layout = record_constructor_layouts.find(con_name);
    if (layout == record_constructor_layouts.end())
        layout = record_constructor_layouts.find(get_unqualified_name(con_name));
    if (layout != record_constructor_layouts.end())
        return layout->second;

    auto con_info = m.constructor_info(con_name);
    if (not con_info or not con_info->field_names)
        return {};

    record_constructor_layout semantic_layout;
    semantic_layout.arity = con_info->arity();
    for(int i=0; i<con_info->field_names->size(); i++)
        add_field_layout_name(semantic_layout, (*con_info->field_names)[i], i);

    record_constructor_layouts[con_info->name] = semantic_layout;
    record_constructor_layouts[get_unqualified_name(con_info->name)] = semantic_layout;
    for(const auto& field_name: *con_info->field_names)
        add_field_constructor(record_field_constructors, field_name, con_info->name);

    return semantic_layout;
}

bound_var_info renamer_state::rename_patterns(Hs::LPats& patterns, bool top)
{
    bound_var_info bound;

    // Rename the arguments
    for(auto& e: patterns)
    {
	auto bound_here =  rename_pattern(e, top);
        auto overlap = intersection(bound, bound_here);
        if (not overlap.empty())
        {
            auto name = *overlap.begin();
            error(Note()<<"Pattern uses variable '"<<name<<"' twice!");
        }
	add(bound, bound_here);
    }

    return bound;
}

// FIXME - can we just call rename_pattern on this directly???
// Convert ids to vars in pattern, and return a set of all names for vars (excluding wildcards, of course)
// A single variable is a valid "pattern" for the purposes of this function.
bound_var_info renamer_state::find_vars_in_pattern(const Hs::LPat& lpat, bool top)
{
    auto& [loc,pat] = lpat;

    assert(not is_apply_exp(pat));
    assert(not pat.is_a<Hs::ApplyExp>());

    // 1. Handle _
    if (pat.is_a<Hs::WildcardPattern>())
	return {};

    // 2. Handle ~pat or !pat
    else if (pat.is_a<Hs::LazyPattern>())
    {
        auto LP = pat.as_<Hs::LazyPattern>();
        return find_vars_in_pattern(LP.pattern, top);
    }
    else if (pat.is_a<Hs::StrictPattern>())
    {
        auto SP = pat.as_<Hs::StrictPattern>();
        return find_vars_in_pattern(SP.pattern, top);
    }
    else if (auto tpat = pat.to<Hs::TypedPattern>())
    {
        return find_vars_in_pattern(tpat->pat, top);
    }
    // 3. Handle x@pat
    else if (pat.is_a<Hs::AsPattern>())
    {
        auto& AP = pat.as_<Hs::AsPattern>();
	assert(not top);

	auto bound = find_vars_in_pattern({noloc,Hs::VarPattern(AP.var)}, top);
	bool overlap = not disjoint_add(bound, find_vars_in_pattern(AP.pattern, top));

	if (overlap)
	    error(Note()<<"Pattern '"<<pat<<"' uses a variable twice!");
	return bound;
    }

    else if (pat.is_a<Hs::ListPattern>())
    {
        auto& L = pat.as_<Hs::ListPattern>();
        return find_vars_in_patterns(L.elements, top);
    }
    else if (pat.is_a<Hs::TuplePattern>())
    {
        auto& T = pat.as_<Hs::TuplePattern>();
        return find_vars_in_patterns(T.elements, top);
    }
    else if (auto v = pat.to<Hs::VarPattern>())
    {
        auto id = unloc(v->var).name;

	if (is_qualified_symbol(id))
        {
            error(lpat.loc, Note()<<"Binder variable '"<<id<<"' is qualified in pattern '"<<pat<<"'!");
            return {};
        }

	// Qualify the id if this is part of a top-level decl
	if (top)
	    id = m.name + "." + id;
	return {id};
    }
    // If its a constructor pattern!
    else if (auto c = pat.to<Hs::ConPattern>())
    {
        auto id = unloc(c->head).name;

        if (not m.is_declared(id))
            error(loc, Note()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!");
        else
        {
            try
            {
                auto S = m.lookup_symbol(id);
                if (S->symbol_type != symbol_type_t::constructor)
                    error(loc, Note()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!");

                // FIXME -- we really want the location of the whole pattern here
                if (*S->arity != c->args.size())
                    error(loc, Note()<<"Pattern has "<<c->args.size()<<" fields, but should have "<<*S->arity<<"!");
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }

        // 11. Return the variables bound
        return find_vars_in_patterns(c->args, top);
    }
    else if (auto r = pat.to<Hs::RecordPattern>())
    {
        if (unloc(r->fbinds).dotdot)
            error(lpat.loc, Note()<<"Record wildcards in patterns are not implemented yet.");

        Hs::LPats field_patterns;
        for(const auto& field: unloc(r->fbinds))
            field_patterns.push_back(unloc(field).pattern);

        return find_vars_in_patterns(field_patterns, top);
    }
    else if (pat.is_a<Hs::LiteralPattern>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}

bound_var_info renamer_state::rename_var_pattern(Hs::LVar& LV, bool top)
{
    auto& [loc,V] = LV;
    auto id = V.name;

    if (is_qualified_symbol(id))
        error(loc, Note() << "Binder variable '"<<id<<"' is qualified in pattern '"<<V.print()<<"'!");
    // Qualify the id if this is part of a top-level decl
    if (top)
        id = m.name + "." + id;
    else
    {
        // FIXME - since we are creating an ID here, we should give it a unique id!
    }
    V.name = id;

    return {id};
}

// Convert ids to vars in pattern, and return a set of all names for vars (excluding wildcards, of course)
// A single variable is a valid "pattern" for the purposes of this function.
bound_var_info renamer_state::rename_pattern(Hs::LPat& lpat, bool top)
{
    auto& [loc,pat] = lpat;

    assert(not is_apply_exp(pat));
    assert(not pat.is_a<Hs::ApplyExp>());

    // 1. Handle _
    if (pat.is_a<Hs::WildcardPattern>())
	return {};

    // 2. Handle ~pat
    if (pat.is_a<Hs::LazyPattern>())
    {
        auto LP = pat.as_<Hs::LazyPattern>();
	auto bound = rename_pattern(LP.pattern, top);
	pat = LP;
	return bound;
    }
    else if (pat.is_a<Hs::StrictPattern>())
    {
        auto SP = pat.as_<Hs::StrictPattern>();
	auto bound = rename_pattern(SP.pattern, top);
	pat = SP;
	return bound;
    }
    else if (auto tpat = pat.to<Hs::TypedPattern>())
    {
        auto TPat = *tpat;
        auto bound = rename_pattern(TPat.pat, top);
        TPat.type = rename_and_quantify_type(TPat.type);
        pat = TPat;
        return bound;
    }

    // 3. Handle x@pat
    if (pat.is_a<Hs::AsPattern>())
    {
        auto AP = pat.as_<Hs::AsPattern>();
	assert(not top);

	auto bound = rename_var_pattern(AP.var, false);
	bool overlap = not disjoint_add(bound, rename_pattern(AP.pattern, false));
        if (overlap)
            error(Note()<<"Pattern '"<<pat<<"' uses a variable twice!");

	pat = AP;

	return bound;
    }
    
    //4. Handle List pattern.
    if (pat.is_a<Hs::ListPattern>())
    {
        auto L = pat.as_<Hs::ListPattern>();
        auto bound = rename_patterns(L.elements,top);
        pat = L;
        return bound;
    }
    //5. Handle List pattern.
    else if (pat.is_a<Hs::TuplePattern>())
    {
        auto T = pat.as_<Hs::TuplePattern>();
        auto bound = rename_patterns(T.elements,top);
        pat = T;
        return bound;
    }
    else if (auto v = pat.to<Hs::VarPattern>())
    {
        auto V = *v;
        auto bound = rename_var_pattern(V.var, top);
        pat = V;
	return bound;
    }
    else if (auto c = pat.to<Hs::ConPattern>())
    {
        auto C = *c;

        // 8. Rename arguments and accumulate bound variables
        vector<expression_ref> args = pat.copy_sub();

        bound_var_info bound;
        // Rename the arguments
        bool overlap = false;
        for(auto& e: C.args)
        {
            auto bound_here =  rename_pattern(e, top);
            overlap = overlap or not disjoint_add(bound, bound_here);
        }

        if (overlap)
            error(Note()<<"Pattern '"<<pat<<"' uses a variable twice!");

        auto id = unloc(C.head).name;

        // 7. Resolve constructor name if identifier is a constructor
        if (not m.is_declared(id))
            error(loc, Note()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!");
        else
        {
            try
            {
                auto S = m.lookup_symbol(id);
                assert(S);
                if (S->symbol_type != symbol_type_t::constructor)
                    error(loc, Note()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!");

                // FIXME -- we really want the location of the whole pattern here
                if (*S->arity != c->args.size())
                    error(loc, Note()<<"Pattern has "<<c->args.size()<<" fields, but should have "<<*S->arity<<"!");

                unloc(C.head).name = S->name;
                unloc(C.head).arity = *S->arity;

                // 10. Construct the renamed pattern
                pat = C;
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }

        // 11. Return the variables bound
        return bound;
    }
    else if (auto r = pat.to<Hs::RecordPattern>())
    {
        auto R = *r;

        if (unloc(R.fbinds).dotdot)
            error(lpat.loc, Note()<<"Record wildcards in patterns are not implemented yet.");

        bound_var_info bound;
        bool overlap = false;
        for(auto& field: unloc(R.fbinds))
        {
            auto bound_here = rename_pattern(unloc(field).pattern, top);
            overlap = overlap or not disjoint_add(bound, bound_here);
        }

        if (overlap)
            error(Note()<<"Pattern '"<<pat<<"' uses a variable twice!");

        auto id = unloc(R.head).name;

        if (not m.is_declared(id))
            error(loc, Note()<<"Unknown id '"<<id<<"' used as constructor in pattern '"<<pat<<"'!");
        else
        {
            try
            {
                auto S = m.lookup_symbol(id);
                assert(S);
                if (S->symbol_type != symbol_type_t::constructor)
                    error(loc, Note()<<"Id '"<<id<<"' is not a constructor in pattern '"<<pat<<"'!");

                unloc(R.head).name = S->name;
                unloc(R.head).arity = *S->arity;

                auto layout = record_layout_for_constructor(S->name);

                if (not layout)
                    error(loc, Note()<<"Constructor '"<<id<<"' is not a record constructor in pattern '"<<pat<<"'!");
                else
                {
                    Hs::LPats args(layout->arity, {noloc,Hs::WildcardPattern()});
                    set<int> used_fields;

                    for(auto& field: unloc(R.fbinds))
                    {
                        auto field_name = unloc(unloc(field).field).name;
                        auto pos = layout->fields.find(field_name);
                        if (pos == layout->fields.end())
                            pos = layout->fields.find(get_unqualified_name(field_name));

                        if (pos == layout->fields.end())
                            error(field.loc, Note()<<"Constructor '"<<id<<"' does not have field '"<<field_name<<"'.");
                        else if (used_fields.count(pos->second))
                            error(field.loc, Note()<<"Field '"<<field_name<<"' appears more than once in pattern '"<<pat<<"'.");
                        else
                        {
                            used_fields.insert(pos->second);
                            args[pos->second] = unloc(field).pattern;
                        }
                    }

                    pat = Hs::ConPattern(R.head, args);
                }
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }

        return bound;
    }
    // 4. Handle literal values
    else if (pat.is_a<Hs::LiteralPattern>())
        return {};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}
