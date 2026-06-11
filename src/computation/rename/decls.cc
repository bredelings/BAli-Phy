#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>
#include <deque>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "util/graph.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::tuple;
using std::set;
using std::optional;
using std::map;
using std::deque;

/*
 * We probably want to move away from using dummies to represent patterns.
 * - Dummies can't represent e.g. irrefutable patterns.
 */

// What would be involved in moving the renamer to a kind of phase 2?
// How do we get the exported symbols before we do the desugaring that depends on imports?

// rename_infix does:
// (i) precedence handling for infix expressions
// (ii) rewrites @ f x y -> f x y for decls
// (iii) rewrites @ C x y -> C x y for patterns

// Consider h:t !! y.  This can be h:(t!!y) or (h:t)!!y

// We might have @ (infix x op y) z.  Infix handling will rewrite this to
// @ (@ op x y) z.  We need to change this to (@ op x y z).
// However, if we have @ (: x y) z, then we don't want to rewrite this to (: x y z).
// What are the rules for well-formed patterns?
// Only one op can be a non-constructor (in decl patterns), and that op needs to end up at the top level.

// tuple<map<Hs::LVar,Hs::LType>, map<Hs::LVar, Hs::inline_pragma_t>, Hs::Decls> group_decls(const Haskell::Decls& decls); // value decls, signature decls, and fixity decls


// Recognize syntax nodes that are already unambiguously pattern-category.
bool is_definitely_pattern(const Haskell::Expression& lhs)
{
    if (lhs.is_a<Haskell::List>())
        return true;
    else if (lhs.is_a<Haskell::Tuple>())
        return true;
    else if (lhs.is_a<Haskell::AsPattern>())
        return true;
    else if (lhs.is_a<Haskell::LazyPattern>())
        return true;
    else if (lhs.is_a<Haskell::StrictPattern>())
        return true;
    else if (lhs.is_a<Haskell::ConPattern>())
        return true;
    else if (lhs.is_a<Haskell::VarPattern>())
        return true;
    else if (lhs.is_a<Haskell::LiteralPattern>())
        return true;

    return false;
}

// Detect the leading prefix-negation marker in an unresolved infix spine.
bool is_prefix_neg(const vector<Hs::LExp>& terms)
{
    return not terms.empty() and unloc(terms[0]).is_a<Hs::Neg>();
}

// Return whether a spine term occupies an operator slot, accounting for prefix negation.
bool is_infix_operator_term(const vector<Hs::LExp>& terms, int index)
{
    if (is_prefix_neg(terms))
        return index != 0 and index % 2 == 0;
    else
        return index % 2 == 1;
}

// Return whether a spine term occupies an operand slot that should be pattern-classified.
bool is_infix_operand_term(const vector<Hs::LExp>& terms, int index)
{
    return not unloc(terms[index]).is_a<Hs::Neg>() and not is_infix_operator_term(terms, index);
}

// Rebuild an unresolved infix expression from a slice of spine terms.
Hs::LExp make_infix_exp(const vector<Hs::LExp>& terms)
{
    assert(not terms.empty());
    if (terms.size() == 1)
        return terms[0];

    auto loc = terms.front().loc;
    for(const auto& term: terms)
        loc = loc * term.loc;

    return {loc, Hs::InfixExp(terms)};
}

Hs::LPat expression_to_pattern_category(Hs::LExp lhs);

// Convert expression-form record fields into pattern-form record fields.
Hs::PatternFieldBindings pattern_field_bindings(const Hs::FieldBindings& fields)
{
    Hs::PatternFieldBindings pattern_fields;
    pattern_fields.dotdot = fields.dotdot;

    for(const auto& lfield: fields)
    {
        const auto& field = unloc(lfield);
        Hs::LPat pattern;
        if (field.value)
            pattern = expression_to_pattern_category(*field.value);
        else
        {
            auto name = get_unqualified_name(unloc(field.field).name);
            pattern = {field.field.loc, Hs::VarPattern({field.field.loc, Hs::Var(name)})};
        }
        pattern_fields.push_back({lfield.loc, Hs::PatternFieldBinding(field.field, pattern)});
    }

    return pattern_fields;
}

// Convert parsed expression-category syntax into pattern-category syntax without resolving fixity.
Hs::LPat expression_to_pattern_category(Hs::LExp lhs)
{
    auto& E = unloc(lhs);

    if (auto I = E.to<Hs::InfixExp>())
    {
        if (I->terms.size() == 1)
            return expression_to_pattern_category(I->terms[0]);

        auto terms = I->terms;
        for(int i=0; i<terms.size(); i++)
            if (is_infix_operand_term(terms, i))
                terms[i] = expression_to_pattern_category(terms[i]);

        return {lhs.loc, Hs::InfixPat(terms)};
    }
    else if (E.is_a<Hs::ParsedApp>())
    {
        auto [head,args] = Hs::decompose_apps(lhs);
        if (auto con = unloc(head).to<Hs::Con>())
        {
            Hs::LPats pat_args;
            for(auto& arg: args)
                pat_args.push_back(expression_to_pattern_category(arg));
            return {lhs.loc, Hs::ConPattern({head.loc, *con}, pat_args)};
        }

        error(lhs.loc, Note()<<"Function application '"<<lhs<<"' is not valid in a pattern unless its head is a constructor.");
        return {lhs.loc, Hs::WildcardPattern()};
    }
    else if (E.is_a<Hs::ApplyExp>())
    {
        auto [head,args] = Hs::decompose_apps(lhs);
        if (auto con = unloc(head).to<Hs::Con>())
        {
            Hs::LPats pat_args;
            for(auto& arg: args)
                pat_args.push_back(expression_to_pattern_category(arg));
            return {lhs.loc, Hs::ConPattern({head.loc, *con}, pat_args)};
        }

        error(lhs.loc, Note()<<"Function application '"<<lhs<<"' is not valid in a pattern unless its head is a constructor.");
        return {lhs.loc, Hs::WildcardPattern()};
    }
    else if (auto r = E.to<Hs::RecordSyntax>())
    {
        auto con = unloc(r->head).to<Hs::Con>();
        if (not con)
        {
            error(lhs.loc, Note()<<"Record syntax '"<<lhs<<"' is not valid in a pattern unless its head is a constructor.");
            return {lhs.loc, Hs::WildcardPattern()};
        }

        return {lhs.loc, Hs::RecordPattern({r->head.loc, *con}, {r->fbinds.loc, pattern_field_bindings(unloc(r->fbinds))})};
    }
    else if (auto r = E.to<Hs::RecordCon>())
        return {lhs.loc, Hs::RecordPattern(r->con, {r->fbinds.loc, pattern_field_bindings(unloc(r->fbinds))})};
    else if (E.is_a<Hs::RecordUpdate>())
    {
        error(lhs.loc, Note()<<"Record update syntax '"<<lhs<<"' is not valid in a pattern.");
        return {lhs.loc, Hs::WildcardPattern()};
    }
    else if (auto l = E.to<Hs::List>())
    {
        Hs::ListPattern P;
        for(auto& element: l->elements)
            P.elements.push_back(expression_to_pattern_category(element));
        return {lhs.loc, P};
    }
    else if (auto t = E.to<Hs::Tuple>())
    {
        Hs::TuplePattern P;
        for(auto& element: t->elements)
            P.elements.push_back(expression_to_pattern_category(element));
        return {lhs.loc, P};
    }
    else if (auto ap = E.to<Hs::AsPattern>())
        return {lhs.loc, Hs::AsPattern(ap->var, expression_to_pattern_category(ap->pattern))};
    else if (auto lp = E.to<Hs::LazyPattern>())
        return {lhs.loc, Hs::LazyPattern(expression_to_pattern_category(lp->pattern))};
    else if (auto sp = E.to<Hs::StrictPattern>())
        return {lhs.loc, Hs::StrictPattern(expression_to_pattern_category(sp->pattern))};
    else if (auto t = E.to<Hs::TypedExp>())
    {
        Hs::TypedPattern P;
        P.pat = expression_to_pattern_category(t->exp);
        P.type = t->type;
        return {lhs.loc, P};
    }
    else if (auto l = E.to<Hs::Literal>())
        return {lhs.loc, Hs::LiteralPattern(*l)};
    else if (auto c = E.to<Hs::Con>())
        return {lhs.loc, Hs::ConPattern({lhs.loc, *c}, {})};
    else if (auto v = E.to<Hs::Var>())
        return {lhs.loc, Hs::VarPattern({lhs.loc, *v})};
    else if (E.is_a<Hs::WildcardPattern>())
        return {lhs.loc, Hs::WildcardPattern()};
    else if (E.is_a<Hs::VarPattern>() or E.is_a<Hs::ConPattern>() or E.is_a<Hs::InfixPat>() or
             E.is_a<Hs::LiteralPattern>() or E.is_a<Hs::ListPattern>() or E.is_a<Hs::TuplePattern>() or
             E.is_a<Hs::RecordPattern>() or E.is_a<Hs::TypedPattern>())
        return lhs;
    else
    {
        error(lhs.loc, Note()<<"Expression '"<<lhs<<"' is not valid in a pattern.");
        return {lhs.loc, Hs::WildcardPattern()};
    }
}

// Decide whether a parsed value binding is a function declaration or pattern declaration.
expression_ref classify_value_decl(const Hs::ValueDecl& D)
{
    // Classify the declaration LHS while carrying arguments peeled from nested applications.
    auto classify_lhs = [&](auto&& self, Hs::LExp lhs, Hs::LPats extra_args) -> expression_ref
    {
        auto& E = unloc(lhs);

        if (auto I = E.to<Hs::InfixExp>())
        {
            if (I->terms.size() == 1)
                return self(self, I->terms[0], extra_args);

            for(int i=0; i<I->terms.size(); i++)
            {
                if (not is_infix_operator_term(I->terms, i))
                    continue;

                if (auto v = unloc(I->terms[i]).to<Hs::Var>())
                {
                    vector<Hs::LExp> left_terms(I->terms.begin(), I->terms.begin() + i);
                    vector<Hs::LExp> right_terms(I->terms.begin() + i + 1, I->terms.end());

                    Hs::LPats pats;
                    pats.push_back(expression_to_pattern_category(make_infix_exp(left_terms)));
                    pats.push_back(expression_to_pattern_category(make_infix_exp(right_terms)));
                    pats.insert(pats.end(), extra_args.begin(), extra_args.end());
                    return Hs::simple_fun_decl({I->terms[i].loc, *v}, pats, D.rhs);
                }
            }

            return Hs::PatDecl(expression_to_pattern_category(lhs), D.rhs);
        }
        else if (E.is_a<Hs::ParsedApp>())
        {
            auto [head,args] = Hs::decompose_apps(lhs);
            Hs::LPats pats;
            for(auto& arg: args)
                pats.push_back(expression_to_pattern_category(arg));
            pats.insert(pats.end(), extra_args.begin(), extra_args.end());

            if (auto v = unloc(head).to<Hs::Var>())
                return Hs::simple_fun_decl({head.loc, *v}, pats, D.rhs);
            else if (unloc(head).is_a<Hs::Con>())
                return Hs::PatDecl(expression_to_pattern_category(lhs), D.rhs);
            else
                return self(self, head, pats);
        }
        else if (E.is_a<Hs::ApplyExp>())
        {
            auto [head,args] = Hs::decompose_apps(lhs);
            Hs::LPats pats;
            for(auto& arg: args)
                pats.push_back(expression_to_pattern_category(arg));
            pats.insert(pats.end(), extra_args.begin(), extra_args.end());

            if (auto v = unloc(head).to<Hs::Var>())
                return Hs::simple_fun_decl({head.loc, *v}, pats, D.rhs);
            else if (unloc(head).is_a<Hs::Con>())
                return Hs::PatDecl(expression_to_pattern_category(lhs), D.rhs);
            else
                return self(self, head, pats);
        }
        else if (auto v = E.to<Hs::Var>())
        {
            if (extra_args.empty())
                return Hs::simple_decl({lhs.loc, *v}, D.rhs);
            else
                return Hs::simple_fun_decl({lhs.loc, *v}, extra_args, D.rhs);
        }
        else
            return Hs::PatDecl(expression_to_pattern_category(lhs), D.rhs);
    };

    return classify_lhs(classify_lhs, D.lhs, {});
}

// Classify a raw value declaration, leaving already-classified declarations unchanged.
expression_ref classify_value_decl(const expression_ref& E)
{
    if (auto D = E.to<Hs::ValueDecl>())
        return classify_value_decl(*D);
    else
        return E;
}

expression_ref rename_infix_decl(const Module& m, const expression_ref& E)
{
    if (E.is_a<Haskell::ValueDecl>())
    {
        return rename_infix_decl(m, classify_value_decl(E));
    }
    else if (auto D = E.to<Hs::PatDecl>())
    {
        auto D2 = *D;
        D2.rhs = rename_infix(m, D2.rhs);
        return D2;
    }
    else if (auto D = E.to<Hs::FunDecl>())
    {
        auto D2 = *D;
        for(auto& match: D2.matches)
            match.rhs = rename_infix(m, match.rhs);
        return D2;
    }
    else if (E.is_a<Hs::TypeSigDecl>())
        return E;
    else if (E.is_a<Hs::FixityDecl>())
        return E;
    else if (E.is_a<Hs::InlinePragma>())
        return E;
    else if (E.is_a<Hs::FamilyDecl>() or E.is_a<Hs::TypeFamilyInstanceDecl>())
    {
        // We get here for type family stuff inside of class declarations.
        // Ignoring infix type names for now?
        return E;
    }
    else
        std::abort();
}

optional<Hs::LVar> fundecl_head(const expression_ref& decl)
{
    if (auto fd = decl.to<Hs::FunDecl>())
        return fd->v;
    else
        return {};
}

// Probably we should first partition by (same x y = x and y are both function decls for the same variable)
tuple<map<Hs::LVar,Hs::LType>, map<Hs::LVar, Hs::inline_pragma_t>, Hs::Decls> group_decls(const Haskell::Decls& decls)
{
    map<Hs::LVar, Hs::LType> type_sigs;

    map<Hs::LVar, Hs::inline_pragma_t> inline_sigs;

    Hs::Decls decls2;

    for(int i=0;i<decls.size();i++)
    {
        auto [loc,decl] = decls[i];
        // Remove signature and fixity decls after recording type_sigs.
        if (auto sd = decl.to<Hs::TypeSigDecl>())
        {
            for(auto& lvar: sd->vars)
            {
                if (type_sigs.count(lvar))
                    throw myexception()<<"Second signature for var '"<<unloc(lvar).name<<"' at location "<<*lvar.loc;
                else
                    type_sigs.insert({lvar, sd->type});
            }
        }
        else if (decl.is_a<Hs::FixityDecl>())
        {
            // FixityDecls should survive up to this point so that we can properly segment decls.
            // But remove them here -> the type-checker shouldn't see them.
        }
        else if (auto d = decl.to<Hs::PatDecl>())
        {
            decls2.push_back({loc,*d});
        }
        else if (auto ip = decl.to<Hs::InlinePragma>())
        {
            Hs::Var x(unloc(ip->var));
            inline_sigs.insert({{ip->var.loc, x}, ip->command});
        }
        else if (auto fvar = fundecl_head(decl))
        {
            Hs::Matches m;
            for(int j=i;j<decls.size();j++)
            {
                auto [loc2,decl2] = decls[j];
                if (fundecl_head(decl2) != fvar) break;

                loc = loc * loc2;
                auto& FD = decl2.as_<Hs::FunDecl>();

                assert(FD.matches.size() == 1);
                m.push_back( FD.matches[0] );

                if (m.back().patterns.size() != m.front().patterns.size())
                    error(loc2, Note()<<"Function '"<<*fvar<<"' has different numbers of arguments!");
            }

            if (m[0].patterns.empty() and m.size() != 1)
                error(loc, Note()<<"Multiple definitions for variable "<<fvar->print()<<"!");

            decls2.push_back( {loc,Hs::FunDecl( *fvar, m )} );

            // skip the other bindings for this function
            i += (m.size()-1);
        }
        else
            std::abort();
    }

    return {type_sigs, inline_sigs, decls2};
}

Hs::Decls group_fundecls(const Haskell::Decls& decls)
{
    Haskell::Decls decls2;

    for(int i=0;i<decls.size();i++)
    {
        auto [loc,decl] = decls[i];
        if (auto d = decl.to<Haskell::PatDecl>())
        {
            decls2.push_back({loc,*d});
        }
        else if (auto fvar = fundecl_head(decl))
        {
            Hs::Matches m;
            for(int j=i;j<decls.size();j++)
            {
                auto [loc2,decl2] = decls[j];
                if (fundecl_head(decl2) != fvar) break;

                auto& FD = decl2.as_<Hs::FunDecl>();
                loc = loc * loc2;

                assert(FD.matches.size() == 1);
                m.push_back( FD.matches[0] );

                if (m.back().patterns.size() != m.front().patterns.size())
                    throw myexception()<<"Function '"<<*fvar<<"' has different numbers of arguments!";
            }

            if (m[0].patterns.empty() and m.size() != 1)
                throw myexception()<<"Multiple definitions for variable "<<fvar->print()<<"!";

            decls2.push_back( {loc, Hs::FunDecl( *fvar, m )} );

            // skip the other bindings for this function
            i += (m.size()-1);
        }
        else
            std::abort();
    }

    return decls2;
}

bool needs_decl_grouping(const Hs::Decls& decls)
{
    for(const auto& [_, decl]: decls)
        if (decl.is_a<Hs::ValueDecl>() or decl.is_a<Hs::TypeSigDecl>() or
            decl.is_a<Hs::FixityDecl>() or decl.is_a<Hs::InlinePragma>())
            return true;

    return false;
}

// Classify all value declarations and collect signatures before local symbol collection.
Hs::Binds classify_value_decls(Hs::Binds binds)
{
    assert(binds.size() == 1);

    for(auto& [_, e]: binds[0])
        e = classify_value_decl(e);

    auto [type_sigs, inline_sigs, bind0] = group_decls(binds[0]);

    binds.signatures.insert(type_sigs.begin(), type_sigs.end());
    binds.inline_sigs.insert(inline_sigs.begin(), inline_sigs.end());
    binds[0] = bind0;

    return binds;
}

Haskell::Binds rename_infix(const Module& m, Haskell::Binds binds)
{
    assert(binds.size() == 1);

    bool needs_grouping = needs_decl_grouping(binds[0]);

    for(auto& [_, e]: binds[0])
        e = rename_infix_decl(m, e);

    if (needs_grouping)
    {
        auto [type_sigs, inline_sigs, bind0] = group_decls(binds[0]);

        binds.signatures.insert(type_sigs.begin(), type_sigs.end());
        binds.inline_sigs.insert(inline_sigs.begin(), inline_sigs.end());
        binds[0] = bind0;
    }

    return binds;
}

bound_var_info renamer_state::rename_decls(Haskell::Binds& binds, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars, bool top)
{
    set<string> decls_free_vars;
    auto new_binders = rename_decls(binds, plus(bound, binders), decls_free_vars, top);
    add(free_vars, minus(decls_free_vars, binders));
    return new_binders;
}

void renamer_state::rename_signatures(map<Hs::LVar, Hs::LType>& type_sigs, map<Hs::LVar, Hs::inline_pragma_t>& inline_sigs, const bound_var_info& binders, bool top)
{
    map<Hs::LVar, Hs::LType> type_sigs2;
    for(auto& [lvar, ltype]: type_sigs)
    {
        ltype = rename_and_quantify_type(ltype);

        if (is_qualified_symbol(unloc(lvar).name))
        {
            error(lvar.loc, Note()<<"Variable name may not be qualified!");
            continue;
        }

        auto lvar2 = lvar;
        auto& var2 = unloc(lvar2);
        if (top)
            qualify_name(var2.name);
        type_sigs2.insert( {lvar2, ltype} );

        if (not binders.count(var2.name))
        {
            error(lvar.loc, Note()<<"Signature but no definition for '"<<unloc(lvar).name<<"'");
        }
    }
    type_sigs = std::move(type_sigs2);

    map<Hs::LVar, Hs::inline_pragma_t> inline_sigs2;
    for(auto& [lvar, ip]: inline_sigs)
    {
        if (is_qualified_symbol(unloc(lvar).name))
        {
            error(lvar.loc, Note()<<"Variable name may not be qualified!");
            continue;
        }

        auto lvar2 = lvar;
        auto& var2 = unloc(lvar2);
        if (top)
            qualify_name(var2.name);
        inline_sigs2.insert( {lvar2, ip} );

        if (not binders.count(var2.name))
        {
            error(lvar.loc, Note()<<"Signature but no definition for '"<<unloc(lvar).name<<"'");
        }
    }
    inline_sigs = std::move(inline_sigs2);
}

vector<Hs::Decls> split_decls(const Hs::Decls& decls, const vector< vector<int> >& referenced_decls)
{
    // 1. Compute strongly-connected components
    auto components = get_ordered_strong_components( make_graph(referenced_decls) );

    // 2. Divide the decls into groups
    vector<Hs::Decls> bind_groups;
    for(auto& component: components)
    {
        Hs::Decls bdecls;
        for(int i : component)
        {
            auto& decl = decls[i];

            // Collect the value decl
            bdecls.push_back(decl);
        }

        // Check if the decls group is recursive
        if (bdecls.size() >1)
            bdecls.recursive = true;
        else
        {
            int i = component[0];
            bdecls.recursive = includes(referenced_decls[i], i);
        }

        bind_groups.push_back(bdecls);
    }
    return bind_groups;
}

void group_binds(Hs::Binds& binds, const vector< vector<int> >& referenced_decls)
{
    auto& decls = binds[0];
    assert(referenced_decls.size() == decls.size());

    vector<Hs::Decls> new_binds = split_decls(decls, referenced_decls);

    // Split the bindings, but keep the signatures
    (vector<Hs::Decls>&)binds = new_binds;
}


// So... factor out rename_grouped_decl( ), and then make a version that splits into components, and a version that does not?
// Splitting the decls for classes and instances into  components really doesn't make sense...

// maps names in a declaration group to a declaration in the group.
std::tuple<map<string,int>, map<Hs::Var,vector<Hs::LVar>>> get_indices_for_names(const Hs::Decls& decls)
{
    map<string,int> index_for_name;
    map<Hs::Var,std::vector<Hs::LVar>> duplicate_defs;

    for(int i=0;i<decls.size();i++)
    {
        auto& [loc,decl] = decls[i];

        // Get the binder vars introduced by this declaration
        set<Hs::LVar> vars;
        if (auto fd = decl.to<Hs::FunDecl>())
            vars.insert({fd->v});
        else if (auto pd = decl.to<Hs::PatDecl>())
            vars = Hs::vars_in_pattern( pd->lhs );
        else
            std::abort();

        // Record the index for each of those vars
        for(auto& lvar: vars)
        {
	    auto& [loc,var] = lvar;
            auto iter = duplicate_defs.find(var);

            if (iter == duplicate_defs.end())
            {
                // Record the index for each of those vars
                index_for_name.insert({var.name, i});
                // Create an empty list of duplicates.
                duplicate_defs.insert({var,{lvar}});
            }
            else
            {
                // Record a duplicate
                iter->second.push_back(lvar);
            }
        }
    }

    return {index_for_name, duplicate_defs};
}

bool is_strict_binding_pattern(const Hs::LPat& lpat)
{
    auto& pat = unloc(lpat);

    if (pat.is_a<Hs::StrictPattern>())
        return true;
    else if (auto tpat = pat.to<Hs::TypedPattern>())
        return is_strict_binding_pattern(tpat->pat);
    else
        return false;
}

vector<vector<int>> renamer_state::rename_grouped_decls(Haskell::Decls& decls, const bound_var_info& bound, set<string>& free_vars, bool top)
{
    // NOTE: bound already includes the binder names.

    for(int i=0;i<decls.size();i++)
    {
        auto& [loc,decl] = decls[i];

        if (decl.is_a<Hs::PatDecl>())
        {
            auto PD = decl.as_<Hs::PatDecl>();

            if (top and is_strict_binding_pattern(PD.lhs))
                error(PD.lhs.loc, Note()<<"Strict pattern bindings are not allowed at top level.");

            rename_pattern( PD.lhs, top);
            PD.rhs = rename(PD.rhs, bound, PD.rhs_free_vars);
            decl = PD;
        }
        else if (decl.is_a<Hs::FunDecl>())
        {
            auto FD = decl.as_<Hs::FunDecl>();
            auto& name = unloc(FD.v).name;
            assert(not is_qualified_symbol(name));
            if (top)
                name = m.name + "." + name;

            FD.matches = rename(FD.matches, bound, FD.rhs_free_vars);

            decl = FD;
        }
        else
            std::abort();
    }

    // Map the names to indices
    auto [index_for_name, duplicate_defs] = get_indices_for_names(decls);
    for(auto& [first_def, second_defs]: duplicate_defs)
    {
        for(int j=1;j<second_defs.size();j++)
        {
//	    auto& [loc1, first_def] = second_defs[0];
	    auto& [loc , extra_def] = second_defs[j];

            Note note;
            note<<"Name `"<<extra_def.name<<"` redefined.";
//            How do we attach a reference to the first def w/o adding another error?	   
//            if (first_def.name.loc)
//                note<<"\nFirst definition at "<<*first_def.name.loc;
            error( loc, note);
        }
    }

    // Construct referenced decls
    vector<vector<int>> referenced_decls;
    for(auto& [loc,decl]: decls)
    {
        vector<int> refs;
        auto& rhs_free_vars = get_rhs_free_vars(decl);
        for(auto& name: rhs_free_vars)
        {
            auto it = index_for_name.find(name);

            // Skip if this name isn't one of the ids being defined.
            if (it == index_for_name.end()) continue;

            refs.push_back(it->second);
        }
        referenced_decls.push_back( std::move(refs) );

        add(free_vars, rhs_free_vars);
    }

    // NOTE: binder names are removed in the called - rename_decls( ).

    return referenced_decls;
}

bound_var_info renamer_state::rename_decls(Haskell::Binds& binds, const bound_var_info& bound, set<string>& free_vars, bool top)
{
    assert(binds.size() == 1);
    auto& decls = binds[0];

    auto binders = find_bound_vars_in_decls(decls, top);

    rename_signatures(binds.signatures, binds.inline_sigs, binders, top);

    set<string> decls_free_vars;
    auto refs = rename_grouped_decls(decls, plus(bound, binders), decls_free_vars, top);
    group_binds(binds, refs);

    add(free_vars, minus(decls_free_vars,binders));

    return binders;
}
