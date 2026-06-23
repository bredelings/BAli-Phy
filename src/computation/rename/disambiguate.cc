#include <cassert>
#include <cstdlib>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <vector>

#include "rename.H"
#include "computation/record_utils.H"

using std::map;
using std::optional;
using std::set;
using std::string;
using std::tuple;
using std::vector;

Hs::Binds classify_value_decls(Hs::Binds);
Hs::Decls classify_value_decls(Hs::Decls);
Hs::LPat disambiguate_pattern(Hs::LExp E);
Hs::LExp disambiguate_expression(Hs::LExp E);

namespace
{
    Hs::MultiGuardedRHS disambiguate_rhs(Hs::MultiGuardedRHS rhs);
    Hs::Matches disambiguate_matches(Hs::Matches matches);
    Hs::Matches disambiguate_matches(Hs::ParsedMatches matches);
    Hs::Decls disambiguate_classified_decls(Hs::Decls decls);
    Hs::Decls disambiguate_decls(Hs::Decls decls);
    Hs::LStmt disambiguate_stmt(Hs::LStmt stmt);

    using record_utils::record_field_pun_pattern;

    // Convert expression-form record field values into expression-category syntax.
    Located<Hs::FieldBindings> disambiguate_field_bindings(Located<Hs::FieldBindings> fields)
    {
        for(auto& lfield: unloc(fields).fields)
        {
            auto& field = unloc(lfield);
            if (field.value)
                field.value = disambiguate_expression(*field.value);
        }

        return fields;
    }

    // Convert expression-form record fields into pattern-form record fields.
    Hs::PatternFieldBindings disambiguate_pattern_field_bindings(const Hs::FieldBindings& fields)
    {
        Hs::PatternFieldBindings pattern_fields;
        pattern_fields.dotdot = fields.dotdot;

        for(const auto& lfield: fields.fields)
        {
            const auto& field = unloc(lfield);
            Hs::LPat pattern;
            bool pun = not field.value;
            if (field.value)
                pattern = disambiguate_pattern(*field.value);
            else
                pattern = record_field_pun_pattern(field.field);
            pattern_fields.fields.push_back({lfield.loc, Hs::PatternFieldBinding(field.field, pattern, pun)});
        }

        return pattern_fields;
    }

    // Recognize declarations that still need parser-level classification or grouping.
    bool needs_decl_grouping(const Hs::Decls& decls)
    {
        for(const auto& [_, decl]: decls)
            if (decl.is_a<Hs::ValueDecl>() or decl.is_a<Hs::TypeSigDecl>() or
                decl.is_a<Hs::FixityDecl>() or decl.is_a<Hs::InlinePragma>())
                return true;

        return false;
    }

    // Convert parser-only expression syntax inside a statement.
    Hs::LStmt disambiguate_stmt(Hs::LStmt stmt)
    {
        auto& S = unloc(stmt);

        if (auto sq = S.to<Hs::SimpleQual>())
        {
            auto Q = *sq;
            Q.exp = disambiguate_expression(Q.exp);
            return {stmt.loc, Q};
        }
        else if (auto pq = S.to<Hs::PatQual>())
        {
            auto Q = *pq;
            Q.exp = disambiguate_expression(Q.exp);
            return {stmt.loc, Q};
        }
        else if (auto pq = S.to<Hs::ParsedPatQual>())
        {
            auto Q = *pq;
            return {stmt.loc, Hs::PatQual(disambiguate_pattern(Q.bindpat), disambiguate_expression(Q.exp))};
        }
        else if (auto lq = S.to<Hs::LetQual>())
        {
            auto Q = *lq;
            unloc(Q.binds) = disambiguate_binds(unloc(Q.binds));
            return {stmt.loc, Q};
        }
        else if (auto rec = S.to<Hs::RecStmt>())
        {
            auto R = *rec;
            for(auto& rec_stmt: R.stmts.stmts)
                rec_stmt = disambiguate_stmt(rec_stmt);
            return {stmt.loc, R};
        }
        else
            std::abort();
    }

    // Convert parser-only expression syntax inside a guarded RHS.
    Hs::MultiGuardedRHS disambiguate_rhs(Hs::MultiGuardedRHS rhs)
    {
        if (rhs.decls)
            unloc(*rhs.decls) = disambiguate_binds(unloc(*rhs.decls));

        for(auto& guarded_rhs: rhs.guarded_rhss)
        {
            for(auto& guard: guarded_rhs.guards)
                guard = disambiguate_stmt(guard);
            guarded_rhs.body = disambiguate_expression(guarded_rhs.body);
        }

        return rhs;
    }

    // Convert parser-only expression syntax inside match patterns and bodies.
    Hs::Matches disambiguate_matches(Hs::Matches matches)
    {
        for(auto& match: matches)
            match.rhs = disambiguate_rhs(match.rhs);

        return matches;
    }

    // Convert parser-phase match patterns into final patterns and recurse into RHSs.
    // The parsed alias is removed here so later passes only see Hs::Matches.
    Hs::Matches disambiguate_matches(Hs::ParsedMatches parsed_matches)
    {
        Hs::Matches matches;
        for(auto& parsed_match: parsed_matches)
        {
            Hs::LPats patterns;
            for(auto& pattern: parsed_match.patterns)
                patterns.push_back(disambiguate_pattern(pattern));
            matches.push_back(Hs::MRule(patterns, disambiguate_rhs(parsed_match.rhs)));
        }

        return matches;
    }

    // Convert parser-only expression syntax inside already-classified declarations.
    Hs::Decls disambiguate_classified_decls(Hs::Decls decls)
    {
        for(auto& [_, decl]: decls)
        {
            if (auto f = decl.to<Hs::FunDecl>())
            {
                auto F = *f;
                F.matches = disambiguate_matches(F.matches);
                decl = F;
            }
            else if (auto p = decl.to<Hs::PatDecl>())
            {
                auto P = *p;
                P.rhs = disambiguate_rhs(P.rhs);
                decl = P;
            }
            else if (decl.is_a<Hs::TypeSigDecl>() or decl.is_a<Hs::FixityDecl>() or
                     decl.is_a<Hs::InlinePragma>())
            { }
            else
                std::abort();
        }

        return decls;
    }

    // Classify declarations and convert parser-only expression syntax inside them.
    Hs::Decls disambiguate_decls(Hs::Decls decls)
    {
        return disambiguate_classified_decls(classify_value_decls(decls));
    }

    // Classify parsed syntax inside type declarations that carry value bindings.
    Hs::Decls disambiguate_type_decls(Hs::Decls decls)
    {
        for(auto& [_, decl]: decls)
        {
            if (auto c = decl.to<Hs::ClassDecl>())
            {
                auto C = *c;
                C.default_method_decls = disambiguate_decls(C.default_method_decls);
                decl = C;
            }
            else if (auto i = decl.to<Hs::InstanceDecl>())
            {
                auto I = *i;
                I.method_decls = disambiguate_decls(I.method_decls);
                decl = I;
            }
        }

        return decls;
    }

    // Decide whether a parsed value binding is a function declaration or pattern declaration.
    Hs::Decl classify_value_decl(const Hs::ValueDecl& D)
    {
        // Classify the declaration LHS while carrying arguments peeled from nested applications.
        auto classify_lhs = [&](auto&& self, Hs::LExp lhs, Hs::LPats extra_args) -> Hs::Decl
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
                        pats.push_back(disambiguate_pattern(make_infix_exp(left_terms)));
                        pats.push_back(disambiguate_pattern(make_infix_exp(right_terms)));
                        pats.insert(pats.end(), extra_args.begin(), extra_args.end());
                        return Hs::simple_fun_decl({I->terms[i].loc, *v}, pats, D.rhs);
                    }
                }

                return Hs::PatDecl(disambiguate_pattern(lhs), D.rhs);
            }
            else if (E.is_a<Hs::ParsedApp>())
            {
                auto [head,args] = Hs::decompose_parsed_app(lhs);
                Hs::LPats pats;
                for(auto& arg: args)
                    pats.push_back(disambiguate_pattern(arg));
                pats.insert(pats.end(), extra_args.begin(), extra_args.end());

                if (auto v = unloc(head).to<Hs::Var>())
                    return Hs::simple_fun_decl({head.loc, *v}, pats, D.rhs);
                else if (unloc(head).is_a<Hs::Con>())
                    return Hs::PatDecl(disambiguate_pattern(lhs), D.rhs);
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
                return Hs::PatDecl(disambiguate_pattern(lhs), D.rhs);
        };

        return classify_lhs(classify_lhs, D.lhs, {});
    }

    // Classify a raw value declaration, leaving already-classified declarations unchanged.
    Hs::Decl classify_value_decl(const Hs::Decl& E)
    {
        if (auto D = E.to<Hs::ValueDecl>())
            return classify_value_decl(*D);
        else
            return E;
    }
}

// Recursively disambiguate parser-only syntax inside a binding collection.
Hs::Binds disambiguate_binds(Hs::Binds binds)
{
    binds = classify_value_decls(binds);

    for(auto& decls: binds)
        decls = disambiguate_classified_decls(decls);

    return binds;
}

// Recursively disambiguate parser-only syntax inside a module before renaming.
Hs::ModuleDecls disambiguate_module(Hs::ModuleDecls M)
{
    M.type_decls = disambiguate_type_decls(M.type_decls);
    M.value_decls = disambiguate_binds(M.value_decls);
    return M;
}

// Disambiguate expression-category syntax without resolving fixity.
Hs::LExp disambiguate_expression(Hs::LExp lhs)
{
    auto& E = unloc(lhs);

    if (auto I = E.to<Hs::InfixExp>())
    {
        auto terms = I->terms;
        for(int i=0; i<terms.size(); i++)
            if (is_infix_operand_term(terms, i))
                terms[i] = disambiguate_expression(terms[i]);
        return {lhs.loc, Hs::InfixExp(terms)};
    }
    else if (auto app = E.to<Hs::ParsedApp>())
    {
        if (app->terms.empty())
        {
            error(lhs.loc, Note()<<"Empty parsed application.");
            return lhs;
        }

        vector<Hs::LExp> terms;
        for(auto& term: app->terms)
            terms.push_back(disambiguate_expression(term));
        return Hs::apply(terms);
    }
    else if (auto r = E.to<Hs::RecordSyntax>())
    {
        auto R = *r;
        R.head = disambiguate_expression(R.head);
        R.fbinds = disambiguate_field_bindings(R.fbinds);
        if (auto con = unloc(R.head).to<Hs::Con>())
            return {lhs.loc, Hs::RecordCon({R.head.loc, *con}, R.fbinds)};
        else
            return {lhs.loc, Hs::RecordUpdate(R.head, R.fbinds)};
    }
    else if (auto l = E.to<Hs::List>())
    {
        auto L = *l;
        for(auto& element: L.elements)
            element = disambiguate_expression(element);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;
        L.from = disambiguate_expression(L.from);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;
        L.from = disambiguate_expression(L.from);
        L.then = disambiguate_expression(L.then);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;
        L.from = disambiguate_expression(L.from);
        L.to = disambiguate_expression(L.to);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;
        L.from = disambiguate_expression(L.from);
        L.then = disambiguate_expression(L.then);
        L.to = disambiguate_expression(L.to);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListComprehension>())
    {
        auto L = *l;
        L.body = disambiguate_expression(L.body);
        for(auto& qual: L.quals)
            qual = disambiguate_stmt(qual);
        return {lhs.loc, L};
    }
    else if (auto s = E.to<Hs::LeftSection>())
    {
        auto S = *s;
        S.l_arg = disambiguate_expression(S.l_arg);
        return {lhs.loc, S};
    }
    else if (auto s = E.to<Hs::RightSection>())
    {
        auto S = *s;
        S.r_arg = disambiguate_expression(S.r_arg);
        return {lhs.loc, S};
    }
    else if (auto t = E.to<Hs::Tuple>())
    {
        auto T = *t;
        for(auto& element: T.elements)
            element = disambiguate_expression(element);
        return {lhs.loc, T};
    }
    else if (auto t = E.to<Hs::TypedExp>())
    {
        auto T = *t;
        T.exp = disambiguate_expression(T.exp);
        return {lhs.loc, T};
    }
    else if (auto c = E.to<Hs::Case>())
    {
        auto C = *c;
        C.object = disambiguate_expression(C.object);
        C.alts = disambiguate_matches(C.alts);
        return {lhs.loc, C};
    }
    else if (auto c = E.to<Hs::ParsedCase>())
    {
        auto C = *c;
        C.object = disambiguate_expression(C.object);
        return {lhs.loc, Hs::Case(C.object, disambiguate_matches(C.alts))};
    }
    else if (auto l = E.to<Hs::Lambda>())
    {
        auto L = *l;
        L.match = disambiguate_matches({L.match})[0];
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ParsedLambda>())
    {
        auto L = *l;
        return {lhs.loc, Hs::Lambda(disambiguate_matches(Hs::ParsedMatches{L.match})[0])};
    }
    else if (auto l = E.to<Hs::Let>())
    {
        auto L = *l;
        unloc(L.binds) = disambiguate_binds(unloc(L.binds));
        L.body = disambiguate_expression(L.body);
        return {lhs.loc, L};
    }
    else if (auto i = E.to<Hs::If>())
    {
        auto I = *i;
        I.condition = disambiguate_expression(I.condition);
        I.true_branch = disambiguate_expression(I.true_branch);
        I.false_branch = disambiguate_expression(I.false_branch);
        return {lhs.loc, I};
    }
    else if (auto d = E.to<Hs::Do>())
    {
        auto D = *d;
        for(auto& stmt: D.stmts.stmts)
            stmt = disambiguate_stmt(stmt);
        return {lhs.loc, D};
    }
    else if (auto d = E.to<Hs::MDo>())
    {
        auto D = *d;
        for(auto& stmt: D.stmts.stmts)
            stmt = disambiguate_stmt(stmt);
        return {lhs.loc, D};
    }
    else if (E.is_a<Hs::ParsedAsPattern>() or E.is_a<Hs::ParsedLazyPattern>() or
             E.is_a<Hs::ParsedStrictPattern>() or E.is_a<Hs::ParsedWildcardPattern>())
    {
        error(lhs.loc, Note()<<"Pattern syntax '"<<lhs<<"' is not valid as an expression.");
        return {lhs.loc, Hs::Var("<pattern-syntax-error>")};
    }
    else if (E.is_a<Hs::Var>() or E.is_a<Hs::Con>() or E.is_a<Hs::Literal>())
        return lhs;
    else
        std::abort();
}

// Disambiguate pattern-category syntax without resolving fixity.
Hs::LPat disambiguate_pattern(Hs::LExp lhs)
{
    auto& E = unloc(lhs);

    if (auto I = E.to<Hs::InfixExp>())
    {
        if (I->terms.size() == 1)
            return disambiguate_pattern(I->terms[0]);

        return {lhs.loc, Hs::InfixPat(I->terms)};
    }
    else if (E.is_a<Hs::ParsedApp>())
    {
        auto [head,args] = Hs::decompose_parsed_app(lhs);
        if (auto con = unloc(head).to<Hs::Con>())
        {
            Hs::LPats pat_args;
            for(auto& arg: args)
                pat_args.push_back(disambiguate_pattern(arg));
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

        return {lhs.loc, Hs::RecordPattern({r->head.loc, *con}, {r->fbinds.loc, disambiguate_pattern_field_bindings(unloc(r->fbinds))})};
    }
    else if (auto l = E.to<Hs::List>())
    {
        Hs::ListPattern P;
        for(auto& element: l->elements)
            P.elements.push_back(disambiguate_pattern(element));
        return {lhs.loc, P};
    }
    else if (auto t = E.to<Hs::Tuple>())
    {
        Hs::TuplePattern P;
        for(auto& element: t->elements)
            P.elements.push_back(disambiguate_pattern(element));
        return {lhs.loc, P};
    }
    else if (auto ap = E.to<Hs::ParsedAsPattern>())
        return {lhs.loc, Hs::AsPattern(ap->var, disambiguate_pattern(ap->pattern))};
    else if (auto lp = E.to<Hs::ParsedLazyPattern>())
        return {lhs.loc, Hs::LazyPattern(disambiguate_pattern(lp->pattern))};
    else if (auto sp = E.to<Hs::ParsedStrictPattern>())
        return {lhs.loc, Hs::StrictPattern(disambiguate_pattern(sp->pattern))};
    else if (E.is_a<Hs::ParsedWildcardPattern>())
        return {lhs.loc, Hs::WildcardPattern()};
    else if (auto t = E.to<Hs::TypedExp>())
    {
        Hs::TypedPattern P;
        P.pat = disambiguate_pattern(t->exp);
        P.type = t->type;
        return {lhs.loc, P};
    }
    else if (auto l = E.to<Hs::Literal>())
        return {lhs.loc, Hs::LiteralPattern(*l)};
    else if (auto c = E.to<Hs::Con>())
        return {lhs.loc, Hs::ConPattern({lhs.loc, *c}, {})};
    else if (auto v = E.to<Hs::Var>())
        return {lhs.loc, Hs::VarPattern({lhs.loc, *v})};
    else
    {
        error(lhs.loc, Note()<<"Expression '"<<lhs<<"' is not valid in a pattern.");
        return {lhs.loc, Hs::WildcardPattern()};
    }
}

// Classify all value declarations and collect signatures before local symbol collection.
Hs::Binds classify_value_decls(Hs::Binds binds)
{
    assert(binds.size() == 1);

    if (not needs_decl_grouping(binds[0]))
        return binds;

    for(auto& [_, e]: binds[0])
        e = classify_value_decl(e);

    auto [type_sigs, inline_sigs, bind0] = group_decls(binds[0]);

    binds.signatures.insert(type_sigs.begin(), type_sigs.end());
    binds.inline_sigs.insert(inline_sigs.begin(), inline_sigs.end());
    binds[0] = bind0;

    return binds;
}

// Classify value declarations in a declaration list that has no Binds-level signature maps.
Hs::Decls classify_value_decls(Hs::Decls decls)
{
    if (not needs_decl_grouping(decls))
        return decls;

    for(auto& [_, e]: decls)
        e = classify_value_decl(e);

    return group_fundecls(decls);
}
