#include <cassert>
#include <cstdlib>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <vector>

#include "rename.H"
#include "computation/expression/apply.H"

using std::map;
using std::optional;
using std::set;
using std::string;
using std::tuple;
using std::vector;

namespace
{
    Hs::MultiGuardedRHS parsed_expression_rhs(Hs::MultiGuardedRHS rhs);
    Hs::Matches parsed_expression_matches(Hs::Matches matches);
    Hs::Binds parsed_expression_binds(Hs::Binds binds);
    Hs::LExp parsed_expression_stmt(Hs::LExp stmt);

    Hs::LPat record_field_pun_pattern(const Hs::LVar& field)
    {
        auto name = get_unqualified_name(unloc(field).name);
        return {field.loc, Hs::VarPattern({field.loc, Hs::Var(name)})};
    }

    // Convert expression-form record field values into expression-category syntax.
    Located<Hs::FieldBindings> parsed_expression_field_bindings(Located<Hs::FieldBindings> fields)
    {
        for(auto& lfield: unloc(fields))
        {
            auto& field = unloc(lfield);
            if (field.value)
                field.value = parsed_expression_to_expression(*field.value);
        }

        return fields;
    }

    // Convert expression-form record fields into pattern-form record fields.
    Hs::PatternFieldBindings parsed_pattern_field_bindings(const Hs::FieldBindings& fields)
    {
        Hs::PatternFieldBindings pattern_fields;
        pattern_fields.dotdot = fields.dotdot;

        for(const auto& lfield: fields)
        {
            const auto& field = unloc(lfield);
            Hs::LPat pattern;
            if (field.value)
                pattern = parsed_expression_to_pattern(*field.value);
            else
                pattern = record_field_pun_pattern(field.field);
            pattern_fields.push_back({lfield.loc, Hs::PatternFieldBinding(field.field, pattern)});
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
    Hs::LExp parsed_expression_stmt(Hs::LExp stmt)
    {
        auto& S = unloc(stmt);

        if (auto sq = S.to<Hs::SimpleQual>())
        {
            auto Q = *sq;
            Q.exp = parsed_expression_to_expression(Q.exp);
            return {stmt.loc, Q};
        }
        else if (auto pq = S.to<Hs::PatQual>())
        {
            auto Q = *pq;
            Q.bindpat = parsed_expression_to_pattern(Q.bindpat);
            Q.exp = parsed_expression_to_expression(Q.exp);
            return {stmt.loc, Q};
        }
        else if (auto lq = S.to<Hs::LetQual>())
        {
            auto Q = *lq;
            unloc(Q.binds) = parsed_expression_binds(unloc(Q.binds));
            return {stmt.loc, Q};
        }
        else if (auto rec = S.to<Hs::RecStmt>())
        {
            auto R = *rec;
            for(auto& rec_stmt: R.stmts.stmts)
                rec_stmt = parsed_expression_stmt(rec_stmt);
            return {stmt.loc, R};
        }
        else
            std::abort();
    }

    // Convert parser-only expression syntax inside a guarded RHS.
    Hs::MultiGuardedRHS parsed_expression_rhs(Hs::MultiGuardedRHS rhs)
    {
        if (rhs.decls)
            unloc(*rhs.decls) = parsed_expression_binds(unloc(*rhs.decls));

        for(auto& guarded_rhs: rhs.guarded_rhss)
        {
            for(auto& guard: guarded_rhs.guards)
                guard = parsed_expression_stmt(guard);
            guarded_rhs.body = parsed_expression_to_expression(guarded_rhs.body);
        }

        return rhs;
    }

    // Convert parser-only expression syntax inside match patterns and bodies.
    Hs::Matches parsed_expression_matches(Hs::Matches matches)
    {
        for(auto& match: matches)
        {
            for(auto& pattern: match.patterns)
                pattern = parsed_expression_to_pattern(pattern);
            match.rhs = parsed_expression_rhs(match.rhs);
        }

        return matches;
    }

    // Convert parser-only expression syntax inside local declarations.
    Hs::Binds parsed_expression_binds(Hs::Binds binds)
    {
        binds = classify_value_decls(binds);

        for(auto& decls: binds)
        {
            for(auto& [_, decl]: decls)
            {
                if (auto f = decl.to<Hs::FunDecl>())
                {
                    auto F = *f;
                    F.matches = parsed_expression_matches(F.matches);
                    decl = F;
                }
                else if (auto p = decl.to<Hs::PatDecl>())
                {
                    auto P = *p;
                    P.lhs = parsed_expression_to_pattern(P.lhs);
                    P.rhs = parsed_expression_rhs(P.rhs);
                    decl = P;
                }
                else if (decl.is_a<Hs::TypeSigDecl>() or decl.is_a<Hs::FixityDecl>() or
                         decl.is_a<Hs::InlinePragma>())
                { }
                else
                    std::abort();
            }
        }

        return binds;
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
                        pats.push_back(parsed_expression_to_pattern(make_infix_exp(left_terms)));
                        pats.push_back(parsed_expression_to_pattern(make_infix_exp(right_terms)));
                        pats.insert(pats.end(), extra_args.begin(), extra_args.end());
                        return Hs::simple_fun_decl({I->terms[i].loc, *v}, pats, D.rhs);
                    }
                }

                return Hs::PatDecl(parsed_expression_to_pattern(lhs), D.rhs);
            }
            else if (E.is_a<Hs::ParsedApp>())
            {
                auto [head,args] = Hs::decompose_apps(lhs);
                Hs::LPats pats;
                for(auto& arg: args)
                    pats.push_back(parsed_expression_to_pattern(arg));
                pats.insert(pats.end(), extra_args.begin(), extra_args.end());

                if (auto v = unloc(head).to<Hs::Var>())
                    return Hs::simple_fun_decl({head.loc, *v}, pats, D.rhs);
                else if (unloc(head).is_a<Hs::Con>())
                    return Hs::PatDecl(parsed_expression_to_pattern(lhs), D.rhs);
                else
                    return self(self, head, pats);
            }
            else if (E.is_a<Hs::ApplyExp>())
            {
                auto [head,args] = Hs::decompose_apps(lhs);
                Hs::LPats pats;
                for(auto& arg: args)
                    pats.push_back(parsed_expression_to_pattern(arg));
                pats.insert(pats.end(), extra_args.begin(), extra_args.end());

                if (auto v = unloc(head).to<Hs::Var>())
                    return Hs::simple_fun_decl({head.loc, *v}, pats, D.rhs);
                else if (unloc(head).is_a<Hs::Con>())
                    return Hs::PatDecl(parsed_expression_to_pattern(lhs), D.rhs);
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
                return Hs::PatDecl(parsed_expression_to_pattern(lhs), D.rhs);
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
}

// Convert parser-only expression syntax into expression-category syntax without resolving fixity.
Hs::LExp parsed_expression_to_expression(Hs::LExp lhs)
{
    auto& E = unloc(lhs);

    if (auto I = E.to<Hs::InfixExp>())
    {
        auto terms = I->terms;
        for(int i=0; i<terms.size(); i++)
            if (is_infix_operand_term(terms, i))
                terms[i] = parsed_expression_to_expression(terms[i]);
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
            terms.push_back(parsed_expression_to_expression(term));
        return Hs::apply(terms);
    }
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto A = *app;
        A.head = parsed_expression_to_expression(A.head);
        A.arg = parsed_expression_to_expression(A.arg);
        return {lhs.loc, A};
    }
    else if (auto r = E.to<Hs::RecordSyntax>())
    {
        auto R = *r;
        R.head = parsed_expression_to_expression(R.head);
        R.fbinds = parsed_expression_field_bindings(R.fbinds);
        return {lhs.loc, R};
    }
    else if (auto r = E.to<Hs::RecordCon>())
    {
        auto R = *r;
        R.fbinds = parsed_expression_field_bindings(R.fbinds);
        return {lhs.loc, R};
    }
    else if (auto r = E.to<Hs::RecordUpdate>())
    {
        auto R = *r;
        R.object = parsed_expression_to_expression(R.object);
        R.fbinds = parsed_expression_field_bindings(R.fbinds);
        return {lhs.loc, R};
    }
    else if (auto l = E.to<Hs::List>())
    {
        auto L = *l;
        for(auto& element: L.elements)
            element = parsed_expression_to_expression(element);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;
        L.from = parsed_expression_to_expression(L.from);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;
        L.from = parsed_expression_to_expression(L.from);
        L.then = parsed_expression_to_expression(L.then);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;
        L.from = parsed_expression_to_expression(L.from);
        L.to = parsed_expression_to_expression(L.to);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;
        L.from = parsed_expression_to_expression(L.from);
        L.then = parsed_expression_to_expression(L.then);
        L.to = parsed_expression_to_expression(L.to);
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::ListComprehension>())
    {
        auto L = *l;
        L.body = parsed_expression_to_expression(L.body);
        for(auto& qual: L.quals)
            qual = parsed_expression_stmt(qual);
        return {lhs.loc, L};
    }
    else if (auto s = E.to<Hs::LeftSection>())
    {
        auto S = *s;
        S.l_arg = parsed_expression_to_expression(S.l_arg);
        return {lhs.loc, S};
    }
    else if (auto s = E.to<Hs::RightSection>())
    {
        auto S = *s;
        S.r_arg = parsed_expression_to_expression(S.r_arg);
        return {lhs.loc, S};
    }
    else if (auto t = E.to<Hs::Tuple>())
    {
        auto T = *t;
        for(auto& element: T.elements)
            element = parsed_expression_to_expression(element);
        return {lhs.loc, T};
    }
    else if (auto t = E.to<Hs::TypedExp>())
    {
        auto T = *t;
        T.exp = parsed_expression_to_expression(T.exp);
        return {lhs.loc, T};
    }
    else if (auto c = E.to<Hs::CaseExp>())
    {
        auto C = *c;
        C.object = parsed_expression_to_expression(C.object);
        C.alts = parsed_expression_matches(C.alts);
        return {lhs.loc, C};
    }
    else if (auto l = E.to<Hs::LambdaExp>())
    {
        auto L = *l;
        L.match = parsed_expression_matches({L.match})[0];
        return {lhs.loc, L};
    }
    else if (auto l = E.to<Hs::LetExp>())
    {
        auto L = *l;
        unloc(L.binds) = parsed_expression_binds(unloc(L.binds));
        L.body = parsed_expression_to_expression(L.body);
        return {lhs.loc, L};
    }
    else if (auto i = E.to<Hs::IfExp>())
    {
        auto I = *i;
        I.condition = parsed_expression_to_expression(I.condition);
        I.true_branch = parsed_expression_to_expression(I.true_branch);
        I.false_branch = parsed_expression_to_expression(I.false_branch);
        return {lhs.loc, I};
    }
    else if (auto d = E.to<Hs::Do>())
    {
        auto D = *d;
        for(auto& stmt: D.stmts.stmts)
            stmt = parsed_expression_stmt(stmt);
        return {lhs.loc, D};
    }
    else if (auto d = E.to<Hs::MDo>())
    {
        auto D = *d;
        for(auto& stmt: D.stmts.stmts)
            stmt = parsed_expression_stmt(stmt);
        return {lhs.loc, D};
    }
    else if (E.is_a<Hs::Var>() or E.is_a<Hs::Con>() or E.is_a<Hs::Literal>() or
             E.is_int() or E.is_log_double() or E.is_double() or E.is_char())
        return lhs;
    else
        std::abort();
}

// Convert parsed expression-category syntax into pattern-category syntax without resolving fixity.
Hs::LPat parsed_expression_to_pattern(Hs::LExp lhs)
{
    auto& E = unloc(lhs);

    if (auto I = E.to<Hs::InfixExp>())
    {
        if (I->terms.size() == 1)
            return parsed_expression_to_pattern(I->terms[0]);

        auto terms = I->terms;
        for(int i=0; i<terms.size(); i++)
            if (is_infix_operand_term(terms, i))
                terms[i] = parsed_expression_to_pattern(terms[i]);

        return {lhs.loc, Hs::InfixPat(terms)};
    }
    else if (E.is_a<Hs::ParsedApp>())
    {
        auto [head,args] = Hs::decompose_apps(lhs);
        if (auto con = unloc(head).to<Hs::Con>())
        {
            Hs::LPats pat_args;
            for(auto& arg: args)
                pat_args.push_back(parsed_expression_to_pattern(arg));
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
                pat_args.push_back(parsed_expression_to_pattern(arg));
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

        return {lhs.loc, Hs::RecordPattern({r->head.loc, *con}, {r->fbinds.loc, parsed_pattern_field_bindings(unloc(r->fbinds))})};
    }
    else if (auto r = E.to<Hs::RecordCon>())
        return {lhs.loc, Hs::RecordPattern(r->con, {r->fbinds.loc, parsed_pattern_field_bindings(unloc(r->fbinds))})};
    else if (E.is_a<Hs::RecordUpdate>())
    {
        error(lhs.loc, Note()<<"Record update syntax '"<<lhs<<"' is not valid in a pattern.");
        return {lhs.loc, Hs::WildcardPattern()};
    }
    else if (auto l = E.to<Hs::List>())
    {
        Hs::ListPattern P;
        for(auto& element: l->elements)
            P.elements.push_back(parsed_expression_to_pattern(element));
        return {lhs.loc, P};
    }
    else if (auto t = E.to<Hs::Tuple>())
    {
        Hs::TuplePattern P;
        for(auto& element: t->elements)
            P.elements.push_back(parsed_expression_to_pattern(element));
        return {lhs.loc, P};
    }
    else if (auto ap = E.to<Hs::AsPattern>())
        return {lhs.loc, Hs::AsPattern(ap->var, parsed_expression_to_pattern(ap->pattern))};
    else if (auto lp = E.to<Hs::LazyPattern>())
        return {lhs.loc, Hs::LazyPattern(parsed_expression_to_pattern(lp->pattern))};
    else if (auto sp = E.to<Hs::StrictPattern>())
        return {lhs.loc, Hs::StrictPattern(parsed_expression_to_pattern(sp->pattern))};
    else if (auto t = E.to<Hs::TypedExp>())
    {
        Hs::TypedPattern P;
        P.pat = parsed_expression_to_pattern(t->exp);
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
