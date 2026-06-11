#include <string>
#include <vector>
#include <set>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "util/set.H"

// We really should move apply expressions into the haskell ast.
#include "computation/expression/apply.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;

namespace
{
    Hs::LExp record_field_pun_exp(const Hs::LVar& field)
    {
        auto name = get_unqualified_name(unloc(field).name);
        return {field.loc, Hs::Var(name)};
    }

    std::optional<std::set<std::string>> intersect_record_constructors(
        const std::optional<std::set<std::string>>& constructors1,
        const std::set<std::string>& constructors2)
    {
        if (constructors1)
            return intersection(*constructors1, constructors2);
        else
            return constructors2;
    }

    Hs::LExp record_update_binder(const std::optional<yy::location>& loc, const std::string& con_name, int index)
    {
        auto name = "v$record_update$" + get_unqualified_name(con_name) + "$" + std::to_string(index);
        return {loc, Hs::Var(name)};
    }
}

Hs::Exp renamer_state::rename(const Hs::Exp& E, const bound_var_info& bound, set<string>& free_vars)
{
    return unloc(rename({noloc, E}, bound, free_vars));
}


Hs::LExp renamer_state::rename(Hs::LExp LE, const bound_var_info& bound, set<string>& free_vars)
{
    LE = parsed_expression_to_expression(LE);

    auto& E = unloc(LE);
    auto& loc = LE.loc;

    if (E.is_a<Hs::List>())
    {
        auto L = E.as_<Hs::List>();
        for(auto& element: L.elements)
            element = rename(element, bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListFrom>())
    {
        auto L = E.as_<Hs::ListFrom>();
        L.from = rename(L.from, bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListFromThen>())
    {
        auto L = E.as_<Hs::ListFromThen>();
        L.from = rename(L.from, bound, free_vars);
        L.then = rename(L.then, bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListFromTo>())
    {
        auto L = E.as_<Hs::ListFromTo>();
        L.from = rename(L.from, bound, free_vars);
        L.to   = rename(L.to  , bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListFromThenTo>())
    {
        auto L = E.as_<Hs::ListFromThenTo>();
        L.from = rename(L.from, bound, free_vars);
        L.then = rename(L.then, bound, free_vars);
        L.to   = rename(L.to  , bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListComprehension>())
    {
        auto L = E.as_<Hs::ListComprehension>();
        auto rn = child();

        bound_var_info binders;
        for(auto& qual: L.quals)
            add(binders, rn.rename_stmt(qual, bound, binders, free_vars));

        L.body = rn.rename(L.body, bound, binders, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::LeftSection>())
    {
        auto S = E.as_<Hs::LeftSection>();
        S.l_arg = rename(S.l_arg, bound, free_vars);
        S.op = rename(S.op, bound, free_vars);
        E = S;
    }
    else if (E.is_a<Hs::RightSection>())
    {
        auto S = E.as_<Hs::RightSection>();
        S.op = rename(S.op, bound, free_vars);
        S.r_arg = rename(S.r_arg, bound, free_vars);
        E = S;
    }
    else if (E.is_a<Hs::Tuple>())
    {
        auto T = E.as_<Hs::Tuple>();
        for(auto& element: T.elements)
            element = rename(element, bound, free_vars);
        E = T;
    }
    else if (E.is_a<Hs::Var>())
    {
        auto V = E.as_<Hs::Var>();
        auto& name = V.name;

        // Local vars bind id's tighter than global vars.
        if (includes(bound,name))
        {
            free_vars.insert(name);
        }
        // If the variable is free, then try top-level names.
        else if (m.is_declared(name))
        {
            try
            {
                auto S = m.lookup_symbol(name);
                string qualified_name = S->name;
                name = qualified_name;
                if (get_module_name(qualified_name) == m.name)
                    free_vars.insert(qualified_name);
                E = V;
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }
        else
        {
            error(loc, Note()<<"Variable `"<<name<<"` not in scope.");
        }
    }
    else if (auto con = E.to<Hs::Con>())
    {
        auto C = *con;
        auto& name = C.name;

        // FIXME: we should look the constructor up in a constructor environment
        // Does that mean that we look up constructors in a different table?
        if (m.is_declared(name))
        {
            try
            {
                auto S = m.lookup_symbol(name);
                name = S->name; // use the qualified name
                // We return a reference to a lambda function, in case the constructor isn't fully applied.
                C.arity = S->arity;
                E = C;
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }
        else
        {
            error(loc, Note()<<"Data constructor `"<<name<<"` not in scope.");
        }
    }
    else if (E.is_a<Hs::RecStmt>())
    {
        auto rn = child();
        bound_var_info binders;
        auto R = E.as_<Hs::RecStmt>();
        for(auto& stmt: R.stmts.stmts)
            add(binders, rn.rename_stmt(stmt, bound, binders, free_vars));

        E = R;
    }
    else if (E.is_a<Hs::Do>())
    {
        auto rn = child();
        bound_var_info binders;
        auto D = E.as_<Hs::Do>();
        for(auto& stmt: D.stmts.stmts)
            add(binders, rn.rename_stmt(stmt, bound, binders, free_vars));

        if (D.stmts.stmts.empty())
            error(loc, Note()<<"Empty do block.");
        else
        {
            auto& last = D.stmts.stmts.back();
            if (not unloc(last).is_a<Hs::SimpleQual>())
                error(last.loc, Note()<<"Do block does not end in an expression.");
        }
        E = D;
    }
    else if (E.is_a<Hs::MDo>())
    {
            /*
         * See "The mdo notation" in https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html
         *
         * "Like let and where bindings, name shadowing is not allowed within an mdo-expression or a rec-block"
         *
         * mdo { a <- getChar      ===> do { a <- getChar
         *     ; b <- f a c                ; rec { b <- f a c
         *     ; c <- f b a                ;     ; c <- f b a }
         *     ; z <- h a b                ; z <- h a b
         *     ; d <- g d e                ; rec { d <- g d e
         *     ; e <- g a z                ;     ; e <- g a z }
         *     ; putChar c }               ; putChar c }
         */

        /* So... you start the the last statement (e <- g a z) and then find the first statement that has an "e" in the
           free vars of the rhs.  Thats a rec block, regardless of what anything else does, since you can reorder the stmts.
           Then, presumably, you start with the last ungrouped statement, and continue the algorithm.

           The statement itself could have an "e" in the rhs: e <- g a e.

           If NO statement has a e in the rhs, then we move to the second-to-last stmt and continue the algorithm.

           If a statement has e in the lhs, then no other stmts can have e in the lhs, because of the "name shadowing
           not allowed" rule.
         */

        /*
         * See ghc/compiler/rename/RnExpr.hs: Note [Segmenting mdo]
         * What does rec {a;c  mean here?
         *                b;d}
         * I think it means rec {a;c;b;d}, but emphasizes that we have
         * to issue all the stmts from the first recursive group a;c, before
         * we issue any of the stmts from the second group b;d}.
         */

        // Hmm... as a dumb segmentation, we could take all the stmts except the last one and put them in a giant rec...
        // FIXME: implement segmentation, and insert recs.

        auto rn = child();
        bound_var_info binders;
        auto MD = E.as_<Hs::MDo>();
        for(auto& stmt: MD.stmts.stmts)
            add(binders, rn.rename_stmt(stmt, bound, binders, free_vars));
        E = MD;
    }
    else if (auto te = E.to<Hs::TypedExp>())
    {
        auto TE = *te;
        TE.exp = rename(TE.exp, bound, free_vars);
        TE.type = rename_and_quantify_type(TE.type);
        E = TE;
    }
    else if (auto rec = E.to<Hs::RecordSyntax>())
    {
        auto Rec = *rec;
        if (auto con = unloc(Rec.head).to<Hs::Con>())
            E = Hs::RecordCon({Rec.head.loc, *con}, Rec.fbinds);
        else
            E = Hs::RecordUpdate(Rec.head, Rec.fbinds);
        return rename(LE, bound, free_vars);
    }
    else if (auto rec = E.to<Hs::RecordUpdate>())
    {
        auto Rec = *rec;
        auto object = rename(Rec.object, bound, free_vars);
        std::vector<std::pair<std::string,Hs::LExp>> updates;
        optional<set<string>> constructors;
        set<string> used_field_names;

        if (unloc(Rec.fbinds).dotdot)
            error(Rec.fbinds.loc, Note()<<"Record wildcards in updates are not implemented yet.");

        for(auto& field: unloc(Rec.fbinds))
        {
            auto field_name = unloc(unloc(field).field).name;
            auto field_key = get_unqualified_name(field_name);
            if (used_field_names.count(field_key))
                error(field.loc, Note()<<"Field '"<<field_name<<"' appears more than once in record update.");
            used_field_names.insert(field_key);

            auto field_constructors = record_field_constructors().find(field_name);
            if (field_constructors == record_field_constructors().end())
                field_constructors = record_field_constructors().find(field_key);

            if (field_constructors == record_field_constructors().end())
                error(field.loc, Note()<<"Record field '"<<field_name<<"' not in scope for update.");
            else
                constructors = intersect_record_constructors(constructors, field_constructors->second);

            auto value = unloc(field).value ? *unloc(field).value : record_field_pun_exp(unloc(field).field);
            updates.push_back({field_name, rename(value, bound, free_vars)});
        }

        if (not constructors or constructors->empty())
            error(loc, Note()<<"No record constructor has all fields used in this update.");
        else
        {
            vector<Located<Hs::Alt>> alts;
            for(const auto& con_name: *constructors)
            {
                const auto& layout = record_constructor_layouts().at(con_name);
                Hs::LPats patterns;
                vector<Hs::LExp> args;

                for(int i=0; i<layout.arity; i++)
                {
                    auto var = record_update_binder(loc, con_name, i);
                    Hs::LVar lvar = {loc, unloc(var).as_<Hs::Var>()};
                    patterns.push_back({loc, Hs::VarPattern(lvar)});
                    args.push_back(var);
                }

                for(const auto& [field_name, value]: updates)
                {
                    auto pos = layout.fields.find(field_name);
                    if (pos == layout.fields.end())
                        pos = layout.fields.find(get_unqualified_name(field_name));
                    assert(pos != layout.fields.end());
                    args[pos->second] = value;
                }

                Hs::LCon head = {loc, Hs::Con(con_name, layout.arity)};
                Hs::LPat pattern = {loc, Hs::ConPattern(head, patterns)};
                auto rhs = Hs::SimpleRHS(Hs::apply({loc, unloc(head)}, args));
                alts.push_back({loc, Hs::Alt(pattern, rhs)});
            }
            E = Hs::CaseExp(object, Hs::Alts(alts));
        }
    }
    else if (auto rec = E.to<Hs::RecordCon>())
    {
        auto Rec = *rec;
        auto con = unloc(Rec.con);

        if (not m.is_declared(con.name))
        {
            error(Rec.con.loc, Note()<<"Data constructor `"<<con.name<<"` not in scope.");
        }
        else
        {
            try
            {
                auto S = m.lookup_symbol(con.name);
                if (S->symbol_type != symbol_type_t::constructor)
                    error(Rec.con.loc, Note()<<"Id '"<<con.name<<"' is not a constructor in record construction.");

                auto Con = con;
                Con.name = S->name;
                Con.arity = S->arity;

                auto layout = record_layout_for_constructor(S->name);

                if (not layout)
                    error(Rec.con.loc, Note()<<"Constructor '"<<con.name<<"' is not a record constructor.");
                else
                {
                    vector<optional<Hs::LExp>> fields(layout->arity);
                    set<int> used_fields;
                    bool has_dotdot = unloc(Rec.fbinds).dotdot;

                    if (has_dotdot)
                        error(Rec.fbinds.loc, Note()<<"Record wildcards in construction are not implemented yet.");

                    for(auto& field: unloc(Rec.fbinds))
                    {
                        auto field_name = unloc(unloc(field).field).name;
                        auto pos = layout->fields.find(field_name);
                        if (pos == layout->fields.end())
                            pos = layout->fields.find(get_unqualified_name(field_name));

                        if (pos == layout->fields.end())
                            error(field.loc, Note()<<"Constructor '"<<con.name<<"' does not have field '"<<field_name<<"'.");
                        else if (used_fields.count(pos->second))
                            error(field.loc, Note()<<"Field '"<<field_name<<"' appears more than once in record construction.");
                        else
                        {
                            used_fields.insert(pos->second);
                            auto value = unloc(field).value ? *unloc(field).value : record_field_pun_exp(unloc(field).field);
                            fields[pos->second] = rename(value, bound, free_vars);
                        }
                    }

                    Hs::LExp head = {Rec.con.loc, Con};
                    vector<Hs::LExp> args;
                    for(int i=0; i<fields.size(); i++)
                    {
                        if (fields[i])
                            args.push_back(*fields[i]);
                        else if (not has_dotdot)
                            error(loc, Note()<<"Missing field "<<i+1<<" in record construction for constructor '"<<con.name<<"'.");
                    }

                    E = unloc(Hs::apply(head, args));
                }
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }
    }
    else if (auto c = E.to<Hs::CaseExp>())
    {
        auto C = *c;

        C.object = rename(C.object, bound, free_vars);
        C.alts   = rename(C.alts,    bound, free_vars);

        E = C;
    }
    else if (E.is_a<Hs::LambdaExp>())
    {
        auto L = E.as_<Hs::LambdaExp>();
        L.match = rename(L.match, bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::LetExp>())
    {
        auto L = E.as_<Hs::LetExp>();

        auto rn = child();
        rn.fixity_env = rn.add_fixities_from_decls(rn.fixity_env, unloc(L.binds)[0]);
        auto binders = rn.rename_decls(unloc(L.binds), bound, free_vars);
        L.body = rn.rename(L.body, bound, binders, free_vars);

        E = L;
    }
    else if (E.is_a<Hs::IfExp>())
    {
        auto I = E.as_<Hs::IfExp>();

        I.condition    = rename(I.condition, bound, free_vars);
        I.true_branch  = rename(I.true_branch, bound, free_vars);
        I.false_branch = rename(I.false_branch, bound, free_vars);

        E = I;
    }
    else if (E.is_int() or E.is_log_double() or E.is_double() or E.is_char())
    { }
    else if (E.is_a<Hs::Literal>())
    { }
    else if (E.is_a<Hs::ParsedApp>())
        error(loc, Note()<<"Internal error: parsed application reached expression renaming.");
    else if (auto I = E.to<Hs::InfixExp>())
    {
        try
        {
            LE = desugar_infix(*this, I->terms);
            return rename(LE, bound, free_vars);
        }
        catch (myexception& e)
        {
            error(loc, Note()<<e.what());
            E = Hs::Var("<infix-error>");
        }
    }
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto App = *app;
        App.head = rename(App.head, bound, free_vars);
        App.arg  = rename(App.arg,  bound, free_vars);
        E = App;
    }
    else if (E.is_a<Hs::WildcardPattern>())
    {
        error(loc, Note()<<"Wildcard pattern in expression!");
    }
    else
    {
        error(loc, Note()<<"Unknown syntax in expression!");
    }

    return LE;
}
