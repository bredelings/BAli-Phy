#include <string>
#include <vector>
#include <set>

#include "rename.H"
#include "records.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;

namespace
{
    struct mdo_stmt_info
    {
        Hs::LStmt stmt;
        bound_var_info bound;
        bound_var_info free;
    };

    struct mdo_segment
    {
        Hs::LStmt stmt;
        bool generated_rec = false;
    };

    int dependency_end(const vector<mdo_stmt_info>& stmts, const bound_var_info& deps, int start)
    {
        int end = -1;
        for(int i = start; i < stmts.size(); i++)
        {
            if (not intersection(deps, stmts[i].bound).empty())
                end = i;
        }
        return end;
    }

    vector<mdo_segment> segment_mdo_stmts(const vector<mdo_stmt_info>& stmts)
    {
        vector<mdo_segment> segmented;

        for(int i = 0; i < stmts.size();)
        {
            bound_var_info block_free = stmts[i].free;
            int end = dependency_end(stmts, block_free, i);

            if (end < i)
            {
                segmented.push_back({stmts[i].stmt});
                i++;
                continue;
            }

            for(int j = i; j <= end; j++)
            {
                add(block_free, stmts[j].free);
                int new_end = dependency_end(stmts, block_free, i);
                if (new_end > end)
                    end = new_end;
            }

            vector<Hs::LStmt> rec_stmts;
            for(int j = i; j <= end; j++)
                rec_stmts.push_back(stmts[j].stmt);
            segmented.push_back({{stmts[i].stmt.loc, Hs::RecStmt(Hs::Stmts(rec_stmts))}, true});
            i = end + 1;
        }

        return segmented;
    }
}

Hs::Exp renamer_state::rename(const Hs::Exp& E, const bound_var_info& bound, set<string>& free_vars)
{
    return unloc(rename({noloc, E}, bound, free_vars));
}


Hs::LExp renamer_state::rename(Hs::LExp LE, const bound_var_info& bound, set<string>& free_vars)
{
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
        if (not m.language_extensions.has_extension(LangExt::RecursiveDo))
            error(loc, Note()<<"mdo expression requires the RecursiveDo extension.");

        auto rn = child();
        auto MD = E.as_<Hs::MDo>();
        bound_var_info mdo_binders;

        for(auto& stmt: MD.stmts.stmts)
        {
            bool overlap = not disjoint_add(mdo_binders, rn.find_bound_vars_in_stmt(stmt));
            if (overlap)
                throw myexception()<<"mdo expression '"<<MD.print()<<"' uses a variable twice!";
        }

        vector<mdo_stmt_info> stmt_infos;
        for(auto& stmt: MD.stmts.stmts)
        {
            set<string> stmt_free_vars;
            auto stmt_binders = rn.rename_stmt(stmt, plus(bound, mdo_binders), stmt_free_vars);
            add(free_vars, minus(stmt_free_vars, mdo_binders));
            stmt_infos.push_back({stmt, stmt_binders, intersection(stmt_free_vars, mdo_binders)});
        }

        auto segments = segment_mdo_stmts(stmt_infos);
        vector<Hs::LStmt> segmented_stmts;
        for(auto& segment: segments)
        {
            if (segment.generated_rec)
            {
                auto R = unloc(segment.stmt).as_<Hs::RecStmt>();
                rn.rename_rec_stmt_ops(R, plus(bound, mdo_binders), free_vars);
                unloc(segment.stmt) = R;
            }
            segmented_stmts.push_back(segment.stmt);
        }

        if (segmented_stmts.empty())
            error(loc, Note()<<"Empty mdo block.");
        else if (not unloc(segmented_stmts.back()).is_a<Hs::SimpleQual>())
            error(segmented_stmts.back().loc, Note()<<"MDo block does not end in an expression.");

        E = Hs::Do(Hs::Stmts(segmented_stmts));
    }
    else if (auto te = E.to<Hs::TypedExp>())
    {
        auto TE = *te;
        TE.exp = rename(TE.exp, bound, free_vars);
        TE.type = rename_and_quantify_type(TE.type);
        E = TE;
    }
    else if (auto rec = E.to<Hs::RecordUpdate>())
    {
        auto Rec = *rec;
        Rec.object = rename(Rec.object, bound, free_vars);
        record_rename::reject_record_update_wildcard(*this, Rec.fbinds);
        set<string> used_field_names;

        for(auto& field: unloc(Rec.fbinds).fields)
        {
            auto& f = unloc(field);
            auto field_name = unloc(f.field).name;
            record_rename::check_duplicate_field(*this, used_field_names, field.loc, field_name, "record update");

            record_rename::expand_expression_pun(*this, field);
            f.value = rename(*f.value, bound, free_vars);
        }
        E = Rec;
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
                Rec.con = {Rec.con.loc, Con};

                if (unloc(Rec.fbinds).dotdot)
                    record_rename::require_record_extension(*this, *unloc(Rec.fbinds).dotdot, LangExt::RecordWildCards, "RecordWildCards", "Record wildcard '..'");
                record_rename::resolve_constructor_field_identities(*this, S->name, unloc(Rec.fbinds));

                for(auto& field: unloc(Rec.fbinds).fields)
                {
                    auto& f = unloc(field);
                    record_rename::expand_expression_pun(*this, field);
                    f.value = rename(*f.value, bound, free_vars);
                }

                E = Rec;
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }
    }
    else if (auto c = E.to<Hs::Case>())
    {
        auto C = *c;

        C.object = rename(C.object, bound, free_vars);
        C.alts   = rename(C.alts,    bound, free_vars);

        E = C;
    }
    else if (E.is_a<Hs::Lambda>())
    {
        auto L = E.as_<Hs::Lambda>();
        L.match = rename(L.match, bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::Let>())
    {
        auto L = E.as_<Hs::Let>();

        auto rn = child();
        rn.fixity_env = rn.add_fixities_from_decls(rn.fixity_env, unloc(L.binds)[0]);
        auto binders = rn.rename_decls(unloc(L.binds), bound, free_vars);
        L.body = rn.rename(L.body, bound, binders, free_vars);

        E = L;
    }
    else if (E.is_a<Hs::If>())
    {
        auto I = E.as_<Hs::If>();

        I.condition    = rename(I.condition, bound, free_vars);
        I.true_branch  = rename(I.true_branch, bound, free_vars);
        I.false_branch = rename(I.false_branch, bound, free_vars);

        E = I;
    }
    else if (E.is_a<Hs::Literal>())
    { }
    else if (E.is_a<Hs::ParsedApp>())
        error(loc, Note()<<"Internal error: parsed application reached expression renaming.");
    else if (E.is_a<Hs::ParsedLambda>() or E.is_a<Hs::ParsedCase>() or
             E.is_a<Hs::ParsedAsPattern>() or E.is_a<Hs::ParsedLazyPattern>() or
             E.is_a<Hs::ParsedStrictPattern>() or E.is_a<Hs::ParsedWildcardPattern>())
        error(loc, Note()<<"Internal error: parser-only expression/pattern syntax reached expression renaming.");
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
    else if (auto app = E.to<Hs::Apply>())
    {
        auto App = *app;
        App.head = rename(App.head, bound, free_vars);
        App.arg  = rename(App.arg,  bound, free_vars);
        E = App;
    }
    else
    {
        error(loc, Note()<<"Unknown syntax in expression!");
    }

    return LE;
}
