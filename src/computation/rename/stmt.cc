#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>

#include "rename.H"
#include "computation/module.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;
using std::map;

/*
         * See "Recursive binding groups" in https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html
         *
         * "Like let and where bindings, name shadowing is not allowed within an mdo-expression or a rec-block"
         *
         * As an example:           ===>
         *   rec { b <- f a c              (b,c) <- mfix (\ ~(b,c) -> do { b <- f a c
         *       ; c <- f b a }                                          ; c <- f b a
         *                                                               ; return (b,c) } )
         *
         * See ghc/compiler/rename/RnExpr.hs
         */

void renamer_state::rename_rec_stmt_ops(Hs::RecStmt& R, const bound_var_info& bound, set<string>& free_vars)
{
    R.returnOp = Hs::Var("return");

    set<string> op_free_vars;
    auto return_op = rename({noloc, R.returnOp}, bound, op_free_vars);
    add(free_vars, op_free_vars);
    R.returnOp = unloc(return_op).as_<Hs::Var>();
}

// Here we want to find all the variables bound by the list of stmts, and make sure that they don't overlap.
// Getting the list of variables bound by a "rec" should return all the variables bound by the statements inside the rec.
bound_var_info renamer_state::rename_rec_stmt(Hs::LExp& lrec_stmt, const bound_var_info& bound, set<string>& free_vars)
{
    auto& rec_stmt = unloc(lrec_stmt);

    if (not m.language_extensions.has_extension(LangExt::RecursiveDo))
        error(lrec_stmt.loc, Note()<<"rec statement requires the RecursiveDo extension.");

    auto R = rec_stmt.as_<Hs::RecStmt>();

    bound_var_info rec_bound;
    for(auto& stmt: R.stmts.stmts)
    {
        bool overlap = not disjoint_add(rec_bound, find_bound_vars_in_stmt(stmt));
	if (overlap)
	    throw myexception()<<"rec command '"<<rec_stmt<<"' uses a variable twice!";
    }

    rename_rec_stmt_ops(R, bound, free_vars);

    auto rn = child();
    for(auto& stmt: R.stmts.stmts)
        rn.rename_stmt(stmt, bound, rec_bound, free_vars);

    rec_stmt = R;
    return rec_bound;
}

bound_var_info
renamer_state::rename_stmt(Hs::LExp& stmt, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars)
{
    set<string> stmt_free_vars;
    auto new_binders = rename_stmt(stmt, plus(bound, binders), stmt_free_vars);
    add(free_vars, minus(stmt_free_vars, binders));
    return new_binders;
}

bound_var_info renamer_state::rename_stmt(Hs::LExp& lstmt, const bound_var_info& bound, set<string>& free_vars)
{
    auto& stmt = unloc(lstmt);

    if (stmt.is_a<Hs::SimpleQual>())
    {
        auto SQ = stmt.as_<Hs::SimpleQual>();
	SQ.exp = rename(SQ.exp, bound, free_vars);
        stmt = SQ;
	return {};
    }
    else if (stmt.is_a<Haskell::PatQual>())
    {
        auto PQ = stmt.as_<Haskell::PatQual>();
	PQ.exp = rename(PQ.exp, bound, free_vars);
	auto bound_vars = rename_pattern(PQ.bindpat);
        stmt = PQ;
	return bound_vars;
    }
    else if (stmt.is_a<Haskell::LetQual>())
    {
        auto LQ = stmt.as_<Haskell::LetQual>();
        fixity_env = add_fixities_from_decls(fixity_env, unloc(LQ.binds)[0]);
	auto bound_vars = rename_decls(unloc(LQ.binds), bound, {}, free_vars);
	stmt = LQ;
	return bound_vars;
    }
    else if (stmt.is_a<Haskell::RecStmt>())
    {
        return rename_rec_stmt(lstmt, bound, free_vars);
    }
    else
	std::abort();
}
