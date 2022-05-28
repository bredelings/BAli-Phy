#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>

#include "rename.H"
#include "computation/module.H"
#include "computation/expression/tuple.H" // for tuple_head( )
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

// Here we want to find all the variables bound by the list of stmts, and make sure that they don't overlap.
// Getting the list of variables bound by a "rec" should return all the variables bound by the statements inside the rec.
bound_var_info renamer_state::rename_rec_stmt(expression_ref& rec_stmt, const bound_var_info& bound, set<string>& free_vars)
{
    bound_var_info rec_bound;
    for(auto& stmt: rec_stmt.as_<Haskell::RecStmt>().stmts.stmts)
    {
        bool overlap = not disjoint_add(rec_bound, find_bound_vars_in_stmt(stmt));
	if (overlap)
	    throw myexception()<<"rec command '"<<rec_stmt<<"' uses a variable twice!";
    }
    // 2. Construct the tuple
    vector<expression_ref> vars;
    for(auto& var_name: rec_bound)
        vars.push_back(Hs::Var({noloc,var_name}));
    expression_ref rec_tuple;
    if (vars.size() == 1)
        rec_tuple = vars[0];
    else
    {
        rec_tuple = Hs::Con({noloc, tuple_head(vars.size()).name()},vars.size());
        for(auto var: vars)
            rec_tuple = {rec_tuple, var};
    }

    // 3. Construct the do stmt
    auto stmts = rec_stmt.as_<Haskell::RecStmt>().stmts.stmts;
    expression_ref rec_return = Hs::Var({noloc, "return"});
    expression_ref rec_return_stmt = {rec_return, rec_tuple};
    stmts.push_back(Hs::SimpleQual(rec_return_stmt));
    auto rec_do = Haskell::Do(Haskell::Stmts(stmts));

    // 4. Construct the lambda function
    expression_ref rec_tuple_pattern = unapply(rec_tuple); // This makes the tuple expression into a pattern by translating `@ (@ ((,) x))` into `(,) x y`
    expression_ref rec_lambda = Haskell::LambdaExp({Haskell::LazyPattern(rec_tuple_pattern)}, rec_do);      // \ ~(b,c) -> do { ... }

    // 5. Construct rec_tuple_pattern <- mfix rec_lambda
    expression_ref mfix = Hs::Var({noloc, "mfix"});
    rec_stmt = Haskell::PatQual(rec_tuple_pattern, expression_ref{mfix, rec_lambda});

    // Combine the set of bound variables and rename our rewritten statement;
    return rename_stmt(rec_stmt, bound, rec_bound, free_vars);
}

bound_var_info
renamer_state::rename_stmt(expression_ref& stmt, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars)
{
    set<string> stmt_free_vars;
    auto new_binders = rename_stmt(stmt, plus(bound, binders), stmt_free_vars);
    add(free_vars, minus(stmt_free_vars, binders));
    return new_binders;
}

bound_var_info renamer_state::rename_stmt(expression_ref& stmt, const bound_var_info& bound, set<string>& free_vars)
{
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
	auto bound_vars = rename_decls(unloc(LQ.binds), bound, {}, free_vars);
	stmt = LQ;
	return bound_vars;
    }
    else if (stmt.is_a<Haskell::RecStmt>())
    {
        return rename_rec_stmt(stmt, bound, free_vars);
    }
    else
	std::abort();
}
