#include "cse.H"
#include "immer/map.hpp"

using std::vector;

/*
 * TODO:
 * - get references to small decls (including constants) from other modules?
 * - distinguish rec and non-rec single variable decls.
 *   + This might require eliminating expression/convert.cc
 *     Otherwise we will lose the rec/nonrec info on round-trip from Decls <-> CDecls
 * - Maybe first change Decl to Rec [Decl], before adding NonRec Decl?
 * - Maybe first only CSE constants and 0-argument constructors?
 */

/* Note: Implementing CSE
 * (See ghc/compiler/GHC/Core/Opt/CSE.hs)
 *
 * It relies on the ability to map from Core expressions to variables.
 * (See GHC.Core.Map.Expr)
 *
 * To avoid being sensitive to binder names, this map transforms to DeBruijn indices on the fly!
 * This is implemented by the DeBruijn type in See GHC.Core.Map.Type.
 * An expression is debruijnized by adding an empty environment.
 * The environment maps integers <-> variable names.
 * It seems like only equality is required on the debruijnized expressions?
 * A "TrieMap" is a map where the key is a structured value like Core2::Exp.
 *   See paper "A Generalization of the Trie Data Structure" by Connelly and Morris.
 *   The Trie stores strings using a radix tree where we have out-edges for each next letter.
 *     Thus strings with shared prefixes store the prefix only once.
 *
 * Apparently handling recursive binder groups is hard, but for self-recursive bindings like
 *    f = \x -> ...f ...
 * They can store then as \f \x -> ... f ...
 * Then the deBruijn indices allow noting the equivalence.
 * A separate looking environment is used for these.
 *
 * The CSE environment (CSEnv) has three parts:
 * - cs_subst :: in_var -> out_var (or trivial out-expression?)
 *               This substitution is applied before cs_map
 * - cs_map :: out_exp -> out_var (or trivial out-expression?)
 * - cs_rec_map :: out_exp -> out_exp.  This is a separate map for self-recursive bindings.
 */

struct cse_env
{
    immer::map<Core2::Exp<>, Core2::Var<>> existing_bindings;
    immer::map<Core2::Var<>, Core2::Var<>> replace_var;

    const simplifier_options& opts;

    FreshVarState& fvstate;

    const Module& M;
    
    cse_env(const simplifier_options& o, FreshVarState& f, const Module& m)
        :opts(o), fvstate(f), M(m)
        {
        }

    Core2::Decls<> cse_decls(const Core2::Decls<>& decls);
};

Core2::Decls<> cse_env::cse_decls(const Core2::Decls<>& decls)
{
    // We need to separate non-rec, rec (1) and rec (>1).
    // But we don't have this at the AST level?

    // Right now we only want to handle non-rec, which means a single decl that doesn't refer to itself.

    return decls;
}


vector<Core2::Decls<>> cse_module(const simplifier_options& opts, FreshVarState& fvstate, Module& m, const vector<Core2::Decls<>>& decl_groups_in)
{
    cse_env env(opts, fvstate, m);

    vector<Core2::Decls<>> decl_groups_out;

    for(auto& decl_group: decl_groups_in)
        decl_groups_out.push_back( env.cse_decls(decl_group) );

    return decl_groups_out;
}
