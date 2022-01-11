#include <range/v3/all.hpp>
#include "computation/module.H"
#include <deque>
#include <set>
#include <tuple>
#include <utility>
#include "util/io.H"
#include "models/parameters.H"
#include "computation/loader.H"
#include "computation/expression/apply.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/constructor.H"
#include "computation/expression/tuple.H"
#include "computation/expression/list.H"
#include "computation/expression/lambda.H"
#include "computation/expression/var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"
#include "desugar.H"
#include "rename.H" // for group_decls
#include "util/assert.hh"
#include "util/range.H"
#include "computation/parser/haskell.H"

using std::string;
using std::vector;
using std::set;
using std::deque;
using std::pair;

//  -----Prelude: http://www.haskell.org/onlinereport/standard-prelude.html

var make_var(const Hs::Var& v)
{
    return var(unloc(v.name));
}


desugar_state::desugar_state(const Module& m_)
    :fresh_vars(1),  // We shouldn't be making any numerical vars in rename.cc, only var(-1) and var(string).
     m(m_)
{}

bool is_irrefutable_pat(const expression_ref& E)
{
    assert(not E.is_a<Haskell::WildcardPattern>());
    assert(not E.is_a<Haskell::Var>());
    return E.head().is_a<var>();
}

failable_expression desugar_state::desugar_gdrh(const Haskell::GuardedRHS& grhs)
{
    auto F = failable_expression(desugar(grhs.body));

    for(auto& guard: std::reverse(grhs.guards))
    {
	if (guard.is_a<Hs::SimpleQual>())
	{
            auto& SQ = guard.as_<Hs::SimpleQual>();
	    auto condition = desugar(SQ.exp);
	    // F' = case True of True -> F
	    if (is_bool_true(condition) or is_otherwise(condition))
		;
	    // F' = case condition of True -> F
	    else
		F = case_expression(condition,{true},{F});
	}
	else if (guard.is_a<Haskell::LetQual>())
	{
            auto& LQ = guard.as_<Haskell::LetQual>();
	    auto binds = desugar_decls(unloc(LQ.binds));

	    F.add_binding(binds);
	}
        else if (guard.is_a<Haskell::PatQual>())
        {
            auto &PQ = guard.as_<Haskell::PatQual>();
            auto pat = desugar_pattern(PQ.bindpat);
            auto E = desugar(PQ.exp);

            F = case_expression(E,{pat},{F});
        }
	else
	    std::abort();
    }

    return F;
}

CDecls desugar_state::desugar_decls(const Haskell::Decls& v)
{
    // Now we go through and translate groups of FunDecls.
    CDecls decls;
    for(int i=0;i<v.size();i++)
    {
	auto& decl = v[i];

        if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto pat = desugar_pattern(pd->lhs);
            auto rhs = desugar_rhs(pd->rhs);
            var z = get_fresh_var();
            if (pat.is_a<Haskell::AsPattern>())
            {
                z = pat.as_<Haskell::AsPattern>().var.as_<var>();
                pat = pat.as_<Haskell::AsPattern>().pattern;
            }

            // z = pat
	    decls.push_back( {z,rhs.result(0)});
	    assert(not rhs.can_fail);

	    // x = case z of pat -> x
	    for(auto& x: get_free_indices(pat))
		decls.push_back( {x ,case_expression(z, {pat}, {failable_expression(x)}).result(error("pattern binding: failed pattern match"))});
        }
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            auto fvar = make_var(fd->v);

            vector<equation_info_t> equations;

            for(auto& rule: fd->match.rules)
            {
                auto rhs = desugar_rhs(rule.rhs);

                auto patterns = rule.patterns;
                for(auto& pattern: patterns)
                    pattern = desugar_pattern(pattern);

                equations.push_back({ patterns, rhs});
            }
            auto otherwise = error(fvar.name+": pattern match failure");
            decls.push_back( {fvar , def_function(equations, otherwise) } );
        }
        else if (decl.is_a<Hs::ValueDecl>())
            std::abort();
        else
            continue; // std::abort();
    }
    return decls;
}

CDecls desugar_state::desugar_decls(const Haskell::Binds& binds)
{
    CDecls all_decls;
    for(auto& decls: binds)
        ranges::insert(all_decls, all_decls.end(), desugar_decls(decls));
    return all_decls;
}

failable_expression desugar_state::desugar_rhs(const Hs::MultiGuardedRHS& R)
{
    vector<failable_expression> gdrhs;
    for(auto& guarded_rhs: R.guarded_rhss)
        gdrhs.push_back(desugar_gdrh(guarded_rhs));

    auto rhs = fold(gdrhs);

    if (R.decls)
        rhs.add_binding(desugar_decls(unloc(*R.decls)));

    return rhs;
}

expression_ref desugar_state::desugar_pattern(const expression_ref & E)
{
    if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& element: L.elements)
            element = desugar_pattern(element);
        return get_list(L.elements);
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto T = E.as_<Haskell::Tuple>();
        for(auto& element: T.elements)
            element = desugar_pattern(element);
        return get_tuple(T.elements);
    }
    else if (auto v = E.to<Haskell::Var>())
        return make_var(*v);
    else if (auto c = E.to<Haskell::Con>())
        return constructor(unloc(c->name), *c->arity);
    else if (auto c = E.head().to<Haskell::Con>())
    {
        auto C = constructor(unloc(c->name), *c->arity);
        auto pats = E.sub();
        for(auto& pat: pats)
            pat = desugar_pattern(pat);
        return expression_ref(C,pats);
    }
    else if (E.is_a<Haskell::WildcardPattern>())
        return var(-1);
    else if (E.is_a<Haskell::AsPattern>())
    {
        auto& AP = E.as_<Haskell::AsPattern>();
        return Haskell::AsPattern(desugar_pattern(AP.var), desugar_pattern(AP.pattern));
    }
    else if (E.is_a<Haskell::LazyPattern>())
    {
        auto LP = E.as_<Haskell::LazyPattern>();
        return Haskell::LazyPattern(desugar_pattern(LP.pattern));
    }
    else if (E.is_a<Haskell::StrictPattern>())
    {
        auto SP = E.as_<Haskell::StrictPattern>();
        SP.pattern = desugar_pattern(SP.pattern);
        return SP;
    }
    else if (E.is_int() or E.is_double() or E.is_char() or E.is_log_double())
        return E;

    throw myexception()<<"I don't understand pattern '"<<E<<"'";
}

//TODO: make functions that do e.g.
//      * desugar_decls -> CDecls
//      * desugar_decl  -> CDecl
// I guess this would be AST-izing?
//
// One general issue with AST-izing is maybe needing to use object_ptr<T> to avoid copying things.

expression_ref desugar_state::desugar(const expression_ref& E)
{
    if (E.is_a<Haskell::ClassDecl>())
    {
        auto C = E.as_<Haskell::ClassDecl>();
//        if (C.decls)
//            unloc(*C.decls) = desugar_decls(unloc(*C.decls));
        return C;
    }
    else if (E.is_a<Haskell::InstanceDecl>())
    {
        auto I = E.as_<Haskell::InstanceDecl>();
//        if (I.decls)
//            unloc(*I.decls) = desugar_decls(unloc(*I.decls));
        return I;
    }
    else if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& element: L.elements)
            element = desugar(element);
        return get_list(L.elements);
    }
    else if (auto L = E.to<Haskell::ListFrom>())
    {
        expression_ref E2 = var("Compiler.Enum.enumFrom");
        E2 = {E2, L->from};
        return desugar(E2);
    }
    else if (auto L = E.to<Haskell::ListFromTo>())
    {
        expression_ref E2 = var("Compiler.Enum.enumFromTo");
        E2 = {E2, L->from, L->to};
        return desugar(E2);
    }
    else if (auto L = E.to<Haskell::ListFromThen>())
    {
        expression_ref E2 = var("Compiler.Enum.enumFromThen");
        E2 = {E2, L->from, L->then};
        return desugar(E2);
    }
    else if (auto L = E.to<Haskell::ListFromThenTo>())
    {
        expression_ref E2 = var("Compiler.Enum.enumFromThenTo");
        E2 = {E2, L->from, L->then, L->to};
        return desugar(E2);
    }
    else if (E.is_a<Haskell::ListComprehension>())
    {
        auto L = E.as_<Haskell::ListComprehension>();

        // [ e | True   ]  =  [ e ]
        // [ e | q      ]  =  [ e | q, True ]
        // [ e | b, Q   ]  =  if b then [ e | Q ] else []
        // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in concatMap ok l
        // [ e | let decls, Q] = let decls in [ e | Q ]

        // [ e | True   ]  =  [ e ]
        if (L.quals.size() == 1 and L.quals[0].is_a<Hs::SimpleQual>() and L.quals[0].as_<Hs::SimpleQual>().exp == bool_true)
        {
            return desugar( List({L.body}) );
        }

        // [ e | q      ]  =  [ e | q, True ]
        else if (L.quals.size() == 1)
        {
            L.quals.push_back( Hs::SimpleQual(bool_true) );
            return desugar( L );
        }

        else
        {
            // Pop the next qual from the FRONT of the list
            expression_ref B = L.quals[0];
            L.quals.erase(L.quals.begin());
            expression_ref E2 = L;

            // [ e | b, Q   ]  =  if b then [ e | Q ] else []
            if (auto cond = B.to<Hs::SimpleQual>())
                return desugar( Haskell::IfExp({noloc, cond->exp}, {noloc, E2}, {noloc, Haskell::List({})}) );
            // [ e | let decls, Q] = let decls in [ e | Q ]
            else if (auto LQ = B.to<Haskell::LetQual>())
                return desugar( Haskell::LetExp( LQ->binds, {noloc, E2} ) );
            // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in concatMap ok l
            else if (auto PQ = B.to<Haskell::PatQual>())
            {
                auto bindpat = desugar_pattern(PQ->bindpat);
                if (is_irrefutable_pat(bindpat))
                {
                    expression_ref f = Haskell::LambdaExp({PQ->bindpat}, L);
                    return desugar( {var("Data.List.concatMap"), f, PQ->exp} );
                }
                else
                {
                    // let {ok bindpat = L; ok _ = []} in concatMap ok PQ->exp
                    auto ok = get_fresh_Var("ok");
                    expression_ref fail = Hs::List({});
                    auto rule1 = Hs::MRule{ {PQ->bindpat},           Hs::SimpleRHS({noloc, L})        };
                    auto rule2 = Hs::MRule{ {Hs::WildcardPattern()}, Hs::SimpleRHS({noloc, fail})     };
                    auto decl  = Hs::FunDecl(ok, Hs::Match{{rule1, rule2}});

                    expression_ref body = {var("Data.List.concatMap"), ok, PQ->exp};
                    return desugar( Haskell::LetExp({noloc,{{{decl}}}}, {noloc,body}) );
                }
            }
        }
        std::abort();
    }
    else if (auto S = E.to<Haskell::LeftSection>())
    {
        return expression_ref({desugar(S->op), desugar(S->l_arg)});
    }
    else if (auto S = E.to<Haskell::RightSection>())
    {
        auto x = get_fresh_var();
        return lambda_quantify(x, {desugar(S->op), x, desugar(S->r_arg)} );
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto T = E.as_<Haskell::Tuple>();
        for(auto& element: T.elements)
            element = desugar(element);
        return get_tuple(T.elements);
    }
    else if (auto v = E.to<Haskell::Var>())
        return make_var(*v);
    else if (auto c = E.to<Haskell::Con>())
        return var(unloc(c->name));
    else if (E.is_a<Haskell::Do>())
    {
        auto stmts = E.as_<Haskell::Do>().stmts.stmts;

        if (stmts.empty())
            throw myexception()<<"Empty do block!";

        if (not stmts.back().is_a<Hs::SimpleQual>())
            throw myexception()<<"The last statement in a do block must be an expression!";

        // do { e }  =>  e
        if (stmts.size() == 1) {
            auto& stmt = stmts.front();
            auto exp = stmt.as_<Hs::SimpleQual>().exp;
            return desugar(exp);
        }

        auto first = stmts[0];
        stmts.erase(stmts.begin());
        expression_ref do_stmts = Haskell::Do(Haskell::Stmts(stmts));
        expression_ref result;

        // do {e ; stmts }  =>  e >> do { stmts }
        if (first.is_a<Hs::SimpleQual>())
        {
            expression_ref e = first.as_<Hs::SimpleQual>().exp;
            result = {var("Compiler.Base.>>"), e, do_stmts};
        }

        // do { p <- e ; stmts} => let {ok p = do {stmts}; ok _ = fail "..."} in e >>= ok
        // do { v <- e ; stmts} => e >>= (\v -> do {stmts})
        else if (first.is_a<Haskell::PatQual>())
        {
            auto& PQ = first.as_<Haskell::PatQual>();
            expression_ref p = desugar_pattern(PQ.bindpat);
            expression_ref e = PQ.exp;
            expression_ref qop = var("Compiler.Base.>>=");

            if (is_irrefutable_pat(p))
            {
                expression_ref lambda = Haskell::LambdaExp({PQ.bindpat}, do_stmts);
                result = {qop,e,lambda};
            }
            else
            {
                // let {ok bindpat = do_stmts; ok _ = fail} in e >>= ok
                auto ok = get_fresh_Var("ok");
                expression_ref fail = {var("Compiler.Base.fail"),"Fail!"};
                auto rule1 = Hs::MRule{ {PQ.bindpat},            Hs::SimpleRHS({noloc,do_stmts}) };
                auto rule2 = Hs::MRule{ {Hs::WildcardPattern()}, Hs::SimpleRHS({noloc,fail})     };
                auto decl  = Hs::FunDecl(ok, Hs::Match{{rule1, rule2}});

                expression_ref body = {qop,e,ok};
                result = Haskell::LetExp({noloc,{{{decl}}}}, {noloc,body});
            }
        }
        // do {let decls ; rest} = let decls in do {stmts}
        else if (first.is_a<Haskell::LetQual>())
        {
            auto& LQ = first.as_<Haskell::LetQual>();
            result = Haskell::LetExp( LQ.binds, {noloc, do_stmts});
        }
        else
            std::abort();

        return desugar(result);
    }
    else if (E.is_a<Haskell::LambdaExp>())
    {
        auto L = E.as_<Haskell::LambdaExp>();

        for(auto& arg: L.args)
            arg = desugar_pattern(arg);

        // 2. Desugar the body, binding vars mentioned in the lambda patterns.
        auto rhs = desugar_rhs(L.body);

        return def_function({{L.args, rhs}}, error("lambda: pattern match failure"));
    }
    else if (E.is_a<Haskell::LetExp>())
    {
        auto& L = E.as_<Haskell::LetExp>();

        CDecls decls = desugar_decls(unloc(L.binds));
        auto body = desugar(unloc(L.body));

        // construct the new let expression.
        return let_expression(decls, body);
    }
    else if (E.is_a<Haskell::IfExp>())
    {
        auto& I = E.as_<Haskell::IfExp>();

        auto condition = desugar(unloc(I.condition));
        auto true_branch = desugar(unloc(I.true_branch));
        auto false_branch = desugar(unloc(I.false_branch));

        return case_expression(condition,{true},{failable_expression(true_branch)}).result(false_branch);
    }
    else if (E.is_a<Haskell::CaseExp>())
    {
        auto& C = E.as_<Haskell::CaseExp>();

        expression_ref obj = desugar(C.object);

        vector<expression_ref> patterns;
        vector<failable_expression> bodies;
        for(const auto& alt: C.alts)
        {
            patterns.push_back( desugar_pattern( unloc(alt).pattern) );
            bodies.push_back( desugar_rhs(unloc(alt).rhs) );
        }
        return case_expression(obj, patterns, bodies).result(error("case: failed pattern match"));
    }
    else if (E.is_a<Haskell::ValueDecl>())
        std::abort();

    auto head = E.head();
    vector<expression_ref> v = E.copy_sub();

    if (auto c = head.to<Hs::Con>())
        head = constructor(unloc(c->name), *c->arity);
    
    for(auto& e: v)
	e = desugar(e);
    if (E.size())
	return expression_ref{head,v};
    else
	return head;
}

expression_ref desugar(const Module& m, const expression_ref& E)
{
    desugar_state ds(m);
    return ds.desugar(E);
}

CDecls desugar(const Module& m, const Haskell::Decls& decls)
{
    desugar_state ds(m);
    return ds.desugar_decls(decls);
}

CDecls desugar(const Module& m, const Haskell::Binds& binds)
{
    desugar_state ds(m);
    return ds.desugar_decls(binds);
}
