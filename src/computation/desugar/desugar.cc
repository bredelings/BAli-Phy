#include <range/v3/all.hpp>
#include "computation/module.H"
#include <deque>
#include <set>
#include <tuple>
#include <utility>
#include "util/io.H"
#include "models/parameters.H"
#include "computation/loader.H"
#include "computation/expression/core.H"
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
#include "util/assert.hh"
#include "util/range.H"
#include "computation/haskell/haskell.H"
#include "computation/haskell/ids.H"

#include "range/v3/all.hpp"
namespace views = ranges::views;

using std::string;
using std::vector;
using std::set;
using std::deque;
using std::pair;

//  -----Prelude: http://www.haskell.org/onlinereport/standard-prelude.html

desugar_state::desugar_state(const Module& m_, FreshVarState& state)
    : FreshVarSource(state),
      m(m_)
{}

expression_ref desugar_string_expression(const std::string& s)
{
    return Core::unpack_cpp_string(s);
}

Hs::VarPattern make_VarPattern(const var& v)
{
    assert(v.index == 0);
    Hs::Var V(v.name);
    return {V};
}

bool is_irrefutable_pat(const Module& m, const Hs::LPat& lpat)
{
    auto& P = unloc(lpat);
    assert(not P.head().is_a<var>());
    assert(not P.head().is_a<Hs::Var>());

    if (P.is_a<Hs::WildcardPattern>())
	return true;
    else if (P.is_a<Hs::VarPattern>())
	return true;
    else if (P.is_a<Hs::LazyPattern>())
	return true;
    else if (auto cp = P.to<Hs::ConPattern>())
    {
	auto& con_name = cp->head.name;
	auto C = m.lookup_resolved_symbol(con_name);
	string pattern_type = C->parent.value();
	auto T = m.lookup_resolved_type(pattern_type);
	auto D = T->is_data();
	assert(D);
	assert(not D->constructors.empty());
	if (D->constructors.size() > 1) return false;

	for(auto& arg: cp->args)
	    if (not is_irrefutable_pat(m, arg))
		return false;

	return true;
    }
    else if (auto tp = P.to<Hs::TypedPattern>())
    {
	return is_irrefutable_pat(m, tp->pat);
    }
    else if (auto tp = P.to<Hs::TuplePattern>())
    {
	for(auto& arg: tp->elements)
	    if (not is_irrefutable_pat(m, arg))
		return false;

	return true;
    }
    else if (P.is_a<Hs::ListPattern>())
    {
	return false;
    }
    else if (auto ap = P.to<Hs::AsPattern>())
    {
	return is_irrefutable_pat(m, ap->pattern);
    }
    else if (auto sp = P.to<Hs::StrictPattern>())
    {
	return is_irrefutable_pat(m, sp->pattern);
    }
    else if (P.is_a<Hs::LiteralPattern>())
    {
	return false;
    }
    else
	std::abort();

}

failable_expression desugar_state::desugar_gdrh(const Hs::GuardedRHS& grhs)
{
    auto F = failable_expression(desugar(grhs.body));

    for(auto& lguard: std::reverse(grhs.guards))
    {
        auto& guard = unloc(lguard);
	if (guard.is_a<Hs::SimpleQual>())
	{
            auto& SQ = guard.as_<Hs::SimpleQual>();
	    auto condition = desugar(SQ.exp);
	    // F' = case True of True -> F
	    if (is_bool_true(condition) or is_otherwise(condition))
		;
	    // F' = case condition of True -> F
	    else
		F = case_expression(condition,{Hs::TruePat()},{F});
	}
	else if (guard.is_a<Hs::LetQual>())
	{
            auto& LQ = guard.as_<Hs::LetQual>();
	    auto binds = desugar_decls(unloc(LQ.binds));

	    F.add_binding(binds);
	}
        else if (guard.is_a<Hs::PatQual>())
        {
            auto& PQ = guard.as_<Hs::PatQual>();

            F = case_expression(desugar(PQ.exp), {unloc(PQ.bindpat)}, {F});
        }
	else
	    std::abort();
    }

    return F;
}

equation_info_t desugar_state::desugar_match(const Hs::MRule& rule)
{
    std::vector<Core::Pat> patterns;
    for(auto& pattern: rule.patterns)
        patterns.push_back( unloc(pattern) );
    return {patterns, desugar_rhs(rule.rhs)};
}

vector<equation_info_t> desugar_state::desugar_matches(const Hs::Matches& matches)
{
    vector<equation_info_t> equations;

    for(auto& rule: matches)
        equations.push_back( desugar_match(rule) );

    return equations;
}

CDecls desugar_state::desugar_decls(const Hs::Decls& v)
{
    // Now we go through and translate groups of FunDecls.
    CDecls decls;
    for(int i=0;i<v.size();i++)
    {
	auto& [_,decl] = v[i];

        if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto pat = pd->lhs;
            auto rhs = desugar_rhs(pd->rhs);
            var z = get_fresh_var();
            if (unloc(pat).is_a<Hs::AsPattern>())
            {
                // Special-case for top-level as-patterns
                // This isn't needed, but generates simpler code.
                z = make_var(unloc(pat).as_<Hs::AsPattern>().var);
                pat = unloc(pat).as_<Hs::AsPattern>().pattern;
            }

            decls.push_back( {z,rhs.result(0)});
	    assert(not rhs.can_fail);

	    // x = case z of pat -> x
	    for(auto& v: Hs::vars_in_pattern( pat ) )
            {
                auto x = make_var(v);
		std::ostringstream o;
		o<<*pat.loc<<": pattern binding " + pat.print() + ": failed pattern match";
		    decls.push_back( {x ,case_expression(z, {unloc(pat)}, {failable_expression(x)}).result(Core::error(o.str()))});
            }
        }
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            auto fvar = make_var(unloc(fd->v));

            auto equations = desugar_matches(fd->matches);
            auto otherwise = Core::error(m.name + "." + fvar.name+": pattern match failure");

            decls.push_back( {fvar , def_function(equations, otherwise) } );
        }
        else if (auto gb = decl.to<Hs::GenBind>())
        {
            vector<var> binders;
            for(auto& [name, info]: gb->bind_infos)
            {
                var x_inner = make_var(info.inner_id);
                binders.push_back( x_inner );
            }

            const int N = gb->bind_infos.size();
            assert(N >= 1);

            // tup = \dict1 dict2 ... dictn -> let dict_binds in let {x_inner[1]=..;...;x_inner[n]=..} in (x_inner[1],x_inner[2],...x_inner[n])
            expression_ref tup_body = Core::Let ( *(gb->dict_decls),
                                                  Core::Let ( desugar_decls(gb->body),
                                                              maybe_tuple(binders) ) );
            expression_ref tup_lambda = Core::Lambda( gb->dict_args, tup_body );

            if (N == 1)
            {
                // x_outer[i] = \info.dict_args => let info.binds in case (tup dict1 .. dictn) of (_,_,x_inner[i],_,_) -> x_inner[i]
                auto& info = gb->bind_infos.begin()->second;

                var x_outer = make_var(info.outer_id);

                decls.push_back({x_outer, info.wrap(tup_lambda)});
            }
            else
            {
                auto tup = get_fresh_var("tup");
                decls.push_back({tup, tup_lambda});

                // x_outer[i] = \info.dict_args => let info.binds in case (tup dict1 .. dictn) of (_,_,x_inner[i],_,_) -> x_inner[i]
                int i=0;
                for(auto& [name, info]: gb->bind_infos)
                {
                    var x_outer = make_var(info.outer_id);
                    var x_inner = make_var(info.inner_id);
                    var x_tmp   = get_fresh_var();

                    vector<expression_ref> fields(N, wildcard());
                    fields[i] = x_inner;
                    expression_ref pattern = get_tuple(fields);

                    // \dargs -> case (tup dargs) of (..fields..) -> field
                    Core::Exp x_tmp_body = Core::Lambda(gb->dict_args,
                                                        Core::Case( Core::Apply(tup, gb->dict_args),
                                                                    {pattern},{x_inner}) );

                    decls.push_back({x_tmp, x_tmp_body});

                    decls.push_back({x_outer, info.wrap(x_tmp)});

                    i++;
                }
            }
        }
        else if (decl.is_a<Hs::ValueDecl>())
            std::abort();
        else
            continue; // std::abort();
    }
    return decls;
}

CDecls desugar_state::desugar_decls(const Hs::Binds& binds)
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


//TODO: make functions that do e.g.
//      * desugar_decls -> CDecls
//      * desugar_decl  -> CDecl
// I guess this would be AST-izing?
//
// One general issue with AST-izing is maybe needing to use object_ptr<T> to avoid copying things.

Core::Exp desugar_state::desugar(const Hs::LExp& LE)
{
    return desugar(unloc(LE));
}

Core::Exp desugar_state::desugar(const Hs::Exp& E)
{
    if (E.is_a<Hs::ClassDecl>())
    {
        auto C = E.as_<Hs::ClassDecl>();
//        if (C.decls)
//            unloc(*C.decls) = desugar_decls(unloc(*C.decls));
        return C;
    }
    else if (E.is_a<Hs::InstanceDecl>())
    {
        auto I = E.as_<Hs::InstanceDecl>();
//        if (I.decls)
//            unloc(*I.decls) = desugar_decls(unloc(*I.decls));
        return I;
    }
    else if (E.is_a<Hs::List>())
    {
        auto L = E.as_<Hs::List>();
        vector<Core::Exp> elements;
        for(auto& element: L.elements)
            elements.push_back(desugar(element));
        return get_list(elements);
    }
    else if (auto L = E.to<Hs::ListFrom>())
    {
        Core::Exp enumFrom = var("Compiler.Enum.enumFrom");
        enumFrom = desugar(L->enumFromOp);

        return {enumFrom, desugar(L->from)};
    }
    else if (auto L = E.to<Hs::ListFromTo>())
    {
        expression_ref enumFromTo = var("Compiler.Enum.enumFromTo");
        enumFromTo = desugar(L->enumFromToOp);

        return {enumFromTo, desugar(L->from), desugar(L->to)};
    }
    else if (auto L = E.to<Hs::ListFromThen>())
    {
        expression_ref enumFromThen = var("Compiler.Enum.enumFromThen");
        enumFromThen = desugar(L->enumFromThenOp);
        return {enumFromThen, desugar(L->from), desugar(L->then)};
    }
    else if (auto L = E.to<Hs::ListFromThenTo>())
    {
        expression_ref enumFromThenTo = var("Compiler.Enum.enumFromThenTo");
        enumFromThenTo = desugar(L->enumFromThenToOp);

        return {enumFromThenTo, desugar(L->from), desugar(L->then), desugar(L->to)};
    }
    else if (E.is_a<Hs::ListComprehension>())
    {
        auto L = E.as_<Hs::ListComprehension>();

        // [ e | True   ]  =  [ e ]
        // [ e | q      ]  =  [ e | q, True ]
        // [ e | b, Q   ]  =  if b then [ e | Q ] else []
        // [ e | let decls, Q] = let decls in [ e | Q ]
        // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in concatMap ok l
        // [ e | x<-l, Q]  =  concatMap (\x -> [e | q ]) l
        // [ e | x<-l]  =  concatMap (\x -> [e]) l = map (\x -> e) l



        // [ e | True   ]  =  [ e ]
        if (L.quals.size() == 1 and unloc(L.quals[0]).is_a<Hs::SimpleQual>() and unloc(unloc(L.quals[0]).as_<Hs::SimpleQual>().exp) == bool_true)
        {
            return desugar( Hs::List({L.body}) );
        }

        // [ e | q      ]  =  [ e | q, True ]
        else if (L.quals.size() == 1)
        {
            L.quals.push_back( {noloc,Hs::SimpleQual({noloc,bool_true})} );
            return desugar( L );
        }

        else
        {
            // Pop the next qual from the FRONT of the list
            expression_ref B = unloc(L.quals[0]);
            L.quals.erase(L.quals.begin());
            expression_ref E2 = L;

            // [ e | b, Q   ]  =  if b then [ e | Q ] else []
            if (auto cond = B.to<Hs::SimpleQual>())
                return desugar( Hs::IfExp( cond->exp, {noloc, E2}, {noloc, Hs::List({})}) );
            // [ e | let decls, Q] = let decls in [ e | Q ]
            else if (auto LQ = B.to<Hs::LetQual>())
                return desugar( Hs::LetExp( LQ->binds, {noloc, E2} ) );
            // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in concatMap ok l
            else if (auto PQ = B.to<Hs::PatQual>())
            {
                Hs::Var concatMap("Data.OldList.concatMap");
		// [ e | p<-l, Q]  =  concatMap (\p -> [e | q ]) l
                if (is_irrefutable_pat(m, PQ->bindpat))
                {
		    // [ e | p<-l]  =  concatMap (\p -> [e]) l
		    //              =  map (\p -> e) l
		    if (L.quals.empty())
		    {
			expression_ref f = Hs::LambdaExp({PQ->bindpat}, L.body);
			return desugar( {Hs::Var("Data.OldList.map"), f, unloc(PQ->exp)} );
		    }
		    // [ e | p<-l, Q]  =  concatMap (\p -> [e | q ]) l
		    else
		    {
			expression_ref f = Hs::LambdaExp({PQ->bindpat}, {noloc, L});
			return desugar( {concatMap, f, unloc(PQ->exp)} );
		    }
                }
                else
                {
                    // let {ok bindpat = L; ok _ = []} in concatMap ok PQ->exp
                    auto ok = get_fresh_Var("ok", false);
                    expression_ref fail = Hs::List({});
                    auto _ = Hs::LPat{noloc, Hs::WildcardPattern()};
                    auto rule1 = Hs::MRule{ { PQ->bindpat }, Hs::SimpleRHS({noloc, L})        };
                    auto rule2 = Hs::MRule{ { _ },           Hs::SimpleRHS({noloc, fail})     };
                    auto decl  = Hs::FunDecl({noloc,ok}, Hs::Matches{{rule1, rule2}});

                    expression_ref body = {concatMap, ok, unloc(PQ->exp)};
                    return desugar( Hs::LetExp({noloc,{{{{noloc,decl}}}}}, {noloc,body}) );
                }
            }
        }
        std::abort();
    }
    else if (auto S = E.to<Hs::LeftSection>())
    {
        return expression_ref({desugar(S->op), desugar(S->l_arg)});
    }
    else if (auto S = E.to<Hs::RightSection>())
    {
        auto x = get_fresh_var();
        return lambda_quantify(x, {desugar(S->op), x, desugar(S->r_arg)} );
    }
    else if (E.is_a<Hs::Tuple>())
    {
        auto T = E.as_<Hs::Tuple>();
        vector<Core::Exp> elements;
        for(auto& element: T.elements)
            elements.push_back( desugar(element) );
        return get_tuple( elements );
    }
    else if (auto v = E.to<Hs::Var>())
    {
        // Why does make_var not want any wrappers?
        auto V = *v;
        V.wrap = {};
        Core::Exp E = make_var(V);
        E = v->wrap(E);
        return E;
    }
    else if (auto c = E.to<Hs::Con>())
    {
        // Sometimes c->wrap isn't set because we make up constructors on the fly for e.g. []
        Core::Exp E = var(c->name);
        E = c->wrap(E);
        return E;
    }
    else if (E.is_a<Hs::Do>())
    {
        auto stmts = E.as_<Hs::Do>().stmts.stmts;

        if (stmts.empty())
            throw myexception()<<"Empty do block!";

        if (not unloc(stmts.back()).is_a<Hs::SimpleQual>())
            throw myexception()<<"The last statement in a do block must be an expression!";

        // do { e }  =>  e
        if (stmts.size() == 1) {
            auto& stmt = unloc(stmts.front());
            auto exp = stmt.as_<Hs::SimpleQual>().exp;
            return desugar(exp);
        }

        auto first = unloc(stmts[0]);
        stmts.erase(stmts.begin());
        expression_ref do_stmts = Hs::Do(Hs::Stmts(stmts));
        expression_ref result;

        // do {e ; stmts }  =>  e >> do { stmts }
        if (auto sq = first.to<Hs::SimpleQual>())
        {
            expression_ref e = unloc(first.as_<Hs::SimpleQual>().exp);
            result = {sq->andThenOp, e, do_stmts};
        }

        // do { p <- e ; stmts} => let {ok p = do {stmts}; ok _ = fail "..."} in e >>= ok
        // do { v <- e ; stmts} => e >>= (\v -> do {stmts})
        else if (first.is_a<Hs::PatQual>())
        {
            auto& PQ = first.as_<Hs::PatQual>();

            if (is_irrefutable_pat(m, PQ.bindpat))
            {
                expression_ref lambda = Hs::LambdaExp({PQ.bindpat}, {noloc,do_stmts});
                result = {PQ.bindOp, unloc(PQ.exp), lambda};
            }
            else
            {
                // let {ok bindpat = do_stmts; ok _ = fail} in e >>= ok
                auto ok = get_fresh_Var("ok", false);
                expression_ref fail = {Hs::Var("Control.Monad.fail"), Hs::Literal(Hs::String("Fail!"))};
                if (PQ.failOp)
                    fail = *PQ.failOp;
                auto _ = Hs::LPat{noloc, Hs::WildcardPattern()};
                auto rule1 = Hs::MRule{ { PQ.bindpat }, Hs::SimpleRHS({noloc,do_stmts}) };
                auto rule2 = Hs::MRule{ { _ },          Hs::SimpleRHS({noloc,fail})     };
                auto decl  = Hs::FunDecl({noloc,ok}, Hs::Matches{{rule1, rule2}});

                expression_ref body = {PQ.bindOp, unloc(PQ.exp), ok};
                result = Hs::LetExp({noloc,{{{{noloc,decl}}}}}, {noloc,body});
            }
        }
        // do {let decls ; rest} = let decls in do {stmts}
        else if (first.is_a<Hs::LetQual>())
        {
            auto& LQ = first.as_<Hs::LetQual>();
            result = Hs::LetExp( LQ.binds, {noloc, do_stmts});
        }
        else
            std::abort();

        return desugar(result);
    }
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        return texp->wrap( desugar( texp->exp) );
    }
    else if (E.is_a<Hs::LambdaExp>())
    {
        auto L = E.as_<Hs::LambdaExp>();

        auto equation = desugar_match(L.match);
	// what top-level function is the lambda in?
	// what line is it on?
        expression_ref otherwise = Core::error(m.name + " lambda: pattern match failure");

        return def_function({equation}, otherwise);
    }
    else if (E.is_a<Hs::LetExp>())
    {
        auto& L = E.as_<Hs::LetExp>();

        CDecls decls = desugar_decls(unloc(L.binds));
        auto body = desugar(L.body);

        // construct the new let expression.
        return let_expression(decls, body);
    }
    else if (E.is_a<Hs::IfExp>())
    {
        auto& I = E.as_<Hs::IfExp>();

        auto condition = desugar(I.condition);
        auto true_branch = desugar(I.true_branch);
        auto false_branch = desugar(I.false_branch);

        return case_expression(condition,{Hs::TruePat()},{failable_expression(true_branch)}).result(false_branch);
    }
    else if (auto c = E.to<Hs::CaseExp>())
    {
        auto& C = *c;

        expression_ref obj = desugar(C.object);

        vector<expression_ref> patterns;
        vector<failable_expression> bodies;
        for(const auto& [alt_patterns, alt_rhs]: C.alts)
        {
            patterns.push_back( unloc(alt_patterns[0]) );
            bodies.push_back( desugar_rhs( alt_rhs ) );
        }
        return case_expression(obj, patterns, bodies).result(Core::error("case: failed pattern match"));
    }
    else if (E.is_a<Hs::ValueDecl>())
        std::abort();
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        Core::Exp A = desugar(app->head);

        Core::Exp arg = desugar( app->arg );

        arg = app->arg_wrapper( arg );

        A = apply_expression(A, arg);

        A = app->res_wrapper( A );

        return A;
    }
    else if (auto L = E.to<Hs::Literal>())
    {
        if (auto c = L->is_Char())
        {
            return *c;
        }
        else if (auto i = L->is_Integer())
        {
            Hs::Integer I = std::get<Hs::Integer>(L->literal);
            if (I.fromIntegerOp)
                return {desugar(I.fromIntegerOp), Integer(I.value)};
            else
                return Integer(I.value);
        }
        else if (auto d = L->is_Double())
        {
            Hs::Double D = std::get<Hs::Double>(L->literal);
            if (D.fromRationalOp)
                return {desugar(D.fromRationalOp), D.value};
            else
                return D.value;

            return *d;
        }
        else if (auto s = L->is_String())
        {
            return desugar_string_expression(*s);
        }
        else if (auto i = L->is_BoxedInteger())
        {
            return i->convert_to<int>();
        }
        else
            std::abort();
    }
    else if (is_apply_exp(E))
    {
        // why are we desugaring this?
        // whouldn't it be an applyexp that we are desugaring?

        auto args = E.copy_sub();
        for(auto& arg: args)
            arg = desugar(arg);

        assert(args.size());
        return expression_ref{E.head(),args};
    }

    std::abort();
}

expression_ref desugar(const Module& m, FreshVarState& state, const expression_ref& E)
{
    desugar_state ds(m, state);
    return ds.desugar(E);
}

CDecls desugar(const Module& m, FreshVarState& state, const Hs::Decls& decls)
{
    desugar_state ds(m, state);
    return ds.desugar_decls(decls);
}

CDecls desugar(const Module& m, FreshVarState& state, const Hs::Binds& binds)
{
    desugar_state ds(m, state);
    return ds.desugar_decls(binds);
}
