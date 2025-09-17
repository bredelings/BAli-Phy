#include <range/v3/all.hpp>
#include "computation/module.H"
#include <deque>
#include <set>
#include <tuple>
#include <utility>
#include "util/io.H"
#include "models/parameters.H"
#include "computation/loader.H"
#include "desugar.H"
#include "util/assert.hh"
#include "util/range.H"
#include "computation/haskell/haskell.H"
#include "computation/haskell/ids.H"
#include "computation/core/func.H"

#include "range/v3/all.hpp"
namespace views = ranges::views;

using std::string;
using std::vector;
using std::set;
using std::deque;
using std::pair;
using std::tuple;

//  -----Prelude: http://www.haskell.org/onlinereport/standard-prelude.html

void failable_expression::add_binding(const Core2::Decls<>& decls)
{
    auto result2 = result;

    result = [result2,decls](const Core2::Exp<>& o) {return make_let(decls,result2(o));};
}


desugar_state::desugar_state(const Module& m_, FreshVarState& state)
    : FreshVarSource(state),
      m(m_)
{}

Hs::VarPattern make_VarPattern(const Core2::Var<>& v)
{
    assert(v.index == 0);
    Hs::Var V(v.name);
    return {{noloc,V}};
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
	auto& con_name = unloc(cp->head).name;
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

Core2::Decls<> desugar_state::desugar_decls(const Hs::Decls& v)
{
    // Now we go through and translate groups of FunDecls.
    Core2::Decls<> decls;
    for(int i=0;i<v.size();i++)
    {
	auto& [_,decl] = v[i];

        if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto pat = pd->lhs;
            auto rhs = desugar_rhs(pd->rhs);
            Core2::Var<> z = get_fresh_core_var("z");
            if (unloc(pat).is_a<Hs::AsPattern>())
            {
                // Special-case for top-level as-patterns
                // This isn't needed, but generates simpler code.
                z = make_core_var(unloc(unloc(pat).as_<Hs::AsPattern>().var));
                pat = unloc(pat).as_<Hs::AsPattern>().pattern;
            }

            decls.push_back( {z,rhs.result(Core2::Constant{0})});
	    assert(not rhs.can_fail);

	    // x = case z of pat -> x
	    for(auto& [_,v]: Hs::vars_in_pattern( pat ) )
            {
                auto x = make_core_var(v);
		std::ostringstream o;
		o<<*pat.loc<<": pattern binding " + pat.print() + ": failed pattern match";
                decls.push_back( {x ,case_expression(z, {unloc(pat)}, {failable_expression(x)}).result(Core2::error(o.str()))});
            }
        }
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            auto fvar = make_core_var(unloc(fd->v));

            auto equations = desugar_matches(fd->matches);
            auto otherwise = Core2::error(m.name + "." + fvar.name+": pattern match failure");

            decls.push_back( {fvar , def_function(equations, otherwise) } );
        }
        else if (auto gb = decl.to<Hs::GenBind>())
        {
            vector<Core2::Var<>> binders;
            for(auto& [name, info]: gb->bind_infos)
            {
                auto x_inner = make_core_var(info.inner_id);
                binders.push_back( x_inner );
            }

            const int N = gb->bind_infos.size();
            assert(N >= 1);

            // tup = \dict1 dict2 ... dictn -> let dict_binds in let {x_inner[1]=..;...;x_inner[n]=..} in (x_inner[1],x_inner[2],...x_inner[n])
            auto binders_exp = binders | ranges::to<vector<Core2::Exp<>>>;
            Core2::Exp<> tup_body = make_let( desugar_decls(gb->body), Tuple(binders_exp) );

            for(auto& dd: gb->dict_decls | views::reverse)
                tup_body = make_let (*dd, tup_body);
        
            auto tup_lambda = lambda_quantify( gb->dict_args, tup_body );

            if (N == 1)
            {
                // x_outer[i] = \info.dict_args => let info.binds in case (tup dict1 .. dictn) of (_,_,x_inner[i],_,_) -> x_inner[i]
                auto& info = gb->bind_infos.begin()->second;

                auto x_outer = make_core_var(info.outer_id);

                x_outer.is_exported = info.is_exported;
                    
                decls.push_back({x_outer, info.wrap(tup_lambda)});
            }
            else
            {
                auto tup = get_fresh_core_var("tup");
                decls.push_back({tup, tup_lambda});

                // x_outer[i] = \info.dict_args => let info.binds in case (tup dict1 .. dictn) of (_,_,x_inner[i],_,_) -> x_inner[i]
                int i=0;
                for(auto& [name, info]: gb->bind_infos)
                {
                    auto x_outer = make_core_var(info.outer_id);
                    auto x_inner = make_core_var(info.inner_id);
                    auto x_tmp   = get_fresh_core_var("tmp");

                    x_outer.is_exported = info.is_exported;
                    
                    vector<Core2::Var<>> fields;
                    for(int j=0;j<N;j++)
                    {
                        if (i == j)
                            fields.push_back( x_inner );
                        else
                            fields.push_back( get_fresh_core_var("w") );
                    }
                    auto pattern = TuplePat(fields);

                    // \dargs -> case (tup dargs) of (..fields..) -> field
                    auto x_tmp_body = lambda_quantify(gb->dict_args,
                                                      Core2::Exp<>(Core2::Case<>{ make_apply<>(Core2::Exp<>(tup), gb->dict_args),
                                                              {{pattern, x_inner}}}) );

                    decls.push_back({x_tmp, x_tmp_body});

                    decls.push_back({x_outer, info.wrap(x_tmp)});

                    i++;
                }
            }
        }
        else
            std::abort();
    }
    return decls;
}

Core2::Decls<> desugar_state::desugar_decls(const Hs::Binds& binds)
{
    Core2::Decls<> all_decls;
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

Core2::Exp<> desugar_state::safe_apply(const Core2::Exp<>& head, const vector<Core2::Exp<>>& args)
{
    return ::safe_apply(head, args, *this);
}

Core2::Exp<> desugar_state::desugar(const Hs::LExp& LE)
{
    return desugar(unloc(LE));
}

Hs::Exp noloc_apply(const std::vector<Hs::Exp>& E)
{
    assert(E.size());
    Hs::LExp LE = {noloc, E[0]};
    for(int i=1;i<E.size();i++)
    {
        LE = {noloc, Hs::ApplyExp(LE, {noloc, E[i]})};
    }
    return unloc(LE);
}

Core2::Exp<> desugar_state::desugar(const Hs::Exp& E)
{
    if (auto L = E.to<Hs::List>())
    {
        Core2::Exp<> CL = Core2::ConApp<>("[]",{});
        for(auto& element: reverse(L->elements))
            CL = Core2::ConApp<>(":", {desugar(element), CL});
        return CL;
    }
    else if (auto L = E.to<Hs::ListFrom>())
    {
        Core2::Exp<> enumFrom = Core2::Var<>("Compiler.Enum.enumFrom");
        enumFrom = desugar(L->enumFromOp);

        return safe_apply(enumFrom, {desugar(L->from)});
    }
    else if (auto L = E.to<Hs::ListFromTo>())
    {
        Core2::Exp<> enumFromTo = Core2::Var<>("Compiler.Enum.enumFromTo");
        enumFromTo = desugar(L->enumFromToOp);

        return safe_apply(enumFromTo, {desugar(L->from), desugar(L->to)});
    }
    else if (auto L = E.to<Hs::ListFromThen>())
    {
        Core2::Exp<> enumFromThen = Core2::Var<>("Compiler.Enum.enumFromThen");
        enumFromThen = desugar(L->enumFromThenOp);
        return safe_apply(enumFromThen, {desugar(L->from), desugar(L->then)});
    }
    else if (auto L = E.to<Hs::ListFromThenTo>())
    {
        Core2::Exp<> enumFromThenTo = Core2::Var<>("Compiler.Enum.enumFromThenTo");
        enumFromThenTo = desugar(L->enumFromThenToOp);

        return safe_apply(enumFromThenTo, {desugar(L->from), desugar(L->then), desugar(L->to)});
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
        if (L.quals.size() == 1 and unloc(L.quals[0]).is_a<Hs::SimpleQual>() and unloc(unloc(L.quals[0]).as_<Hs::SimpleQual>().exp) == Hs::Con("Data.Bool.True"))
        {
            return desugar( Hs::List({L.body}) );
        }

        // [ e | q      ]  =  [ e | q, True ]
        else if (L.quals.size() == 1)
        {
            L.quals.push_back( {noloc,Hs::SimpleQual({noloc,Hs::Con("Data.Bool.True")})} );
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
			return desugar( noloc_apply({Hs::Var("Data.OldList.map"), f, unloc(PQ->exp)}) );
		    }
		    // [ e | p<-l, Q]  =  concatMap (\p -> [e | q ]) l
		    else
		    {
			expression_ref f = Hs::LambdaExp({PQ->bindpat}, {noloc, L});
			return desugar( noloc_apply({concatMap, f, unloc(PQ->exp)}) );
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

                    auto body = noloc_apply({concatMap, ok, unloc(PQ->exp)});
                    return desugar( Hs::LetExp({noloc,{{{{noloc,decl}}}}}, {noloc, body}) );
                }
            }
        }
        std::abort();
    }
    else if (auto S = E.to<Hs::LeftSection>())
    {
        return safe_apply(desugar(S->op), {desugar(S->l_arg)});
    }
    else if (auto S = E.to<Hs::RightSection>())
    {
        auto x = get_fresh_core_var("x");
        return lambda_quantify({x}, safe_apply(desugar(S->op), {x, desugar(S->r_arg)}) );
    }
    else if (auto T = E.to<Hs::Tuple>())
    {
        vector<Core2::Exp<>> elements;
        for(auto& element: T->elements)
            elements.push_back( desugar(element) );
        return Tuple(elements);
    }
    else if (auto v = E.to<Hs::Var>())
    {
        // Why does make_var not want any wrappers?
        auto V = *v;
        V.wrap = {};
        Core2::Exp<> E = make_core_var(V);
        E = v->wrap(E);
        return E;
    }
    else if (auto c = E.to<Hs::Con>())
    {
        // Sometimes c->wrap isn't set because we make up constructors on the fly for e.g. []
        Core2::Exp<> E = Core2::Var<>(c->name);
        E = c->wrap(E);
        return E;
    }
    else if (E.is_a<Hs::Do>())
    {
        // FIXME! Do the rewrite after rename and immediately before typechecking.
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
            result = noloc_apply({sq->andThenOp, e, do_stmts});
        }

        // do { p <- e ; stmts} => let {ok p = do {stmts}; ok _ = fail "..."} in e >>= ok
        // do { v <- e ; stmts} => e >>= (\v -> do {stmts})
        else if (first.is_a<Hs::PatQual>())
        {
            auto& PQ = first.as_<Hs::PatQual>();

            if (is_irrefutable_pat(m, PQ.bindpat))
            {
                expression_ref lambda = Hs::LambdaExp({PQ.bindpat}, {noloc,do_stmts});
                result = noloc_apply({PQ.bindOp, unloc(PQ.exp), lambda});
            }
            else
            {
                // let {ok bindpat = do_stmts; ok _ = fail} in e >>= ok
                auto ok = get_fresh_Var("ok", false);
                auto rule1 = Hs::MRule{ { PQ.bindpat }, Hs::SimpleRHS({noloc,do_stmts}) };
                auto decl  = Hs::FunDecl({noloc,ok}, Hs::Matches{{rule1}});
                if (PQ.failOp)
                {
                    Hs::Literal msg(Hs::String("Pattern match failed at '" +PQ.bindpat.print() + "'"));
                    expression_ref fail = noloc_apply({*PQ.failOp, msg});
                    auto _ = Hs::LPat{noloc, Hs::WildcardPattern()};
                    auto rule2 = Hs::MRule{ { _ },          Hs::SimpleRHS({noloc, fail})};
                    decl.matches.push_back(rule2);
                }

                expression_ref body = noloc_apply({PQ.bindOp, unloc(PQ.exp), ok});
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
        auto otherwise = Core2::error(m.name + " lambda: pattern match failure");

        return def_function({equation}, otherwise);
    }
    else if (auto L = E.to<Hs::LetExp>())
    {
        auto decls = desugar_decls(unloc(L->binds));
        auto body = desugar(L->body);

        // construct the new let expression.
        return make_let(decls, body);
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

        auto obj = desugar(C.object);

        vector<expression_ref> patterns;
        vector<failable_expression> bodies;
        for(const auto& [alt_patterns, alt_rhs]: C.alts)
        {
            patterns.push_back( unloc(alt_patterns[0]) );
            bodies.push_back( desugar_rhs( alt_rhs ) );
        }
        return case_expression(obj, patterns, bodies).result(Core2::error("case: failed pattern match"));
    }
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto A = desugar(app->head);

        auto arg = desugar( app->arg );

        arg = app->arg_wrapper( arg );

        A = safe_apply(A, {arg});

        A = app->res_wrapper( A );

        return A;
    }
    else if (auto L = E.to<Hs::Literal>())
    {
        if (auto c = L->is_Char())
        {
            return Core2::Constant{*c};
        }
        else if (auto i = L->is_Integer())
        {
            Hs::Integer I = std::get<Hs::Integer>(L->literal);
            if (I.fromIntegerOp)
                return safe_apply(desugar(I.fromIntegerOp), {Core2::Constant{Integer(I.value)}});
            else
                return Core2::Constant{Integer(I.value)};
        }
        else if (auto r = L->is_Floating())
        {
            auto F = std::get<Hs::Floating>(L->literal);

	    auto ratio = safe_apply(Core2::Var<>("Compiler.Ratio.Ratio"), {Core2::Constant{Integer(r->numerator())}, Core2::Constant{Integer(r->denominator())}});

            if (F.fromRationalOp)
                return safe_apply(desugar(F.fromRationalOp), {ratio});
            else
                return ratio;
        }
        else if (auto s = L->is_String())
        {
            auto core_string = Core2::unpack_cpp_string(*s);
            Hs::String S = std::get<Hs::String>(L->literal);
            if(S.fromStringOp)
                return safe_apply(desugar(S.fromStringOp),{core_string});
            else
                return core_string;
        }
        else if (auto i = L->is_BoxedInteger())
        {
            return Core2::Constant{i->convert_to<int>()};
        }
        else
            std::abort();
    }
    else
        throw myexception()<<"desugar: unknown expression "<<E.print();
}

Core2::Exp<> desugar(const Module& m, FreshVarState& state, const expression_ref& E)
{
    desugar_state ds(m, state);
    return ds.desugar(E);
}

Core2::Decls<> desugar(const Module& m, FreshVarState& state, const Hs::Decls& decls)
{
    desugar_state ds(m, state);
    return ds.desugar_decls(decls);
}

Core2::Decls<> desugar(const Module& m, FreshVarState& state, const Hs::Binds& binds)
{
    desugar_state ds(m, state);
    return ds.desugar_decls(binds);
}
