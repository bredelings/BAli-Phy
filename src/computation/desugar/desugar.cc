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


expression_ref desugar_string_pattern(const std::string& s)
{
    vector<expression_ref> chars;
    for(char c: s)
	chars.push_back(c);
    return get_list(chars);
}

expression_ref desugar_string_expression(const std::string& s)
{
    return Core::unpack_cpp_string(s);
}

bool is_irrefutable_pat(const expression_ref& E)
{
    assert(not E.is_a<Hs::WildcardPattern>());
    assert(not E.is_a<Hs::Var>());
    return E.head().is_a<var>();
}

failable_expression desugar_state::desugar_gdrh(const Hs::GuardedRHS& grhs)
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
	else if (guard.is_a<Hs::LetQual>())
	{
            auto& LQ = guard.as_<Hs::LetQual>();
	    auto binds = desugar_decls(unloc(LQ.binds));

	    F.add_binding(binds);
	}
        else if (guard.is_a<Hs::PatQual>())
        {
            auto &PQ = guard.as_<Hs::PatQual>();
            auto pat = desugar_pattern(PQ.bindpat);
            auto E = desugar(PQ.exp);

            F = case_expression(E,{pat},{F});
        }
	else
	    std::abort();
    }

    return F;
}

equation_info_t desugar_state::desugar_match(const Hs::MRule& rule)
{
    auto rhs = desugar_rhs(rule.rhs);

    auto patterns = rule.patterns;
    for(auto& pattern: patterns)
        pattern = desugar_pattern(pattern);

    return {patterns, rhs};
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
	auto& decl = v[i];

        if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto pat = desugar_pattern( unloc(pd->lhs) );
            auto rhs = desugar_rhs(pd->rhs);
            var z = get_fresh_var();
            if (pat.is_a<Hs::AsPattern>())
            {
                z = make_var(pat.as_<Hs::AsPattern>().var);
                pat = pat.as_<Hs::AsPattern>().pattern;
            }

            // z = pat
	    decls.push_back( {z,rhs.result(0)});
	    assert(not rhs.can_fail);

	    // x = case z of pat -> x
	    for(auto& x: get_free_indices(pat))
		decls.push_back( {x ,case_expression(z, {pat}, {failable_expression(x)}).result(Core::error("pattern binding: failed pattern match"))});
        }
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            auto fvar = make_var(fd->v);

            auto equations = desugar_matches(fd->matches);
            auto otherwise = Core::error(fvar.name+": pattern match failure");

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
            expression_ref tup_body = let_expression ( *(gb->dict_decls),
                                      let_expression ( desugar_decls(gb->body),
                                      maybe_tuple(binders) ) );
            expression_ref tup_lambda = lambda_quantify( gb->dict_args, tup_body );

            auto tup = get_fresh_var("tup");
            decls.push_back({tup, tup_lambda});

            if (N == 1)
            {
                // x_outer[i] = \info.dict_args => let info.binds in case (tup dict1 .. dictn) of (_,_,x_inner[i],_,_) -> x_inner[i]
                auto& info = gb->bind_infos.begin()->second;

                var x_outer = make_var(info.outer_id);

                Core::Exp x_body = Core::Lambda( info.dict_args, Core::Let( info.default_decls, Core::Apply(tup, gb->dict_args) ) );

                decls.push_back({x_outer, x_body});
            }
            else
            {
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
                                                        make_case_expression( Core::Apply(tup, gb->dict_args)
                                                                              ,{{pattern}},{x_inner}) );

                    decls.push_back({x_tmp, x_tmp_body});

                    Core::Exp x_body = Core::Lambda( info.dict_args, Core::Let( info.default_decls, Core::Apply(x_tmp, gb->dict_args) ) );

                    decls.push_back({x_outer, x_body});

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

expression_ref desugar_state::desugar_pattern(const expression_ref & E)
{
    // FIXME - maybe we should keep these elements to use in desugar-case?
    if (E.is_a<Hs::ListPattern>())
    {
        auto L = E.as_<Hs::ListPattern>();
        for(auto& element: L.elements)
            element = desugar_pattern(element);
        return get_list(L.elements);
    }
    else if (E.is_a<Hs::TuplePattern>())
    {
        auto T = E.as_<Hs::TuplePattern>();
        for(auto& element: T.elements)
            element = desugar_pattern(element);
        return get_tuple(T.elements);
    }
    else if (auto v = E.to<Hs::VarPattern>())
        return make_var(v->var);
    else if (auto c = E.to<Hs::ConPattern>())
    {
        auto C = constructor(unloc(c->head.name), *c->head.arity);
        vector<expression_ref> args;
        for(auto& darg: c->dict_args())
            args.push_back(darg);
        for(auto& pat: c->args)
            args.push_back(desugar_pattern(pat));
        return expression_ref(C, args);
    }
    else if (E.is_a<Hs::WildcardPattern>())
        return var(-1);
    else if (E.is_a<Hs::AsPattern>())
    {
        auto& AP = E.as_<Hs::AsPattern>();
        return Hs::AsPattern(AP.var, desugar_pattern(AP.pattern));
    }
    else if (E.is_a<Hs::LazyPattern>())
    {
        auto LP = E.as_<Hs::LazyPattern>();
        return Hs::LazyPattern(desugar_pattern(LP.pattern));
    }
    else if (E.is_a<Hs::StrictPattern>())
    {
        auto SP = E.as_<Hs::StrictPattern>();
        SP.pattern = desugar_pattern(SP.pattern);
        return SP;
    }
    else if (auto tpat = E.to<Hs::TypedPattern>())
    {
        // FIXME - what do we do with the wrapper?
        return desugar_pattern(tpat->pat);
    }
    else if (E.is_log_double())
        std::abort();
    else if (auto L = E.to<Hs::LiteralPattern>())
    {
        if (auto c = L->lit.is_Char())
        {
            return *c;
        }
        else if (auto i = L->lit.is_Integer())
        {
            return *i;
        }
        else if (auto d = L->lit.is_Double())
        {
            // FIXME: we want to actually compare with fromFractional(E)
            return *d;
        }
        else if (auto s = L->lit.is_String())
        {
            return desugar_string_pattern(*s);
        }
        else if (auto i = L->lit.is_BoxedInteger())
        {
            return *i;
        }
        else
            std::abort();
    }

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
        for(auto& element: L.elements)
            element = desugar(element);
        return get_list(L.elements);
    }
    else if (auto L = E.to<Hs::ListFrom>())
    {
        expression_ref enumFrom = var("Compiler.Enum.enumFrom");
        if (L->enumFromOp)
            enumFrom = desugar(L->enumFromOp);

        return {enumFrom, desugar(L->from)};
    }
    else if (auto L = E.to<Hs::ListFromTo>())
    {
        expression_ref enumFromTo = var("Compiler.Enum.enumFromTo");
        if (L->enumFromToOp)
            enumFromTo = desugar(L->enumFromToOp);

        return {enumFromTo, desugar(L->from), desugar(L->to)};
    }
    else if (auto L = E.to<Hs::ListFromThen>())
    {
        expression_ref enumFromThen = var("Compiler.Enum.enumFromThen");
        if (L->enumFromThenOp)
            enumFromThen = desugar(L->enumFromThenOp);
        return {enumFromThen, desugar(L->from), desugar(L->then)};
    }
    else if (auto L = E.to<Hs::ListFromThenTo>())
    {
        expression_ref enumFromThenTo = var("Compiler.Enum.enumFromThenTo");
        if (L->enumFromThenToOp)
            enumFromThenTo = desugar(L->enumFromThenToOp);

        return {enumFromThenTo, desugar(L->from), desugar(L->then), desugar(L->to)};
    }
    else if (E.is_a<Hs::ListComprehension>())
    {
        auto L = E.as_<Hs::ListComprehension>();

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
                return desugar( Hs::IfExp({noloc, cond->exp}, {noloc, E2}, {noloc, Hs::List({})}) );
            // [ e | let decls, Q] = let decls in [ e | Q ]
            else if (auto LQ = B.to<Hs::LetQual>())
                return desugar( Hs::LetExp( LQ->binds, {noloc, E2} ) );
            // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in concatMap ok l
            else if (auto PQ = B.to<Hs::PatQual>())
            {
                auto bindpat = desugar_pattern(PQ->bindpat);
                if (is_irrefutable_pat(bindpat))
                {
                    expression_ref f = Hs::LambdaExp({PQ->bindpat}, L);
                    return desugar( {var("Data.List.concatMap"), f, PQ->exp} );
                }
                else
                {
                    // let {ok bindpat = L; ok _ = []} in concatMap ok PQ->exp
                    auto ok = get_fresh_Var("ok", false);
                    expression_ref fail = Hs::List({});
                    auto rule1 = Hs::MRule{ {PQ->bindpat},           Hs::SimpleRHS({noloc, L})        };
                    auto rule2 = Hs::MRule{ {Hs::WildcardPattern()}, Hs::SimpleRHS({noloc, fail})     };
                    auto decl  = Hs::FunDecl(ok, Hs::Matches{{rule1, rule2}});

                    expression_ref body = {var("Data.List.concatMap"), ok, PQ->exp};
                    return desugar( Hs::LetExp({noloc,{{{decl}}}}, {noloc,body}) );
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
        for(auto& element: T.elements)
            element = desugar(element);
        return get_tuple(T.elements);
    }
    else if (auto v = E.to<Hs::Var>())
    {
        auto V = *v;
        V.wrap = {};
        Core::Exp E = make_var(V);
        if (v->wrap)
            E = (*v->wrap)(E);
        return E;
    }
    else if (auto c = E.to<Hs::Con>())
        return var(unloc(c->name));
    else if (E.is_a<Hs::Do>())
    {
        auto stmts = E.as_<Hs::Do>().stmts.stmts;

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
        expression_ref do_stmts = Hs::Do(Hs::Stmts(stmts));
        expression_ref result;

        // do {e ; stmts }  =>  e >> do { stmts }
        if (auto sq = first.to<Hs::SimpleQual>())
        {
            expression_ref e = first.as_<Hs::SimpleQual>().exp;
            expression_ref and_then = var("Compiler.Base.>>");
            if (sq->andThenOp)
                and_then = desugar(sq->andThenOp);
            result = {and_then, e, do_stmts};
        }

        // do { p <- e ; stmts} => let {ok p = do {stmts}; ok _ = fail "..."} in e >>= ok
        // do { v <- e ; stmts} => e >>= (\v -> do {stmts})
        else if (first.is_a<Hs::PatQual>())
        {
            auto& PQ = first.as_<Hs::PatQual>();
            expression_ref p = desugar_pattern(PQ.bindpat);
            expression_ref e = PQ.exp;
            expression_ref bind = var("Compiler.Base.>>=");
            if (PQ.bindOp)
                bind = desugar(PQ.bindOp);

            if (is_irrefutable_pat(p))
            {
                expression_ref lambda = Hs::LambdaExp({PQ.bindpat}, do_stmts);
                result = {bind,e,lambda};
            }
            else
            {
                // let {ok bindpat = do_stmts; ok _ = fail} in e >>= ok
                auto ok = get_fresh_Var("ok", false);
                expression_ref fail = {var("Control.Monad.fail"), desugar_string_expression("Fail!")};
                if (PQ.failOp)
                    fail = desugar(PQ.failOp);
                auto rule1 = Hs::MRule{ {PQ.bindpat},            Hs::SimpleRHS({noloc,do_stmts}) };
                auto rule2 = Hs::MRule{ {Hs::WildcardPattern()}, Hs::SimpleRHS({noloc,fail})     };
                auto decl  = Hs::FunDecl(ok, Hs::Matches{{rule1, rule2}});

                expression_ref body = {bind,e,ok};
                result = Hs::LetExp({noloc,{{{decl}}}}, {noloc,body});
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
        expression_ref otherwise = Core::error("lambda: pattern match failure");

        return def_function({equation}, otherwise);
    }
    else if (E.is_a<Hs::LetExp>())
    {
        auto& L = E.as_<Hs::LetExp>();

        CDecls decls = desugar_decls(unloc(L.binds));
        auto body = desugar(unloc(L.body));

        // construct the new let expression.
        return let_expression(decls, body);
    }
    else if (E.is_a<Hs::IfExp>())
    {
        auto& I = E.as_<Hs::IfExp>();

        auto condition = desugar(unloc(I.condition));
        auto true_branch = desugar(unloc(I.true_branch));
        auto false_branch = desugar(unloc(I.false_branch));

        return case_expression(condition,{true},{failable_expression(true_branch)}).result(false_branch);
    }
    else if (E.is_a<Hs::CaseExp>())
    {
        auto& C = E.as_<Hs::CaseExp>();

        expression_ref obj = desugar(C.object);

        vector<expression_ref> patterns;
        vector<failable_expression> bodies;
        for(const auto& [alt_patterns, alt_rhs]: C.alts)
        {
            patterns.push_back( desugar_pattern( alt_patterns[0] ) );
            bodies.push_back( desugar_rhs( alt_rhs ) );
        }
        return case_expression(obj, patterns, bodies).result(Core::error("case: failed pattern match"));
    }
    else if (E.is_a<Hs::ValueDecl>())
        std::abort();
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        Core::Exp A = desugar(app->head);

        for(int i=0; i < app->args.size(); i++)
        {
            auto arg = desugar( app->args[i] );
            if (not app->arg_wrappers.empty())
                arg = app->arg_wrappers[i]( arg );

            A = apply_expression(A, arg);

            if (not app->res_wrappers.empty())
                A = app->res_wrappers[i]( A );
        }

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
                return {desugar(I.fromIntegerOp), I.value};
            else
                return I.value;
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
            return *i;
        }
        else
            std::abort();
    }

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
