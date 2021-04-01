#include "computation/module.H"
#include <deque>
#include <set>
#include <tuple>
#include <utility>
#include "util/io.H"
#include "models/parameters.H"
#include "computation/loader.H"
#include "computation/expression/AST_node.H"
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
#include "computation/parser/haskell.H"

using std::string;
using std::vector;
using std::set;
using std::deque;
using std::pair;

//  -----Prelude: http://www.haskell.org/onlinereport/standard-prelude.html

// See list in computation/loader.C
//

int max_index(const expression_ref& x)
{
    int index = -1;
    if (x)
    {
	if (x.head().is_a<var>())
	    index =  std::max(index,x.head().as_<var>().index);

	if (x.size())
	    for(auto& e: x.sub())
		index = std::max(max_index(e),index);
    }
    return index;
}

desugar_state::desugar_state(const Module& m_)
    :fresh_vars(1+max_index(m_.topdecls)), m(m_)
{}

bool is_irrefutable_pat(const expression_ref& E)
{
    return E.head().is_a<var>();
}

vector<expression_ref> get_patterns(const expression_ref& decl)
{
    assert(is_AST(decl,"Decl"));

    expression_ref lhs = decl.sub()[0];
    assert(lhs.head().is_a<var>());

    return lhs.sub();
}

failable_expression get_body(const expression_ref& decl)
{
    auto& rhs = decl.sub()[1];
    return rhs.as_<failable_expression>();
}


failable_expression desugar_state::desugar_gdrh(const expression_ref& E)
{
    assert(is_AST(E,"gdrh") or is_AST(E,"gdpat"));

    auto& guards = E.sub()[0];
    auto& body   = E.sub()[1];

    assert(is_AST(guards,"guards"));

    auto F = failable_expression(desugar(body));

    for(auto& guard: std::reverse(guards.sub()))
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
	    auto binds = desugar_decls(LQ.binds);

	    F.add_binding(binds);
	}
        else if (guard.is_a<Haskell::PatQual>())
        {
            auto &PQ = guard.as_<Haskell::PatQual>();
            auto &pat = PQ.bindpat;
            auto E = desugar(PQ.exp);

            F = case_expression(E,{pat},{F});
        }
	else
	    std::abort();
    }

    return F;
}


vector<expression_ref> desugar_state::parse_fundecls(const vector<expression_ref>& v)
{
    // Now we go through and translate groups of FunDecls.
    vector<expression_ref> decls;
    for(int i=0;i<v.size();i++)
    {
	auto& decl = v[i];

	// This is a declaration, but not a type we're handling here.
	if (not is_AST(decl,"Decl"))
	{
	    decls.push_back( decl );
	    continue;
	}

	auto lhs = desugar(decl.sub()[0]);
	auto& rhs = decl.sub()[1];
	auto& rhs_fail = rhs.as_<failable_expression>();

        // If its just a variable with no args, don't call def_function because ... its complicated?
	if (lhs.is_a<var>())
	{
	    assert(not rhs_fail.can_fail);
	    decls.push_back(decl.head() + lhs + rhs_fail.result(0));
	    continue;
	}

	auto& f = lhs.head();

	// Skip pattern bindings
	if (f.is_a<constructor>())
	{
	    auto& pat = lhs;
	    auto z = get_fresh_var();

	    assert(not rhs_fail.can_fail);
	    decls.push_back(decl.head()+z+rhs_fail.result(0));
	    // Probably we shouldn't have desugared the RHS yet. (?)
	    for(auto& x: get_free_indices(pat))
		decls.push_back(decl.head()+x+case_expression(z, {pat}, {failable_expression(x)}).result(error("pattern binding: failed pattern match")));
	    continue;
	}

	auto fvar = f.as_<var>();

	vector<equation_info_t> equations;
	equations.push_back({ get_patterns(decl), get_body(decl) });

	for(int j=i+1;j<v.size();j++)
	{
	    if (not is_AST(v[j],"Decl")) break;
	    auto& j_lhs = v[j].sub()[0];
	    auto& j_f   = j_lhs.head();
	    if (j_f.is_a<constructor>()) break;

	    if (j_f.as_<var>() != fvar) break;

	    equations.push_back({ get_patterns(v[j]), get_body(v[j])});

	    if (equations.back().patterns.size() != equations.front().patterns.size())
		throw myexception()<<"Function '"<<fvar<<"' has different numbers of arguments!";
	}
	auto otherwise = error(fvar.name+": pattern match failure");
	decls.push_back(AST_node("Decl") + fvar + def_function(equations, otherwise) );

	// skip the other bindings for this function
	i += (equations.size()-1);
    }
    return decls;
}

CDecls desugar_state::desugar_decls(const expression_ref& E)
{
    assert(is_AST(E,"Decls"));

    auto E2 = desugar(E);

    CDecls decls;
    for(const auto& decl: E2.sub())
    {
	assert(is_AST(decl,"Decl"));

	var x = decl.sub()[0].as_<var>();
	auto F = decl.sub()[1];

	decls.push_back({x,F});
    }

    return decls;
}

failable_expression desugar_state::desugar_rhs(const expression_ref& E)
{
    // FIXME - the fact that we are duplicating the add_binding( )
    //         suggests that the binding should be on another level of the AST
    //         with an optional Decls
    //       - if we do this we would put back the ralt rule in the parser
    //         and extract the (ralt: -> exp| gdpats) rule from alt_rhs again.

    if (is_AST(E,"rhs"))
    {
	auto rhs = failable_expression(desugar(E.sub()[0]));
	if (E.size() == 2)
	    rhs.add_binding(desugar_decls(E.sub()[1]));
	return rhs;
    }
    else if (is_AST(E,"gdrhs"))
    {
	auto& guards = E.sub()[0];
	vector<failable_expression> gdrhs;
	for(auto& guard: guards.sub())
	    gdrhs.push_back(desugar_gdrh(guard));

	auto rhs = fold(gdrhs);

	if (E.size() == 2)
	    rhs.add_binding(desugar_decls(E.sub()[1]));
	return rhs;
    }
    else
	std::abort();
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
        C.decls.obj = desugar(C.decls.obj);
        return C;
    }
    else if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& element: L.elements)
            element = desugar(element);
        return get_list(L.elements);
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto T = E.as_<Haskell::Tuple>();
        for(auto& element: T.elements)
            element = desugar(element);
        return get_tuple(T.elements);
    }
    else if (E.is_a<Located<Hs::ID>>())
        std::abort();
    else if (E.is_a<Haskell::WildcardPattern>())
        return var(-1);
    else if (E.is_a<Haskell::AsPattern>())
    {
        auto& AP = E.as_<Haskell::AsPattern>();
        return Haskell::AsPattern(AP.var, desugar(AP.pattern));
    }
    else if (E.is_a<Haskell::LazyPattern>())
    {
        auto LP = E.as_<Haskell::LazyPattern>();
        return Haskell::LazyPattern(desugar(LP.pattern));
    }
    else if (E.is_a<Haskell::InstanceDecl>())
    {
        auto I = E.as_<Haskell::InstanceDecl>();
        I.decls.obj = desugar(I.decls.obj);
        return I;
    }
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
            expression_ref p = PQ.bindpat;
            expression_ref e = PQ.exp;
            expression_ref qop = var("Compiler.Base.>>=");

            if (is_irrefutable_pat(p))
            {
                expression_ref lambda = AST_node("Lambda") + p + do_stmts;
                result = {qop,e,lambda};
            }
            else
            {
                expression_ref ok = get_fresh_var("ok");
                expression_ref lhs1 = ok + p;
                expression_ref rhs1 = AST_node("rhs") + do_stmts;
                expression_ref decl1 = AST_node("Decl") + lhs1 + rhs1;

                expression_ref fail = {var("Compiler.Base.fail"),"Fail!"};
                expression_ref lhs2 = ok + var(-1);
                expression_ref rhs2 = AST_node("rhs") + fail;
                expression_ref decl2 = AST_node("Decl") + lhs2 + rhs2;

                expression_ref decls = AST_node("Decls") + decl1 +  decl2;

                expression_ref body = {qop,e,ok};

                result = AST_node("Let") + decls + body;
            }
        }
        // do {let decls ; rest} = let decls in do {stmts}
        else if (first.is_a<Haskell::LetQual>())
        {
            auto& LQ = first.as_<Haskell::LetQual>();
            result = AST_node("Let") + LQ.binds + do_stmts;
        }
        else
            std::abort();

        return desugar(result);
    }

    vector<expression_ref> v = E.copy_sub();

    if (E.head().is_a<AST_node>())
    {
	auto& n = E.head().as_<AST_node>();
	if (n.type == "infixexp")
	    std::abort();
	else if (n.type == "Decls" or n.type == "TopDecls")
	{
	    // translate each individual decl
	    for(auto& e: v)
		e = desugar(e);

	    // Convert fundecls to normal decls
	    vector<expression_ref> decls = parse_fundecls(v);

	    return expression_ref{E.head(),decls};
	}
	else if (n.type == "Decl")
	{

            v[0] = desugar(v[0]);      // Desugar list and tuple patterns to constructors.
	    v[1] = desugar_rhs(v[1]);
            // FIXME: don't desugar a Decl except from Decls
            // Pattern bindings should be processed before we get here!
            //
            // auto& lhs = E.sub()[0];
            // assert(not lhs.head().is_a<constructor>());

	    return expression_ref{E.head(),std::move(v)};
	}
	else if (n.type == "rhs")
	    std::abort();
	else if (n.type == "gdrhs")
	    std::abort();
	else if (n.type == "ListComprehension")
	{
	    expression_ref E2 = E;
	    // [ e | True   ]  =  [ e ]
	    // [ e | q      ]  =  [ e | q, True ]
	    // [ e | b, Q   ]  =  if b then [ e | Q ] else []
	    // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in concatMap ok l
	    // [ e | let decls, Q] = let decls in [ e | Q ]

	    assert(v.size() >= 2);
	    if (v.size() == 2 and v[0].is_a<Hs::SimpleQual>() and v[0].as_<Hs::SimpleQual>().exp == bool_true)
		E2 = {var(":"),v[1],var("[]")};
	    else if (v.size() == 2)
		E2 = E.head() + v[0] + Hs::SimpleQual(bool_true) + v[1];
	    else
	    {
		// Pop the next qual from the FRONT of the list
		expression_ref B = v[0];
		v.erase(v.begin());
		E2 = expression_ref{E.head(),v};

		if (B.is_a<Hs::SimpleQual>())
                {
                    auto cond = B.as_<Hs::SimpleQual>().exp;
		    E2 = AST_node("If") + cond + E2 + var("[]");
                }
		else if (B.is_a<Haskell::PatQual>())
		{
                    auto& PQ = B.as_<Haskell::PatQual>();
		    expression_ref p = PQ.bindpat;
		    expression_ref l = PQ.exp;
		    if (is_irrefutable_pat(p))
		    {
			expression_ref f  = AST_node("Lambda") + p + E2;
			E2 = {var("Data.List.concatMap"),f,l};
		    }
		    else
		    {
			// Problem: "ok" needs to be a fresh variable.
			expression_ref ok = get_fresh_var("ok");
			expression_ref lhs1 = ok + p;
			expression_ref rhs1 = AST_node("rhs") + E2;
			expression_ref decl1 = AST_node("Decl") + lhs1 + rhs1;

			expression_ref lhs2 = ok + var(-1);
			expression_ref rhs2 = AST_node("rhs") + var("[]");
			expression_ref decl2 = AST_node("Decl") + lhs2 + rhs2;

			expression_ref decls = AST_node("Decls") + decl1 + decl2;
			expression_ref body = {var("Data.List.concatMap"),ok,l};

			E2 = AST_node("Let") + decls + body;
		    }
		}
		else if (B.is_a<Haskell::LetQual>())
                {
                    auto& LQ = B.as_<Haskell::LetQual>();
		    E2 = AST_node("Let") + LQ.binds + E2;
                }
		else
		    std::abort();
	    }
	    return desugar(E2);
	}
	else if (n.type == "Lambda")
	{
	    // 1. Extract the lambda body
	    expression_ref body = v.back();
	    v.pop_back();

            vector<expression_ref> args = v;
            for(auto& arg: args)
                arg = desugar(arg);

	    // 2. Desugar the body, binding vars mentioned in the lambda patterns.
	    body = desugar(body);

	    return def_function({{args,failable_expression(body)}}, error("lambda: pattern match failure"));
	}
	else if (n.type == "Do")
	{
            std::abort();
	}
	else if (n.type == "If")
	{
	    for(auto& e: v)
		e = desugar(e);

	    return case_expression(v[0],{true},{failable_expression(v[1])}).result(v[2]);
	}
	else if (n.type == "LeftSection")
	{
	    for(auto& e: v)
		e = desugar(e);

	    return apply_expression(v[1],v[0]);
	}
	else if (n.type == "RightSection")
	{
	    for(auto& e: v)
		e = desugar(e);

	    auto x = get_fresh_var();
	    return lambda_quantify(x,apply_expression(apply_expression(v[0],x),v[1]));
	}
	else if (n.type == "Let")
	{
	    CDecls decls = desugar_decls(v[0]);
	    auto body = desugar(v[1]);

	    // construct the new let expression.
	    return let_expression(decls, body);
	}
	else if (n.type == "Case")
	{
	    expression_ref obj = desugar(v[0]);
	    assert(v[1].is_a<Haskell::Alts>());

	    auto alts = v[1].as_<Haskell::Alts>().alts;
	    vector<expression_ref> patterns;
	    vector<failable_expression> bodies;
	    for(const auto& alt: alts)
	    {
		patterns.push_back( desugar(alt.pattern) );
		bodies.push_back( desugar_rhs(alt.rhs) );
	    }
	    return case_expression(obj, patterns, bodies).result(error("case: failed pattern match"));
	}
	else if (n.type == "enumFrom")
	{
	    expression_ref E2 = var("Compiler.Enum.enumFrom");
	    for(auto& e: v)
		E2 = {E2, e};
	    return desugar(E2);
	}
	else if (n.type == "enumFromTo")
	{
	    expression_ref E2 = var("Compiler.Enum.enumFromTo");
	    for(auto& e: v)
		E2 = {E2, e};
	    return desugar(E2);
	}
	else if (n.type == "enumFromThen")
	{
	    expression_ref E2 = var("Compiler.Enum.enumFromThen");
	    for(auto& e: v)
		E2 = {E2, e};
	    return desugar(E2);
	}
	else if (n.type == "enumFromThenTo")
	{
	    expression_ref E2 = var("Compiler.Enum.enumFromThenTo");
	    for(auto& e: v)
		E2 = {E2, e};
	    return desugar(E2);
	}
    }

    for(auto& e: v)
	e = desugar(e);
    if (E.size())
	return expression_ref{E.head(),v};
    else
	return E;
}

expression_ref desugar(const Module& m, const expression_ref& E)
{
    desugar_state ds(m);
    return ds.desugar(E);
}

