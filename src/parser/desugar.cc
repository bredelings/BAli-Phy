#include "computation/module.H"
#include <deque>
#include <set>
#include <tuple>
#include <utility>
#include "io.H"
#include "models/parameters.H"
#include "computation/loader.H"
#include "computation/expression/expression.H"
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
#include "desugar.H"
#include "util/assert.hh"
#include "desugar-case.H"

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
	if (is_AST(guard,"SimpleQual"))
	{
	    auto condition = desugar(guard.sub()[0]);
	    // F' = case True of True -> F
	    if (is_bool_true(condition) or is_otherwise(condition))
		;
	    // F' = case condition of True -> F
	    else
		F = case_expression(condition,{true},{F});
	}
	else if (is_AST(guard,"LetQual"))
	{
	    auto& decls = guard.sub()[0];
	    auto binds = desugar_decls(decls);

	    F.add_binding(binds);
	}
	else if (is_AST(guard,"PatQual"))
	{
	    auto& pat = guard.sub()[0];
	    auto E = desugar(guard.sub()[1]);

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

	auto& lhs = decl.sub()[0];
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
	else if (n.type == "WildcardPattern")
	{
	    return var(-1);
	}
	else if (n.type == "id")
	    std::abort();
	else if (n.type == "ListComprehension")
	{
	    expression_ref E2 = E;
	    // [ e | True   ]  =  [ e ]
	    // [ e | q      ]  =  [ e | q, True ]
	    // [ e | b, Q   ]  =  if b then [ e | Q ] else []
	    // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in concatMap ok l
	    // [ e | let decls, Q] = let decls in [ e | Q ]

	    expression_ref True = AST_node("SimpleQual") + bool_true;

	    assert(v.size() >= 2);
	    if (v.size() == 2 and (v[0] == True))
		E2 = {var(":"),v[1],var("[]")};
	    else if (v.size() == 2)
		E2 = E.head() + v[0] + True + v[1];
	    else
	    {
		// Pop the next qual from the FRONT of the list
		expression_ref B = v[0];
		v.erase(v.begin());
		E2 = expression_ref{E.head(),v};

		if (is_AST(B, "SimpleQual"))
		    E2 = AST_node("If") + B.sub()[0] + E2 + var("[]");
		else if (is_AST(B, "PatQual"))
		{
		    expression_ref p = B.sub()[0];
		    expression_ref l = B.sub()[1];
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
		else if (is_AST(B, "LetQual"))
		    E2 = AST_node("Let") + B.sub()[0] + E2;
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

	    // 2. Desugar the body, binding vars mentioned in the lambda patterns.
	    body = desugar(body);

	    return def_function({{v,failable_expression(body)}}, error("lambda: pattern match failure"));
	}
	else if (n.type == "Do")
	{
	    auto& stmts = v;

	    if (stmts.empty())
		throw myexception()<<"Empty do block!";

	    if (not is_AST(stmts.back(), "SimpleQual"))
		throw myexception()<<"The last statement in a do block must be an expression!";

	    // do { e }  =>  e
	    if (stmts.size() == 1) {
		auto& stmt = stmts.front();
		assert(is_AST(stmt, "SimpleQual"));
		return desugar(stmt.sub()[0]);
	    }

	    auto first = stmts[0];
	    stmts.erase(stmts.begin());
	    expression_ref do_stmts = expression_ref{AST_node("Do"),stmts};
	    expression_ref result;
      
	    // do {e ; stmts }  =>  e >> do { stmts }
	    if (is_AST(first,"SimpleQual"))
	    {
		expression_ref e = first.sub()[0];
		result = {var("Compiler.Base.>>"), e, do_stmts};
	    }

	    // do { p <- e ; stmts} => let {ok p = do {stmts}; ok _ = fail "..."} in e >>= ok
	    // do { v <- e ; stmts} => e >>= (\v -> do {stmts})
	    else if (is_AST(first,"PatQual"))
	    {
		expression_ref p = first.sub()[0];
		expression_ref e = first.sub()[1];
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
	    else if (is_AST(first,"LetQual"))
	    {
		expression_ref decls = first.sub()[0];
		result = AST_node("Let") + decls + do_stmts;
	    }
	    else
		std::abort();

	    return desugar(result);
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
	    assert(is_AST(v[1],"alts"));
	    vector<expression_ref> alts = v[1].sub();
	    vector<expression_ref> patterns;
	    vector<failable_expression> bodies;
	    for(const auto& alt: alts)
	    {
		assert(is_AST(alt,"alt"));
		auto& pat = alt.sub()[0];
		auto& rhs = alt.sub()[1];

		patterns.push_back( pat );
		bodies.push_back( desugar_rhs(rhs) );
	    }
	    return case_expression(obj, patterns, bodies).result(error("case: failed pattern match"));
	}
	else if (n.type == "enumFrom")
	{
	    expression_ref E2 = var("Prelude.enumFrom");
	    for(auto& e: v)
		E2 = {E2, e};
	    return desugar(E2);
	}
	else if (n.type == "enumFromTo")
	{
	    expression_ref E2 = var("Prelude.enumFromTo");
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

bool is_all_space(const string& line)
{
    for(int i=0;i<line.size();i++)
	if (not ::isspace(line[i])) return false;
    return true;
}

Module read_model(const string& filename)
{
    // 1. Read module
    return module_loader({}).load_module_from_file(filename);
}

void read_add_model(Model& M, const std::string& filename)
{
    auto m = read_model(filename);
    M += m;
    add_model(M, m.name);
}

void add_model(Model& M, const std::string& name)
{
    M += name;
    string prefix = name;
    M.perform_expression({var("Distributions.do_log"),prefix,{var("Distributions.gen_model"),var(name+".main")}});
}
