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

expression_ref get_body(const expression_ref& decl)
{
    expression_ref rhs = decl.sub()[1];
    assert(is_AST(rhs,"rhs"));
    assert(rhs.size() == 1);
    return rhs.sub()[0];
}

vector<expression_ref> parse_fundecls(const vector<expression_ref>& v)
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

	// If its just a variable with no args, don't call def_function because ... its complicated?
	if (lhs.is_a<var>())
	{
	    decls.push_back(decl.head() + lhs + decl.sub()[1].sub()[0]);
	    continue;
	}

	auto& f = lhs.head();

	// Skip pattern bindings
	if (f.is_a<constructor>())
	{
	    decls.push_back(decl);
	    continue;
	}

	vector<vector<expression_ref> > patterns;
	vector<expression_ref> bodies;
	string name = f.as_<var>().name;
	patterns.push_back( get_patterns(decl) );
	bodies.push_back( get_body(decl) );

	for(int j=i+1;j<v.size();j++)
	{
	    if (not is_AST(v[j],"Decl")) break;
	    auto& j_lhs = v[j].sub()[0];
	    auto& j_f   = j_lhs.head();
	    if (j_f.is_a<constructor>()) break;

	    if (j_f.as_<var>().name != name) break;

	    patterns.push_back( get_patterns(v[j]) );
	    bodies.push_back( get_body(v[j]) );

	    if (patterns.back().size() != patterns.front().size())
		throw myexception()<<"Function '"<<name<<"' has different numbers of arguments!";
	}
	decls.push_back(AST_node("Decl") + var(name) + def_function(patterns,bodies) );

	// skip the other bindings for this function
	i += (patterns.size()-1);
    }
    return decls;
}

expression_ref get_fresh_id(const string& s, const expression_ref& /* E */)
{
    return var(s);
}

expression_ref desugar(const Module& m, const expression_ref& E)
{
    vector<expression_ref> v;
    if (E.is_expression())
	v = E.sub();

    if (E.head().is_a<AST_node>())
    {
	auto& n = E.head().as_<AST_node>();
	if (n.type == "infixexp")
	    std::abort();
	else if (n.type == "Decls" or n.type == "TopDecls")
	{
	    // translate each individual decl
	    for(auto& e: v)
		e = desugar(m, e);

	    // Convert fundecls to normal decls
	    vector<expression_ref> decls = parse_fundecls(v);

	    return expression_ref{E.head(),decls};
	}
	else if (n.type == "Decl")
	{
	    auto& lhs = E.sub()[0];
	    auto& f = lhs.head();
	    if (f.is_a<constructor>())
	    {
		// Pattern x y z = E
		//      to
		// fresh = E
		// x = case fresh of Pattern x y z
		// y = case fresh of Pattern x y z
		// z = case fresh of Pattern x y z
	    }

            // Replace bound vars in (a) the patterns and (b) the body
	    for(auto& e: v)
		e = desugar(m, e);

	    return expression_ref{E.head(),v};
	}
	else if (n.type == "rhs")
	{
	    if (E.size() == 2) // where decls
	    {
		expression_ref decls = E.sub()[1];
		assert(is_AST(decls,"Decls"));
		expression_ref E2 = AST_node("Let") + decls + E.sub()[0];
		E2 = AST_node("rhs") + E2;
		return desugar(m,E2);
	    }
	    else
	    { }      // Fall through and let the standard case handle this.
	}
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
			expression_ref ok = get_fresh_id("ok",E);
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
	    return desugar(m,E2);
	}
	else if (n.type == "Lambda")
	{
	    // FIXME: Try to preserve argument names (in block_case( ), probably) when they are irrefutable apat_var's.

	    // 1. Extract the lambda body
	    expression_ref body = v.back();
	    v.pop_back();

	    // 2. Desugar the body, binding vars mentioned in the lambda patterns.
	    body = desugar(m, body);

	    return def_function({v},{body}); 
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
		return desugar(m,stmt.sub()[0]);
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
		    expression_ref ok = get_fresh_id("ok",E);
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
	    else if (is_AST(first,"EmptyStmt"))
		result = do_stmts;
	    else
		std::abort();

	    return desugar(m,result);
	}
	else if (n.type == "If")
	{
	    for(auto& e: v)
		e = desugar(m, e);

	    return case_expression(v[0],true,v[1],v[2]);
	}
	else if (n.type == "LeftSection")
	{
	    // FIXME... the infixexp needs to parse the same as if it was parenthesized.
	    // FIXME... probably we need to do a disambiguation on the infix expression. (infixexp op x)
	    std::set<var> free_vars;
	    for(auto& e: v) {
		e = desugar(m, e);
		add(free_vars, get_free_indices(e));
	    }
	    return apply_expression(v[1],v[0]);
	}
	else if (n.type == "RightSection")
	{
	    // FIXME... probably we need to do a disambiguation on the infix expression. (x op infixexp)
	    // FIXME... the infixexp needs to parse the same as if it was parenthesized.
	    std::set<var> free_vars;
	    for(auto& e: v) {
		e = desugar(m, e);
		add(free_vars, get_free_indices(e));
	    }
	    int safe_var_index = 0;
	    if (not free_vars.empty())
		safe_var_index = max_index(free_vars)+1;
	    var vsafe(safe_var_index);
	    return lambda_quantify(vsafe,apply_expression(apply_expression(v[0],vsafe),v[1]));
	}
	else if (n.type == "Let")
	{
	    expression_ref decls_ = v[0];
	    assert(is_AST(decls_,"Decls"));
	    expression_ref body = v[1];

	    // transform "let (a,b) = E in F" => "case E of (a,b) -> F"
	    {
		expression_ref decl = decls_.sub()[0];
		if (is_AST(decl,"Decl"))
		{
		    expression_ref pat = decl.sub()[0];
		    expression_ref rhs = decl.sub()[1];
		    // let pat = rhs in body -> case rhs of {pat->body}
		    if (pat.head().is_a<constructor>())
		    {
			std::abort();
			assert(is_AST(rhs,"rhs"));
			expression_ref pat0 = pat.sub()[0];
			if (is_AST(pat0,"Tuple"))
			{
			    if (decls_.size() != 1) throw myexception()<<"Can't currently handle pattern let with more than one decl.";
			    expression_ref alt = AST_node("alt") + pat + body;
			    expression_ref alts = AST_node("alts") + alt;
			    expression_ref EE = AST_node("Case") + rhs.sub()[0] + alts;
			    return desugar(m, EE);
			}
		    }
		}
	    }

	    // parse the decls and bind declared names internally to the decls.
	    v[0] = desugar(m, v[0]);

	    vector<pair<var,expression_ref>> decls;

	    // find the bound var names + construct arguments to let_obj()
	    for(const auto& decl: v[0].sub())
	    {
		if (is_AST(decl,"EmptyDecl")) continue;

		var x = decl.sub()[0].as_<var>().name;
		auto E = decl.sub()[1];

		decls.push_back({x,E});
	    }

	    // finally desugar let-body, now that we know the bound vars.
	    body = desugar(m, body);

	    // construct the new let expression.
	    return let_expression(decls, body);
	}
	else if (n.type == "Case")
	{
	    expression_ref case_obj = desugar(m, v[0]);
	    vector<expression_ref> alts = v[1].sub();
	    vector<expression_ref> patterns;
	    vector<expression_ref> bodies;
	    for(const auto& alt: alts)
	    {
		patterns.push_back(desugar(m, alt.sub()[0]) );

		// Handle where-clause.
		assert(alt.size() == 2 or alt.size() == 3);
		expression_ref body = alt.sub()[1];

		if (is_AST(body,"GdPat"))
		    throw myexception()<<"Guard patterns not yet implemented!";

		if (alt.size() == 3)
		{
		    assert(is_AST(alt.sub()[2],"Decls"));
		    body = AST_node("Let") + alt.sub()[2] + body;
		}

		bodies.push_back(desugar(m, body) );
	    }
	    return case_expression(case_obj, patterns, bodies);
	}
	else if (n.type == "enumFrom")
	{
	    expression_ref E2 = var("Prelude.enumFrom");
	    for(auto& e: v)
		E2 = {E2, e};
	    return desugar(m, E2);
	}
	else if (n.type == "enumFromTo")
	{
	    expression_ref E2 = var("Prelude.enumFromTo");
	    for(auto& e: v)
		E2 = {E2, e};
	    return desugar(m, E2);
	}
    }

    for(auto& e: v)
	e = desugar(m, e);
    if (E.size())
	return expression_ref{E.head(),v};
    else
	return E;
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
