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



int find_object(const vector<expression_ref>& v, const expression_ref& E)
{
    for(int i=0;i<v.size();i++)
	if (E == v[i])
	    return i;
    return -1;
}

bool is_irrefutable_pattern(const expression_ref& E)
{
    return E.is_a<var>();
}

/// Is this either (a) irrefutable, (b) a constant, or (c) a constructor whose arguments are irrefutable patterns?
bool is_simple_pattern(const expression_ref& E)
{
    // (a) Is this irrefutable?
    if (is_irrefutable_pattern(E)) return true;

    // (b) Is this a constant with no arguments? (This can't be an irrefutable pattern, since we've already bailed on variables.)
    if (not E.size()) return true;

    assert(E.head().is_a<constructor>());

    // Arguments of multi-arg constructors must all be irrefutable patterns
    for(int j=0;j<E.size();j++)
	if (not is_irrefutable_pattern(E.sub()[j]))
	    return false;

    // (c) Is this a constructor who arguments are irrefutable patterns?
    return true;
}


#include "computation/expression/substitute.H"

// FIXME: we perform 3 case operations in the case of zip x:xs [] because we create an 'otherwise' let-var that
//        performs a case on y:ys that has already been done.

/*
 * case (x[0],..,x[N-1]) of (p[0...M-1][0...N-1] -> b[0..M-1])
 *
 * 1. Categorize each rule according to the type of its top-level pattern.
 * 2. Substitute for the irrefutable rules to find the 'otherwise' branch.
 * 3. Find the bodies for what happens after we match the various constants.
 *
 * If the otherwise branch is used twice, then construct a let-expression for it.
 *
 */
expression_ref block_case(const vector<expression_ref>& x, const vector<vector<expression_ref>>& p, const vector<expression_ref>& b)
{
    const int N = x.size();
    const int M = p.size();

    assert(p.size() == b.size());

    // Each pattern must have N components.
    for(int j=0;j<M;j++)
	assert(p[j].size() == N);

    if (not x.size())
	return b[0];

    // 1. Categorize each rule according to the type of its top-level pattern
    vector<expression_ref> constants;
    vector< vector<int> > rules;
    vector<int> irrefutable_rules;
    for(int j=0;j<M;j++)
    {
	if (is_var(p[j][0]))
	{
	    irrefutable_rules.push_back(j);
	    continue;
	}

	expression_ref C = p[j][0].head();
	int which = find_object(constants, C);

	if (which == -1)
	{
	    which = constants.size();
	    constants.push_back(C);
	    rules.push_back(vector<int>{});
	}

	rules[which].push_back(j);
    }

    // 2. Substitute for the irrefutable rules to find the 'otherwise' branch
    // This is substitute(x[1],p[2..m][1], case x2...xN of p[2..M][i] -> b[2..M] )
    expression_ref otherwise;
    if (irrefutable_rules.empty())
	; // otherwise = NULL
    else
    {
	vector<expression_ref> x2 = x;
	x2.erase(x2.begin());

	vector<vector<expression_ref>> p2;
	vector<expression_ref> b2;
	for(int i=0;i<irrefutable_rules.size();i++)
	{
	    int r = irrefutable_rules[i];
	    p2.push_back(p[r]);
	    p2.back().erase(p2.back().begin());

	    b2.push_back(b[r]);

	    if (is_wildcard(p[r][0]))
		// This is a var.
		; //assert(d->name.size() == 0);
	    else
	    {
		// FIXME! What if x[0] isn't a var?
		// Then if *d occurs twice, then we should use a let expression, right?
		b2[i] = substitute(b2[i], p[r][0].as_<var>(), x[0]);
	    }
	}
      
	if (x2.empty())
	{
	    // If (b2.size() > 1) then we have duplicate irrefutable rules, but that's OK.
	    // This can even be generated in the process of simplifying block_case expressions.	
	    otherwise = b2[0];
	}
	else
	    otherwise = block_case(x2, p2, b2);
    }
      
    // If there are no conditions on x[0], then we are done.
    if (constants.empty())
    {
	assert(otherwise);
	return otherwise;
    }

    // Find the first safe var index
    std::set<var> free;

    for(int i=0;i<x.size();i++)
	add(free, get_free_indices(x[i]));

    for(int i=0;i<p.size();i++)
    {
	add(free, get_free_indices(b[i]));
  
	for(int j=0; j<p[i].size(); j++)
	    add(free, get_free_indices(p[i][j]));
    }
  
    int var_index = 0;
    if (not free.empty()) var_index = max_index(free)+1;

    // WHEN should we put the otherwise expression into a LET variable?
    expression_ref O;
    if (otherwise) O = var(var_index++);

    // 3. Find the modified bodies for the various constants
    vector<expression_ref> simple_patterns;
    vector<expression_ref> simple_bodies;
    bool all_simple_followed_by_irrefutable = true;

    for(int c=0;c<constants.size();c++)
    {
	// Find the arity of the constructor
	int arity = 0;
	if (constants[c].head().is_a<constructor>())
	    arity = constants[c].head().as_<constructor>().n_args();

	// Construct the simple pattern for constant C
	expression_ref H = constants[c];

	vector<expression_ref> S(arity);
	for(int j=0;j<arity;j++)
	    S[j] = var(var_index+j);

	int r0 = rules[c][0];

	simple_patterns.push_back(expression_ref{H,S});
	simple_bodies.push_back({});
    
	// Construct the objects for the sub-case expression: x2[i] = v1...v[arity], x[2]...x[N]
	vector<expression_ref> x2;
	for(int j=0;j<arity;j++)
	    x2.push_back(S[j]);
	x2.insert(x2.end(), x.begin()+1, x.end());

	// Are all refutable patterns on x[1] simple and followed by irrefutable patterns on x[2]...x[N]?
	bool future_patterns_all_irrefutable = true;

	// Construct the various modified bodies and patterns
	vector<expression_ref> b2;
	vector<vector<expression_ref> > p2;
	for(int i=0;i<rules[c].size();i++)
	{
	    int r = rules[c][i];

	    // Add the pattern
	    p2.push_back(vector<expression_ref>{});
	    assert(p[r][0].size() == arity);

	    // Add sub-patterns of p[r][1]
	    for(int k=0;k<arity;k++)
		p2.back().push_back(p[r][0].sub()[k]);

	    p2.back().insert(p2.back().end(), p[r].begin()+1, p[r].end());

	    // Add the body
	    b2.push_back(b[r]);

	    // Check if p2[i] are all irrefutable
	    for(int i=0;i<p2.back().size();i++)
		if (not is_irrefutable_pattern(p2.back()[i]))
		{
		    future_patterns_all_irrefutable = false;
		    all_simple_followed_by_irrefutable = false;
		}
	}

	// If x[1] matches a simple pattern in the only alternative, we may as well
	// not change the variable names for the match slots in this pattern.
	if (rules[c].size() == 1 and is_simple_pattern(p[r0][0]))
	{
	    simple_patterns.back() = p[r0][0];

	    // case x[1] of p[r0][1] -> case (x[2],..,x[N]) of (p[r0][2]....p[r0][N]) -> b[r0]
	    x2 = x;
	    x2.erase(x2.begin());

	    p2.back() = p[r0];
	    p2.back().erase( p2.back().begin() );
	}

	// If all future patterns are irrefutable, then we won't need to backtrack to the otherwise case.
	if (future_patterns_all_irrefutable)
	{
	    // There can be only one alternative.
	    assert(rules[c].size() == 1);

	    if (x2.size())
		simple_bodies.back() = block_case(x2, p2, b2);
	    else
		simple_bodies.back() = b[r0];
	}
	else
	{
	    if (otherwise)
	    {
		p2.push_back(vector<expression_ref>(x2.size(), var(-1)));
		// Since we could backtrack, use the var.  It will point to otherwise
		b2.push_back(O);
	    }
	    simple_bodies.back() = block_case(x2, p2, b2);
	}
    }

    if (otherwise)
    {
	simple_patterns.push_back(var(-1));
	// If we have any backtracking, then use the otherwise var, like the bodies.
	if (not all_simple_followed_by_irrefutable)
	    simple_bodies.push_back(O);
	else
	    simple_bodies.push_back(otherwise);
    }

    // Construct final case expression
    expression_ref CE = make_case_expression(x[0], simple_patterns, simple_bodies);

    if (otherwise and not all_simple_followed_by_irrefutable)
	CE = let_expression({{O.as_<var>(), otherwise}}, CE);

    return CE;
}

// Create the expression 'case T of {patterns[i] -> bodies[i]}'
// Create the expression 'case (T) of {(patterns[i]) -> bodies[i]}'
expression_ref case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
    vector<vector<expression_ref>> multi_patterns;
    for(const auto& p:patterns)
	multi_patterns.push_back({p});
    return block_case({T}, multi_patterns, bodies);
}

expression_ref case_expression(const expression_ref& T, const expression_ref& pattern, const expression_ref& body, const expression_ref& otherwise)
{
    vector<expression_ref> patterns = {pattern};
    vector<expression_ref> bodies = {body};
    if (otherwise and not pattern.is_a<var>())
    {
	patterns.push_back(var(-1));
	bodies.push_back(otherwise);
    }
    return case_expression(T,patterns, bodies);
}

expression_ref def_function(const vector< vector<expression_ref> >& patterns, const vector<expression_ref>& bodies)
{
    // Find the first safe var index
    std::set<var> free;

    for(int i=0;i<patterns.size();i++)
    {
	add(free, get_free_indices(bodies[i]));
  
	for(int j=0; j<patterns[i].size(); j++)
	    add(free, get_free_indices(patterns[i][j]));
    }
  
    int var_index = 0;
    if (not free.empty()) var_index = max_index(free)+1;

    // All versions of the function must have the same arity
    assert(patterns.size());
    for(int i=1;i<patterns.size();i++)
	assert(patterns[0].size() == patterns[i].size());

    // Construct the dummies
    vector<expression_ref> args;
    for(int i=0;i<patterns[0].size();i++)
	args.push_back(var(var_index+i));
    
    // Construct the case expression
    expression_ref E = block_case(args, patterns, bodies);

    // Turn it into a function
    for(int i=patterns[0].size()-1;i>=0;i--)
	E = lambda_quantify(var_index+i, E);

    return E;
}


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

struct desugar_state
{
    const Module& m;

    int var_index;

    var get_fresh_wildcard() { return var(-var_index++);}
    var get_fresh_var() { return var(var_index++);}
    var get_fresh_var(const string& name) {return var(name,var_index++);}

    vector<expression_ref> parse_fundecls(const vector<expression_ref>& v);
    expression_ref desugar(const expression_ref& E);

    desugar_state(const Module& m_)
	:m(m_),var_index(1+max_index(m.topdecls))
    {}
};

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

	// If its just a variable with no args, don't call def_function because ... its complicated?
	if (lhs.is_a<var>())
	{
	    assert(is_AST(decl.sub()[1],"rhs"));
	    decls.push_back(decl.head() + lhs + decl.sub()[1].sub()[0]);
	    continue;
	}

	auto& f = lhs.head();

	// Skip pattern bindings
	if (f.is_a<constructor>())
	{
	    auto& pat = lhs;
	    auto z = get_fresh_var();

	    assert(is_AST(decl.sub()[1],"rhs"));
	    decls.push_back(decl.head()+z+decl.sub()[1].sub()[0]);
	    // Probably we shouldn't have desugared the RHS yet.
	    for(auto& x: get_free_indices(pat))
		decls.push_back(decl.head()+x+case_expression(z,pat,x,{var("Compiler.Base.error"),"Failed pattern match"}));
	    continue;
	}

	vector<vector<expression_ref> > patterns;
	vector<expression_ref> bodies;
	auto fvar = f.as_<var>();
	patterns.push_back( get_patterns(decl) );
	bodies.push_back( get_body(decl) );

	for(int j=i+1;j<v.size();j++)
	{
	    if (not is_AST(v[j],"Decl")) break;
	    auto& j_lhs = v[j].sub()[0];
	    auto& j_f   = j_lhs.head();
	    if (j_f.is_a<constructor>()) break;

	    if (j_f.as_<var>() != fvar) break;

	    patterns.push_back( get_patterns(v[j]) );
	    bodies.push_back( get_body(v[j]) );

	    if (patterns.back().size() != patterns.front().size())
		throw myexception()<<"Function '"<<fvar<<"' has different numbers of arguments!";
	}
	decls.push_back(AST_node("Decl") + fvar + def_function(patterns,bodies) );

	// skip the other bindings for this function
	i += (patterns.size()-1);
    }
    return decls;
}

expression_ref desugar_state::desugar(const expression_ref& E)
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
		e = desugar(e);

	    // Convert fundecls to normal decls
	    vector<expression_ref> decls = parse_fundecls(v);

	    return expression_ref{E.head(),decls};
	}
	else if (n.type == "Decl")
	{
            // FIXME: don't desugar a Decl except from Decls
            // Pattern bindings should be processed before we get here!
            //
            // auto& lhs = E.sub()[0];
            // assert(not lhs.head().is_a<constructor>());

            // Replace bound vars in (a) the patterns and (b) the body
	    for(auto& e: v)
		e = desugar(e);

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
		return desugar(E2);
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
	    // FIXME: Try to preserve argument names (in block_case( ), probably) when they are irrefutable apat_var's.

	    // 1. Extract the lambda body
	    expression_ref body = v.back();
	    v.pop_back();

	    // 2. Desugar the body, binding vars mentioned in the lambda patterns.
	    body = desugar(body);

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
	    else if (is_AST(first,"EmptyStmt"))
		result = do_stmts;
	    else
		std::abort();

	    return desugar(result);
	}
	else if (n.type == "If")
	{
	    for(auto& e: v)
		e = desugar(e);

	    return case_expression(v[0],true,v[1],v[2]);
	}
	else if (n.type == "LeftSection")
	{
	    return apply_expression(v[1],v[0]);
	}
	else if (n.type == "RightSection")
	{
	    auto x = get_fresh_var();
	    return lambda_quantify(x,apply_expression(apply_expression(v[0],x),v[1]));
	}
	else if (n.type == "Let")
	{
	    expression_ref decls_ = v[0];
	    assert(is_AST(decls_,"Decls"));
	    expression_ref body = v[1];

	    // parse the decls and bind declared names internally to the decls.
	    v[0] = desugar(v[0]);

	    vector<pair<var,expression_ref>> decls;

	    // find the bound vars + construct arguments to let_obj()
	    for(const auto& decl: v[0].sub())
	    {
		if (is_AST(decl,"EmptyDecl")) continue;

		var x = decl.sub()[0].as_<var>();
		auto E = decl.sub()[1];

		decls.push_back({x,E});
	    }

	    // finally desugar let-body, now that we know the bound vars.
	    body = desugar(body);

	    // construct the new let expression.
	    return let_expression(decls, body);
	}
	else if (n.type == "Case")
	{
	    expression_ref case_obj = desugar(v[0]);
	    vector<expression_ref> alts = v[1].sub();
	    vector<expression_ref> patterns;
	    vector<expression_ref> bodies;
	    for(const auto& alt: alts)
	    {
		patterns.push_back(desugar(alt.sub()[0]) );

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

		bodies.push_back(desugar(body) );
	    }
	    return case_expression(case_obj, patterns, bodies);
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
