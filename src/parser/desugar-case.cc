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
#include "desugar-case.H"
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

template <typename T>
vector<T> remove_first(const vector<T>& v1)
{
    vector<T> v2;
    for(int i=1;i<v1.size();i++)
	v2.push_back(v1[i]);
    return v2;

    /*
    vector<T> v2;
    v2.insert(v2.end(),v1.begin()+1,v1.end());
    return v2;
    */
}

template <typename T>
vector<T> remove_first(vector<T>&& v1)
{
    vector<T> v2;
    for(int i=1;i<v1.size();i++)
	v2.push_back(std::move(v1[i]));
    return v2;
}


// FIXME: we perform 3 case operations in the case of zip x:xs [] because we create an 'otherwise' let-var that
//        performs a case on y:ys that has already been done.


enum class pattern_type
{
    constructor,
    var,
    null
};

pattern_type classify_equation(const equation_info_t& equation)
{
    assert(equation.patterns.size());
    if (is_var(equation.patterns[0]))
	return pattern_type::var;
    else
	return pattern_type::constructor;
}

vector<pair<pattern_type,vector<equation_info_t>>> partition(const vector<equation_info_t>& equations)
{
    vector<pair<pattern_type,vector<equation_info_t>>> partitions;
    auto prev_type = pattern_type::null;
    for(auto& e: equations)
    {
	auto current_type = classify_equation(e);
	if (current_type == prev_type)
	    partitions.back().second.push_back(e);
	else
	    partitions.push_back({current_type,{e}});
	prev_type = current_type;
    }
    return partitions;
}

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
expression_ref desugar_state::match(const vector<expression_ref>& xs, const vector<vector<expression_ref>>& p, const vector<failable_expression>& b, const expression_ref& otherwise)
{
    assert(p.size() == b.size());

    vector<equation_info_t> equations;
    for(int i=0;i<p.size();i++)
	equations.push_back({p[i],b[i]});

    return match(xs, equations, otherwise);
}

expression_ref desugar_state::match_constant(const vector<expression_ref>& x, const vector<equation_info_t>& equations, expression_ref otherwise)
{
    const int N = x.size();
    const int M = equations.size();

#ifndef NDEBUG
    assert(N > 0);
    assert(M > 0);
    assert(equations[0].patterns.size());
    for(auto& eqn:equations)
	assert(not is_var(eqn.patterns[0]));
#endif

    // 1. Categorize each rule according to the type of its top-level pattern
    vector<expression_ref> constants;
    vector< vector<int> > rules;
    for(int j=0;j<M;j++)
    {
	assert(not is_var(equations[j].patterns[0]));

	expression_ref C = equations[j].patterns[0].head();
	int which = find_object(constants, C);

	if (which == -1)
	{
	    which = constants.size();
	    constants.push_back(C);
	    rules.push_back(vector<int>{});
	}

	rules[which].push_back(j);
    }

    // 2. Bind the otherwise branch to a let var.
    CDecls binds;
    {
	auto o = get_fresh_var();
	binds.push_back({o, otherwise});
	otherwise = o;
    }

    // 3. Find the alternatives in the simple case expression
    vector<expression_ref> simple_patterns;
    vector<expression_ref> simple_bodies;

    for(int c=0;c<constants.size();c++)
    {
	expression_ref C = constants[c];

	// 3.1 Find the arity of the constructor
	int arity = 0;
	if (C.is_a<constructor>())
	    arity = C.as_<constructor>().n_args();

	// 3.2 Construct the simple pattern for constant C
	vector<expression_ref> args(arity);
	for(int j=0;j<arity;j++)
	    args[j] = get_fresh_var();

	auto pat = C;
	if (args.size())
	    pat = expression_ref{C,args};

	// 3.3 Construct the objects for the sub-case expression: x2[i] = v1...v[arity], x[2]...x[N]
	vector<expression_ref> x2;
	for(int j=0;j<arity;j++)
	    x2.push_back(args[j]);
	x2.insert(x2.end(), x.begin()+1, x.end());

	// 3.4 Construct the various modified bodies and patterns
	vector<equation_info_t> equations2;
	for(int r: rules[c])
	{
	    assert(equations[r].patterns[0].size() == arity);

	    // pattern: Add the sub-partitions of the first top-level pattern at the beginning.
	    auto patterns = equations[r].patterns[0].copy_sub();
	    // pattern: Add the remaining top-level patterns (minus the first).
	    patterns.insert(patterns.end(), equations[r].patterns.begin()+1, equations[r].patterns.end());

	    // Add the equation
	    auto eqn = equation_info_t{std::move(patterns), equations[r].rhs};
	    equations2.push_back(std::move(eqn));
	}

	simple_patterns.push_back( pat );
	simple_bodies.push_back( match(x2, equations2, otherwise) );
    }

    simple_patterns.push_back(var(-1));
    simple_bodies.push_back(otherwise);

    // Construct final case expression
    return let_expression(binds, make_case_expression(x[0], simple_patterns, simple_bodies));
}


expression_ref desugar_state::match_var(const vector<expression_ref>& x, const vector<equation_info_t>& equations, expression_ref otherwise)
{
    const int N = x.size();
    const int M = equations.size();

#ifndef NDEBUG
    assert(N > 0);
    assert(M > 0);
    assert(equations[0].patterns.size());
    for(auto& eqn:equations)
	assert(is_var(eqn.patterns[0]));
#endif

    vector<equation_info_t> equations2;
    for(auto& eqn: equations)
	equations2.push_back({remove_first(eqn.patterns), eqn.rhs});
      
    // Should these binds should be pushed all the way into the rhs?
    // Maybe not, since earlier failure branches might need the otherwise var.

    CDecls binds;
    if (not is_var(otherwise))
    {
	auto o = get_fresh_var();
	binds.push_back({o, otherwise});
	otherwise = o;
    }
    return let_expression(binds, match(remove_first(x), equations2, otherwise));
}


expression_ref desugar_state::match_empty(const vector<expression_ref>& x, const vector<equation_info_t>& equations, expression_ref otherwise)
{
    assert(x.size() == 0);

    auto E = otherwise;

//  can fail is true if we could use the otherwise expression.
//  bool can_fail = true;
    for(auto& e: std::reverse(equations))
    {
	E = e.rhs.result(E);
//	can_fail = can_fail and not e.rhs.can_fail;
    }
    return E;
}


// Make this a member function of equation_info_t?
void desugar_state::clean_up_pattern(const expression_ref& x, equation_info_t& eqn)
{
    auto& patterns = eqn.patterns;
    auto& pat1 = patterns[0];
    auto& rhs = eqn.rhs;
    assert(patterns.size());

    // case x of y -> rhs  =>  case x of _ => let {y=x} in rhs
    if (is_var(pat1) and not is_wildcard(pat1))
    {
	auto y = pat1.as_<var>();
	rhs.add_binding({{y, x}});
	pat1 = var(-1);
    }

    // case x of ~pat -> rhs  =>  case x of _ -> let pat=x in rhs
    else if (is_AST(pat1,"LazyPattern"))
    {
	auto& pat2 = pat1.sub()[0];
	CDecls binds = {};
	for(auto& y: get_free_indices(pat2))
	    binds.push_back({y,case_expression(x, pat2, failable_expression(x), error("lazy pattern: failed pattern match"))});
	rhs.add_binding(binds);
	pat1 = var(-1);
    }

    // case x of y@pat2 -> rhs  => case x of pat2 => let{y=x} in rhs
    else if (is_AST(pat1,"AsPattern"))
    {
	auto y = pat1.sub()[0].as_<var>();
	auto pat2 = pat1.sub()[1];
	rhs.add_binding({{y, x}});
	pat1 = pat2;
    }
}

expression_ref desugar_state::match(const vector<expression_ref>& x, const vector<equation_info_t>& equations, expression_ref otherwise)
{
    const int N = x.size();
    const int M = equations.size();

    for(int j=0;j<N;j++)
	assert(is_var(x[j]));

    // Each pattern must have N components.
    for(int j=0;j<M;j++)
	assert(equations[j].patterns.size() == N);

    // 1. If there are no equations, return the otherwise expression
    if (not equations.size())
	return otherwise;

    // 2. If there are not xs left, follow the empty case
    if (not x.size())
	return match_empty(x, equations, otherwise);

    // 3. Tidy the equations
    auto equations2 = equations;
    for(auto& e: equations2)
	clean_up_pattern(x[0],e);
    
    // 4. Follow the "mixture rule" from Wadler in SLPJ
    auto partitions = partition(equations2);

    expression_ref E = otherwise;

    for(auto& block: std::reverse(partitions))
    {
	assert(block.first != pattern_type::null);
	if (block.first == pattern_type::var)
	    E = match_var(x, block.second, E);
	else
	    E = match_constant(x, block.second, E);
    }
    return E;
}

// Create the expression 'case T of {patterns[i] -> bodies[i]}'
// Create the expression 'case (T) of {(patterns[i]) -> bodies[i]}'
expression_ref desugar_state::case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<failable_expression>& bodies, const expression_ref& otherwise)
{
    vector<vector<expression_ref>> multi_patterns;
    for(const auto& p:patterns)
	multi_patterns.push_back({p});

    // Maybe only do this if T isn't a var already?
    auto x = get_fresh_var();
    CDecls binds{{x,T}};
    return let_expression(binds, match({x}, multi_patterns, bodies, otherwise));
}

expression_ref desugar_state::case_expression(const expression_ref& T, const expression_ref& pattern, const failable_expression& body, const expression_ref& otherwise)
{
    vector<expression_ref> patterns = {pattern};
    vector<failable_expression> bodies = {body};
    if (otherwise and not pattern.is_a<var>())
    {
	patterns.push_back(var(-1));
	bodies.push_back(failable_expression(otherwise));
    }
    return case_expression(T,patterns, bodies, otherwise);
}

expression_ref desugar_state::def_function(const vector< vector<expression_ref> >& patterns, const vector<failable_expression>& bodies, const expression_ref& otherwise)
{
    // Construct the dummies
    vector<expression_ref> args;
    for(int i=0;i<patterns[0].size();i++)
	args.push_back(get_fresh_var());

    // Construct the case expression
    
    expression_ref E = match(args, patterns, bodies, otherwise);

    // Turn it into a function
    for(int i=args.size()-1;i>=0;i--)
	E = lambda_quantify(args[i], E);

    return E;
}

