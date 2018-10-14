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


#include "computation/expression/substitute.H"

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

vector<pair<pattern_type,vector<int>>> partition(const vector<equation_info_t>& equations)
{
    vector<pair<pattern_type,vector<int>>> partitions;
    auto prev_type = pattern_type::null;
    for(int i=0;i<equations.size();i++)
    {
	auto current_type = classify_equation(equations[i]);
	if (current_type == prev_type)
	    partitions.back().second.push_back(i);
	else
	    partitions.push_back({current_type,{i}});
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
expression_ref desugar_state::block_case(const vector<expression_ref>& xs, const vector<vector<expression_ref>>& p, const vector<expression_ref>& b, const expression_ref& otherwise)
{
    assert(p.size() == b.size());

    vector<equation_info_t> equations;
    for(int i=0;i<p.size();i++)
	equations.push_back({p[i],b[i]});

    return block_case(xs, equations, otherwise);
}

expression_ref desugar_state::block_case_constant(const vector<expression_ref>& x, const vector<equation_info_t>& equations, expression_ref otherwise)
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

	    equation_info_t eqn;
	    eqn.rhs = equations[r].rhs;

	    // pattern: Add the sub-partitions of the first top-level pattern at the beginning.
	    if (equations[r].patterns[0].size())
		eqn.patterns = equations[r].patterns[0].sub();
	    // pattern: Add the remaining top-level patterns (minus the first).
	    eqn.patterns.insert(eqn.patterns.end(), equations[r].patterns.begin()+1, equations[r].patterns.end());

	    // Add the equation
	    equations2.push_back(std::move(eqn));
	}

	simple_patterns.push_back( pat );
	simple_bodies.push_back( block_case(x2, equations2, otherwise) );
    }

    simple_patterns.push_back(var(-1));
    simple_bodies.push_back(otherwise);

    // Construct final case expression
    return let_expression(binds, make_case_expression(x[0], simple_patterns, simple_bodies));
}


expression_ref desugar_state::block_case_var(const vector<expression_ref>& x, const vector<equation_info_t>& equations, expression_ref otherwise)
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
    {
	equation_info_t eqn2{remove_first(eqn.patterns), eqn.rhs};

	// FIXME - This should really go in tidy().
	if (not is_wildcard(eqn.patterns[0]))
	    eqn2.rhs = substitute(eqn2.rhs, eqn.patterns[0].as_<var>(), x[0]);

	equations2.push_back(eqn2);
    }
      
    // Should these binds should be pushed all the way into the rhs?
    // Maybe not, since earlier branches might need the otherwise var.

    CDecls binds;
    if (not is_var(otherwise))
    {
	auto o = get_fresh_var();
	binds.push_back({o, otherwise});
	otherwise = o;
    }
    return let_expression(binds, block_case(remove_first(x), equations2, otherwise));
}


expression_ref desugar_state::block_case_empty(const vector<expression_ref>& x, const vector<equation_info_t>& equations, expression_ref otherwise)
{
    assert(x.size() == 0);
    // Actually we should combine E0 [] E1 [] ... [] EN [] otherwise
    if (equations.size())
	return equations[0].rhs;
    else
	return otherwise;
}


expression_ref desugar_state::block_case(const vector<expression_ref>& x, const vector<equation_info_t>& equations, expression_ref otherwise)
{
    const int N = x.size();
    const int M = equations.size();

    for(int j=0;j<N;j++)
	assert(is_var(x[j]));

    // Each pattern must have N components.
    for(int j=0;j<M;j++)
	assert(equations[j].patterns.size() == N);

    if (not equations.size())
	return otherwise;

    if (not x.size())
	return block_case_empty(x, equations, otherwise);

    auto partitions = partition(equations);

    // This implements the "mixture rule" from Wadler in SLPJ
    expression_ref E = otherwise;

    for(int i=partitions.size()-1; i >= 0; i--)
    {
	vector<equation_info_t> equations_part;
	for(int j: partitions[i].second)
	    equations_part.push_back(equations[j]);

	assert(partitions[i].first != pattern_type::null);
	if (partitions[i].first == pattern_type::var)
	    E = block_case_var(x, equations_part, E);
	else
	    E = block_case_constant(x, equations_part, E);
    }
    return E;
}

// Create the expression 'case T of {patterns[i] -> bodies[i]}'
// Create the expression 'case (T) of {(patterns[i]) -> bodies[i]}'
expression_ref desugar_state::case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies, const expression_ref& otherwise)
{
    vector<vector<expression_ref>> multi_patterns;
    for(const auto& p:patterns)
	multi_patterns.push_back({p});

    // Maybe only do this if T isn't a var already?
    auto x = get_fresh_var();
    CDecls binds{{x,T}};
    return let_expression(binds, block_case({x}, multi_patterns, bodies, otherwise));
}

expression_ref desugar_state::case_expression(const expression_ref& T, const expression_ref& pattern, const expression_ref& body, const expression_ref& otherwise)
{
    vector<expression_ref> patterns = {pattern};
    vector<expression_ref> bodies = {body};
    if (otherwise and not pattern.is_a<var>())
    {
	patterns.push_back(var(-1));
	bodies.push_back(otherwise);
    }
    return case_expression(T,patterns, bodies, otherwise);
}

expression_ref desugar_state::def_function(const vector< vector<expression_ref> >& patterns, const vector<expression_ref>& bodies, const expression_ref& otherwise)
{
    // Construct the dummies
    vector<expression_ref> args;
    for(int i=0;i<patterns[0].size();i++)
	args.push_back(get_fresh_var());

    // Construct the case expression
    
    expression_ref E = block_case(args, patterns, bodies, otherwise);

    // Turn it into a function
    for(int i=args.size()-1;i>=0;i--)
	E = lambda_quantify(args[i], E);

    return E;
}

