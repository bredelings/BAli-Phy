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
expression_ref desugar_state::block_case(const vector<expression_ref>& xs, const vector<vector<expression_ref>>& p, const vector<expression_ref>& b)
{
    assert(p.size() == b.size());

    vector<equation_info_t> equations;
    for(int i=0;i<p.size();i++)
	equations.push_back({p[i],b[i]});
    return block_case(xs, equations);
}



expression_ref desugar_state::block_case(const vector<expression_ref>& x, const vector<equation_info_t>& equations)
{
    const int N = x.size();
    const int M = equations.size();

    // Each pattern must have N components.
    for(int j=0;j<M;j++)
	assert(equations[j].patterns.size() == N);

    if (not x.size())
	return equations[0].rhs;


    auto partitions = partition(equations);


    // 1. Categorize each rule according to the type of its top-level pattern
    vector<expression_ref> constants;
    vector< vector<int> > rules;
    vector<int> irrefutable_rules;
    for(int j=0;j<M;j++)
    {
	if (is_var(equations[j].patterns[0]))
	{
	    irrefutable_rules.push_back(j);
	    continue;
	}

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

    // 2. Substitute for the irrefutable rules to find the 'otherwise' branch
    // This is substitute(x[1],p[2..m][1], case x2...xN of p[2..M][i] -> b[2..M] )
    expression_ref otherwise;
    if (irrefutable_rules.empty())
	; // otherwise = NULL
    else
    {
	vector<expression_ref> x2 = x;
	x2.erase(x2.begin());

	vector<equation_info_t> equations2;
	for(int i=0;i<irrefutable_rules.size();i++)
	{
	    int r = irrefutable_rules[i];
	    equations2.push_back(equations[r]);

	    auto& last_patterns = equations2.back().patterns;
	    last_patterns.erase(last_patterns.begin());

	    if (is_wildcard(equations[r].patterns[0]))
		// This is a var.
		; //assert(d->name.size() == 0);
	    else
	    {
		// FIXME! What if x[0] isn't a var?
		// Then if *d occurs twice, then we should use a let expression, right?
		equations2[i].rhs = substitute(equations2[i].rhs, equations[r].patterns[0].as_<var>(), x[0]);
	    }
	}
      
	if (x2.empty())
	{
	    // If (b2.size() > 1) then we have duplicate irrefutable rules, but that's OK.
	    // This can even be generated in the process of simplifying block_case expressions.	
	    otherwise = equations2[0].rhs;
	}
	else
	{
	    otherwise = block_case(x2, equations2);
	}
    }
      
    // If there are no conditions on x[0], then we are done.
    if (constants.empty())
    {
	assert(otherwise);
	return otherwise;
    }

    // WHEN should we put the otherwise expression into a LET variable?
    expression_ref O;
    if (otherwise) O = get_fresh_var();

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
	    S[j] = get_fresh_var();

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
	vector<equation_info_t> equations2;
	for(int i=0;i<rules[c].size();i++)
	{

	    int r = rules[c][i];

	    assert(equations[r].patterns[0].size() == arity);

	    // Add the pattern
	    equation_info_t eqn;
	    eqn.rhs = equations[r].rhs;

	    // Add the sub-partitions of the first top-level pattern at the beginning.
	    if (equations[r].patterns[0].size())
		eqn.patterns = equations[r].patterns[0].sub();
	    // Add the remaining top-level patterns (minus the first).
	    eqn.patterns.insert(eqn.patterns.end(), equations[r].patterns.begin()+1, equations[r].patterns.end());

	    // Add the equation
	    equations2.push_back(std::move(eqn));

	    // Check if p2[i] are all irrefutable
	    for(int i=0;i<equations2.back().patterns.size();i++)
		if (not is_irrefutable_pattern(equations2.back().patterns[i]))
		{
		    future_patterns_all_irrefutable = false;
		    all_simple_followed_by_irrefutable = false;
		}
	}

	// If x[1] matches a simple pattern in the only alternative, we may as well
	// not change the variable names for the match slots in this pattern.
	if (rules[c].size() == 1 and is_simple_pattern(equations[r0].patterns[0]))
	{
	    simple_patterns.back() = equations[r0].patterns[0];

	    // case x[1] of p[r0][1] -> case (x[2],..,x[N]) of (p[r0][2]....p[r0][N]) -> b[r0]
	    x2 = remove_first(x);

	    equations2.back().patterns = remove_first(equations[r0].patterns);
	}

	// If all future patterns are irrefutable, then we won't need to backtrack to the otherwise case.
	if (future_patterns_all_irrefutable)
	{
	    // There can be only one alternative.
	    assert(rules[c].size() == 1);

	    if (x2.size())
		simple_bodies.back() = block_case(x2, equations2);
	    else
		simple_bodies.back() = equations[r0].rhs;
	}
	else
	{
	    if (otherwise)
	    {
		// Since we could backtrack, use the var.  It will point to otherwise
		equations2.push_back({vector<expression_ref>(x2.size(), var(-1)), O});
	    }
	    simple_bodies.back() = block_case(x2, equations2);
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
expression_ref desugar_state::case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
    vector<vector<expression_ref>> multi_patterns;
    for(const auto& p:patterns)
	multi_patterns.push_back({p});
    return block_case({T}, multi_patterns, bodies);
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
    return case_expression(T,patterns, bodies);
}

expression_ref desugar_state::def_function(const vector< vector<expression_ref> >& patterns, const vector<expression_ref>& bodies)
{
    // Construct the dummies
    vector<expression_ref> args;
    for(int i=0;i<patterns[0].size();i++)
	args.push_back(get_fresh_var());

    // Construct the case expression
    expression_ref E = block_case(args, patterns, bodies);

    // Turn it into a function
    for(int i=args.size()-1;i>=0;i--)
	E = lambda_quantify(args[i], E);

    return E;
}

