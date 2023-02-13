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
#include "computation/haskell/haskell.H"
#include "desugar.H"
#include "util/assert.hh"
#include "util/range.H"
#include "computation/typecheck/typecheck.H"

using std::string;
using std::vector;
using std::set;
using std::deque;
using std::pair;

//  -----Prelude: http://www.haskell.org/onlinereport/standard-prelude.html

// See list in computation/loader.C
//

failable_expression fail_identity()
{
    return failable_expression{true, [](const expression_ref& o) {return o;}};
}

failable_expression desugar_state::combine(const failable_expression& E1, const failable_expression& E2)
{
    if (not E1.can_fail) return E1;

    std::function<expression_ref(const expression_ref&)> result;

    // result = \o -> let o2 = e2 o in e1 o2
    auto o2 = get_fresh_var();
    result = [E1,E2,o2](const expression_ref& o) {
	return let_expression({{o2,E2.result(o)}},E1.result(o2));
    };

    return failable_expression{E2.can_fail, result};
}

failable_expression desugar_state::fold(const vector<failable_expression>& Es)
{
    failable_expression E = fail_identity();

    for(auto& e: std::reverse(Es))
	E = combine(e,E);

    return E;
}

template <typename T>
std::optional<int> find_object(const vector<T>& v, const T& E)
{
    for(int i=0;i<v.size();i++)
	if (E == v[i])
	    return i;
    return {};
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
    literal,
    var,
    bang,
    wrap,
    null
};

pattern_type classify_equation(const equation_info_t& equation)
{
    assert(equation.patterns.size());
    auto& pat = equation.patterns[0];

    // Var
    if (pat.is_a<Hs::VarPattern>())
	return pattern_type::var;
    else if (pat.is_a<Hs::WildcardPattern>())
	return pattern_type::var;
    // Should we classify lazy patterns here, or clean them up before?

    // Con
    else if (pat.is_a<Hs::ConPattern>())
	return pattern_type::constructor;

    // Literal
    else if (pat.is_a<Hs::LiteralPattern>())
	return pattern_type::literal;

    // Strict
    else if (pat.is_a<Haskell::StrictPattern>())
    {
	throw myexception()<<"The BangPattern extension is not implemented!";
        return pattern_type::bang;
    }

    // ??
    else
	throw myexception()<<"I don't understand pattern '"<<pat<<"'";
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

failable_expression desugar_state::match_constructor(const vector<var>& x, const vector<equation_info_t>& equations)
{
    const int N = x.size();
    const int M = equations.size();

    assert(N > 0);
    assert(M > 0);
    assert(equations[0].patterns.size());

    // 1. Categorize each rule according to the type of its top-level pattern
    vector< Hs::Con > constants;
    vector< vector<int> > rules;
    for(int j=0;j<M;j++)
    {
	auto con_pat = equations[j].patterns[0].to<Hs::ConPattern>();
        assert(con_pat);

	auto C = con_pat->head;
	auto which = find_object(constants, C);

	if (not which)
	{
	    which = constants.size();
	    constants.push_back(C);
	    rules.push_back(vector<int>{});
	}

	rules[*which].push_back(j);
    }

    // 2. Find the alternatives in the simple case expression
    vector<expression_ref> simple_patterns;
    vector<failable_expression> simple_bodies;

    for(int c=0;c<constants.size();c++)
    {
	auto& C = constants[c];

	// 2.1 Find the arity of the constructor
        string name = C.name;
        auto info = tc_state().constructor_info(C);
        int dict_arity  = info.dict_arity();
        int field_arity = info.arity();
        assert(field_arity == *C.arity);
        int total_arity = dict_arity + field_arity;

	// 2.2 Construct the simple pattern for constant C
	vector<var> args;
	for(int j=0;j< total_arity; j++)
	    args.push_back( get_fresh_var() );

	expression_ref pat = constructor(name, total_arity);
	if (args.size())
        {
            vector<expression_ref> args2;
            for(auto& arg: args)
                args2.push_back(arg);
	    pat = expression_ref{pat,args2};
        }

	// 2.3 Construct the objects for the sub-case expression: x2[i] = v1...v[arity], x[2]...x[N]
	vector<var> x2 = args;
	x2.insert(x2.end(), x.begin()+1, x.end());

	// 2.4 Construct the various modified bodies and patterns
	vector<equation_info_t> equations2;
	for(int r: rules[c])
	{
	    // pattern: Add the sub-partitions of the first top-level pattern at the beginning.
            auto con_pat = equations[r].patterns[0].to<Hs::ConPattern>();
            vector<expression_ref> patterns;
            for(auto& dvar: con_pat->given_dict_vars)
                patterns.push_back(make_VarPattern(dvar));
            for(auto& sub_pat: con_pat->args)
                patterns.push_back(unloc(sub_pat));
	    assert(patterns.size() == total_arity);

	    // pattern: Add the remaining top-level patterns (minus the first).
	    patterns.insert(patterns.end(), equations[r].patterns.begin()+1, equations[r].patterns.end());

            // Add the ev_binds for the ConPattern to the rhs
            auto rhs = equations[r].rhs;
            if (con_pat->ev_binds)
                rhs.add_binding(*con_pat->ev_binds);

	    // Add the equation
	    auto eqn = equation_info_t{std::move(patterns), std::move(rhs)};
	    equations2.push_back(std::move(eqn));
	}

	simple_patterns.push_back( pat );
	simple_bodies.push_back( match(x2, equations2) );
    }
    simple_patterns.push_back(var(-1));
    simple_bodies.push_back(fail_identity());

    // 3. Construct a failable_expression.
    auto x0 = x[0];

    // What if we substitute into the failable_result twice?
    // Its not clear how that could cause incorrect scoping, but we could have different vars with the same index?
    auto o = get_fresh_var();

    auto result = [=](const expression_ref& otherwise)
    {
	// Bind the otherwise branch to a let var.
	CDecls binds = {{o,otherwise}};

	vector<expression_ref> simple_bodies2;
	for(auto& body: simple_bodies)
	    simple_bodies2.push_back(body.result(o));

	return let_expression({{o,otherwise}}, make_case_expression(x0, simple_patterns, simple_bodies2));
    };

    return failable_expression{true, result};
}


failable_expression desugar_state::match_literal(const vector<var>& x, const vector<equation_info_t>& equations)
{
    const int N = x.size();
    const int M = equations.size();

    assert(N > 0);
    assert(M > 0);
    assert(equations[0].patterns.size());

    // 1. Categorize each rule according to the type of its top-level pattern
    vector<Hs::LiteralPattern> constants;
    vector< vector<int> > rules;
    for(int j=0;j<M;j++)
    {
	auto L = equations[j].patterns[0].head().as_<Hs::LiteralPattern>();
	auto which = find_object(constants, L);

	if (not which)
	{
	    which = constants.size();
	    constants.push_back(L);
	    rules.push_back(vector<int>{});
	}

	rules[*which].push_back(j);
    }

    // 2. Find the alternatives in the simple case expression
    vector<failable_expression> simple_bodies;

    for(int c=0;c<constants.size();c++)
    {
	auto pat = constants[c];

	// 2.3 Construct the objects for the sub-case expression: x2[i] = v1...v[arity], x[2]...x[N]
	vector<var> x2;
	x2.insert(x2.end(), x.begin()+1, x.end());

	// 2.4 Construct the various modified bodies and patterns
	vector<equation_info_t> equations2;
	for(int r: rules[c])
	{
	    assert(equations[r].patterns[0].size() == 0);

	    // pattern: Add the sub-partitions of the first top-level pattern at the beginning.
	    auto patterns = equations[r].patterns[0].copy_sub();
	    // pattern: Add the remaining top-level patterns (minus the first).
	    patterns.insert(patterns.end(), equations[r].patterns.begin()+1, equations[r].patterns.end());

	    // Add the equation
	    auto eqn = equation_info_t{std::move(patterns), equations[r].rhs};
	    equations2.push_back(std::move(eqn));
	}

	simple_bodies.push_back( match(x2, equations2) );
    }

    // 3. Construct a failable_expression.
    auto x0 = x[0];

    // What if we substitute into the failable_result twice?
    // Its not clear how that could cause incorrect scoping, but we could have different vars with the same index?

    std::function<expression_ref(const expression_ref&)> result = [=,this](const expression_ref& otherwise)
    {
        expression_ref E = otherwise;

        for(int i=constants.size()-1; i>= 0; i--)
        {
            auto& LP = constants[i];

            // condition = (x == constants[i])
            expression_ref condition = {desugar(LP.equalsOp), x0, desugar(LP.lit)};

            // let o = E in case condition of True -> true_branch(o); 
            auto o = get_fresh_var();
            Core::Exp true_branch = simple_bodies[i].result(o);
            E = let_expression({{o,E}},
                               case_expression(condition,
                                               {Hs::TruePat()},{failable_expression(true_branch)}).result(o));
        }
        return E;
    };

    return failable_expression{true, result};
}


failable_expression desugar_state::match_var(const vector<var>& x, const vector<equation_info_t>& equations)
{
    const int N = x.size();
    const int M = equations.size();

    assert(N > 0);
    assert(M > 0);
    assert(equations[0].patterns.size());

    vector<equation_info_t> equations2;
    for(auto& eqn: equations)
    {
        assert(eqn.patterns[0].is_a<Hs::WildcardPattern>());

	equations2.push_back({remove_first(eqn.patterns), eqn.rhs});
    }
      
    return match(remove_first(x), equations2);
}


failable_expression desugar_state::match_empty(const vector<var>& x, const vector<equation_info_t>& equations)
{
    assert(x.size() == 0);

    failable_expression E = fail_identity();

    for(auto& e: std::reverse(equations))
	E = combine(e.rhs, E);

    return E;
}

// Make this a member function of equation_info_t?
void desugar_state::clean_up_pattern(const var& x, equation_info_t& eqn)
{
    auto& patterns = eqn.patterns;
    auto& pat1 = patterns[0];
    auto& rhs = eqn.rhs;
    assert(patterns.size());

    if (auto L = pat1.to<Hs::ListPattern>())
        pat1 = Hs::to_con_pat(*L);
    else if (auto T = pat1.to<Hs::TuplePattern>())
        pat1 = Hs::to_con_pat(*T);

    // case x of y -> rhs  =>  case x of _ => let {y=x} in rhs
    if (auto v = pat1.to<Hs::VarPattern>())
    {
	auto y = make_var(v->var);
	rhs.add_binding({{y, x}});
	pat1 = Hs::WildcardPattern();
    }
    // case x of ~pat -> rhs  =>  case x of _ -> let pat=x in rhs
    else if (pat1.is_a<Haskell::LazyPattern>())
    {
        auto& LP = pat1.as_<Haskell::LazyPattern>();
	CDecls binds = {};
	for(auto& v: Hs::vars_in_pattern(LP.pattern))
        {
            auto y = make_var(v);
	    binds.push_back({y,case_expression(x, {unloc(LP.pattern)}, {failable_expression(y)}).result(Core::error("lazy pattern: failed pattern match"))});
        }
	rhs.add_binding(binds);
	pat1 = Hs::WildcardPattern();
    }

    // case x of {!pat -> rhs; _ -> rhs_fail}  =>  x `seq` case x of {pat -> rhs, _ -> rhs_fail}
    else if (pat1.is_a<Haskell::StrictPattern>())
    {
        // Note that let !(x,y) = e is handled differently -- this is considered a bang-pattern binding,
        // and is not really part of handling case x of !pat.

        // "A bang only really has an effect if it precedes a variable or wild-card pattern: "
        // f !(x,y) = E  is the same as f (x,y) = E, since we have f = \z -> case z of (x,y) -> E.

        // Recursively clean up the strict pattern underneat the strictness mark,
        //  then restore the strictness mark.
        // We could end up with (for example) a strict wildcard.
        auto SP = pat1.as_<Haskell::StrictPattern>();
        pat1 = unloc(SP.pattern);
        clean_up_pattern(x, eqn);
        pat1 = Haskell::StrictPattern({noloc,pat1});

        // FIXME: does this work?

	throw myexception()<<"The BangPattern extension is not implemented!";
    }

    // case x of y@pat2 -> rhs  => case x of pat2 => let{y=x} in rhs
    else if (pat1.is_a<Haskell::AsPattern>())
    {
        auto& AP = pat1.as_<Haskell::AsPattern>();
	auto y = make_var(AP.var);
	rhs.add_binding({{y, x}});
	pat1 = unloc(AP.pattern);
    }
    // case x of (pat::type) -> rhs  => case wrap(x) of pat -> rhs
    else if (auto tp = pat1.to<Haskell::TypedPattern>())
    {
        // FIXME: this ignores the wrapper!
	pat1 = unloc(tp->pat);
        clean_up_pattern(x, eqn);
    }
}

failable_expression desugar_state::match(const vector<var>& x, const vector<equation_info_t>& equations)
{
    const int N = x.size();
    const int M = equations.size();

    // Each pattern must have N components.
    for(int j=0;j<M;j++)
	assert(equations[j].patterns.size() == N);

    // 1. If there are not xs left, follow the empty case
    if (not x.size())
	return match_empty(x, equations);

    // 2. Tidy the equations
    auto equations2 = equations;
    for(auto& e: equations2)
	clean_up_pattern(x[0],e);
    
    // 3. Follow the "mixture rule" from Wadler in SLPJ
    auto partitions = partition(equations2);

    auto E = fail_identity();

    for(auto& block: std::reverse(partitions))
    {
	assert(block.first != pattern_type::null);
	if (block.first == pattern_type::var)
	    E = combine(match_var(x, block.second), E);
        else if (block.first == pattern_type::literal)
	    E = combine(match_literal(x, block.second), E);
	else
	    E = combine(match_constructor(x, block.second), E);
    }
    return E;
}

// For `case T of patterns[i] -> bodies[i]`
failable_expression desugar_state::case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<failable_expression>& bodies)
{
    assert(patterns.size() == bodies.size());
    vector<equation_info_t> equations;
    for(int i=0; i<patterns.size(); i++)
	equations.push_back( { {patterns[i]}, bodies[i]} );

    auto x = get_fresh_var();
    auto FE = match({x}, equations);
    FE.add_binding({{x,T}});
    return FE;
}

expression_ref desugar_state::def_function(const vector< equation_info_t >& equations, const expression_ref& otherwise)
{
    // 1. Get fresh vars for the arguments
    vector<var> args;
    for(int i=0;i<equations[0].patterns.size();i++)
	args.push_back(get_fresh_var());

    // 2. Construct the case expression
    expression_ref E = match(args, equations).result(otherwise);

    // 3. Turn it into a function
    for(int i=args.size()-1;i>=0;i--)
	E = lambda_quantify(args[i], E);

    return E;
}

