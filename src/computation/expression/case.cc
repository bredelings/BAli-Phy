#include <set>
#include "lambda.H"
#include "var.H"
#include "case.H"
#include "let.H"
#include "substitute.H"
#include "AST_node.H"
#include "computation/operations.H"

using std::vector;

void parse_alts(const expression_ref& alts, vector<expression_ref>& patterns, vector<expression_ref>& bodies)
{
    for(auto& alt: alts.sub())
    {
	patterns.push_back( alt.sub()[0] );
	bodies.push_back( alt.sub()[1] );
    }
}

/// R = case T of {patterns[i] -> bodies[i]}
bool parse_case_expression(const expression_ref& E, expression_ref& object, vector<expression_ref>& patterns, vector<expression_ref>& bodies)
{
    patterns.clear();
    bodies.clear();

    if (not is_case(E)) return false;

    object = E.sub()[0];

    parse_alts(E.sub()[1], patterns, bodies);

    return true;
}

expression_ref make_alt(const expression_ref& pattern, const expression_ref& body)
{
    assert(pattern);
    assert(body);

    object_ptr<expression> alt = new expression(AST_node("alt"));
    alt->sub.push_back(pattern);
    alt->sub.push_back(body);
    return alt;
}

expression_ref make_alts(const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
    assert(patterns.size() == bodies.size());
    assert(not patterns.empty());

    object_ptr<expression> alts = new expression(AST_node("alts"));
    for(int i=0;i<patterns.size();i++)
	alts->sub.push_back(make_alt(patterns[i],bodies[i]));
    return alts;
}


expression_ref make_case_expression(const expression_ref& object, const expression_ref& alts)
{
    assert(object);
    assert(alts);

    object_ptr<expression> E = new expression(Case());
    E->sub.push_back(object);
    E->sub.push_back(alts);

    return E;
}

expression_ref make_case_expression(const expression_ref& object, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
    return make_case_expression(object,make_alts(patterns,bodies));
}

expression_ref make_if_expression(const expression_ref& condition, const expression_ref& true_branch, const expression_ref& false_branch)
{
    return make_case_expression(condition,{true,false},{true_branch, false_branch});
}

bool is_case(const expression_ref& E)
{
    return E.head().type() == case_type;
}

