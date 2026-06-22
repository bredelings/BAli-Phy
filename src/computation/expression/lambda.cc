#include "lambda.H"
#include "var.H"

#include "range/v3/all.hpp"
namespace views = ranges::views;

using std::string;
using std::vector;

string lambda::print() const {
    return "lambda";
}

bool lambda::operator==(const Object& o) const 
{
    return dynamic_cast<const lambda*>(&o);
}

expression_ref lambda_quantify(const expression_ref& var, const expression_ref& R)
{
    return new expression(lambda(),{var, R});
}

expression_ref lambda_quantify(const vector<expression_ref>& vars, expression_ref& body)
{
    auto L = body;
    for(auto& v: vars | views::reverse)
        L = lambda_quantify(v, L);
    return L;
}

expression_ref lambda_quantify(const std::vector<var>& vars, const expression_ref& body)
{
    auto L = body;
    for(auto& v: vars | views::reverse)
        L = lambda_quantify(v, L);
    return L;
}

