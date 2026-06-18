#include "models/rule-template.H"

#include "computation/expression/apply.H"
#include "computation/expression/lambda.H"
#include "computation/expression/list.H"
#include "computation/expression/tuple.H"
#include "computation/expression/var.H"
#include "computation/haskell/haskell.H"
#include "util/myexception.H"

using std::map;
using std::set;
using std::string;
using std::vector;

namespace
{

// Returns true for the legacy rule-template spelling of a trailing submodel
// argument that should be compiled as `submodel +> f`.
bool is_template_submodel_arg(const CM::Arg<CM::NoAnn>& arg)
{
    if (not arg.name.empty() or not arg.value)
        return false;

    auto arg_ref = arg.value->to<CM::ArgRef>();
    return arg_ref and arg_ref->name == "submodel";
}

// Converts a rule call/computed template from CmdModel to Haskell expression
// code and records every @arg reference seen while lowering.
expression_ref lower_rule_template_expr_impl(const CM::UntypedExpr& expr, const map<string,expression_ref>& simple_args, set<string>& referenced_args)
{
    return expr.visit(CM::overloaded{
        [](const CM::IntLiteral& x) -> expression_ref { return x.value; },
        [](const CM::DoubleLiteral& x) -> expression_ref { return x.value; },
        [](const CM::BoolLiteral& x) -> expression_ref { return x.value; },
        [](const CM::StringLiteral& x) -> expression_ref { return String(x.value); },
        [](const CM::Var& x) -> expression_ref { return var(x.name); },
        // Looks up a rule-template argument reference in the already generated
        // argument environment.
        [&](const CM::ArgRef& x) -> expression_ref
        {
            referenced_args.insert(x.name);
            try
            {
                return simple_args.at(x.name);
            }
            catch(...)
            {
                throw myexception()<<"cannot find argument '"<<x.name<<"'";
            }
        },
        [](const CM::Placeholder&) -> expression_ref { throw myexception()<<"Placeholder is not allowed in rule templates."; },
        [](const CM::GetState&) -> expression_ref { throw myexception()<<"get_state is not allowed in rule templates."; },
        // Compiles a rule-template call as Haskell application, preserving the
        // legacy final-submodel rewrite.
        [&](const CM::Call<CM::NoAnn>& call) -> expression_ref
        {
            expression_ref E = var(call.function);
            for(int i=0;i<call.args.size();i++)
            {
                auto& arg = call.args[i];
                if (not arg.name.empty())
                    throw myexception()<<"Named arguments are not allowed in rule templates.";
                if (not arg.value)
                    throw myexception()<<"Missing arguments are not allowed in rule templates.";

                auto arg_expr = lower_rule_template_expr_impl(*arg.value, simple_args, referenced_args);
                // Compatibility behavior: rule templates encode `submodel +> f`
                // as a final @submodel argument. Remove when bindings spell this directly.
                if (i == call.args.size()-1 and is_template_submodel_arg(arg))
                    E = {var("+>"), arg_expr, E};
                else
                    E = {E, arg_expr};
            }
            return E;
        },
        // Compiles a rule-template list by translating each element into a
        // located Haskell expression.
        [&](const CM::List<CM::NoAnn>& list) -> expression_ref
        {
            vector<Hs::LExp> located_args;
            for(auto& element: list.elements)
                located_args.push_back({noloc, lower_rule_template_expr_impl(element, simple_args, referenced_args)});
            return Hs::List(located_args);
        },
        // Compiles a rule-template tuple by translating each element into a
        // located Haskell expression.
        [&](const CM::Tuple<CM::NoAnn>& tuple) -> expression_ref
        {
            vector<Hs::LExp> located_args;
            for(auto& element: tuple.elements)
                located_args.push_back({noloc, lower_rule_template_expr_impl(element, simple_args, referenced_args)});
            return Hs::Tuple(located_args);
        },
        [](const CM::Let<CM::NoAnn>&) -> expression_ref { throw myexception()<<"let expressions are not allowed in rule templates."; },
        // Preserves the old template lambda support by folding adjacent lambda
        // nodes into one Haskell lambda expression.
        [&](const CM::Lambda<CM::NoAnn>& lambda) -> expression_ref
        {
            auto pattern = lambda.pattern.to<CM::VarPattern>();
            if (not pattern)
                throw myexception()<<"Only variable lambda patterns are allowed in rule templates.";

            auto body = lower_rule_template_expr_impl(lambda.body, simple_args, referenced_args);
            Hs::LPat p = {noloc, Hs::VarPattern({noloc,Hs::Var(pattern->name)})};

            if (auto L = body.to<Hs::LambdaExp>())
            {
                auto LE = *L;
                auto& pats = LE.match.patterns;
                pats.insert(pats.begin(), p);
                return LE;
            }
            else
                return Hs::LambdaExp({p}, {noloc, body});
        },
        // Compatibility behavior: rule templates share the model parser, so
        // sample(@dist) is parsed as Sample but means a Haskell sample call here.
        [&](const CM::Sample<CM::NoAnn>& sample) -> expression_ref
        {
            auto dist = lower_rule_template_expr_impl(sample.dist, simple_args, referenced_args);
            return {var("sample"), dist};
        }
    });
}

}

// Lowers a rule template with the code-generation semantics and returns both
// the Haskell expression and the set of referenced JSON argument names.
RuleTemplateLoweringResult lower_rule_template_expr(const CM::UntypedExpr& expr, const map<string, expression_ref>& simple_args)
{
    RuleTemplateLoweringResult result;
    result.expr = lower_rule_template_expr_impl(expr, simple_args, result.referenced_args);
    return result;
}

// Compatibility wrapper for existing codegen call sites that only need the
// lowered expression. Remove once all callers consume RuleTemplateLoweringResult.
expression_ref make_rule_template_expr(const CM::UntypedExpr& expr, const map<string, expression_ref>& simple_args)
{
    return lower_rule_template_expr(expr, simple_args).expr;
}
