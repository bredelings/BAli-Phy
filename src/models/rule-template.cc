#include "models/rule-template.H"

#include "computation/expression/apply.H"
#include "computation/expression/lambda.H"
#include "computation/expression/list.H"
#include "computation/expression/tuple.H"
#include "computation/expression/var.H"
#include "computation/haskell/haskell.H"
#include "computation/module.H"
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

// Wraps a synthetic Haskell expression with no source location, since binding
// templates are parsed from model JSON rather than Haskell source files.
Hs::LExp hs_exp(expression_ref expr)
{
    return {noloc, std::move(expr)};
}

// Builds an unresolved variable for codegen, or a local/resolved Haskell
// variable when inference supplies a compiled import context.
Hs::LExp hs_var(const string& name, const RuleTemplateLoweringOptions& options)
{
    if (options.local_vars.count(name))
        return hs_exp(Hs::Var(name));

    if (options.inference_module)
    {
        auto symbol = options.inference_module->lookup_symbol(name);
        if (options.resolved_symbols)
            options.resolved_symbols->push_back(symbol->name);
        return hs_exp(Hs::Var(symbol->name));
    }

    return hs_exp(var(name));
}

// Builds one synthetic Haskell application node with no source location.
Hs::LExp hs_apply(const Hs::LExp& head, const Hs::LExp& arg)
{
    return hs_exp(Hs::ApplyExp(head, arg));
}

// Converts a substituted template argument into located Haskell expression form,
// resolving inference locals while preserving arbitrary codegen expressions.
Hs::LExp hs_template_arg(expression_ref expr, const RuleTemplateLoweringOptions& options)
{
    if (auto v = expr.to<var>(); v and options.inference_module and options.local_vars.count(v->name))
        return hs_exp(Hs::Var(v->name));
    return hs_exp(std::move(expr));
}

// Converts a rule call/computed template from CmdModel to Haskell expression
// code and records every @arg reference seen while lowering.
Hs::LExp lower_rule_template_expr_impl(const CM::UntypedExpr& expr, const map<string,expression_ref>& simple_args, const RuleTemplateLoweringOptions& options, set<string>& referenced_args)
{
    return expr.visit(CM::overloaded{
        [](const CM::IntLiteral& x) -> Hs::LExp { return hs_exp(x.value); },
        [](const CM::DoubleLiteral& x) -> Hs::LExp { return hs_exp(x.value); },
        [](const CM::BoolLiteral& x) -> Hs::LExp { return hs_exp(x.value); },
        [](const CM::StringLiteral& x) -> Hs::LExp { return hs_exp(String(x.value)); },
        [&](const CM::Var& x) -> Hs::LExp { return hs_var(x.name, options); },
        // Looks up a rule-template argument reference in the already generated
        // argument environment.
        [&](const CM::ArgRef& x) -> Hs::LExp
        {
            referenced_args.insert(x.name);
            try
            {
                return hs_template_arg(simple_args.at(x.name), options);
            }
            catch(...)
            {
                throw myexception()<<"cannot find argument '"<<x.name<<"'";
            }
        },
        [](const CM::Placeholder&) -> Hs::LExp { throw myexception()<<"Placeholder is not allowed in rule templates."; },
        [](const CM::GetState&) -> Hs::LExp { throw myexception()<<"get_state is not allowed in rule templates."; },
        // Compiles a rule-template call as Haskell ApplyExp nodes, preserving
        // the legacy final-submodel rewrite.
        [&](const CM::Call<CM::NoAnn>& call) -> Hs::LExp
        {
            Hs::LExp E = hs_var(call.function, options);
            for(int i=0;i<call.args.size();i++)
            {
                auto& arg = call.args[i];
                if (not arg.name.empty())
                    throw myexception()<<"Named arguments are not allowed in rule templates.";
                if (not arg.value)
                    throw myexception()<<"Missing arguments are not allowed in rule templates.";

                auto arg_expr = lower_rule_template_expr_impl(*arg.value, simple_args, options, referenced_args);
                // Compatibility behavior: rule templates encode `submodel +> f`
                // as a final @submodel argument. Remove when bindings spell this directly.
                if (i == call.args.size()-1 and is_template_submodel_arg(arg))
                    E = hs_apply(hs_apply(hs_var("+>", options), arg_expr), E);
                else
                    E = hs_apply(E, arg_expr);
            }
            return E;
        },
        // Compiles a rule-template list by translating each element into a
        // located Haskell expression.
        [&](const CM::List<CM::NoAnn>& list) -> Hs::LExp
        {
            vector<Hs::LExp> located_args;
            for(auto& element: list.elements)
                located_args.push_back(lower_rule_template_expr_impl(element, simple_args, options, referenced_args));
            return hs_exp(Hs::List(located_args));
        },
        // Compiles a rule-template tuple by translating each element into a
        // located Haskell expression.
        [&](const CM::Tuple<CM::NoAnn>& tuple) -> Hs::LExp
        {
            vector<Hs::LExp> located_args;
            for(auto& element: tuple.elements)
                located_args.push_back(lower_rule_template_expr_impl(element, simple_args, options, referenced_args));
            return hs_exp(Hs::Tuple(located_args));
        },
        [](const CM::Let<CM::NoAnn>&) -> Hs::LExp { throw myexception()<<"let expressions are not allowed in rule templates."; },
        // Preserves the old template lambda support by folding adjacent lambda
        // nodes into one Haskell lambda expression.
        [&](const CM::Lambda<CM::NoAnn>& lambda) -> Hs::LExp
        {
            auto pattern = lambda.pattern.to<CM::VarPattern>();
            if (not pattern)
                throw myexception()<<"Only variable lambda patterns are allowed in rule templates.";

            auto lambda_options = options;
            lambda_options.local_vars.insert(pattern->name);
            auto body = lower_rule_template_expr_impl(lambda.body, simple_args, lambda_options, referenced_args);
            Hs::LPat p = {noloc, Hs::VarPattern({noloc,Hs::Var(pattern->name)})};

            if (auto L = unloc(body).to<Hs::LambdaExp>())
            {
                auto LE = *L;
                auto& pats = LE.match.patterns;
                pats.insert(pats.begin(), p);
                return hs_exp(LE);
            }
            else
                return hs_exp(Hs::LambdaExp({p}, body));
        },
        // Compatibility behavior: rule templates share the model parser, so
        // sample(@dist) is parsed as Sample but means a Haskell sample call here.
        [&](const CM::Sample<CM::NoAnn>& sample) -> Hs::LExp
        {
            auto dist = lower_rule_template_expr_impl(sample.dist, simple_args, options, referenced_args);
            return hs_apply(hs_var("sample", options), dist);
        }
    });
}

}

// Lowers a rule template with the code-generation semantics and returns both
// the Haskell expression and the set of referenced JSON argument names.
RuleTemplateLoweringResult lower_rule_template_expr(const CM::UntypedExpr& expr, const map<string, expression_ref>& simple_args)
{
    return lower_rule_template_expr(expr, simple_args, {});
}

// Lowers a rule template with optional inference name resolution layered over
// the same structural lowering used by code generation.
RuleTemplateLoweringResult lower_rule_template_expr(const CM::UntypedExpr& expr, const map<string, expression_ref>& simple_args, const RuleTemplateLoweringOptions& options)
{
    RuleTemplateLoweringResult result;
    result.expr = lower_rule_template_expr_impl(expr, simple_args, options, result.referenced_args);
    return result;
}

// Compatibility wrapper: current codegen consumes expression_ref directly.
// Remove once codegen carries located Haskell expressions through this boundary.
expression_ref make_rule_template_expr(const CM::UntypedExpr& expr, const map<string, expression_ref>& simple_args)
{
    return unloc(lower_rule_template_expr(expr, simple_args).expr);
}
