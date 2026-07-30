#include <iostream>
#include <set>
#include <vector>
#include "computation/message.H"
#include "models/parse.H"
#include "util/myexception.H"
#include "util/string/convert.H"
#include "util/string/join.H"
#include "rules.H"
#include "unification.H"
#include "models/driver.hh"

using std::vector;
using std::string;
using std::pair;
using std::optional;
using std::set;
using namespace CmdModel;

// Turn an expression of the form head[arg1, arg2, ..., argn] -> {head, arg1, arg2, ..., argn}.
vector<string> split_args(string s)
{
    if (s.empty()) throw myexception()<<"Term is empty!";
    vector<string> args;

    // 1. Get the head
    int pos = s.find('[');
    if (pos == -1)
    {
	args = {s};
	return args;
    }

    if (pos == 0)
        args = {"List"};
    else
        args = { s.substr(0,pos) };
    s = s.substr(pos);

    //2. Get the arguments from '[arg1, arg2, ... , argn]'.
    int depth = 0;
    int start = 1;
    for(int i=0;i<s.size();i++)
    {
	// Record finished arg.
	if ((s[i] == ']' or s[i] == ',') and depth == 1)
	{
	    assert(i >= start);
            auto arg = s.substr(start,i-start);
            if (not arg.empty() or s[i] == ',')
                args.push_back(arg);
	    start = i+1;
	}

	if (s[i] == '[')
	    depth++;
	else if (s[i] == ']')
	{
	    depth--;
	    if (depth < 0) throw myexception()<<"Malformed expression '"<<s<<"': ']' has no matching '['";
	}
    }
    if (depth > 0) throw myexception()<<"Malformed expression '"<<s<<"': missing ']'";

    return args;
}

CM::Type parse_type(const string& s)
{
    return parse_type(s,"type");
}

namespace
{

struct PendingOperator
{
    Located<string> symbol;
    ::Infix::Fixity fixity;
    bool prefix;
};

// Render a fixity in the syntax used by declarations and diagnostics.
string show_fixity(const ::Infix::Fixity& fixity)
{
    string name;
    if (fixity.associativity == ::Infix::Associativity::left)
        name = "infixl";
    else if (fixity.associativity == ::Infix::Associativity::right)
        name = "infixr";
    else if (fixity.associativity == ::Infix::Associativity::none)
        name = "infix";
    else
        name = "unknown fixity";
    return name + " " + std::to_string(fixity.precedence);
}

void resolve_fixities(UntypedExpr&, const Rules&, const set<string>&, vector<Message>&);

// Adds a stacked argument to a call, replacing one placeholder before falling
// back to prepending a positional argument.
UntypedExpr add_stacked_arg(UntypedExpr arg, UntypedExpr callee)
{
    if (auto var = callee.to<CM::Var>())
    {
        auto name = var->name;
        callee = {CM::NoAnn{}, CM::Call<CM::NoAnn>{std::move(name), {}}};
    }

    auto call = callee.to<CM::Call<CM::NoAnn>>();
    if (not call)
        throw myexception()<<"Right side of +> must be a function call or function name.";

    optional<int> placeholder;
    for(int i = 0; i < call->args.size(); i++)
    {
        auto& call_arg = call->args[i];
        if (call_arg.value and call_arg.value->is<CM::Placeholder>())
        {
            if (placeholder)
                throw myexception()<<"Placeholder '_' may only occur once.";
            placeholder = i;
        }
    }

    CM::Arg<CM::NoAnn> new_arg{"", std::move(arg), false, false, std::nullopt};
    if (placeholder)
        call->args[*placeholder] = std::move(new_arg);
    else
        call->args.insert(call->args.begin(), std::move(new_arg));

    return callee;
}

// Reduce the latest unary or binary operator after its right operand is complete.
void reduce_operator(vector<UntypedExpr>& operands, vector<PendingOperator>& operators)
{
    assert(not operators.empty());
    auto op = std::move(operators.back());
    operators.pop_back();

    assert(not operands.empty());
    auto rhs = std::move(operands.back());
    operands.pop_back();

    if (op.prefix)
    {
        vector<CM::Arg<CM::NoAnn>> args;
        args.push_back({"", std::move(rhs), false, false, std::nullopt});
        operands.push_back({CM::NoAnn{}, CM::Call<CM::NoAnn>{"negate", std::move(args)}});
        return;
    }

    assert(not operands.empty());
    auto lhs = std::move(operands.back());
    operands.pop_back();

    if (op.symbol.obj == "+>")
        operands.push_back(add_stacked_arg(std::move(lhs), std::move(rhs)));
    else
    {
        vector<CM::Arg<CM::NoAnn>> args;
        args.push_back({"", std::move(lhs), false, false, std::nullopt});
        args.push_back({"", std::move(rhs), false, false, std::nullopt});
        operands.push_back({CM::NoAnn{}, CM::Call<CM::NoAnn>{std::move(op.symbol.obj), std::move(args)}});
    }
}

// Resolve one flat operator chain using the shunting-yard invariant: the stack
// contains exactly the operators whose right operands are not yet complete.
UntypedExpr resolve_infix(CM::Infix<NoAnn> infix, const Rules& R, const set<string>& bound_names,
                          vector<Message>& messages)
{
    vector<UntypedExpr> operands;
    vector<PendingOperator> operators;

    // Push prefix minuses before their operand so higher-precedence binary
    // operators are reduced before the synthetic infixl-6 negation.
    auto push_operand = [&](UntypedExpr operand)
    {
        while (auto neg = operand.to<CM::PrefixNeg<NoAnn>>())
        {
            auto minus = std::move(neg->minus);
            auto inner_operand = std::move(neg->operand);
            operators.push_back({std::move(minus), {::Infix::Associativity::left, 6}, true});
            operand = std::move(inner_operand);
        }
        resolve_fixities(operand, R, bound_names, messages);
        operands.push_back(std::move(operand));
    };

    push_operand(std::move(infix.first));
    for(auto& [symbol, operand]: infix.rest)
    {
        // +> is dedicated stacking syntax, so lexical variables cannot shadow it.
        auto fixity = bound_names.contains(symbol.obj) and symbol.obj != "+>"
                    ? ::Infix::Fixity{::Infix::Associativity::left, 9}
                    : R.get_fixity(symbol.obj);
        PendingOperator incoming{std::move(symbol), fixity, false};
        while (not operators.empty())
        {
            auto comparison = ::Infix::compare(operators.back().fixity, incoming.fixity);
            if (comparison == ::Infix::Comparison::associate_right)
                break;

            if (comparison == ::Infix::Comparison::conflict)
            {
                auto& previous = operators.back();
                Note note;
                note<<"Must use parentheses to order operators '"<<previous.symbol.obj<<"' ("
                    <<show_fixity(previous.fixity)<<") and '"<<incoming.symbol.obj<<"' ("
                    <<show_fixity(incoming.fixity)<<").";
                messages.push_back(error(incoming.symbol.loc, note));
            }
            reduce_operator(operands, operators);
        }

        operators.push_back(std::move(incoming));
        push_operand(std::move(operand));
    }

    while (not operators.empty())
        reduce_operator(operands, operators);

    assert(operands.size() == 1);
    return std::move(operands.back());
}

// Recursively resolve parser-only operator syntax before later model passes see it.
void resolve_fixities(UntypedExpr& expr, const Rules& R, const set<string>& bound_names, vector<Message>& messages)
{
    if (auto infix = expr.to<CM::Infix<NoAnn>>())
    {
        expr = resolve_infix(std::move(*infix), R, bound_names, messages);
        return;
    }

    if (auto neg = expr.to<CM::PrefixNeg<NoAnn>>())
    {
        auto operand = std::move(neg->operand);
        resolve_fixities(operand, R, bound_names, messages);
        vector<CM::Arg<CM::NoAnn>> args;
        args.push_back({"", std::move(operand), false, false, std::nullopt});
        expr = {CM::NoAnn{}, CM::Call<CM::NoAnn>{"negate", std::move(args)}};
        return;
    }

    expr.visit(overloaded{
        // Resolve operators in every supplied call argument.
        [&](Call<NoAnn>& call)
        {
            for(auto& arg: call.args)
                if (arg.value)
                    resolve_fixities(*arg.value, R, bound_names, messages);
        },
        // Resolve each element of a list expression.
        [&](List<NoAnn>& list)
        {
            for(auto& element: list.elements)
                resolve_fixities(element, R, bound_names, messages);
        },
        // Resolve each element of a tuple expression.
        [&](Tuple<NoAnn>& tuple)
        {
            for(auto& element: tuple.elements)
                resolve_fixities(element, R, bound_names, messages);
        },
        // Resolve declaration values and the body of a let expression.
        [&](Let<NoAnn>& let)
        {
            auto body_scope = bound_names;
            for(auto& [name, value]: let.decls)
            {
                body_scope.insert(name);
                resolve_fixities(value, R, body_scope, messages);
            }
            resolve_fixities(let.body, R, body_scope, messages);
        },
        // Resolve operators nested in a lambda body.
        [&](Lambda<NoAnn>& lambda)
        {
            auto body_scope = bound_names;
            auto pattern_names = vars_in_pattern(lambda.pattern);
            body_scope.insert(pattern_names.begin(), pattern_names.end());
            resolve_fixities(lambda.body, R, body_scope, messages);
        },
        // Resolve operators nested in a sampled distribution.
        [&](Sample<NoAnn>& sample)
        {
            resolve_fixities(sample.dist, R, bound_names, messages);
        },
        [](auto&) {}
    });
}

// Resolve all declaration values while collecting conflicts across the source.
void resolve_fixities(Decls<NoAnn>& decls, const Rules& R, const set<string>& initial_bound_names,
                      vector<Message>& messages)
{
    auto bound_names = initial_bound_names;
    for(auto& [name, value]: decls)
    {
        bound_names.insert(name);
        resolve_fixities(value, R, bound_names, messages);
    }
}

}

// Resolve one parsed expression and report all fixity conflicts against its source.
void resolve_model_fixities(UntypedExpr& expr, const Rules& R, const string& source, const string& what,
                            const set<string>& bound_names)
{
    vector<Message> messages;
    resolve_fixities(expr, R, bound_names, messages);
    if (not messages.empty())
        show_messages({what, source}, std::cerr, messages);
}

// Converts parser-produced positional call arguments to rule keyword arguments
// directly on the model AST, while preserving calls to local function binders.
void handle_positional_args(UntypedExpr& expr, const Rules& R, const set<string>& bound_names)
{
    expr.visit(overloaded{
        // Rewrites ordinary call arguments after first normalizing the argument
        // subexpressions.
        [&](Call<NoAnn>& call)
        {
            for(auto& arg: call.args)
                if (arg.value)
                    handle_positional_args(*arg.value, R, bound_names);

            if (call.args.empty())
                return;

            if (call.function == "Pair")
            {
                if (call.args.size() != 2)
                    throw myexception()<<"Pair's must have two elements: "<<unparse(expr);

                Tuple<NoAnn> tuple;
                for(auto& arg: call.args)
                {
                    if (not arg.name.empty())
                        throw myexception()<<"Tuple: entry '"<<arg.name<<"="<<unparse(require_arg_value(arg))<<"' must not have a variable name!";
                    if (not arg.value)
                        throw myexception()<<"Tuple element must have a value.";
                    tuple.elements.push_back(std::move(*arg.value));
                }
                expr.node = Box<Tuple<NoAnn>>(new Tuple<NoAnn>(std::move(tuple)));
                return;
            }

            if (bound_names.contains(call.function))
                return;

            if (not R.get_rule_for_func(call.function))
            {
                for(auto& arg: call.args)
                    if (not arg.name.empty())
                        throw myexception()<<"Named argument '"<<arg.name<<"' not allowed for function '"<<call.function<<"' with no rule.";
                return;
            }

            auto rule = R.require_rule_for_func(call.function);
            bool seen_keyword = false;
            for(const auto& arg: call.args)
            {
                if (not arg.name.empty())
                    seen_keyword = true;
                else if (seen_keyword)
                    throw myexception()<<"Positional argument after keyword argument in '"<<unparse(expr)<<"'!";
            }

            int i = 0;
            vector<Arg<NoAnn>> args;

            for(auto& arg: call.args)
            {
                auto arg2 = std::move(arg);
                if (arg2.name.empty())
                {
                    if (not arg2.value)
                    {
                        i++;
                        continue;
                    }

                    auto keyword = get_keyword_for_positional_arg(rule, i);
                    for(auto& existing: call.args)
                        if (existing.name == keyword)
                            throw myexception()<<"Trying to set value for "<<call.function<<"."<<keyword<<" both by position and by keyword: \n"<<unparse(expr);
                    arg2.name = keyword;
                }

                if (arg2.name.empty())
                    throw myexception()<<"No keyword in argument for "<<call.function<<"?";
                args.push_back(std::move(arg2));
                i++;
            }

            call.args = std::move(args);
        },
        // Recurses into list elements without rewriting the list node itself.
        [&](List<NoAnn>& list)
        {
            for(auto& element: list.elements)
                handle_positional_args(element, R, bound_names);
        },
        // Recurses into tuple elements without rewriting the tuple node itself.
        [&](Tuple<NoAnn>& tuple)
        {
            for(auto& element: tuple.elements)
                handle_positional_args(element, R, bound_names);
        },
        // Recurses into declarations and the body without rewriting the let node
        // itself.
        [&](Let<NoAnn>& let)
        {
            auto body_scope = bound_names;
            for(auto& [name, value]: let.decls)
            {
                body_scope.insert(name);
                handle_positional_args(value, R, body_scope);
            }
            handle_positional_args(let.body, R, body_scope);
        },
        // Normalizes the lambda body in the scope introduced by its pattern.
        [&](Lambda<NoAnn>& lambda)
        {
            auto body_scope = bound_names;
            auto pattern_names = vars_in_pattern(lambda.pattern);
            body_scope.insert(pattern_names.begin(), pattern_names.end());
            handle_positional_args(lambda.body, R, body_scope);
        },
        // Recurses into the sampled distribution without rewriting the sample
        // node itself.
        [&](Sample<NoAnn>& sample)
        {
            handle_positional_args(sample.dist, R, bound_names);
        },
        [](auto&) {}
    });
}

// Converts parser-produced positional call arguments in declaration values to
// rule keyword arguments, while tracking earlier local declaration binders.
void handle_positional_args(Decls<NoAnn>& decls, const Rules& R, const set<string>& initial_bound_names)
{
    auto bound_names = initial_bound_names;
    for(auto& [name, value]: decls)
    {
        bound_names.insert(name);
        handle_positional_args(value, R, bound_names);
    }
}

// Parses one command-line model expression through the legacy grammar, then
// normalizes positional arguments directly on the untyped model AST.
UntypedExpr parse_model_expr(const Rules& R, const string& s, const string& what, const set<string>& bound_names)
{
    auto model = parse_expression(s, what);
    resolve_model_fixities(model, R, s, what, bound_names);
    handle_positional_args(model, R, bound_names);
    return model;
}

// Parses command-line model declarations through the legacy grammar, then
// normalizes positional arguments directly on untyped AST declarations.
Decls<NoAnn> parse_model_decls(const Rules& R, const string& s, const set<string>& bound_names)
{
    const string what = "declarations";
    auto decls = parse_defs(s, what);
    vector<Message> messages;
    resolve_fixities(decls, R, bound_names, messages);
    if (not messages.empty())
        show_messages({what, s}, std::cerr, messages);
    handle_positional_args(decls, R, bound_names);
    return decls;
}

bool is_operator(const string& s)
{
    return (s == "+" or s == "-" or s == "*" or s == "/");
}

// Removes leading sample sugar from an untyped AST expression, preserving a
// surrounding let when the sampled body can be peeled.
optional<UntypedExpr> peel_sample(const UntypedExpr& expr)
{
    if (auto sample = expr.to<Sample<NoAnn>>())
        return sample->dist;
    else if (auto let = expr.to<Let<NoAnn>>())
    {
        if (auto new_body = peel_sample(let->body))
        {
            auto result = expr;
            result.as<Let<NoAnn>>().body = *new_body;
            return result;
        }
    }

    return {};
}

// Renders untyped AST declarations in the same compact form as legacy decls.
string unparse(const Decls<NoAnn>& decls)
{
    vector<string> items;
    for(auto& [name, value]: decls)
        items.push_back(name + " = " + unparse(value));
    return join(items, "; ");
}

// Renders an untyped model pattern using the lambda-pattern syntax.
string unparse(const UntypedPattern& pattern)
{
    return pattern.visit(overloaded{
        [](const VarPattern& x) {return x.name;},
        // Renders tuple patterns in the same parenthesized tuple syntax as
        // tuple expressions.
        [](const TuplePattern<NoAnn>& x)
        {
            vector<string> items;
            for(auto& item: x.elements)
                items.push_back(unparse(item));
            return "(" + join(items, ", ") + ")";
        }
    });
}

// Renders an untyped model AST expression using the legacy command-line syntax.
string unparse(const UntypedExpr& expr)
{
    using namespace std::string_literals;

    return expr.visit(overloaded{
        [](const IntLiteral& x) {return convertToString(x.value);},
        // Renders doubles while trimming insignificant trailing zeroes.
        [](const DoubleLiteral& x)
        {
            auto s = convertToString(x.value);
            if (s.find('.') != string::npos)
                while(s.size() > 3 and s.back() == '0')
                    s.pop_back();
            return s;
        },
        [](const BoolLiteral& x) {return convertToString(x.value);},
        [](const StringLiteral& x) {return "\"" + x.value + "\"";},
        [](const Var& x) {return x.name;},
        [](const ArgRef& x) {return "@" + x.name;},
        [](const Placeholder&) {return "_"s;},
        [](const GetState& x) {return "get_state(" + x.state_name + ")";},
        // Displays an unresolved chain in source order so parser and fixity
        // diagnostics retain the expression that the user wrote.
        [](const CM::Infix<NoAnn>& x)
        {
            auto result = unparse(x.first);
            for(auto& [op, operand]: x.rest)
                result += " " + op.obj + " " + unparse(operand);
            return "(" + result + ")";
        },
        [](const CM::PrefixNeg<NoAnn>& x) {return x.minus.obj + unparse(x.operand);},
        // Renders list elements in command-line bracket syntax.
        [](const List<NoAnn>& x)
        {
            vector<string> items;
            for(auto& item: x.elements)
                items.push_back(unparse(item));
            return "[" + join(items, ", ") + "]";
        },
        // Renders tuple elements in command-line parenthesized syntax.
        [](const Tuple<NoAnn>& x)
        {
            vector<string> items;
            for(auto& item: x.elements)
                items.push_back(unparse(item));
            return "(" + join(items, ", ") + ")";
        },
        [](const Let<NoAnn>& x)
        {
            return unparse(x.body) + " where {" + unparse(x.decls) + "}";
        },
        [](const Lambda<NoAnn>& x)
        {
            return "|" + unparse(x.pattern) + ":" + unparse(x.body) + "|";
        },
        [](const Sample<NoAnn>& x)
        {
            return "~" + unparse(x.dist);
        },
        // Renders calls using legacy shorthand for operators, conversions,
        // submodels, samples, and named arguments.
        [](const Call<NoAnn>& x)
        {
            auto s = x.function;

            if (s == "negate" and x.args.size())
                return "-" + unparse(require_arg_value(x.args.front()));
            else if (is_operator(s) and x.args.size() == 2)
                return unparse(require_arg_value(x.args[0])) + s + unparse(require_arg_value(x.args[1]));
            else if (s == "intToDouble")
            {
                for(auto& arg: x.args)
                    if (arg.name == "x")
                        return unparse(require_arg_value(arg));
            }
            else if (s == "unit_mixture")
            {
                for(auto& arg: x.args)
                    if (arg.name == "submodel")
                        return unparse(require_arg_value(arg));
            }

            vector<string> args;
            optional<string> submodel;
            int pos = 0;
            for(auto& arg: x.args)
            {
                if (not arg.value)
                {
                    pos++;
                    continue;
                }

                if (arg.name == "submodel" and pos == 0)
                {
                    assert(not submodel);
                    submodel = unparse(*arg.value);
                }
                else if (arg.name.empty())
                    args.push_back(unparse(*arg.value));
                else
                {
                    auto arg2 = peel_sample(*arg.value);
                    if (arg2)
                        args.push_back(arg.name + "~" + unparse(*arg2));
                    else
                        args.push_back(arg.name + "=" + unparse(*arg.value));
                }

                pos++;
            }
            while (args.size() and args.back() == "")
                args.pop_back();
            if (not args.empty())
                s = s + "(" + join(args, ", ") + ")";
            if (submodel)
                s = *submodel + " +> " + s;
            return s;
        }
    });
}

// Removes leading sample sugar from a typed AST expression, preserving a
// surrounding let when the sampled body can be peeled.
optional<TypedExpr> peel_sample_annotated(const TypedExpr& expr)
{
    if (auto sample = expr.to<Sample<Ann>>())
        return sample->dist;
    else if (auto let = expr.to<Let<Ann>>())
    {
        if (auto new_body = peel_sample_annotated(let->body))
        {
            auto result = expr;
            result.as<Let<Ann>>().body = *new_body;
            return result;
        }
    }

    return {};
}

// Renders typed AST declarations using the annotated model display form.
string unparse_typed_decls(const TypedDecls& decls)
{
    vector<string> items;
    for(auto& [name, value]: decls)
        items.push_back(name + " " + show_model_annotated(value));
    return join(items, "; ");
}

// Renders a typed model pattern using the annotated lambda-pattern syntax.
string unparse_annotated(const TypedPattern& pattern)
{
    return pattern.visit(overloaded{
        [](const VarPattern& x) {return x.name;},
        // Renders tuple patterns in the same parenthesized tuple syntax as
        // tuple expressions.
        [](const TuplePattern<Ann>& x)
        {
            vector<string> items;
            for(auto& item: x.elements)
                items.push_back(unparse_annotated(item));
            return "(" + join(items, ", ") + ")";
        }
    });
}

// Renders a typed model AST expression using the annotated command-line syntax.
string unparse_annotated(const TypedExpr& expr)
{
    using namespace std::string_literals;

    return expr.visit(overloaded{
        [](const IntLiteral& x) {return convertToString(x.value);},
        // Renders doubles while trimming insignificant trailing zeroes.
        [](const DoubleLiteral& x)
        {
            auto s = convertToString(x.value);
            if (s.find('.') != string::npos)
                while(s.size() > 3 and s.back() == '0')
                    s.pop_back();
            return s;
        },
        [](const BoolLiteral& x) {return convertToString(x.value);},
        [](const StringLiteral& x) {return "\"" + x.value + "\"";},
        [](const Var& x) {return x.name;},
        [](const ArgRef& x) {return "@" + x.name;},
        [](const Placeholder&) {return "_"s;},
        [](const GetState& x) {return "get_state(" + x.state_name + ")";},
        // Displays an unexpected typed chain rather than losing its structure
        // if an internal-error diagnostic reaches annotated unparsing.
        [](const CM::Infix<Ann>& x)
        {
            auto result = unparse_annotated(x.first);
            for(auto& [op, operand]: x.rest)
                result += " " + op.obj + " " + unparse_annotated(operand);
            return "(" + result + ")";
        },
        [](const CM::PrefixNeg<Ann>& x) {return x.minus.obj + unparse_annotated(x.operand);},
        [](const Let<Ann>& x)
        {
            return unparse_annotated(x.body) + " where {" + unparse_typed_decls(x.decls) + "}";
        },
        [](const Lambda<Ann>& x)
        {
            return "|" + unparse_annotated(x.pattern) + ":" + unparse_annotated(x.body) + "|";
        },
        // Renders typed lists, preserving the legacy map-like display for lists
        // of string/value pairs.
        [](const List<Ann>& x)
        {
            bool list_of_pairs = true;
            vector<string> pairs;
            for(auto& item: x.elements)
            {
                auto [head, args] = get_type_apps(item.ann.type);
                if (head == "Tuple" and args.size() == 2)
                {
                    auto tuple = item.to<Tuple<Ann>>();
                    if (not tuple or tuple->elements.size() != 2)
                    {
                        list_of_pairs = false;
                        break;
                    }
                    pairs.push_back(unparse_annotated(tuple->elements[0]) + ": " + unparse_annotated(tuple->elements[1]));
                }
                else
                {
                    list_of_pairs = false;
                    break;
                }
            }
            if (list_of_pairs)
                return "{" + join(pairs, ", ") + "}";

            vector<string> items;
            for(auto& item: x.elements)
                items.push_back(unparse_annotated(item));
            return "[" + join(items, ", ") + "]";
        },
        // Renders typed tuple elements in command-line parenthesized syntax.
        [](const Tuple<Ann>& x)
        {
            vector<string> items;
            for(auto& item: x.elements)
                items.push_back(unparse_annotated(item));
            return "(" + join(items, ", ") + ")";
        },
        [](const Sample<Ann>& x)
        {
            return "~" + unparse_annotated(x.dist);
        },
        // Renders typed calls using legacy shorthand for operators, conversions,
        // defaults, submodels, samples, and named arguments.
        [](const Call<Ann>& x)
        {
            auto s = x.function;

            if (s == "negate" and x.args.size())
                return "-" + unparse_annotated(require_arg_value(x.args.front()));
            else if (is_operator(s) and x.args.size() == 2)
                return unparse_annotated(require_arg_value(x.args[0])) + s + unparse_annotated(require_arg_value(x.args[1]));
            else if (s == "intToDouble")
            {
                for(auto& arg: x.args)
                    if (arg.name == "x")
                        return unparse_annotated(require_arg_value(arg));
            }
            else if (s == "unit_mixture")
            {
                for(auto& arg: x.args)
                    if (arg.name == "submodel")
                        return unparse_annotated(require_arg_value(arg));
            }

            vector<string> args;
            optional<string> submodel;
            bool positional = true;
            for(auto& arg: x.args)
            {
                if (not arg.value)
                    positional = false;
                else if (arg.name == "submodel")
                {
                    positional = false;
                    assert(not submodel);
                    submodel = unparse_annotated(*arg.value);
                }
                else if (arg.is_default_value and arg.value->is<GetState>())
                    positional = false;
                else if (arg.suppress_default and arg.is_default_value)
                    positional = false;
                else if (positional)
                    args.push_back(unparse_annotated(*arg.value));
                else if (auto arg2 = peel_sample_annotated(*arg.value))
                    args.push_back(arg.name + "~" + unparse_annotated(*arg2));
                else
                    args.push_back(arg.name + "=" + unparse_annotated(*arg.value));
            }
            while (args.size() and args.back() == "")
                args.pop_back();
            if (not args.empty())
                s = s + "(" + join(args, ", ") + ")";
            if (submodel)
                s = *submodel + " +> " + s;
            return s;
        }
    });
}

// Shows an untyped AST expression as an assignment or sampled model shorthand.
string show_model(const UntypedExpr& p)
{
    if (auto q = peel_sample(p))
	return "~ " + unparse(*q);
    else
	return "= " + unparse(p);
}

// Shows a typed AST expression as an assignment or sampled model shorthand.
string show_model_annotated(const TypedExpr& p)
{
    if (auto q = peel_sample_annotated(p))
	return "~ " + unparse_annotated(*q);
    else
	return "= " + unparse_annotated(p);
}
