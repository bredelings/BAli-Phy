#include <iostream>
#include <set>
#include <vector>
#include <algorithm>
#include "models/parse.H"
#include "util/myexception.H"
#include "util/string/convert.H"
#include "util/string/join.H"
#include "util/range.H" // for reverse( )
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

ptree add_sample(const ptree& p)
{
    ptree p2("sample");
    p2.children().push_back({"",p});
    return p2;
}

ptree add_submodel(ptree term, const ptree& submodel)
{
    if (term.count("submodel"))
        throw myexception()<<"Trying to specify a submodel with '+' when submodel already specified by keyword!";

    term.children().push_back({"submodel", submodel});
    return term;
}

// Converts parser-produced positional call arguments to rule keyword arguments
// directly on the model AST, while preserving calls to local function binders.
void handle_positional_args(UntypedExpr& expr, const Rules& R, const set<string>& local_functions = {})
{
    expr.visit(overloaded{
        // Rewrites ordinary call arguments after first normalizing the argument
        // subexpressions.
        [&](Call<NoAnn>& call)
        {
            for(auto& arg: call.args)
                if (arg.value)
                    handle_positional_args(*arg.value, R, local_functions);

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

            if (local_functions.count(call.function))
                return;

            if (not R.get_rule_for_func(call.function))
            {
                for(auto& arg: call.args)
                    if (not arg.name.empty())
                        throw myexception()<<"Named argument '"<<arg.name<<"' not allowed for function '"<<call.function<<"' with no rule.";
                return;
            }

            auto rule = R.require_rule_for_func(call.function);
            int i = 0;
            bool seen_keyword = false;
            vector<Arg<NoAnn>> args;

            for(auto& arg: call.args)
            {
                auto arg2 = std::move(arg);
                if (not arg2.name.empty())
                    seen_keyword = true;
                else if (seen_keyword)
                    throw myexception()<<"Positional argument after keyword argument in '"<<unparse(expr)<<"'!";
                else if (not arg2.value)
                {
                    i++;
                    continue;
                }
                else
                {
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
                handle_positional_args(element, R, local_functions);
        },
        // Recurses into tuple elements without rewriting the tuple node itself.
        [&](Tuple<NoAnn>& tuple)
        {
            for(auto& element: tuple.elements)
                handle_positional_args(element, R, local_functions);
        },
        // Recurses into declarations and the body without rewriting the let node
        // itself.
        [&](Let<NoAnn>& let)
        {
            auto local_functions2 = local_functions;
            for(auto& [name, value]: let.decls)
            {
                local_functions2.insert(name);
                handle_positional_args(value, R, local_functions2);
            }
            handle_positional_args(let.body, R, local_functions2);
        },
        // Recurses into the body without rewriting the lambda node itself.
        // Patterns contain no call arguments to normalize.
        [&](Lambda<NoAnn>& lambda)
        {
            handle_positional_args(lambda.body, R, local_functions);
        },
        // Recurses into the sampled distribution without rewriting the sample
        // node itself.
        [&](Sample<NoAnn>& sample)
        {
            handle_positional_args(sample.dist, R, local_functions);
        },
        [](auto&) {}
    });
}

// Converts parser-produced positional call arguments in declaration values to
// rule keyword arguments, while tracking earlier local declaration binders.
void handle_positional_args(Decls<NoAnn>& decls, const Rules& R)
{
    set<string> local_functions;
    for(auto& [name, value]: decls)
    {
        local_functions.insert(name);
        handle_positional_args(value, R, local_functions);
    }
}

// Parses one command-line model expression through the legacy grammar, then
// normalizes positional arguments directly on the untyped model AST.
UntypedExpr parse_model_expr(const Rules& R, const string& s, const string& what)
{
    auto model = parse_expression(s, what);
    handle_positional_args(model, R);
    return model;
}

// Parses command-line model declarations through the legacy grammar, then
// normalizes positional arguments directly on untyped AST declarations.
Decls<NoAnn> parse_model_decls(const Rules& R, const string& s)
{
    auto decls = parse_defs(s, "declarations");
    handle_positional_args(decls, R);
    return decls;
}

optional<ptree> peel_sample(ptree p)
{
    if (p.has_value<string>() and p.get_value<string>() == "sample")
	return p.children()[0].second;
    else if (p.has_value<string>() and p.get_value<string>() == "!let")
    {
        if (auto new_body = peel_sample(p.children()[1].second))
        {
            p.children()[1].second = *new_body;
            return p;
        }
    }

    return {};
}

optional<ptree> peel_sample_annotated(ptree p)
{
    auto& v = p.get_child("value");
    if (v.has_value<string>() and v.get_value<string>() == "sample")
	return v.children()[0].second;
    else if (v.has_value<string>() and v.get_value<string>() == "!let")
    {
        if (auto new_body = peel_sample_annotated(v.children()[1].second))
        {
            v.children()[1].second = *new_body;
            return p;
        }
    }

    return {};
}

bool is_operator(const string& s)
{
    return (s == "+" or s == "-" or s == "*" or s == "/");
}

// To properly parenthesize infix expressions, we could ... pass in a context argument
// that says when they are the left or right argument of an infix operator?

string unparse(const ptree& p)
{
    using namespace std::string_literals;

    if (p.is_null())
	return "null";
    if (p.is_a<int>())
	return convertToString(p.get_value<int>());
    else if (p.is_a<double>())
    {
	auto s = convertToString(p.get_value<double>());
	if (s.find('.') != std::string::npos)
	    while(s.size() > 3 and s.back() == '0')
		s.pop_back();
	return s;
    }
    else if (p.is_a<bool>())
	return convertToString(p.get_value<bool>());
    else if (p.is_a<string>() and is_constant(p))
	return p.get_value<string>();

    assert(p.has_value<string>());
    string s = p.get_value<string>();
    if (s == "!let")
    {
	string name = p.children()[0].first;
	return name+show_model(p.children()[0].second)+";"+unparse(p.children()[1].second);
    }
    else if (s == "function")
    {
	string name = unparse(p.children()[0].second);
	return "|"+name+":"+unparse(p.children()[1].second)+"|";
    }
    else if (s == "List")
    {
        vector<string> items;
        for(auto& [_,item]: p.children())
            items.push_back(unparse(item));
        return "[" + join(items,", ") + "]";
    }
    else if (s == "Tuple")
    {
        vector<string> items;
        for(auto& [_,item]: p.children())
            items.push_back(unparse(item));
        return "(" + join(items,", ") + ")";
    }
    else if (s == "sample")
	return "~" + unparse(p.children().begin()->second);
    else if (s == "negate")
	return "-" + unparse(p.children().begin()->second);
    else if (is_operator(s) and p.children().size() == 2)
    {
	// sometimes we might need parenthesis, right?
        return unparse(p.children()[0].second) + s + unparse(p.children()[1].second);
    }

    else if (s == "intToDouble")
	return unparse(p.get_child("x"));
    else if (s == "unit_mixture" or s == "multiMixtureModel")
	if (auto child = p.get_child_optional("submodel"))
	    return unparse(*child);
    vector<string> args;
    optional<string> submodel;
    bool positional = true;
    int pos = 0;
    for(const auto& [arg_name, arg]: p.children())
    {
	if (arg.is_null())
            positional = false;

	// Don't print submodel arguments: move out to submodel + <this>
	else if (arg_name == "submodel" and pos == 0)
	{
            positional = false;
	    assert(not submodel);
	    submodel = unparse(arg);
	}
	else if (positional)
	    args.push_back( unparse(arg) );
        else if (auto arg2 = peel_sample(arg))
            args.push_back( arg_name + "~" + unparse(*arg2));
        else
            args.push_back( arg_name + "=" + unparse(arg));

	pos++;
	// With annotations, we don't print get_state[state_name] if its a default value.
    }
    while (args.size() and args.back() == "")
	args.pop_back();
    if (not args.empty())
	s = s + "(" + join(args,", ") + ")";
    if (submodel)
	s = *submodel + " +> " + s;
    return s;
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
            else if ((s == "unit_mixture" or s == "multiMixtureModel"))
            {
                for(auto& arg: x.args)
                    if (arg.name == "submodel")
                        return unparse(require_arg_value(arg));
            }

            vector<string> args;
            optional<string> submodel;
            bool positional = true;
            int pos = 0;
            for(auto& arg: x.args)
            {
                if (not arg.value)
                    positional = false;
                else if (arg.name == "submodel" and pos == 0)
                {
                    positional = false;
                    assert(not submodel);
                    submodel = unparse(*arg.value);
                }
                else if (positional)
                    args.push_back(unparse(*arg.value));
                else if (auto arg2 = peel_sample(*arg.value))
                    args.push_back(arg.name + "~" + unparse(*arg2));
                else
                    args.push_back(arg.name + "=" + unparse(*arg.value));

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

// Preserves the old unparse signature for callers that still pass Rules.
string unparse(const ptree& p, const Rules&)
{
    return unparse(p);
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
            else if ((s == "unit_mixture" or s == "multiMixtureModel"))
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

string unparse_annotated(const ptree& ann)
{
    using namespace std::string_literals;

    ptree p = ann.get_child("value");

    if (p.is_null())
	return "null";
    if (p.is_a<int>())
	return convertToString(p.get_value<int>());
    else if (p.is_a<double>())
    {
	auto s = convertToString(p.get_value<double>());
	if (s.find('.') != std::string::npos)
	    while(s.size() > 3 and s.back() == '0')
		s.pop_back();
	return s;
    }
    else if (p.is_a<bool>())
	return convertToString(p.get_value<bool>());
    else if (p.is_a<string>() and is_constant(p))
	return p.get_value<string>();

    string s = p.get_value<string>();
    if (s == "!let")
    {
        vector<string> decls;
        for(auto& [name, value]: p.children()[0].second.children())
            decls.push_back(name + " " + show_model_annotated(value));
        return unparse_annotated(p.children()[1].second) + " where {" + join(decls, "; ") + "}";
    }
    else if (s == "function")
    {
	return "|"+unparse_annotated(p.children()[0].second)+":"+unparse_annotated(p.children()[1].second)+"|";
    }
    else if (s == "List")
    {
        bool list_of_pairs = true;
        vector<string> pairs;
        for(auto& [_,item]: p.children())
        {
            auto type = item.get_child("type");
            if (type.get_value<string>() == "Tuple" and type.children().size() == 2)
            {
                auto value = item.get_child("value");
                pairs.push_back(unparse_annotated(value.children()[0].second)+": "+unparse_annotated(value.children()[1].second));
            }
            else
            {
                list_of_pairs = false;
                break;
            }
        }
        if (list_of_pairs)
        {
            return "{" + join(pairs,", ") + "}";
        }

        vector<string> items;
        for(auto& [_,item]: p.children())
            items.push_back(unparse_annotated(item));
        return "[" + join(items,", ") + "]";
    }
    else if (s == "Tuple")
    {
        vector<string> items;
        for(auto& [_,item]: p.children())
            items.push_back(unparse_annotated(item));
        return "(" + join(items,", ") + ")";
    }
    else if (s == "sample")
	return "~" + unparse_annotated(p.children().begin()->second);
    else if (s == "negate")
	return "-" + unparse_annotated(p.children().begin()->second);
    else if (is_operator(s) and p.children().size() == 2)
    {
	// sometimes we might need parenthesis, right?
        return unparse_annotated(p.children()[0].second) + s + unparse_annotated(p.children()[1].second);
    }

    else if (s == "intToDouble")
	return unparse_annotated(p.get_child("x"));
    else if (s == "unit_mixture" or s == "multiMixtureModel")
	if (auto child = p.get_child_optional("submodel"))
	    return unparse_annotated(*child);
    vector<string> args;
    optional<string> submodel;
    bool positional = true;
    for(const auto& [arg_name, arg]: p.children())
    {
	if (arg.is_null())
            positional = false;

	// Don't print submodel arguments: move out to submodel + <this>
	else if (arg_name == "submodel")
	{
            positional = false;
	    assert(not submodel);
	    submodel = unparse_annotated(arg);
	}

	else if (arg.get_child("value").has_value<string>() and arg.get_child("value").get_value<string>() == "get_state" and arg.get_child("is_default_value") == true)
            positional = false;
	else if (auto suppress = arg.get_child_optional("suppress_default"); suppress and (*suppress == true) and arg.get_child("is_default_value") == true)
            positional = false;
	else if (positional)
	    args.push_back( unparse_annotated(arg) );
        else if (auto arg2 = peel_sample_annotated(arg))
            args.push_back( arg_name + "~" + unparse_annotated(*arg2));
        else
            args.push_back( arg_name + "=" + unparse_annotated(arg));
    }
    while (args.size() and args.back() == "")
	args.pop_back();
    if (not args.empty())
	s = s + "(" + join(args,", ") + ")";
    if (submodel)
	s = *submodel + " +> " + s;
    return s;
}

// Temporary compatibility helper: old annotated-ptree display still stores
// type annotations as ptree. Remove with the remaining annotated-ptree display.
string unparse_ptree_type(const ptree& p)
{
    if (p.is_null()) return "NOTYPE";

    vector<ptree> args;
    auto head = p;
    while(head.children().size() > 0)
    {
        args.push_back(head.children()[1].second);
        head = head.children()[0].second;
    }
    std::reverse(args.begin(), args.end());

    vector<string> sargs;
    for(const auto& arg: args)
	sargs.push_back( unparse_ptree_type(arg) );

    if (head == "Tuple")
	return "("+join(sargs,",")+")";
    else if (head == "Function")
    {
	assert(sargs.size() == 2);

	return sargs[0] + " -> " + sargs[1];
    }
    else if (not args.empty())
	return head.get_value<string>() + "<" + join(sargs,',') + ">";
    else
	return head;
}

string unparse_abbrev(ptree p, int length)
{
    string output = unparse(p);
    if (output.size() > length)
    {
	output = convertToString(p.value);
	if (p.children().size())
	    output += "(..)";
    }
    return output;
}

string show_model(ptree p)
{
    if (auto q = peel_sample(p))
	return "~ " + unparse(*q);
    else
	return "= " + unparse(p);
}

// Shows an untyped AST expression as an assignment or sampled model shorthand.
string show_model(const UntypedExpr& p)
{
    if (auto q = peel_sample(p))
	return "~ " + unparse(*q);
    else
	return "= " + unparse(p);
}


string show_model_annotated(ptree p)
{
    if (auto q = peel_sample_annotated(p))
	return "~ " + unparse_annotated(*q);
    else
	return "= " + unparse_annotated(p);
}

// Shows a typed AST expression as an assignment or sampled model shorthand.
string show_model_annotated(const TypedExpr& p)
{
    if (auto q = peel_sample_annotated(p))
	return "~ " + unparse_annotated(*q);
    else
	return "= " + unparse_annotated(p);
}


string show_model_abbrev(ptree p, int length)
{
    if (auto q = peel_sample(p))
	return "~ " + unparse_abbrev(*q, length-2);
    else
	return "= " + unparse_abbrev( p, length-2);
}

bool is_constant(const ptree& model)
{
    if (model.value_is_empty())
	throw myexception()<<"is_constant( ): got a null value!";

    if (model.is_a<int>() or model.is_a<double>() or model.is_a<bool>()) return true;

    assert(model.has_value<string>());

    string name = model.get_value<string>();

    return (name.size()>=2 and name[0] == '"' and name.back() == '"');
}
