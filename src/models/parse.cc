#include <iostream>
#include <vector>
#include <list>
#include "models/parse.H"
#include "models/model-expr-ptree.H"
#include "util/myexception.H"
#include "util/string/convert.H"
#include "util/string/join.H"
#include "util/range.H" // for reverse( )
#include "rules.H"
#include "unification.H"
#include "models/driver.hh"

using std::vector;
using std::list;
using std::string;
using std::pair;
using std::optional;


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

ptree parse_type(const string& s)
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

void ptree_map(std::function<void(ptree&)> f, ptree& p)
{
    std::function<void(ptree&)> f2;

    // generate recursive lambda
    f2 = [&f,&f2](ptree& q) {
	for(auto& child: q.children())
	    f2(child.second);
	f(q);
    };

    f2(p);
}

bool is_nontype_variable(const ptree& model)
{
    if (not model.is_a<string>()) return false;

    if (model.get_value<string>().empty()) return false;

    if (model.get_value<string>() == "List") return false;

    if (model.get_value<string>() == "Tuple") return false;

    return true;
}

bool is_list(const ptree& model)
{
    if (not model.has_value<string>()) return false;

    if (model.get_value<string>() != "List") return false;

    for(int i=0;i<model.children().size();i++)
    {
        if (not model.children()[i].first.empty())
            throw myexception()<<"List: entry "<<i+1<<" '"<<model.children()[i].first<<"="<<unparse(model.children()[i].second)<<"2 must not have a variable name!";
    }

    return true;
}

bool is_tuple(const ptree& model)
{
    if (not model.has_value<string>()) return false;

    if (model.get_value<string>() != "Tuple") return false;

    if (model.children().size() == 1)
        throw myexception()<<"Tuple's of 1 element not allowed:\n  "<<model.show();

    for(int i=0;i<model.children().size();i++)
    {
        if (not model.children()[i].first.empty())
            throw myexception()<<"Tuple: entry "<<i+1<<" '"<<model.children()[i].first<<"="<<unparse(model.children()[i].second)<<"2 must not have a variable name!";
    }

    return true;
}

bool is_pattern(const ptree& model)
{
    if (is_nontype_variable(model)) return true;

    if (is_list(model) or is_tuple(model))
    {
        for(auto& [_,value]: model.children())
            if (not is_pattern(value)) return false;
        return true;
    }

    return false;
}

bool is_function(const ptree& model)
{
    if (not model.has_value<string>()) return false;

    if (model.get_value<string>() != "function") return false;

    if (model.children().size() != 2)
        throw myexception()<<"function: got "<<model.children().size()<<" arguments, 2 arguments required.\n  "<<model.show();

    if (not model.children()[0].first.empty() or not is_pattern(model.children()[0].second))
        throw myexception()<<"function: first argument must be a variable or pattern!\n  "<<model.show();
    
    if (not model.children()[1].first.empty())
        throw myexception()<<"function: second argument must not have a variable name!\n  "<<model.show();

    return true;
}

void handle_positional_args(ptree& model, const Rules& R)
{
    if (model.children().size() == 0) return;

    assert(not model.is_null());
    assert(not model.value_is_empty());

    auto head = model.get_value<string>();

    if (head == "!let") return;

    if (head == "!Decls") return;

    if (is_function(model)) return;

    if (is_list(model)) return;


    if (head == "Pair")
    {
        if (model.children().size() != 2)
            throw myexception()<<"Pair's must have two elements: "<<unparse(model);

        model.value = string("Tuple");
        head = "Tuple";
    }

    if (is_tuple(model)) return;

    if (head == "get_state")
    {
        if (model.children().size() != 1)
	    throw myexception()<<"set_state: got "<<model.children().size()<<" arguments, 1 argument required.\n  "<<model.show();
	if (not model.children()[0].first.empty() or not model.children()[0].second.is_a<string>())
	    throw myexception()<<"set_state: first argument must be an unquoted state name!\n  "<<model.show();
        return;
    }

    if (not R.get_rule_for_func(head))
    {
	for(auto& [arg_name,value]: model.children())
	    if (not arg_name.empty())
		throw myexception()<<"Named argument '"<<arg_name<<"' not allowed for function '"<<head<<"' with no rule.";
	return;
    }

    auto rule = R.require_rule_for_func(head);

    int i=0;
    bool seen_keyword = false;

    ptree model2(head);

    for(auto& x: model.children())
    {
	pair<string,ptree> child = x;

	if (not child.first.empty())
	    seen_keyword = true;
	else if (seen_keyword)
	    throw myexception()<<"Positional argument after keyword argument in '"<<unparse(model)<<"'!";
	// Empty positional arguments argument can later have their value specified by keyword, so we ignore them completely
	else if (child.second.is_null())
	{
	    i++;
	    continue;
	}
	else
	{
	    auto keyword = get_keyword_for_positional_arg(rule, i);

	    if (model.count(keyword))
		throw myexception()<<"Trying to set value for "<<head<<"."<<keyword<<" both by position and by keyword: \n"<<unparse(model);

	    child.first = keyword;
	}

	// This shouldn't happen.
	if (child.first.empty()) throw myexception()<<"No keyword in argument for "<<head<<"?";

	model2.children().push_back(child);

	i++;
    }

    std::swap(model, model2);
}

// Parse strings of the form head[args] + head[args] + ... + head[args]
ptree parse(const Rules& R, const string& s, const string& what)
{
    // 1. Get the last head[args]
    auto model = parse_expression(s, what);

    // 2. Fill in keywords where they are not given
    auto f2 = [&](ptree& p) {handle_positional_args(p,R);};
    ptree_map(f2, model);

    return model;
}

ptree parse_defs(const Rules& R, const string& s)
{
    // 1. Parse the decls
    auto defs = parse_defs(s, "declarations");

    // 2. Fill in default arguments
    auto f2 = [&](ptree& p) {handle_positional_args(p,R);};
    for(auto& [var,value]: defs.children())
	ptree_map(f2, value);

    return defs;
}

UntypedModelExpr parse_model_expr(const Rules& R, const string& s, const string& what)
{
    return model_expr_from_ptree(parse(R, s, what));
}

ModelDecls<UntypedAnn> parse_model_decls(const Rules& R, const string& s)
{
    return model_decls_from_ptree(parse_defs(R, s));
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

optional<UntypedModelExpr> peel_sample(const UntypedModelExpr& expr)
{
    if (auto sample = std::get_if<Sample<UntypedAnn>>(&expr.node))
        return sample->dist.get();
    else if (auto let = std::get_if<Let<UntypedAnn>>(&expr.node))
    {
        if (auto new_body = peel_sample(let->body.get()))
        {
            auto result = expr;
            std::get<Let<UntypedAnn>>(result.node).body = Box<UntypedModelExpr>(*new_body);
            return result;
        }
    }

    return {};
}

string unparse(const ModelDecls<UntypedAnn>& decls)
{
    vector<string> items;
    for(auto& [name, value]: decls)
        items.push_back(name + " = " + unparse(value));
    return join(items, "; ");
}

string unparse(const UntypedModelExpr& expr)
{
    using namespace std::string_literals;

    return std::visit(overloaded{
        [](const IntLiteral& x) {return convertToString(x.value);},
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
        [](const MissingArg&) {return "null"s;},
        [](const GetState& x) {return "get_state(" + x.state_name + ")";},
        [](const List<UntypedAnn>& x)
        {
            vector<string> items;
            for(auto& item: x.elements)
                items.push_back(unparse(item));
            return "[" + join(items, ", ") + "]";
        },
        [](const Tuple<UntypedAnn>& x)
        {
            vector<string> items;
            for(auto& item: x.elements)
                items.push_back(unparse(item));
            return "(" + join(items, ", ") + ")";
        },
        [](const Let<UntypedAnn>& x)
        {
            return unparse(x.body.get()) + " where {" + unparse(x.decls) + "}";
        },
        [](const Lambda<UntypedAnn>& x)
        {
            return "|" + unparse(x.pattern.get()) + ":" + unparse(x.body.get()) + "|";
        },
        [](const Sample<UntypedAnn>& x)
        {
            return "~" + unparse(x.dist.get());
        },
        [](const Call<UntypedAnn>& x)
        {
            auto s = x.function;

            if (s == "negate" and x.args.size())
                return "-" + unparse(x.args.front().value.get());
            else if (is_operator(s) and x.args.size() == 2)
                return unparse(x.args[0].value.get()) + s + unparse(x.args[1].value.get());
            else if (s == "intToDouble")
            {
                for(auto& arg: x.args)
                    if (arg.name == "x")
                        return unparse(arg.value.get());
            }
            else if ((s == "unit_mixture" or s == "multiMixtureModel"))
            {
                for(auto& arg: x.args)
                    if (arg.name == "submodel")
                        return unparse(arg.value.get());
            }

            vector<string> args;
            optional<string> submodel;
            bool positional = true;
            int pos = 0;
            for(auto& arg: x.args)
            {
                if (std::holds_alternative<MissingArg>(arg.value->node))
                    positional = false;
                else if (arg.name == "submodel" and pos == 0)
                {
                    positional = false;
                    assert(not submodel);
                    submodel = unparse(arg.value.get());
                }
                else if (positional)
                    args.push_back(unparse(arg.value.get()));
                else if (auto arg2 = peel_sample(arg.value.get()))
                    args.push_back(arg.name + "~" + unparse(*arg2));
                else
                    args.push_back(arg.name + "=" + unparse(arg.value.get()));

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
    }, expr.node);
}

string unparse(const ptree& p, const Rules&)
{
    return unparse(p);
}

optional<TypedModelExpr> peel_sample_annotated(const TypedModelExpr& expr)
{
    if (auto sample = std::get_if<Sample<ModelAnn>>(&expr.node))
        return sample->dist.get();
    else if (auto let = std::get_if<Let<ModelAnn>>(&expr.node))
    {
        if (auto new_body = peel_sample_annotated(let->body.get()))
        {
            auto result = expr;
            std::get<Let<ModelAnn>>(result.node).body = Box<TypedModelExpr>(*new_body);
            return result;
        }
    }

    return {};
}

string unparse_typed_decls(const TypedModelDecls& decls)
{
    vector<string> items;
    for(auto& [name, value]: decls)
        items.push_back(name + " " + show_model_annotated(value));
    return join(items, "; ");
}

string unparse_annotated(const TypedModelExpr& expr)
{
    using namespace std::string_literals;

    return std::visit(overloaded{
        [](const IntLiteral& x) {return convertToString(x.value);},
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
        [](const MissingArg&) {return "null"s;},
        [](const GetState& x) {return "get_state(" + x.state_name + ")";},
        [](const Let<ModelAnn>& x)
        {
            return unparse_annotated(x.body.get()) + " where {" + unparse_typed_decls(x.decls) + "}";
        },
        [](const Lambda<ModelAnn>& x)
        {
            return "|" + unparse_annotated(x.pattern.get()) + ":" + unparse_annotated(x.body.get()) + "|";
        },
        [](const List<ModelAnn>& x)
        {
            bool list_of_pairs = true;
            vector<string> pairs;
            for(auto& item: x.elements)
            {
                auto type = item.ann.type;
                if (type.has_value<string>() and type.get_value<string>() == "Tuple" and type.children().size() == 2)
                {
                    auto tuple = std::get_if<Tuple<ModelAnn>>(&item.node);
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
        [](const Tuple<ModelAnn>& x)
        {
            vector<string> items;
            for(auto& item: x.elements)
                items.push_back(unparse_annotated(item));
            return "(" + join(items, ", ") + ")";
        },
        [](const Sample<ModelAnn>& x)
        {
            return "~" + unparse_annotated(x.dist.get());
        },
        [](const Call<ModelAnn>& x)
        {
            auto s = x.function;

            if (s == "negate" and x.args.size())
                return "-" + unparse_annotated(x.args.front().value.get());
            else if (is_operator(s) and x.args.size() == 2)
                return unparse_annotated(x.args[0].value.get()) + s + unparse_annotated(x.args[1].value.get());
            else if (s == "intToDouble")
            {
                for(auto& arg: x.args)
                    if (arg.name == "x")
                        return unparse_annotated(arg.value.get());
            }
            else if ((s == "unit_mixture" or s == "multiMixtureModel"))
            {
                for(auto& arg: x.args)
                    if (arg.name == "submodel")
                        return unparse_annotated(arg.value.get());
            }

            vector<string> args;
            optional<string> submodel;
            bool positional = true;
            for(auto& arg: x.args)
            {
                if (std::holds_alternative<MissingArg>(arg.value->node))
                    positional = false;
                else if (arg.name == "submodel")
                {
                    positional = false;
                    assert(not submodel);
                    submodel = unparse_annotated(arg.value.get());
                }
                else if (arg.is_default_value and std::holds_alternative<GetState>(arg.value->node))
                    positional = false;
                else if (arg.suppress_default and arg.is_default_value)
                    positional = false;
                else if (positional)
                    args.push_back(unparse_annotated(arg.value.get()));
                else if (auto arg2 = peel_sample_annotated(arg.value.get()))
                    args.push_back(arg.name + "~" + unparse_annotated(*arg2));
                else
                    args.push_back(arg.name + "=" + unparse_annotated(arg.value.get()));
            }
            while (args.size() and args.back() == "")
                args.pop_back();
            if (not args.empty())
                s = s + "(" + join(args, ", ") + ")";
            if (submodel)
                s = *submodel + " +> " + s;
            return s;
        }
    }, expr.node);
}

string unparse_annotated(const ptree& ann)
{
    using namespace std::string_literals;

    term_t p = ann.get_child("value");

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

string unparse_type(const ptree& p)
{
    if (p.is_null()) return "NOTYPE";

    auto [head,args] = get_type_apps(p);

    vector<string> sargs;
    for(const auto& arg: args)
	sargs.push_back( unparse_type(arg) );

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

string show_model(const UntypedModelExpr& p)
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

string show_model_annotated(const TypedModelExpr& p)
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

int add_arg_placeholder(ptree& p1, const ptree& arg)
{
    int n_placeholders = 0;

    for(auto& [key,value]: p1.children())
    {
	if (value == "_")
	{
	    n_placeholders++;
	    value = arg;
	}
    }

    return n_placeholders;
}


ptree add_arg(const ptree& p1, const ptree& p2)
{
    ptree p3 = p2;

    int n_placeholders = add_arg_placeholder(p3, p1);
    if (n_placeholders > 1)
	throw myexception()<<"Placeholder '_' may only occur once.";

    if (n_placeholders == 0)
	p3.children().insert(p3.children().begin(), {"",p1});

    return p3;
}
