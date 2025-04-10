#include <iostream>
#include <vector>
#include <list>
#include "models/parse.H"
#include "util/myexception.H"
#include "util/string/join.H"
#include "util/range.H" // for reverse( )
#include "rules.H"
#include "compile.H"
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
    p2.push_back({"",p});
    return p2;
}

ptree add_submodel(ptree term, const ptree& submodel)
{
    if (term.count("submodel"))
        throw myexception()<<"Trying to specify a submodel with '+' when submodel already specified by keyword!";

    term.push_back({"submodel", submodel});
    return term;
}

void ptree_map(std::function<void(ptree&)> f, ptree& p)
{
    std::function<void(ptree&)> f2;

    // generate recursive lambda
    f2 = [&f,&f2](ptree& q) {
	for(auto& child: q)
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

    for(int i=0;i<model.size();i++)
    {
        if (not model[i].first.empty())
            throw myexception()<<"List: entry "<<i+1<<" '"<<model[i].first<<"="<<unparse(model[i].second)<<"2 must not have a variable name!";
    }

    return true;
}

bool is_tuple(const ptree& model)
{
    if (not model.has_value<string>()) return false;

    if (model.get_value<string>() != "Tuple") return false;

    if (model.size() == 1)
        throw myexception()<<"Tuple's of 1 element not allowed:\n  "<<model.show();

    for(int i=0;i<model.size();i++)
    {
        if (not model[i].first.empty())
            throw myexception()<<"Tuple: entry "<<i+1<<" '"<<model[i].first<<"="<<unparse(model[i].second)<<"2 must not have a variable name!";
    }

    return true;
}

bool is_pattern(const ptree& model)
{
    if (is_nontype_variable(model)) return true;

    if (is_list(model) or is_tuple(model))
    {
        for(auto& [_,value]: model)
            if (not is_pattern(value)) return false;
        return true;
    }

    return false;
}

bool is_function(const ptree& model)
{
    if (not model.has_value<string>()) return false;

    if (model.get_value<string>() != "function") return false;

    if (model.size() != 2)
        throw myexception()<<"function: got "<<model.size()<<" arguments, 2 arguments required.\n  "<<model.show();

    if (not model[0].first.empty() or not is_pattern(model[0].second))
        throw myexception()<<"function: first argument must be a variable or pattern!\n  "<<model.show();
    
    if (not model[1].first.empty())
        throw myexception()<<"function: second argument must not have a variable name!\n  "<<model.show();

    return true;
}

void handle_positional_args(ptree& model, const Rules& R)
{
    if (model.size() == 0) return;

    assert(not model.is_null());
    assert(not model.value_is_empty());

    auto head = model.get_value<string>();

    if (head == "!let") return;

    if (head == "!Decls") return;

    if (is_function(model)) return;

    if (is_list(model)) return;


    if (head == "Pair")
    {
        if (model.size() != 2)
            throw myexception()<<"Pair's must have two elements: "<<unparse(model);

        model.value = string("Tuple");
        head = "Tuple";
    }

    if (is_tuple(model)) return;

    if (head == "get_state")
    {
        if (model.size() != 1)
	    throw myexception()<<"set_state: got "<<model.size()<<" arguments, 1 argument required.\n  "<<model.show();
	if (not model[0].first.empty() or not model[0].second.is_a<string>())
	    throw myexception()<<"set_state: first argument must be an unquoted state name!\n  "<<model.show();
        return;
    }

    if (not R.get_rule_for_func(head))
    {
	for(auto& [arg_name,value]: model)
	    if (not arg_name.empty())
		throw myexception()<<"Named argument '"<<arg_name<<"' not allowed for function '"<<head<<"' with no rule.";
	return;
    }

    auto rule = R.require_rule_for_func(head);

    int i=0;
    bool seen_keyword = false;

    ptree model2(head);

    for(auto& x: model)
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

	model2.push_back(child);

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
    for(auto& [var,value]: defs)
	ptree_map(f2, value);

    return defs;
}

optional<ptree> peel_sample(ptree p)
{
    if (p.has_value<string>() and p.get_value<string>() == "sample")
	return p[0].second;
    else if (p.has_value<string>() and p.get_value<string>() == "!let")
    {
        if (auto new_body = peel_sample(p[1].second))
        {
            p[1].second = *new_body;
            return p;
        }
    }

    return {};
}

optional<ptree> peel_sample_annotated(ptree p)
{
    auto& v = p.get_child("value");
    if (v.has_value<string>() and v.get_value<string>() == "sample")
	return v[0].second;
    else if (v.has_value<string>() and v.get_value<string>() == "!let")
    {
        if (auto new_body = peel_sample_annotated(v[1].second))
        {
            v[1].second = *new_body;
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
	string name = p[0].first;
	return name+show_model(p[0].second)+";"+unparse(p[1].second);
    }
    else if (s == "function")
    {
	string name = unparse(p[0].second);
	return "|"+name+":"+unparse(p[1].second)+"|";
    }
    else if (s == "List")
    {
        vector<string> items;
        for(auto& [_,item]: p)
            items.push_back(unparse(item));
        return "[" + join(items,", ") + "]";
    }
    else if (s == "Tuple")
    {
        vector<string> items;
        for(auto& [_,item]: p)
            items.push_back(unparse(item));
        return "(" + join(items,", ") + ")";
    }
    else if (s == "sample")
	return "~" + unparse(p.begin()->second);
    else if (s == "negate")
	return "-" + unparse(p.begin()->second);
    else if (is_operator(s) and p.size() == 2)
    {
	// sometimes we might need parenthesis, right?
        return unparse(p[0].second) + s + unparse(p[1].second);
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
    for(const auto& [arg_name, arg]: p)
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

string unparse(const ptree& p, const Rules&)
{
    return unparse(p);
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
        for(auto& [name, value]: p[0].second)
            decls.push_back(name + " " + show_model_annotated(value));
        return unparse_annotated(p[1].second) + " where {" + join(decls, "; ") + "}";
    }
    else if (s == "function")
    {
	return "|"+unparse_annotated(p[0].second)+":"+unparse_annotated(p[1].second)+"|";
    }
    else if (s == "List")
    {
        bool list_of_pairs = true;
        vector<string> pairs;
        for(auto& [_,item]: p)
        {
            auto type = item.get_child("type");
            if (type.get_value<string>() == "Tuple" and type.size() == 2)
            {
                auto value = item.get_child("value");
                pairs.push_back(unparse_annotated(value[0].second)+": "+unparse_annotated(value[1].second));
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
        for(auto& [_,item]: p)
            items.push_back(unparse_annotated(item));
        return "[" + join(items,", ") + "]";
    }
    else if (s == "Tuple")
    {
        vector<string> items;
        for(auto& [_,item]: p)
            items.push_back(unparse_annotated(item));
        return "(" + join(items,", ") + ")";
    }
    else if (s == "sample")
	return "~" + unparse_annotated(p.begin()->second);
    else if (s == "negate")
	return "-" + unparse_annotated(p.begin()->second);
    else if (is_operator(s) and p.size() == 2)
    {
	// sometimes we might need parenthesis, right?
        return unparse_annotated(p[0].second) + s + unparse_annotated(p[1].second);
    }

    else if (s == "intToDouble")
	return unparse_annotated(p.get_child("x"));
    else if (s == "unit_mixture" or s == "multiMixtureModel")
	if (auto child = p.get_child_optional("submodel"))
	    return unparse_annotated(*child);
    vector<string> args;
    optional<string> submodel;
    bool positional = true;
    for(const auto& [arg_name, arg]: p)
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
	if (p.size())
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


string show_model_annotated(ptree p)
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

    for(auto& [key,value]: p1)
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
	p3.insert(p3.begin(), {"",p1});

    return p3;
}
