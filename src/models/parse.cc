#include <iostream>
#include <vector>
#include <list>
#include "models/parse.H"
#include "models/model.H"
#include "myexception.H"
#include "util.H"
#include "rules.H"
#include "setup.H"

using std::vector;
using std::list;
using std::string;
using std::pair;
using boost::optional;

optional<pair<string,string>> split_keyword(const string& s, char c)
{
    for(int i=0;i<s.size();i++)
    {
	if (s[i] == '[' or s[i] == ']') return boost::none;
	if (s[i] == c)
	    return pair<string,string>({s.substr(0,i),s.substr(i+1)});
    }
    return boost::none;
}

/// \brief Turn an expression of the form h1[a]+h2[b] -> {h1[a],h2[b]}
///
/// \param sstack A stack of strings that represent a substitution model.
/// \param s The model name to match.
/// \param args The possible argument.
///
vector<string> split_top_plus(const string& s)
{
    vector<string> terms;

    // Split on '+' where depth == 0.
    int pos = 0;
    int depth = 0;
    for(int i=0;i<s.size();i++)
    {
	if (s[i] == '[')
	    depth++;
	else if (s[i] == ']')
	{
	    depth--;
	    if (depth < 0) throw myexception()<<"Too many ']' in string '"<<s<<"'";
	}
	else if (depth == 0 and s[i] == '+')
	{
	    terms.push_back(s.substr(pos,i-pos));
	    pos = i+1;
	}
    }

    if (depth != 0)
	throw myexception()<<"Too many '[' in string '"<<s<<"'";
  
    terms.push_back(s.substr(pos,int(s.size())-pos));

    return terms;
}

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

    if (pos == 0) throw myexception()<<"Term has no operator!";

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
	    args.push_back(s.substr(start,i-start));
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
    // 1. Split the head and the arguments
    auto args = split_args(s);

    // 2. Set the head
    string head = args.front();
    args.erase(args.begin());

    ptree result;
    result.put_value(head);

    // 3. Set the arguments
    for(const auto& arg: args)
    {
	if (arg.empty()) throw myexception()<<"Type '"<<s<<"' has empty argument";

	result.push_back({"", parse_type(arg)});
    }

    return result;
}

ptree add_sample(const ptree& p)
{
    ptree p2("sample");
    p2.push_back({"",p});
    return p2;
}

ptree parse(const string& s);

/// Parse strings of the form {~}head[value1, value2, key3=value3, ...]
ptree parse_no_submodel(const string& s)
{
    if (s.empty())
	throw myexception()<<"term is empty!";

    // 0. Handle ~
    if (not s.empty() and s[0] == '~')
	return add_sample(parse_no_submodel(s.substr(1)));

    // 1. Split the head and the arguments
    auto args = split_args(s);

    // 2. Set the head
    string head = args.front();
    args.erase(args.begin());

    ptree result;
    if (auto i = can_be_converted_to<int>(head))
	result.put_value(*i);
    else if (auto d = can_be_converted_to<double>(head))
	result.put_value(*d);
    else if (auto b = can_be_converted_to<bool>(head))
	result.put_value(*b);
    // Currently we leave literal (i.e. quoted) strings as strings w/ quotes.
    else
	result.put_value(head);

    if (args.size() and not result.is_a<string>())
	throw myexception()<<"Error parsing '"<<s<<"': constant '"<<head<<"' should not have arguments, but has "<<args.size()<<"!";

    // 3. Attempt to set the supplied arguments
    for(int i=0;i<args.size();i++)
    {
	pair<string,string> key_value;

	// 3.1. 'key=value'
	if (auto arg = split_keyword(args[i],'='))
	{
	    key_value = *arg;

	    if (arg->first.empty())
		throw myexception()<<"Parameter name missing in argument '"<<args[i]<<"'";
	}
	// 3.2 'key~value'
	else if ((arg = split_keyword(args[i],'~')) and arg->first.size())
	{
	    key_value = *arg;
	    key_value.second = "~"+key_value.second;
	}
	// 3.3. 'value'
	else
	    key_value.second = args[i];

	if (key_value.first.empty() and key_value.second.empty())
	    result.push_back({{},{}});
	else
	    result.push_back({key_value.first, parse(key_value.second)});
    }

    return result;
}

ptree parse(const string& s)
{
    // 1. Get the last head[args]
    vector<string> terms = split_top_plus(s);

    // 2. Parse the terms and handle submodels;
    ptree expression;
    for(int i=0;i<terms.size();i++)
    {
	auto& term = terms[i];
	try
	{
	    ptree parsed_term = parse_no_submodel(term);
	    if (parsed_term.is_null())        throw myexception()<<"Term is empty!";

	    if (not expression.is_null())
	    {
		if (parsed_term.count("submodel"))    throw myexception()<<"Trying to specify a submodel with '+' when submodel already specified by keyword!";
		parsed_term.push_back({"submodel", expression});
	    }
	    expression = parsed_term;

	    assert(not expression.is_null());
	}
	catch (myexception& e)
	{
	    string eterm = term;
	    if (i-1 >= 0)
		eterm = "+"+eterm;
	    if (i+1 < terms.size())
		eterm = eterm + "+";
	    e.prepend("Parsing '"+eterm+"': ");
	    throw e;
	}
    }

    return expression;
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

void handle_positional_args(ptree& model, const Rules& R)
{
    if (model.size() == 0) return;

    assert(not model.is_null());
    assert(not model.value_is_empty());

    auto head = model.get_value<string>();

    if (head == "let")
    {
	if (model.size() != 2)
	    throw myexception()<<"let: got "<<model.size()<<" arguments, 2 arguments required.\n  "<<model.show();
	if (model[0].first.empty())
	    throw myexception()<<"let: first argument must have variable name!\n  "<<model.show();
	if (not model[1].first.empty())
	    throw myexception()<<"let: second argument must not have a variable name!\n  "<<model.show();
	model[1].first = "body";
	return;
    }

    if (head == "function")
    {
	if (model.size() != 2)
	    throw myexception()<<"function: got "<<model.size()<<" arguments, 2 arguments required.\n  "<<model.show();

	// Complain about function[var=value,body]
	if (not model[0].first.empty() or not model[0].second.is_a<string>())
	    throw myexception()<<"function: first argument must be a variable!\n  "<<model.show();
	// Move string value to first element of pair.
	model[0].first = (string)model[0].second;
	model[0].second = {};

	if (not model[1].first.empty())
	    throw myexception()<<"function: second argument must not have a variable name!\n  "<<model.show();
	return;
    }

    auto rule = R.require_rule_for_func(head);

    int i=0;
    bool seen_keyword = false;

    ptree model2(head);

    for(auto& x: model)
    {
	pair<string,ptree> child = x;

	if (child.first.empty())
	{
	    if (seen_keyword)
		throw myexception()<<"Positional argument after keyword argument in '"<<unparse(model, R)<<"'!";

	    auto keyword = get_keyword_for_positional_arg(rule, i);

	    if (model.count(keyword))
		throw myexception()<<"Trying to set value for "<<head<<"."<<keyword<<" both by position and by keyword: \n"<<unparse(model, R);

	    if (child.second.is_null())
	    {
		i++;
		continue;
	    }
	    else
		child.first = keyword;
	}
	else
	    seen_keyword = true;

	if (child.first.empty())
	    throw myexception()<<"No keyword in argument for "<<head<<"?";

	model2.push_back(child);

	i++;
    }

    std::swap(model, model2);
}

// Convert List[x1,x2...] -> Cons[x1,[Cons[x2,...]]
void pass_list(ptree& p)
{
    if (p.has_value<string>() and p.get_value<string>() == "List")
    {
	ptree l = ptree("Nil");
	for(auto& child: reverse(p))
	{
	    ptree l2 = ptree("Cons");
	    l2.push_back({"",child.second});
	    l2.push_back({"",l});
	    std::swap(l,l2);
	}
	std::swap(p,l);
    }
}

// Parse strings of the form head[args] + head[args] + ... + head[args]
ptree parse(const Rules& R, const string& s)
{
    // 1. Get the last head[args]
    auto model = parse(s);

    // 2. Convert List[x1,x2...] -> Cons[x1,[Cons[x2,...]]
    ptree_map(pass_list, model);

    // 3. Fill in keywords where they are not given
    auto f2 = [&](ptree& p) {handle_positional_args(p,R);};
    ptree_map(f2, model);

    return model;
}


optional<list<ptree>> get_list_elements(const ptree& p)
{
    string s = p.get_value<string>();
    if (s == "Nil") return list<ptree>();

    if (s == "Cons")
    {
	ptree h = p.get_child("first");
	ptree t = p.get_child("second");
	auto xs = get_list_elements(t);
	if (xs)
	{
	    (*xs).push_front(h);
	    return xs;
	}
    }
    return boost::none;
}

optional<list<ptree>> get_ann_list_elements(const ptree& ann)
{
    auto& p = ann.get_child("value");

    string s = p.get_value<string>();
    if (s == "Nil") return list<ptree>();

    if (s == "Cons")
    {
	ptree h = p.get_child("first");
	ptree t = p.get_child("second");
	auto xs = get_ann_list_elements(t);
	if (xs)
	{
	    (*xs).push_front(h);
	    return xs;
	}
    }
    return boost::none;
}

string unparse(const ptree& p, const Rules& rules)
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

    string s = p.get_value<string>();
    if (s == "rctmc")
    {
	string Q = unparse(p.get_child("S"), rules);
	string R = unparse(p.get_child("R"), rules);
	return Q + "+" + R;
    }
    else if (s == "let")
    {
	string name = p[0].first;
	return "let["+name+"="+unparse(p[0].second,rules)+","+unparse(p[1].second,rules)+"]";
    }
    else if (s == "function")
    {
	string name = p[0].first;
	return "function["+name+","+unparse(p[1].second,rules)+"]";
    }
    else if (s == "sample")
	return "~" + unparse(p.begin()->second, rules);

    if (auto xs = get_list_elements(p))
    {
	vector<string> ss;
	for(auto& x: *xs)
	    ss.push_back(unparse(x, rules));
	return "List[" + join(ss,",") + "]";
    }
    if (s == "intToDouble")
	return unparse(p.get_child("x"), rules);
    if (s == "unit_mixture" or s == "multiMixtureModel")
	if (auto child = p.get_child_optional("submodel"))
	    return unparse(*child, rules);
    vector<string> args;
    optional<string> submodel;
    for(const auto& pair: p)
    {
	// Don't print submodel arguments: move out to submodel + <this>
	if (pair.second.is_null())
	{
	    args.push_back("");
	    continue;
	}
	else if (pair.first == "submodel")
	{
	    assert(not submodel);
	    submodel = unparse(pair.second, rules);
	    args.push_back("");
	}
	// Don't print alphabet=getAlphabet (FIXME: change to x=getAlphabet, if this is a default value)
	else if (pair.second == "getAlphabet" and
		 get_arg(rules.require_rule_for_func(s), pair.first).count("default_value") and
		 get_arg(rules.require_rule_for_func(s), pair.first).get_child("default_value") == ptree("getAlphabet"))
	    ;
	else
	    args.push_back( unparse(pair.second, rules) );
    }
    while (args.size() and args.back() == "")
	args.pop_back();
    if (not args.empty())
	s = s + "[" + join(args,',') + "]";
    if (submodel)
	s = *submodel + "+" + s;
    return s;
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
    if (s == "rctmc")
    {
	string Q = unparse_annotated(p.get_child("S"));
	string R = unparse_annotated(p.get_child("R"));
	return Q + "+" + R;
    }
    if (s == "let")
    {
	string name = p[0].first;
	return "let["+name+"="+unparse_annotated(p[0].second)+","+unparse_annotated(p[1].second)+"]";
    }
    if (s == "sample")
	return "~" + unparse_annotated(p.begin()->second);
    if (auto xs = get_ann_list_elements(ann))
    {
	vector<string> ss;
	for(auto& x: *xs)
	    ss.push_back(unparse_annotated(x));
	return "List[" + join(ss,",") + "]";
    }
    if (s == "intToDouble")
	return unparse_annotated(p.get_child("x"));
    if (s == "unit_mixture" or s == "multiMixtureModel")
	if (auto child = p.get_child_optional("submodel"))
	    return unparse_annotated(*child);
    vector<string> args;
    optional<string> submodel;
    for(const auto& pair: p)
    {
	// Don't print submodel arguments: move out to submodel + <this>
	if (pair.second.is_null())
	{
	    args.push_back("");
	    continue;
	}
	else if (pair.first == "submodel")
	{
	    assert(not submodel);
	    submodel = unparse_annotated(pair.second);
	    args.push_back("");
	}
	// Don't print alphabet=getAlphabet (FIXME: change to x=getAlphabet, if this is a default value)
	else if (pair.second.get_child("value") == "getAlphabet" and pair.second.get_child("is_default_value") == true)
	    ;
	else
	    args.push_back( unparse_annotated(pair.second) );
    }
    while (args.size() and args.back() == "")
	args.pop_back();
    if (not args.empty())
	s = s + "[" + join(args,',') + "]";
    if (submodel)
	s = *submodel + "+" + s;
    return s;
}

string unparse_type(const ptree& p)
{
    string s = p.get_value<string>();
    vector<string> args;
    for(const auto& pair: p)
	args.push_back( unparse_type(pair.second) );
    if (not args.empty())
	s = s + "[" + join(args,',') + "]";
    return s;
}

optional<ptree> peel_sample(ptree p)
{
    if (p.has_value<string>() and p.get_value<string>() == "sample")
	return p[0].second;
    else
	return boost::none;
}

string unparse_abbrev(ptree p, const Rules& rules, int length)
{
    string output = unparse(p, rules);
    if (output.size() > length)
    {
	output = convertToString(p.value);
	if (p.size())
	    output += "[..]";
    }
    return output;
}

string show_model(ptree p, const Rules& rules)
{
    if (auto q = peel_sample(p))
	return "~ " + unparse(*q, rules);
    else
	return "= " + unparse(p, rules);
}


string show_model_abbrev(ptree p, const Rules& rules, int length)
{
    if (auto q = peel_sample(p))
	return "~ " + unparse_abbrev(*q, rules, length-2);
    else
	return "= " + unparse_abbrev( p, rules, length-2);
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

string translate_structures(const string& name)
{
    vector<string> path = model_split_path(name);

    vector<string> path2;
    optional<int> elem;
    for(auto& x: path)
    {
	if (x == "Cons:second")
	{
	    if (elem)
		elem = 1+*elem;
	    else
		elem = 1;
	}
	else if (x == "Cons:first")
	{
	    if (path2.empty()) path2.push_back("");
	    if (not elem) elem = 0;
	    elem = 1+*elem;

	    path2.back() += "[" + convertToString(*elem) + "]";
	    elem = boost::none;
	}
	else
	{
	    elem = boost::none;
	    if (x == "Pair::first")
	    {
		if (path2.empty()) path2.push_back("");
		path2.back() += "[1]";
	    }
	    else if (x == "Pair::second")
	    {
		if (path2.empty()) path2.push_back("");
		path2.back() += "[2]";
	    }
	    else
		path2.push_back(x);
	}
    }
    return model_path(path2);
}


