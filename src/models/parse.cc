#include <iostream>
#include <vector>
#include <list>
#include "models/parse.H"
#include "myexception.H"
#include "util.H"
#include "rules.H"
#include "setup.H"

using std::vector;
using std::list;
using std::string;
using std::pair;
using boost::property_tree::ptree;
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

/// \brief Turn an expression of the form h1[a]+h2[b] -> h2[h1[a],b].
///
/// \param sstack A stack of strings that represent a substitution model.
/// \param s The model name to match.
/// \param args The possible argument.
///
pair<optional<string>,string> split_last_plus(const string& s)
{
    // 1. Find last '+' on the top level
    int split = -1;
    int depth = 0;
    for(int i=0;i<s.size();i++)
	if (s[i] == '[')
	    depth++;
	else if (s[i] == ']')
	{
	    depth--;
	    if (depth < 0) throw myexception()<<"Too many ']' in string '"<<s<<"'";
	}
	else if (depth == 0 and s[i] == '+')
	    split = i;
    if (depth != 0)
	throw myexception()<<"Too many '[' in string '"<<s<<"'";
  
    // 2. If there are no plus expressions then we can take the string as is.
    if (split == -1)
	return {boost::none, s};
    // 3. Otherwise divide the string on the last plus
    else
	return {s.substr(0,split), s.substr(split+1)};
}

// Turn an expression of the form head[arg1, arg2, ..., argn] -> {head, arg1, arg2, ..., argn}.
vector<string> split_args(string s)
{
    vector<string> args;

    // 1. Get the head
    int pos = s.find('[');
    if (pos == -1)
    {
	args = {s};
	return args;
    }

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
    ptree p2 = {};
    p2.put_value("Sample");
    p2.push_back({"x",p});
    return p2;
}

/// Parse strings of the form {~}head[value1, value2, key3=value3, ...]
ptree parse_no_submodel(const Rules& R, const string& s)
{
    // 0. Handle ~
    if (not s.empty() and s[0] == '~')
	return add_sample(parse(R, s.substr(1)));

    // 1. Split the head and the arguments
    auto args = split_args(s);

    // 2. Set the head
    string head = args.front();
    args.erase(args.begin());

    ptree result;
    result.put_value(head);
  
    if (args.empty()) return result;

    auto rule = R.require_rule_for_func(head);
    bool is_list_rule = rule.get("list_arguments",false);
    bool is_dict_rule = rule.get("pass_arguments",false);

    // 3. Attempt to set the supplied arguments
    bool seen_keyword_arg = false;
    for(int i=0;i<args.size();i++)
    {
	pair<string,string> key_value;

	// 4. Ignore empty arguments
	if (args[i].empty()) continue;

	// 5. If we have a keyword argument, remember it
	if (auto arg = split_keyword(args[i],'='))
	{
	    seen_keyword_arg = true;
	    key_value = *arg;
	    if (arg->first.empty())
		throw myexception()<<"Parameter name missing in argument '"<<args[i]<<"'";
	}
	// 6. If we have a keyword argument, remember it
	else if ((arg = split_keyword(args[i],'~')) and arg->first.size() and arg->first[0] != '~')
	{
	    seen_keyword_arg = true;
	    key_value = *arg;
	    key_value.second = "~"+key_value.second;
	}
	// 7. Otherwise find the keyword for the positional argument
	else
	{
	    if (seen_keyword_arg)
		throw myexception()<<"Positional argument after keyword argument in '"<<s<<"'";
	    if (is_list_rule)
		key_value = {"",args[i]};
	    else if (is_dict_rule)
		throw myexception()<<"Name required for argument "<<i+1<<" in of '"<<head<<"'";
	    else
		key_value = {get_keyword_for_positional_arg(rule, i), args[i]};
	}
	if (is_list_rule and not key_value.first.empty())
	    throw myexception()<<"No name for argument "<<i+1<<" of '"<<head<<"'";

	// 8. Set the key = value after parsing the value.
	if (not key_value.first.empty() and result.count(key_value.first))
	    throw myexception()<<"Trying to set value for "<<head<<"."<<key_value.first<<" a second time!";
	result.push_back({key_value.first, parse(R, key_value.second)});
    }

    return result;
}

// Parse strings of the form head[args] + head[args] + ... + head[args]
ptree parse(const Rules& R, const string& s)
{
    // 1. Get the last head[args]
    auto ss = split_last_plus(s);

    // 2. Parse the last head[args]
    auto result = parse_no_submodel(R, ss.second);

    // 3. Parse the remainder and add it as a "submodel" argument
    if (ss.first)
    {
	if (result.count("submodel"))
	    throw myexception()<<"Trying to specify a submodel with '+' when submodel already specified by keyword!";
	result.push_back({"submodel",parse(R, *ss.first)});
    }

    return result;
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

string unparse(const ptree& p)
{
    string s = p.get_value<string>();
    if (s == "RCTMC")
    {
	string Q = unparse(p.get_child("Q"));
	string R = unparse(p.get_child("R"));
	return Q + " + " + R;
    }
    if (auto xs = get_list_elements(p))
    {
	vector<string> ss;
	for(auto& x: *xs)
	    ss.push_back(unparse(x));
	return "List[" + join(ss,",") + "]";
    }
    if (s == "intToDouble")
	return unparse(p.get_child("x"));
    if (s == "UnitMixture" or s == "MultiMixtureModel")
	if (auto child = p.get_child_optional("submodel"))
	    return unparse(*child);
    vector<string> args;
    string submodel;
    for(const auto& pair: p)
    {
	if (pair.first == "submodel") {
	    submodel = unparse(pair.second);
	    args.push_back("");
	}
	else if (pair.second.get_value<string>() == "Sample")
	{
	    auto p2 = pair.second.begin()->second;
	    args.push_back( "~" + unparse(p2) );
	}
	else
	    args.push_back( unparse(pair.second) );
    }
    if (args.size() and args.back() == "")
	args.pop_back();
    if (not args.empty())
	s = s + "[" + join(args,',') + "]";
    if (not submodel.empty())
	s = submodel + " + " + s;
    return s;
}

string unparse_type(const ptree& p)
{
    string s = p.get_value<string>();
    vector<string> args;
    for(const auto& pair: p)
	args.push_back( unparse(pair.second) );
    if (not args.empty())
	s = s + "[" + join(args,',') + "]";
    return s;
}

string show_model(boost::property_tree::ptree p)
{
    bool top_sample = false;
    if (p.get_value<string>() == "Sample")
    {
	top_sample = true;
	p = p.begin()->second;
    }

    string output = unparse(p);
    string connector = top_sample?"~ ":"= ";

    return connector + output;
}

