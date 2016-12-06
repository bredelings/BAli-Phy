/*
  Copyright (C) 2004-2010 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  You should have received a copy of the GNU General Public License
  along with BAli-Phy; see the file COPYING.  If not see
  <http://www.gnu.org/licenses/>.  */

///
/// \file setup-smodel.C
///
/// \brief Create substitution models from strings of the form model[arg,arg]+...+model[arg,arg].
///


// TODO:
// 1. Why aren't the frequency models in the default_arguments list?
//
// 2. When the user supplies HKY, report this as HKY+F (somehow).
//   - We could either use typed arguments to correct this BEFORE processing (faster).
//     + So the coerce functions would work on the strings instead
//     + And we need a rule to make +F evaluate to frequencies, but HKY+F evaluate to a rate matrix.
//       (See process_stack_frequencies).
//       (I guess this means that gwF[] and gwF[arg] have different types!
//         + For ALL frequency models!
//
// 3. Remove setting of frequencies from the construction of the model.
//   - Could this allow us to determine the alphabet from the model, so that we don't need to
//     write --smodel=M0 --alphabet=Codons?
//   - When no smodel is supplied, we would still have to try parsing the alignment
//   - Also, codons of DNA, or codons of RNA?

//   - So, maybe an INITIAL parse, only to DNA, RNA, or AA, just to determine alphabet?
//   - Then 

// 4b. Make model_expressions for sampling frequency.
// 6b. Print the expanded model expression.
// 7. Allow specifying lists?
// 8. Allow specifying priors to the frequency model.
// 10. Allow using user-specified models from the command line.
// 11. ? How to deal with things that need model-dependent argument names?
// 12. ? How to deal with things that need programmatic model specification?
// 13. ? Why can't we sort the DP rates (which would be a great use of programmatic model specification)?
//*17. Write a function to go BACK from ptrees to strings.
// 18. Allow specifying the alphabet in the smodel, instead of as a command-line option.
// 19. Allow generic parsing of functions completely from their description, instead of writing code for each one.
// 20. Allow specifying the frequencies in the expression as (e.g.) empirical frequencies, for a group of alignments.
// 21. Allow specifying and receiving help information for each function, and for its arguments.
// 22. Eliminate as many ***_model functions as possible.
// 23. Allow GTR aminoacid models.
// 24. Allow a full term-rewriting system, with unification on types.  This will allow passing
//     computing expressions before we know the alphabet that allow the alphabet to be a (type) variable.
//     We can then use these expressions to narrow the alphabets we try when reading the alignment matrix.
//
//     This should also allow propagating an integer argument to default values for other variables, although
//     this will not work for evaluated expressions -- or not very well.

// DONE
// 4a. Make model_expressions for prefixing, logging.
// 5. Associate types with each argument.
// 6a. Expand default arguments in the model expression.
// 9. Allow separately specifying random variables to e.g. indel model.
// 14. Make 'translate' more intelligent, so that it can handle top-level expressions
//     NOT being actions, but arguments BEING actions.
// 15. Specify the types of arguments and results (for coercion).
// 16. Do the coercion on the ptree or string level.

#include <vector>
#include <boost/program_options.hpp>
#include "smodel/setup.H"
#include "util.H"
#include "myexception.H"
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/info_parser.hpp>
#include <boost/optional.hpp>
#include <boost/property_tree/info_parser.hpp>
#include "computation/model_expression.H"
#include "computation/operations.H"

using boost::property_tree::ptree;
using boost::optional;
using std::string;
using std::pair;
using std::vector;
using std::valarray;
using boost::program_options::variables_map;
using boost::shared_ptr;

// N = no logging or arguments
// M = this expression needs to be performed

/*
  Maybe convert 'rule' information to JSON like this:
  {
  name: "Uniform", 
  type: "Double", 
  action: true, 
  call: ["uniform","a","b"], 
  args: [["a","Double",0],["b","Double",1],
  }
*/

typedef ptree equations_t;

typedef vector<vector<string>> Rule;

const vector<Rule> all_default_arguments = 
{
    {{"log","Double","N"}, {"log","x"}, {"x","Double"}},
    {{"Uniform","Double","M"}, {"uniform","a","b"}, {"a","Double"}, {"b","Double"}},
    {{"Normal","Double","M"}, {"normal","mu","sigma"}, {"mu","Double"}, {"sigma","Double"}},
    {{"logNormal","Double","M"}, {"logNormal","lmu","lsigma"}, {"lmu","Double"}, {"lsigma","Double"}},
    {{"EQU","EM[a]"}, {}},
    {{"F81"}, {}, {"alphabet","alphabet"}},
    {{"HKY","EM[a]"}, {"SModel.hky","alphabet","kappa"}, {"kappa","Double","logNormal[log[2],0.25]"}, {"alphabet","alphabet"}, },
    {{"TN","EM[a]"}, {"SModel.tn","alphabet","kappaPur","kappaPyr"}, {"kappaPur","Double","logNormal[log[2],0.25]"}, {"kappaPyr","Double","logNormal[log[2],0.25]"}, {"alphabet","alphabet"}},
    {{"GTR","EM[a]"}, {}, {"*ag"}, {"*at"}, {"*ac"}, {"*gt"}, {"*gc"}, {"*tc"},{"alphabet","alphabet"}},
    {{"HKYx3","EM[a]"}, {}, {"kappa","Double","logNormal[log[2],0.25]"}},
    {{"TNx3","EM[a]"}, {}, {"kappaPur","Double","logNormal[log[2],0.25]"}, {"kappaPyr","Double","logNormal[log[2],0.25]"}},
    {{"GTRx3","EM[a]"}, {}, {"*ag"}, {"*at"}, {"*ac"}, {"*gt"}, {"*gc"}, {"*tc"}},
    {{"PAM","EM[AA]"}, {}, {"alphabet","alphabet","AA"}},
    {{"JTT","EM[AA]"}, {}, {"alphabet","alphabet","AA"}},
    {{"WAG","EM[AA]"}, {}, {"alphabet","alphabet","AA"}},
    {{"LG","EM[AA]"}, {}, {"alphabet","alphabet","AA"}},
    {{"Empirical","EM[a]"}, {}, {"filename"}},
    {{"M0","EM[a]"}, {"SModel.m0", "alphabet", "submodel","omega"}, {"submodel","EM[a]","HKY"}, {"omega","Double","Uniform[0,1]"}, {"alphabet","alphabet"}},
    {{"fMutSel","RA[a]"}, {}, {"submodel","RA[a]"}},
    {{"fMutSel0","RA[a]"}, {}, {"submodel","RA[a]"}},
    {{"INV","MM[a]"}, {}, {"p","Double","Uniform[0,1]"}},
    {{"DP","MM[a]"}, {}, {"n","Int"}, {"submodel","RA[a]"}},
    {{"gamma","MM[a]"}, {}, {"n","Int","4"}, {"*alpha","Double"}, {"submodel","RA[a]"}},
    {{"gamma_inv","MM[a]"}, {}, {"n","Int","4"}, {"*alpha","Double"}, {"p","Double","Uniform[0,1]"}, {"submodel","RA[a]"}},
    {{"log-normal","MM[a]"}, {}, {"n","Int","4"}, {"sigmaOverMu","Double"}, {"submodel","RA[a]"}},
    {{"log-normal_inv","MM[a]"}, {}, {"n","Int","4"}, {"sigmaOverMu","Double"}, {"p","Double","Uniform[0,1]"}, {"submodel","RA[a]"}},
    {{"M1a","MM[a]"}, {}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"M2a","MM[a]"}, {}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"M2a_Test","MM[a]"}, {}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    //    {{"M3u"}, {"3"}, {"nuc_model",""HKY"}, {"freq_model","F61"}},
    {{"M3","MM[a]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"M3_Test","MM[a]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"M7","MM[a]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"M8","MM[a]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"M8a","MM[a]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"M8a_Test","MM[a]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"branch-site","MM[a]"}, {}, {"n","Int","2"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"dp_omega","MM[a]"}, {}, {"n","Int","4"}, {"nuc_model","EM[a]","HKY"}, {"freq_model","FM[a]","F61"}},
    {{"F","FM[a]","M"}, {"plus_f_model","alphabet",}, {"alphabet","alphabet"}},
    {{"F61","FM[a]","M"}, {"plus_f_model","alphabet",}, {"alphabet","alphabet"}},
    {{"gwF","FM[a]","M"}, {"plus_gwf_model","alphabet",}, {"alphabet","alphabet"}},
    {{"F1x4","FM[a]"}, {}},
    {{"F3x4","FM[a]"}, {}},
    {{"MG94","FM[a]"}, {}},
    {{"MG94w9","FM[a]"}, {}},
    {{"DNA","alphabet","N"}, {"dna"}},
    {{"RNA","alphabet"}, {"rna"}},
    {{"AA","alphabet"}, {"aa"}},
    {{"Codons","alphabet"}, {"codons","nuc","aa"}, {"nuc","alphabet"}, {"aa","alphabet","AA"}},
    {{"RCTMC","RA[a]","N"}, {"reversible_markov","Q","R"}, {"Q","EM[a]"}, {"R","FM[a]"}},
    {{"UnitMixture","MM[a]","N"}, {"unit_mixture","submodel"}, {"submodel","RA[a]"}},
    {{"MMM","MMM[a]","N"}, {"mmm","submodel"}, {"submodel","MM[a]"}}
};

vector<string> get_arg(const Rule& args, const string& s)
{
    for(const auto& arg: args)
	if (arg[0] == s)
	    return arg;
    throw myexception()<<"Function "<<args[0][0]<<" has no argument '"<<s<<"'";
}

vector<Rule> get_rules_for_func(const string& s)
{
    vector<Rule> rules;
    for(const auto& rule: all_default_arguments)
	if (rule[0][0] == s)
	    rules.push_back(rule);
    return rules;
}

/// Split a string of the form key=value into {key,value}
ptree parse(const string& s);
ptree parse_type(const string& s);

// given two terms, what equations do we need to unify them?
equations_t unify(const ptree& p1, const ptree& p2);

bool can_unify(const ptree& p1, const ptree& p2)
{
    return unify(p1,p2).get_value<string>() != "fail";
}

equations_t unify(const string& s1, const string& s2)
{
    auto p1 = parse_type(s1);
    auto p2 = parse_type(s2);
    return unify(p1,p2);
}

bool can_unify(const string& s1, const string& s2)
{
    return unify(s1,s2).get_value<string>() != "fail";
}

// given two sets of equations, what further equations do we need to unify them?
bool merge_equations(equations_t& p1, const equations_t& p2)
{
    for(const auto& x: p2)
    {
	const auto& key = x.first;
	assert(x.second.get_value<string>() != "");
	// If p1 has no equality for the the variable, then just copy over p2's equality
	if (not p1.count(key)) // or p1.get<string>(key) != "" or p2.get<string>(key) != "")
	{
	    p1.erase(key);
	    p1.push_back(x);
	}

        // If they BOTH have an equality, then we need to merge the equalities.
	ptree new_equalities = unify(p1.get_child(key),p2.get_child(key));
	if (new_equalities.get_value<string>() == "fail")
	    return false;

	merge_equations(p1,new_equalities);
    }

    return true;
}

bool is_variable(const string& s)
{
    if (s.empty()) return false;
    char first_letter = s[0];
    return (first_letter >= 98 and first_letter <= 123);
}

equations_t unify(const ptree& p1, const ptree& p2)
{
    // 1. If either term is a variable, then we are good.
    string head1 = p1.get_value<string>();
    string head2 = p2.get_value<string>();
    if (head1 == "_" or head2 == "_")
	return {}; // Don't record equations for matching wildcards
    else if (is_variable(head1))
    {
	// Don't record equalities of the form a = a
	if (head1 != head2)
	{
	    equations_t equations;
	    equations.push_back({head1,p2});
	    return equations;
	}
    }
    else if (is_variable(head2))
    {
	equations_t equations;
	equations.push_back({head2,p1});
	return equations;
    }

    // 2. If the heads don't match then unification fails
    if (head1 != head2) return ptree("fail");

    // 3. If the arity doesn't match then unification fails
    if (p1.size() != p2.size()) return ptree("fail");

    // 4. If every argument unifies, then unification succeeds
    equations_t equations;

    auto x = p1.begin();
    auto y = p2.begin();
    for(int i=0; i<p1.size(); i++, x++, y++)
	if (not merge_equations(equations, unify(x->second, y->second)))
	    return ptree("fail");
    
    return equations;
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
    vector<string> args;
    string submodel;
    for(const auto& pair: p)
    {
	if (pair.first == "submodel")
	    submodel = unparse(pair.second);
	else
	    args.push_back( pair.first + "=" + unparse(pair.second) );
    }
    if (not args.empty())
	s = s + "[" + join(args,',') + "]";
    if (not submodel.empty())
	s = submodel + " + " + s;
    return s;
}

string get_type_for_arg(const string& func, const string& arg)
{
    for(const auto& x: all_default_arguments)
    {
	if (x[0][0] != func) continue;

	for(int i=2;i<x.size();i++)
	{
	    const auto& y = x[i];
	    if (y[0] == arg)
		return y[1];
	}
    }
    return "?";
}

string get_type(const string& func)
{
    if (can_be_converted_to<int>(func)) return "Int";
    if (can_be_converted_to<double>(func)) return "Double";

    for(const auto& x: all_default_arguments)
    {
	if (x[0][0] == func)
	    return x[0][1];
    }
    return "?";
}

string get_type(const ptree& model_rep)
{
    return get_type(model_rep.get_value<string>());
}

ptree coerce_to_RA(const ptree& model_rep)
{
    if (can_unify(get_type(model_rep), "RA[_]"))
	return model_rep;

    if (can_unify(get_type(model_rep), "EM[_]"))
    {
	ptree r;
	r.put_value("F");
	ptree result;
	result.put_value("RCTMC");
	result.push_back({"Q",model_rep});
	result.push_back({"R",ptree("F")});
	return result;
    }

    throw myexception()<<"Cannot convert "<<model_rep.get_value<string>()<<" of type "<<get_type(model_rep)<<" to type RA.";
}

ptree coerce_to_MM(const ptree& model_rep)
{
    if (can_unify(get_type(model_rep), "MM[_]"))
	return model_rep;

    try {
	ptree submodel = coerce_to_RA(model_rep);
	ptree result;
	result.put_value("UnitMixture");
	result.push_back({"submodel",submodel});
	return result;
    }
    catch (...) {
	throw myexception()<<"Cannot convert "<<model_rep.get_value<string>()<<" of type "<<get_type(model_rep)<<" to type MM.";
    }
}

ptree coerce_to_MMM(const ptree& model_rep)
{
    if (can_unify(get_type(model_rep), "MMM[_]"))
	return model_rep;

    try {
	ptree submodel = coerce_to_MM(model_rep);
	ptree result;
	result.put_value("MMM");
	result.push_back({"submodel",submodel});
	return result;
    }
    catch (...) {
	throw myexception()<<"Cannot convert "<<model_rep.get_value<string>()<<" of type "<<get_type(model_rep)<<" to type MMM.";
    }
}

optional<pair<string,string>> split_keyword(const string& s)
{
    int pos = s.find('=');
    if (pos == -1)
	return boost::none;
    else
	return pair<string,string>({s.substr(0,pos),s.substr(pos+1)});
}

string show(const ptree& pt, int depth = 0)
{
    string result = "";
    string indent(depth,' ');
    string indent2(depth+2,' ');
    result += "'"+pt.get_value<string>()+"'\n";
    for(auto c: pt)
    {
	result += indent2 + c.first + " : ";
	result += show(c.second,depth+4);
    }
    return result;
}

string show(vector<string> args)
{
    string output = args[0];
    args.erase(args.begin());
    if (args.empty()) return output;

    output += '[';
    output += join(args,',');
    output += ']';
    return output;
}

string get_keyword_for_positional_arg(const string& head, int i)
{
    for(const auto& default_arguments: all_default_arguments)
    {
	if (default_arguments[0][0] != head) continue;

	if (i+2 >= default_arguments.size())
	    throw myexception()<<"Trying to access positional arg "<<i+1<<" for '"<<head<<"', which only has "<<default_arguments.size()-2<<" positional arguments.";

	// Strip leading '*' that indicates required argument
	string keyword = default_arguments[i+2][0];
	if (keyword[0] == '*')
	    keyword = keyword.substr(1);
	auto keyword_pair = split_keyword(keyword);
	if (keyword_pair)
	    return (*keyword_pair).first;
	else
	    return keyword;
    }
    throw myexception()<<"No positional arguments for '"<<head<<"'!";
}

void check_required_args(const ptree& args)
{
    //  std::cout<<"checkout args:\n";
    //  write_info(std::cout, args);
    //  std::cout<<show(args)<<std::endl;
    const string& head = args.get_value<string>();
  
    for(const auto& rule: get_rules_for_func(head))
    {
	for(int i=2;i<rule.size();i++)
	{
	    string keyword = rule[i][0];
	    if (keyword[0] == '*') continue;
	    if (rule[i].size() > 2) continue;

	    if (not args.count(keyword))
		throw myexception()<<"Command '"<<head<<"' missing required argument '"<<keyword<<"'";
	}
    }
}

void check_and_coerce_arg_types(ptree& args)
{
    //  std::cout<<"checkout args:\n";
    //  write_info(std::cout, args);
    //  std::cout<<show(args)<<std::endl;
    const string& head = args.get_value<string>();

    for(const auto& rule: get_rules_for_func(head))
    {
	for(int i=2;i<rule.size();i++)
	{
	    string keyword = rule[i][0];
	    if (keyword[0] == '*')
		keyword = keyword.substr(1);
	    auto& required_type = rule[i][1];

	    if (not args.count(keyword)) continue;
	    auto supplied_type = get_type(args.get_child(keyword));

	    if (can_unify(required_type,supplied_type)) continue;

	    ptree value = args.get_child(keyword);
	    args.erase(keyword);
	    if (can_unify(required_type,"RA[a]"))
		value = coerce_to_RA(value);
	    else if (can_unify(required_type,"MM[a]"))
		value = coerce_to_MM(value);
	    else if (can_unify(required_type,"MMM[a]"))
		value = coerce_to_MMM(value);
	    else if (required_type == "Double" and supplied_type == "Int")
	    { }
	    else
		throw myexception()<<"Can't coerce "<<value.get_value<string>()<<" of type "<<get_type(value)<<" to type "<<required_type<<".";
	    args.push_back({keyword,value});
	}
    }
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

/// Parse strings of the form head[value1, value2, key3=value3, ...]
ptree parse_no_submodel(const string& s)
{
    // 1. Split the head and the arguments
    auto args = split_args(s);

    // 2. Set the head
    string head = args.front();
    args.erase(args.begin());

    ptree result;
    result.put_value(head);
  
    // 3. Attempt to set the supplied arguments
    bool seen_keyword_arg = false;
    for(int i=0;i<args.size();i++)
    {
	pair<string,string> key_value;

	// 4. Ignore empty arguments
	if (args[i].empty()) continue;

	// 5. If we have a keyword argument, remember it
	if (auto arg = split_keyword(args[i]))
	{
	    seen_keyword_arg = true;
	    key_value = *arg;
	}
	// 6. Otherwise find the keyword for the positional argument
	else
	{
	    if (seen_keyword_arg)
		throw myexception()<<"Positional argument after keyword argument in '"<<s<<"'";
	    key_value = {get_keyword_for_positional_arg(head, i), args[i]};
	}

	// 7. Set the key = value after parsing the value.
	if (result.count(key_value.first))
	    throw myexception()<<"Trying to set value for "<<head<<"."<<key_value.first<<" a second time!";
	result.push_back({key_value.first, parse(key_value.second)});
    }

    return result;
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

// Parse strings of the form head[args] + head[args] + ... + head[args]
ptree parse(const string& s)
{
    // 1. Get the last head[args]
    auto ss = split_last_plus(s);

    // 2. Parse the last head[args]
    auto result = parse_no_submodel(ss.second);

    // 3. Parse the remainder and add it as a "submodel" argument
    if (ss.first)
    {
	if (result.count("submodel"))
	    throw myexception()<<"Trying to specify a submodel with '+' when submodel already specified by keyword!";
	result.push_back({"submodel",parse(*ss.first)});
    }

    return result;
}

// Translate pass M+FM -> RCTMC[M,FM]
void pass1(ptree& p)
{
    // 1. Handle children.
    for(auto& child: p)
	pass1(child.second);
    
    // 2. Convert e.g. TN+F -> RCTMC[TN,F]
    if (can_unify(get_type(p),"FM[_]") and p.count("submodel"))
    {
	ptree q = p.get_child("submodel");
	auto& r = p;
	r.erase("submodel");
	
	ptree result = {};
	result.put_value("RCTMC");
	result.push_back({"Q",q});
	result.push_back({"R",r});
	p = result;
    }
}

void pass2(const ptree& type, ptree& p)
{
    // 1. Handle children.
    for(auto& child: p)
	pass2({}, child.second);

    auto name = p.get_value<string>();

    // 2. Substitute default values
    for(const auto& rule: get_rules_for_func(name))
    {
	for(int i=2;i<rule.size();i++)
	{
	    const auto& argument = rule[i];
	    string keyword = argument[0];
	    if (keyword[0] == '*')
		keyword = keyword.substr(1);
	    bool has_default = (argument.size() > 2);
	    if (not has_default) continue;
	    string def = argument[2];

	    if (not p.count(keyword))
	    {
		auto arg = parse(def);
		pass2({}, arg);
		p.push_back({keyword, arg});
	    }
	}
    }

    // 3. Coerce arguments to their given type
    check_and_coerce_arg_types(p);
    
    // 4. Check that required arguments are specified
    check_required_args(p);
}

ptree translate_model(const string& type, const string& model)
{
    auto p = parse(model);
    auto t = parse_type(type);
    pass1(p);
    pass2(t,p);
    return p;
}

/// \brief Return the default substitution model name for alphabet \a a, and "" if there is no default.
string default_markov_model(const alphabet& a) 
{
    if (dynamic_cast<const Nucleotides*>(&a))
	return "TN";
    else if (dynamic_cast<const AminoAcidsWithStop*>(&a))
	return "";
    else if (dynamic_cast<const AminoAcids*>(&a))
	return "LG";
    else if (dynamic_cast<const Codons*>(&a))
	return "M0";
    else if (dynamic_cast<const Triplets*>(&a))
	return "TNx3";
    else
	return "";
}

expression_ref get_smodel_as(const string& type, const ptree& model_rep);

expression_ref process_stack_functions(const ptree& model_rep)
{
    string name = model_rep.get_value<string>();

    auto rules = get_rules_for_func(name);
    if (rules.size() and rules[0][1].size())
    {
	bool no_log = rules[0][0].size() > 2 and rules[0][0][2] == "N";
	expression_ref E = identifier(rules[0][1][0]);
	for(int i=0;i<rules[0].size()-2;i++)
	{
	    string arg_name = rules[0][1][i+1];
	    string type = get_arg(rules[0], arg_name)[1];
	    expression_ref arg = get_smodel_as(type, model_rep.get_child(arg_name));
	    if ((type == "Double" or type == "Int") and (not no_log))
		arg = add_logger(arg_name, arg);
	    E = (E,arg);
	}
	if (rules[0][0].size() > 2 and rules[0][0][2] == "M")
	    E = model_expression(E);
	if (not no_log)
	    E = prefix(name,E);
	
	return E;
    }

    return {};
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
///
expression_ref process_stack_Markov(const ptree& model_rep)
{
    //------ Get the base markov model (Reversible Markov) ------//
    /*
      if (model_rep.get_value<string>() == "EQU")
      {
      return EQU_Model(*a);
      }
      else if (model_rep.get_value<string>() == "F81")
      {
      if (frequencies)
      return F81_Model(*a,*frequencies);
      else
      return F81_Model(*a);
      }
    */

    if (model_rep.get_value<string>() == "JC")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return (identifier("jukes_cantor"), alphabet);
    }
    else if (model_rep.get_value<string>() == "EQU")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return model_expression({identifier("equ_model"), alphabet});
    }
    else if (model_rep.get_value<string>() == "GTR")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return model_expression({identifier("gtr_model"), alphabet});
    }
    /*
      else if (model_rep.get_value<string>() == "EQUx3")) {

      const Triplets* T = dynamic_cast<const Triplets*>(&*a);
      if (T) 
      return Singlet_to_Triplet_Exchange(*T,EQU(T->getNucleotides()));
      else
      throw myexception()<<"EQUx3: '"<<a->name<<"' is not a triplet alphabet.";
      }
    */
    else if (model_rep.get_value<string>() == "HKYx3")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return model_expression({identifier("hkyx3_model"), alphabet});
    }
    else if (model_rep.get_value<string>() == "TNx3")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return model_expression({identifier("tnx3_model"),alphabet});
    }
    else if (model_rep.get_value<string>() == "GTRx3")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return model_expression({identifier("gtrx3_model"),alphabet});
    }
    else if (model_rep.get_value<string>() == "PAM")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));
	
	return (identifier("SModel.pam"), alphabet);
    }
    else if (model_rep.get_value<string>() == "JTT")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return (identifier("SModel.jtt"), alphabet);
    }
    else if (model_rep.get_value<string>() == "WAG") {
	if (not model_rep.count("alphabet"))
	    throw myexception()<<"Model '"<<model_rep.get_value<string>()<<"' is missing parameters 'alphabet'";

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return (identifier("SModel.wag"), alphabet);
    }
    else if (model_rep.get_value<string>() == "LG") {
	if (not model_rep.count("alphabet"))
	    throw myexception()<<"Model '"<<model_rep.get_value<string>()<<"' is missing parameters 'alphabet'";

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return (identifier("SModel.lg"), alphabet);
    }
    else if (model_rep.get_value<string>() == "Empirical") 
    {
	if (not model_rep.count("alphabet"))
	    throw myexception()<<"Model '"<<model_rep.get_value<string>()<<"' is missing parameters 'alphabet'";

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	return (identifier("SModel.empirical"), alphabet, model_rep.get<string>("filename"));
    }
    /*
      else if (model_rep.get_value<string>() == "C10")
      {
      if (*a != AminoAcids())
      throw myexception()<<"C20: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
      return C10_CAT_FixedFrequencyModel();
      }
      else if (model_rep.get_value<string>() == "C20")
      {
      if (*a != AminoAcids())
      throw myexception()<<"C20: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
      return C20_CAT_FixedFrequencyModel();
      }
      else if (model_rep.get_value<string>() == "CAT-Fix") {
      if (*a != AminoAcids())
      throw myexception()<<"CAT-Fix: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
      CAT_FixedFrequencyModel M(*a);
      M.load_file(arg);
      return M;
      }
    */
    else if (model_rep.get_value<string>() == "fMutSel")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref nuc_rm = get_smodel_as("RA[a]",model_rep.get_child("submodel"));

	return model_expression({identifier("fMutSel_model"), alphabet , nuc_rm});
    }
    else if (model_rep.get_value<string>() == "fMutSel0")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref nuc_rm = get_smodel_as("RA[a]",model_rep.get_child("submodel"));

	return model_expression({identifier("fMutSel0_model"), alphabet , nuc_rm});
    }

    return {};
}

optional<vector<double>> get_frequencies_from_tree(const ptree& model_rep, const alphabet& a)
{
    vector<double> pi;
    for(int i=0;i<a.size();i++)
	if (model_rep.count(a.letter(i)))
	    pi.push_back(model_rep.get<double>(a.letter(i)));

    if (pi.size() > 0 and pi.size() < a.size())
    {
	string head = model_rep.get_value<string>();
	throw myexception()<<"For frequency model '"<<head<<"', you must specify all letter frequencies, or none!";
    }

    if (pi.empty())
	return boost::none;
    else
	return pi;
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
/// \param frequencies The initial frequencies for the model.
///
expression_ref process_stack_Frequencies(const ptree& model_rep)
{
    expression_ref R;

    if (model_rep.get_value<string>() == "F" or model_rep.get_value<string>() == "F61")
    {
	expression_ref alphabet_ = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	if (auto pi = get_frequencies_from_tree(model_rep, alphabet_.as_<alphabet>()))
	    R = (identifier("plus_f"), alphabet_, get_list(*pi));
	else
	    R = model_expression({identifier("plus_f_model"),alphabet_});
    }
    else if (model_rep.get_value<string>() == "gwF")
    {
	expression_ref alphabet_ = get_smodel_as("alphabet", model_rep.get_child("alphabet"));
	auto pi = get_frequencies_from_tree(model_rep, alphabet_.as_<alphabet>());
	if (pi and model_rep.count("f"))
	{
	    double f = model_rep.get<double>("f");
	    R = (identifier("plus_gwf"), alphabet_, get_list(*pi), f);
	}
	else
	    R = model_expression({identifier("plus_gwf_model"),alphabet_});
    }
    else if (model_rep.get_value<string>() == "F=uniform")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	R = (identifier("uniform_f_model"),alphabet);
    }
    else if (model_rep.get_value<string>() == "F1x4")
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));
	
	R = model_expression({identifier("f1x4_model"), alphabet});
    }
    else if (model_rep.get_value<string>() == "F3x4") 
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	R = model_expression({identifier("f3x4_model"),alphabet});
    }
    else if (model_rep.get_value<string>() == "MG94") 
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	R = model_expression({identifier("mg94_model"),alphabet});
    }
    else if (model_rep.get_value<string>() == "MG94w9") 
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	R = model_expression({identifier("mg94w9_model"),alphabet});
    }
    /*
      else if (model_rep.get_value<string>() == "F=amino-acids") 
      {
      const Codons* C = dynamic_cast<const Codons*>(&*a);
      if (not C)
      throw myexception()<<"+F=amino-acids: '"<<a->name<<"' is not a codon alphabet.";

      R = AACodonFrequencyModel(*C);
      }
      else if (model_rep.get_value<string>() == "F=triplets") 
      {
      const Triplets* T = dynamic_cast<const Triplets*>(&*a);
      if (not T)
      throw myexception()<<"+F=triplets: '"<<a->name<<"' is not a triplet alphabet.";

      R = TripletsFrequencyModel(*T);
      }
      else if (model_rep.get_value<string>() == "F=codons") 
      {
      const Codons* C = dynamic_cast<const Codons*>(&*a);
      if (not C)
      throw myexception()<<"+F=codons: '"<<a->name<<"' is not a codon alphabet.";

      R = CodonsFrequencyModel(*C);
      }
      else if (model_rep.get_value<string>() == "F=codons2") 
      {
      const Codons* C = dynamic_cast<const Codons*>(&*a);
      if (not C)
      throw myexception()<<"+F=codons2: '"<<a->name<<"' is not a codon alphabet.";

      R = CodonsFrequencyModel2(*C);
      }
    */

    return R;
}

expression_ref process_stack_Multi(const ptree& model_rep)
{
    if (model_rep.get_value<string>() == "gamma") 
    {
	expression_ref submodel = get_smodel_as("RA[a]", model_rep.get_child("submodel"));

	int n = model_rep.get<int>("n");

	return model_expression({identifier("gamma_model"), submodel, n});
    }
    else if (model_rep.get_value<string>() == "gamma_inv") 
    {
	expression_ref base = get_smodel_as("RA[a]", model_rep.get_child("submodel"));

	int n = model_rep.get<int>("n");

	return model_expression({identifier("gamma_inv_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "INV") 
    {
	expression_ref base = get_smodel_as("RA[a]", model_rep.get_child("submodel"));

	return model_expression({identifier("inv_model"), base});
    }
    else if (model_rep.get_value<string>() == "log-normal") 
    {
	expression_ref base = get_smodel_as("RA[a]", model_rep.get_child("submodel"));

	int n = model_rep.get<int>("n");

	return model_expression({identifier("log_normal_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "log-normal_inv") 
    {
	expression_ref base = get_smodel_as("RA[a]", model_rep.get_child("submodel"));

	int n = model_rep.get<int>("n");

	return model_expression({identifier("log_normal_inv_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "multi_freq") {
	// Pr(l|m) = Pr(m|l)*Pr(l)/Pr(m)
	// Idea: store Pr(m|l) = probability a letter is in each model
	//       store Pr(l)   = overall frequencies for each letter
	//       store Pr(m)   = probability of each model
	std::abort();
    }
    //  else if (model_rep.get_value<string>() == "INV")
    //  (a) either specify the FREQUENCIES of the model, or
    //  (b) split every model and make a zero-scaled version of it.

    else if (model_rep.get_value<string>() == "DP") 
    {
	expression_ref base = get_smodel_as("RA[a]", model_rep.get_child("submodel"));

	int n = model_rep.get<int>("n");

	return model_expression({identifier("dp_model"), base, n});
    }
    /*
      else if (model_rep.get_value<string>() == "Modulated")
      {
      formula_expression_ref MM = coerce_to_MM(model_rep.get_child("submodel"));

      //    int n = ... n_base_models();
      //    return Modulated_Markov_E(MM, SimpleExchangeModel(n));
      }
      else if (model_rep.get_value<string>() == "Mixture")
      {
      if (model_args.size() < 4)
      throw myexception()<<"Mixture[..] requires at least two sub-models!";
      if (model_args[1] != "")
      throw myexception()<<"Mixture[..] does not take a submodel (i.e. with '+')";

      vector <formula_expression_ref> models;
      for(int i=0;i<model_args.size()-2;i++)
      models.push_back( coerce_to_MM(model_args[i+2]) );

      return Mixture_Model(models);
      }
      else if (model_rep.get_value<string>() == "M2") 
      {
      formula_expression_ref p1 = def_parameter("M2.fAaINV", 1.0/3, between(0,1));
      formula_expression_ref p2 = def_parameter("M2.fNeutral", 1.0/3, between(0,1));
      formula_expression_ref p3 = def_parameter("M2.fSelected", 1.0/3, between(0,1));
      formula_expression_ref m2_omega = def_parameter("M2.omega", 1.0, lower_bound(0));
      formula_expression_ref D = (identifier("DiscreteDistribution"), Tuple(p1,0.0)&
      Tuple(p2,1.0)&
      Tuple(p3,m2_omega)&
      ListEnd
      );

      D.add_expression( constructor(":~",2) + (p1&(p2&(p3&ListEnd))).exp() + (identifier("dirichlet"), List(1.0, 98.0, 1.0)) );
      D.add_expression( constructor(":~",2) + m2_omega.exp() + (identifier("logExponential"), 0.05) );

      formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_rep,2);

      return (identifier("multiParameter"),M0,D);
      }
      else if (model_rep.get_value<string>() == "M3u") // M3u[n,S,F]
      {
      int n = convertTo<int>(model_args[1]);

      formula_expression_ref D = ListEnd;
      formula_expression_ref F = ListEnd;
      for(int i=n-1;i>=0;i--)
      {
      string pname_f = "M3.f" + convertToString(i+1);
      string pname_w = "M3.omega" + convertToString(i+1);
      formula_expression_ref f = def_parameter("M3.f"     + convertToString(i+1), 1.0/n, between(0,1));
      formula_expression_ref w = def_parameter("M3.omega" + convertToString(i+1), 1.0, lower_bound(0), (identifier("uniform"), 0.0, 1.0));

      D = Tuple(f,w)&D;
      F = f&F;
      }
      D = (identifier("DiscreteDistribution"), D);
      D.add_expression( constructor(":~",2) + F.exp() + (identifier("dirichlet'"), n, 4.0));

      formula_expression_ref M0 = get_M0_omega_function(a,frequencies,model_rep,2);

      return (identifier("multiParameter"), M0, D);
      }
    */
    else if (model_rep.get_value<string>() == "M1a") // M2a[S,F]
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m1a_model"),alphabet,S,R});
    }
    else if (model_rep.get_value<string>() == "M2a") // M2a[S,F]
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m2a_model"),alphabet,S,R});
    }
    else if (model_rep.get_value<string>() == "M2a_Test") // M2a[S,F]
    {
	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m2a_test_model"),alphabet,S,R});
    }
    else if (model_rep.get_value<string>() == "M3") // M[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m3_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M3_Test") // M3_Test[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m3_test_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M7")
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m7_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M8")
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m8_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M8a")
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m8a_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M8b")
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m8b_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "M8a_Test") // M8b[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));

	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("m8a_test_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "branch-site")  // branch-site-test[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));
	
	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("branch_site_test_model"),alphabet,n,S,R});
    }
    else if (model_rep.get_value<string>() == "dp-omega")  // branch-site-test[n,S,F]
    {
	int n = model_rep.get<int>("n");

	expression_ref alphabet = get_smodel_as("alphabet", model_rep.get_child("alphabet"));
	
	expression_ref S = get_smodel_as("EM[a]", model_rep.get_child("nuc_model"));

	expression_ref R = get_smodel_as("FM[a]", model_rep.get_child("freq_model"));

	return model_expression({identifier("dp_omega_model"),alphabet,n,S,R});
    }

    return {};
}

expression_ref get_smodel_(const ptree& model_rep)
{
    if (model_rep.empty() and model_rep.data().empty())
	throw myexception()<<"Can't construct substitution model from empty description!";

    //  std::cout<<"smodel = "<<smodel<<std::endl;
    //  auto result = parse(smodel);
    //  std::cout<<result.get_value<string>()<<"\n";
    //  write_info(std::cout, result);
    //  std::cout<<std::endl;
    //  ptree model_rep = parse(smodel);

    // If we are processing an Int, just return an int.
    if (can_be_converted_to<int>(model_rep.get_value<string>()))
	return model_rep.get_value<int>();

    // If we are processing a Double, just return a double
    if (can_be_converted_to<double>(model_rep.get_value<string>()))
	return model_rep.get_value<double>();

    expression_ref m;

    m = process_stack_functions(model_rep);
    if (m) return m;

    m = process_stack_Markov(model_rep);
    if (m) return m;

    m = process_stack_Frequencies(model_rep);
    if (m) return m;

    m = process_stack_Multi(model_rep);
    if (m) return m;

    throw myexception()<<"Couldn't process substitution model description \""<<show(model_rep)<<"\"";
}

expression_ref get_smodel_as(const string& type, const ptree& model_rep)
{
    if (model_rep.empty() and model_rep.data().empty())
    {
	std::cout<<show(model_rep)<<std::endl;
	throw myexception()<<"Can't construct type '"<<type<<"' from empty description!";
    }

    if (type == "Double" and get_type(model_rep) == "Int")
    {
	double d;
	if (can_be_converted_to<double>(model_rep.get_value<string>(), d))
	    return d;
    }

    if (not can_unify(get_type(model_rep),type))
	throw myexception()<<"Expected type "<<type<<" but got "<<model_rep.get_value<string>()<<" of type "<<get_type(model_rep);

    return get_smodel_(model_rep);
}

/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param smodel_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
expression_ref get_smodel(const ptree& model_rep)
{
    // --------- Convert smodel to MultiMixtureModel ------------//
    expression_ref full_smodel = get_smodel_as("MMM[a]", coerce_to_MMM(model_rep));

    if (log_verbose)
	std::cout<<"full_smodel = "<<full_smodel<<std::endl;

    return full_smodel;
}

expression_ref get_smodel(const string& smodel) 
{
//    std::cout<<"smodel1 = "<<smodel<<std::endl;

    auto model_tree = translate_model("MMM[a]", smodel);
    if (log_verbose)
	std::cout<<"smodel = "<<unparse(model_tree)<<std::endl;
    return get_smodel(model_tree);
}
