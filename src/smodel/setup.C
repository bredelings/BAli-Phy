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

const vector<vector<vector<string>>> all_default_arguments = 
{
    {{"log","Double","N"}, {"log","x"}, {"x","Double"}},
    {{"Uniform","Double","M"}, {"uniform","a","b"}, {"a","Double"}, {"b","Double"}},
    {{"Normal","Double","M"}, {"normal","mu","sigma"}, {"mu","Double"}, {"sigma","Double"}},
    {{"logNormal","Double","M"}, {"logNormal","lmu","lsigma"}, {"lmu","Double"}, {"lsigma","Double"}},
    {{"EQU","EM"}, {}},
    {{"F81"}, {}},
    {{"HKY","EM"}, {"SModel.hky","alphabet","kappa"}, {"kappa","Double","logNormal[log[2],0.25]"}, {"alphabet","alphabet","default_alphabet"}, },
    {{"TN","EM"}, {"SModel.tn","alphabet","kappaPur","kappaPyr"}, {"kappaPur","Double","logNormal[log[2],0.25]"}, {"kappaPyr","Double","logNormal[log[2],0.25]"}, {"alphabet","alphabet","default_alphabet"}},
    {{"GTR","EM"}, {}, {"*ag"}, {"*at"}, {"*ac"}, {"*gt"}, {"*gc"}, {"*tc"}},
    {{"HKYx3","EM"}, {}, {"kappa","Double","logNormal[log[2],0.25]"}},
    {{"TNx3","EM"}, {}, {"kappaPur","Double","logNormal[log[2],0.25]"}, {"kappaPyr","Double","logNormal[log[2],0.25]"}},
    {{"GTRx3","EM"}, {}, {"*ag"}, {"*at"}, {"*ac"}, {"*gt"}, {"*gc"}, {"*tc"}},
    {{"PAM","EM"}, {}},
    {{"JTT","EM"}, {}},
    {{"WAG","EM"}, {}},
    {{"LG","EM"}, {}},
    {{"Empirical","EM"}, {}, {"filename"}},
    {{"M0","EM"}, {"SModel.m0", "alphabet", "submodel","omega"}, {"submodel","EM","HKY"}, {"omega","Double","Uniform[0,1]"}, {"alphabet","alphabet","default_alphabet"}},
    {{"fMutSel","RA"}, {}, {"submodel","RA"}},
    {{"fMutSel0","RA"}, {}, {"submodel","RA"}},
    {{"INV","MM"}, {}, {"p","Double","Uniform[0,1]"}},
    {{"DP","MM"}, {}, {"n","Int"}, {"submodel","RA"}},
    {{"gamma","MM"}, {}, {"n","Int","4"}, {"*alpha","Double"}, {"submodel","RA"}},
    {{"gamma_inv","MM"}, {}, {"n","Int","4"}, {"*alpha","Double"}, {"p","Double","Uniform[0,1]"}, {"submodel","RA"}},
    {{"log-normal","MM"}, {}, {"n","Int","4"}, {"sigmaOverMu","Double"}, {"submodel","RA"}},
    {{"log-normal_inv","MM"}, {}, {"n","Int","4"}, {"sigmaOverMu","Double"}, {"p","Double","Uniform[0,1]"}, {"submodel","RA"}},
    {{"M1a","MM"}, {}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"M2a","MM"}, {}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"M2a_Test","MM"}, {}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    //    {{"M3u"}, {"3"}, {"nuc_model",""HKY"}, {"freq_model","F61"}},
    {{"M3","MM"}, {}, {"n","Int","4"}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"M3_Test","MM"}, {}, {"n","Int","4"}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"M7","MM"}, {}, {"n","Int","4"}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"M8","MM"}, {}, {"n","Int","4"}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"M8a","MM"}, {}, {"n","Int","4"}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"M8a_Test","MM"}, {}, {"n","Int","4"}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"branch-site","MM"}, {}, {"n","Int","2"}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"dp_omega","MM"}, {}, {"n","Int","4"}, {"nuc_model","EM","HKY"}, {"freq_model","FM","F61"}},
    {{"F","FM"}, {}},
    {{"gwF","FM"}, {}},
    {{"F1x4","FM"}, {}},
    {{"F3x4","FM"}, {}},
    {{"MG94","FM"}, {}},
    {{"MG94w9","FM"}, {}},
    {{"F61","FM"}, {}},
    {{"default_alphabet","alphabet"}, {}},
    {{"RCTMC","RA","N"}, {"reversible_markov","Q","R"}, {"Q","EM"}, {"R","FM"}},
    {{"UnitMixture","MM","N"}, {"unit_mixture","submodel"}, {"submodel","RA"}},
    {{"MMM","MMM","N"}, {"mmm","submodel"}, {"submodel","MM"}}
};

vector<string> get_arg(const vector<vector<string>>& args, const string& s)
{
    for(const auto& arg: args)
	if (arg[0] == s)
	    return arg;
    throw myexception()<<"Function "<<args[0][0]<<" has no argument '"<<s<<"'";
}

vector<vector<vector<string>>> get_args_for_func(const vector<vector<vector<string>>>& all_args, const string& s)
{
    vector<vector<vector<string>>> args;
    for(const auto& arg: all_args)
	if (arg[0][0] == s)
	    args.push_back(arg);
    return args;
}

/// Split a string of the form key=value into {key,value}
ptree parse(const string& s);

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
    if (get_type(model_rep) == "RA")
	return model_rep;

    if (get_type(model_rep) == "EM")
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
    if (get_type(model_rep) == "MM")
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
    if (get_type(model_rep) == "MMM")
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

void set_default_values(ptree& args)
{
    const string& head = args.get_value<string>();
  
    for(const auto& default_arguments: all_default_arguments)
    {
	if (default_arguments[0][0] != head) continue;

	for(int i=2;i<default_arguments.size();i++)
	{
	    const auto& argument = default_arguments[i];
	    string keyword = argument[0];
	    if (keyword[0] == '*')
		keyword = keyword.substr(1);
	    bool has_default = (argument.size() > 2);
	    if (not has_default) continue;
	    string def = argument[2];
      
	    if (not args.count(keyword))
		args.push_back({keyword, parse(def)});
	}
    }
}

void check_required_args(const ptree& args)
{
    //  std::cout<<"checkout args:\n";
    //  write_info(std::cout, args);
    //  std::cout<<show(args)<<std::endl;
    const string& head = args.get_value<string>();
  
    for(const auto& default_arguments: all_default_arguments)
    {
	if (default_arguments[0][0] != head) continue;

	for(int i=2;i<default_arguments.size();i++)
	{
	    string keyword = default_arguments[i][0];
	    if (keyword[0] == '*') continue;
	    if (default_arguments[i].size() > 2) continue;

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

    for(const auto& default_arguments: all_default_arguments)
    {
	if (default_arguments[0][0] != head) continue;

	for(int i=2;i<default_arguments.size();i++)
	{
	    string keyword = default_arguments[i][0];
	    if (keyword[0] == '*')
		keyword = keyword.substr(1);
	    auto& required_type = default_arguments[i][1];

	    if (not args.count(keyword)) continue;
	    auto supplied_type = get_type(args.get_child(keyword));

	    if (required_type == supplied_type) continue;

	    ptree value = args.get_child(keyword);
	    args.erase(keyword);
	    if (required_type == "RA")
		value = coerce_to_RA(value);
	    else if (required_type == "MM")
		value = coerce_to_MM(value);
	    else if (required_type == "MMM")
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

    // 4. Set default values for top level -- this calls parse recursively to handle type checking for args of default values
    set_default_values(result);
  
    // 5. Convert e.g. TN+F -> RCTMC[TN,F]
    if (get_type(result) == "FM" and result.count("submodel"))
    {
	ptree q = result.get_child("submodel");
	ptree r = result;
	r.erase("submodel");
	result = {};
	result.put_value("RCTMC");
	result.push_back({"Q",q});
	result.push_back({"R",r});
    }

    // 6. Coerce arguments to their given type
    check_and_coerce_arg_types(result);
    
    // 7. Check that required arguments are specified
    check_required_args(result);

    return result;
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

expression_ref get_smodel_as(const string& type, const ptree& model_rep,const object_ptr<const alphabet>& a);

expression_ref get_smodel_as(const string& type, const ptree& model_rep);

expression_ref process_stack_functions(const ptree& model_rep, const object_ptr<const alphabet>& a)
{
    string name = model_rep.get_value<string>();

    auto args = get_args_for_func(all_default_arguments, name);
    if (args.size() and args[0][1].size())
    {
	bool no_log = args[0][0].size() > 2 and args[0][0][2] == "N";
	expression_ref E = identifier(args[0][1][0]);
	for(int i=0;i<args[0].size()-2;i++)
	{
	    string arg_name = args[0][1][i+1];
	    string type = get_arg(args[0], arg_name)[1];
	    expression_ref arg = get_smodel_as(type, model_rep.get_child(arg_name), a);
	    if ((type == "Double" or type == "Int") and (not no_log))
		arg = add_logger(arg_name, arg);
	    E = (E,arg);
	}
	if (args[0][0].size() > 2 and args[0][0][2] == "M")
	    E = model_expression(E);
	if (not no_log)
	    E = prefix(name,E);
	
	return E;
    }

    if (model_rep.get_value<string>() == "default_alphabet")
    {
	if (not a)
	    throw myexception()<<"Default alphabet not specified!";
	return *a;
    }
    return {};
}

/// \brief Construct a model from the top of the string stack
///
/// \param string_stack The list of strings representing the substitution model.
/// \param a The alphabet on which the model lives.
///
expression_ref process_stack_Markov(const ptree& model_rep,
				    const object_ptr<const alphabet>& a)
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
	return (identifier("jukes_cantor"),*a);
    else if (model_rep.get_value<string>() == "EQU")
    {
	const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
	if (not N)
	    throw myexception()<<"EQU: '"<<a->name<<"' is not a nucleotide alphabet.";

	return model_expression({identifier("equ_model"),*a});
    }
    else if (model_rep.get_value<string>() == "GTR")
    {
	const Nucleotides* N = dynamic_cast<const Nucleotides*>(&*a);
	if (not N)
	    throw myexception()<<"GTR: '"<<a->name<<"' is not a nucleotide alphabet.";

	// FIXME - allow/make a general GTR model!

	return model_expression({identifier("gtr_model"),*a});
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
	if (dynamic_cast<const Triplets*>(&*a))
	    return model_expression({identifier("hkyx3_model"),*a});
	else
	    throw myexception()<<"HKYx3: '"<<a->name<<"' is not a triplet alphabet.";
    }
    else if (model_rep.get_value<string>() == "TNx3")
    {
	if (dynamic_cast<const Triplets*>(&*a))
	    return model_expression({identifier("tnx3_model"),*a});
	else
	    throw myexception()<<"TNx3: '"<<a->name<<"' is not a triplet alphabet.";
    }
    else if (model_rep.get_value<string>() == "GTRx3")
    {
	if (dynamic_cast<const Triplets*>(&*a))
	    return model_expression({identifier("gtrx3_model"),*a});
	else
	    throw myexception()<<"GTRx3: '"<<a->name<<"' is not a triplet alphabet.";
    }
    else if (model_rep.get_value<string>() == "PAM")
    {
	if (*a != AminoAcids())
	    throw myexception()<<"PAM: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
	return (identifier("SModel.pam"),a);
    }
    else if (model_rep.get_value<string>() == "JTT") {
	if (*a != AminoAcids())
	    throw myexception()<<"JTT: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
	return (identifier("SModel.jtt"),a);
    }
    else if (model_rep.get_value<string>() == "WAG") {
	if (*a != AminoAcids())
	    throw myexception()<<"WAG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
	return (identifier("SModel.wag"),a);
    }
    else if (model_rep.get_value<string>() == "LG") {
	if (*a != AminoAcids())
	    throw myexception()<<"LG: '"<<a->name<<"' is not an 'Amino-Acids' alphabet.";
	return (identifier("SModel.lg"),a);
    }
    else if (model_rep.get_value<string>() == "Empirical") 
    {
	return (identifier("SModel.empirical"),a,model_rep.get<string>("filename"));
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
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<"fMutSel: '"<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref nuc_rm = get_smodel_as("RA",model_rep.get_child("submodel"), const_ptr(N));

	return model_expression({identifier("fMutSel_model"), a , nuc_rm});
    }
    else if (model_rep.get_value<string>() == "fMutSel0")
    {
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<"fMutSel0: '"<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref nuc_rm = get_smodel_as("RA",model_rep.get_child("submodel"), const_ptr(N));

	return model_expression({identifier("fMutSel0_model"), a , nuc_rm});
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
expression_ref process_stack_Frequencies(const ptree& model_rep,
					 const object_ptr<const alphabet>& a)

{
    expression_ref R;

    if (model_rep.get_value<string>() == "F" or model_rep.get_value<string>() == "F61")
    {
	if (model_rep.get_value<string>() == "F61" and a->size() != 61)
	    throw myexception()<<"Cannot use 'F61' frequency model since alphabet contains "<<a->size()<<" letters.";

	if (auto pi = get_frequencies_from_tree(model_rep, *a))
	    R = (identifier("plus_f"), a, get_list(*pi));
	else
	    R = model_expression({identifier("plus_f_model"),a});
    }
    else if (model_rep.get_value<string>() == "gwF")
    {
	auto pi = get_frequencies_from_tree(model_rep, *a);
	if (pi and model_rep.count("f"))
	{
	    double f = model_rep.get<double>("f");
	    R = (identifier("plus_gwf"), a, get_list(*pi), f);
	}
	else
	    R = model_expression({identifier("plus_gwf_model"),a});
    }
    else if (model_rep.get_value<string>() == "F=uniform") 
	R = (identifier("uniform_f_model"),a);
    else if (model_rep.get_value<string>() == "F1x4")
    {
	if (not dynamic_cast<const Triplets*>(&*a))
	    throw myexception()<<"+F1x4: '"<<a->name<<"' is not a triplet alphabet.";

	if (auto nuc_pi = get_frequencies_from_tree(model_rep, dynamic_cast<const Triplets*>(&*a)->getNucleotides()))
	    R = (identifier("f1x4"), a, get_list(*nuc_pi));
	else
	    R = model_expression({identifier("f1x4_model"), a});
    }
    else if (model_rep.get_value<string>() == "F3x4") 
    {
	if (not dynamic_cast<const Triplets*>(&*a))
	    throw myexception()<<"+F1x4: '"<<a->name<<"' is not a triplet alphabet.";
	R = model_expression({identifier("f3x4_model"),a});
    }
    else if (model_rep.get_value<string>() == "MG94") 
    {
	if (not dynamic_cast<const Triplets*>(&*a))
	    throw myexception()<<"+MG94w9: '"<<a->name<<"' is not a triplet alphabet.";

	R = model_expression({identifier("mg94_model"),a});
    }
    else if (model_rep.get_value<string>() == "MG94w9") 
    {
	if (not dynamic_cast<const Triplets*>(&*a))
	    throw myexception()<<"+MG94w9: '"<<a->name<<"' is not a triplet alphabet.";

	R = model_expression({identifier("mg94w9_model"),a});
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

    if (R and model_rep.count("submodel"))
    {
	// If the frequencies.size() != alphabet.size(), this call will throw a meaningful exception.
	expression_ref s = get_smodel_as("EM",model_rep.get_child("submodel"), a);
	expression_ref mm = model_expression({identifier("reversible_markov_model"), s, R});
	return mm;
    }

    return R;
}

expression_ref process_stack_Multi(const ptree& model_rep,
				   const object_ptr<const alphabet>& a)
{
    if (model_rep.get_value<string>() == "gamma") 
    {
	expression_ref submodel = get_smodel_as("RA", model_rep.get_child("submodel"), a);

	int n = model_rep.get<int>("n");

	return model_expression({identifier("gamma_model"), submodel, n});
    }
    else if (model_rep.get_value<string>() == "gamma_inv") 
    {
	expression_ref base = get_smodel_as("RA", model_rep.get_child("submodel"),a);

	int n = model_rep.get<int>("n");

	return model_expression({identifier("gamma_inv_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "INV") 
    {
	expression_ref base = get_smodel_as("RA", model_rep.get_child("submodel"), a);

	return model_expression({identifier("inv_model"), base});
    }
    else if (model_rep.get_value<string>() == "log-normal") 
    {
	expression_ref base = get_smodel_as("RA", model_rep.get_child("submodel"),a);

	int n = model_rep.get<int>("n");

	return model_expression({identifier("log_normal_model"), base, n});
    }
    else if (model_rep.get_value<string>() == "log-normal_inv") 
    {
	expression_ref base = get_smodel_as("RA", model_rep.get_child("submodel"),a);

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
	expression_ref base = get_smodel_as("RA", model_rep.get_child("submodel"),a);

	int n = model_rep.get<int>("n");

	return model_expression({identifier("dp_model"), base, n});
    }
    /*
      else if (model_rep.get_value<string>() == "Modulated")
      {
      formula_expression_ref MM = coerce_to_MM(model_rep.get_child("submodel"),a);

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
      models.push_back( coerce_to_MM(model_args[i+2], a) );

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
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

	expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

	return model_expression({identifier("m1a_model"),a,S,R});
    }
    else if (model_rep.get_value<string>() == "M2a") // M2a[S,F]
    {
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

	expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

	return model_expression({identifier("m2a_model"),a,S,R});
    }
    else if (model_rep.get_value<string>() == "M2a_Test") // M2a[S,F]
    {
	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
	const Nucleotides& N = C->getNucleotides();

	expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

	expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

	return model_expression({identifier("m2a_test_model"),a,S,R});
    }
    else if (model_rep.get_value<string>() == "M3") // M[n,S,F]
    {
	int n = model_rep.get<int>("n");

	const Codons* C = dynamic_cast<const Codons*>(&*a);
	if (not C)
	    throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

    expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

    return model_expression({identifier("m3_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M3_Test") // M3_Test[n,S,F]
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

    expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

    return model_expression({identifier("m3_test_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M7")
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

    expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

    return model_expression({identifier("m7_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M8")
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

    expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

    return model_expression({identifier("m8_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M8a")
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

    expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

    return model_expression({identifier("m8a_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M8b")
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

    expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

    return model_expression({identifier("m8b_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "M8a_Test") // M8b[n,S,F]
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

    expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

    return model_expression({identifier("m8a_test_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "branch-site")  // branch-site-test[n,S,F]
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

    expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

    return model_expression({identifier("branch_site_test_model"),a,n,S,R});
  }
  else if (model_rep.get_value<string>() == "dp-omega")  // branch-site-test[n,S,F]
  {
    int n = model_rep.get<int>("n");

    const Codons* C = dynamic_cast<const Codons*>(&*a);
    if (not C)
      throw myexception()<<a->name<<"' is not a 'Codons' alphabet";
    const Nucleotides& N = C->getNucleotides();

    expression_ref S = get_smodel_as("EM", model_rep.get_child("nuc_model"), const_ptr(N));

    expression_ref R = get_smodel_as("FM", model_rep.get_child("freq_model"), a);

    return model_expression({identifier("dp_omega_model"),a,n,S,R});
  }

  return {};
}

expression_ref 
get_smodel_(const ptree& model_rep, const object_ptr<const alphabet>& a) 
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

  m = process_stack_functions(model_rep, a);
  if (m) return m;

  m = process_stack_Markov(model_rep, a);
  if (m) return m;

  m = process_stack_Frequencies(model_rep, a);
  if (m) return m;

  m = process_stack_Multi(model_rep, a);
  if (m) return m;

  throw myexception()<<"Couldn't process substitution model description \""<<show(model_rep)<<"\"";
}

expression_ref get_smodel_as(const string& type, const ptree& model_rep, const object_ptr<const alphabet>& a)
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

    if (get_type(model_rep) != type)
	throw myexception()<<"Expected type "<<type<<" but got "<<model_rep.get_value<string>()<<" of type "<<get_type(model_rep);

    return get_smodel_(model_rep, a);
}

expression_ref get_smodel_as(const string& type, const ptree& model_rep)
{
    return get_smodel_as(type, model_rep, {});
}


/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param smodel_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
expression_ref
get_smodel(const ptree& model_rep, const object_ptr<const alphabet>& a)
{
  assert(frequencies->size() == a->size());

  // --------- Convert smodel to MultiMixtureModel ------------//
  expression_ref full_smodel = get_smodel_as("MMM", coerce_to_MMM(model_rep),a);

  if (log_verbose)
      std::cout<<"full_smodel = "<<full_smodel<<std::endl;

  return full_smodel;
}

expression_ref
get_smodel(const string& smodel, const object_ptr<const alphabet>& a) 
{
//    std::cout<<"smodel1 = "<<smodel<<std::endl;

    if (log_verbose)
	std::cout<<"smodel = "<<unparse(parse(smodel))<<std::endl;
    return get_smodel(parse(smodel), a);
}

expression_ref get_smodel(const string& smodel_name,const alignment& A) 
{
  return get_smodel(smodel_name, const_ptr( A.get_alphabet() ));
}
