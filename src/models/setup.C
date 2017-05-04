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
#include "models/setup.H"
#include "util.H"
#include "myexception.H"
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/info_parser.hpp>
#include <boost/optional.hpp>
#include <boost/property_tree/json_parser.hpp>
#include "models/rules.H"
#include "models/parse.H"
#include "models/translate.H"
#include "computation/model_expression.H"
#include "computation/expression/expression.H"
#include "computation/expression/dummy.H"
#include "computation/expression/lambda.H"
#include "computation/expression/tuple.H"
#include "computation/expression/list.H"
#include "computation/operations.H"

using boost::property_tree::ptree;
using boost::optional;
using std::string;
using std::pair;
using std::set;
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

/// Split a string of the form key=value into {key,value}
string show(const ptree& pt, int depth)
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

auto index(const ptree& p, int i)
{
    if (i > p.size())
	throw myexception()<<"Can't get entry "<<i<<" for tree with size "<<p.size();
    auto it = p.begin();
    for(int j=0;j<i;j++)
	it++;
    return *it;
}

ptree array_index(const ptree& p, int i)
{
    return index(p,i).second;
}

bool is_loggable_function(const string& name)
{
    auto rule = get_rule_for_func(name);
    if (not rule) return false;
    return not rule->get("no_log",false);
}

bool is_unlogged_random(const ptree& model)
{
    auto name = model.get_value<string>();

    // 1. If this function is random, then yes.
    if (name == "Sample") return true;

    // 2. If this function is loggable then any random children have already been logged.
    if (is_loggable_function(name)) return false;

    // 3. Otherwise check if children are random and unlogged
    for(const auto& p: model)
	if (is_unlogged_random(p.second))
	    return true;

    return false;
}

bool should_log(const ptree& model, const string& arg_name)
{
    auto name = model.get_value<string>();

    if (not is_loggable_function(name)) return false;

    auto arg = model.get_child(arg_name);

    if (is_unlogged_random(arg))
	return true;
    else
	return false;
}

expression_ref arg_to_apply(const ptree& expression)
{
    expression_ref E;
    string top = expression.get_value<string>();
    if (top.find('.') == string::npos)
	E = dummy(string("arg_")+top);
    else
	E = dummy(top);

    for(auto& arg: expression)
	E = (E, arg_to_apply(arg.second));

    return E;
}

expression_ref apply_args(expression_ref action, const ptree& applied_args)
{
    return (action, arg_to_apply(applied_args));
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

void require_type(const ptree& E, const ptree& required_type, const string& type2)
{
    string name = E.get_value<string>();

    if (not unify(ptree(type2), required_type))
	throw myexception()<<"Expected type '"<<unparse_type(required_type)<<"' but got '"<<name<<"' of type 'Int'";
}

expression_ref get_constant_model(const ptree& required_type, const ptree& model_rep)
{
    string name = model_rep.get_value<string>();

    // 1. If its an integer constant
    if (can_be_converted_to<int>(name))
    {
	if (model_rep.size() != 0)
	    throw myexception()<<"An integer constant cannot have arguments!";

	if (required_type.get_value<string>() == "Double")
	    return (dummy("Prelude.return"), convertTo<double>(name));

	require_type(model_rep, required_type, "Int");

	return (dummy("Prelude.return"), convertTo<int>(name));
    }

    // 2. If its an integer constant
    else if (can_be_converted_to<double>(name))
    {
	if (model_rep.size() != 0)
	    throw myexception()<<"An floating point constant cannot have arguments!";

	require_type(model_rep, required_type, "Double");

	return (dummy("Prelude.return"), convertTo<double>(name));
    }

    // 3. If its a string constant
    else if (name.size() > 2 and name[0] == '"' and name.back() == '"')
    {
	if (model_rep.size() != 0)
	    throw myexception()<<"An string constant cannot have arguments!";

	require_type(model_rep, required_type, "String");

	return (dummy("Prelude.return"), name.substr(1,name.size()-2));
    }

    return {};
}

expression_ref get_variable_model(const ptree& E, const set<string>& scope)
{
    if (not E.size())
    {
	auto name = E.get_value<string>();
	if (scope.find("name") != scope.end())
	    return dummy(string("arg_") + name);
    }
    return {};
}


set<string> extend_scope(const ptree& rule, int skip, const set<string>& scope)
{
    auto scope2 = scope;
    int i=0;
    for(const auto& arg: rule.get_child("args"))
    {
	i++;
	if (i < skip) continue;

	const auto& argument = arg.second;
	string arg_name = argument.get<string>("arg_name");

	scope2.insert(arg_name);
    }

    return scope2;
}

expression_ref get_model_as(const ptree& required_type, const ptree& model_rep, const set<string>& scope)
{
    //  std::cout<<"model = "<<model<<std::endl;
    //  auto result = parse(model);
    //  std::cout<<result.get_value<string>()<<"\n";
    //  write_info(std::cout, result);
    //  std::cout<<std::endl;
    //  ptree model_rep = parse(model);

    // 1. Complain on empty expressions
    if (model_rep.empty() and model_rep.data().empty())
	throw myexception()<<"Can't construct type '"<<unparse_type(required_type)<<"' from empty description!";

    // 2. Handle constant expressions
    if (auto constant = get_constant_model(required_type, model_rep)) return constant;

    // 3. Handle variables
    if (auto variable = get_variable_model(model_rep, scope)) return variable;

    // 4. Now we have a function -- get the rule
    auto name = model_rep.get_value<string>();
    auto rule = get_rule_for_func(name);

    if (not rule) throw myexception()<<"No rule for '"<<name<<"'";
    if (not rule->count("call")) throw myexception()<<"No call for '"<<name<<"'";
	
    // 5. Extract parts of the rule
    bool pass_arguments = rule->get("pass_arguments",false);
    bool is_list_rule = rule->get("list_arguments",false);
    bool generate_function = rule->get("generate_function",false);
    bool no_log = rule->get("no_log",false);
    ptree call = rule->get_child("call");
    ptree args = rule->get_child("args");
    
    expression_ref E = qualified_dummy(call.get_value<string>());
    if (pass_arguments)
    {
	ptree arg_type = get_type_for_arg(*rule, "*");
	vector<expression_ref> arguments;
	for(const auto& child: model_rep)
	{
	    expression_ref arg = get_model_as(arg_type, child.second, scope);
	    arguments.push_back(Tuple(child.first, arg));
	}
	return (E,get_list(arguments));
    }
    else if (is_list_rule)
    {
	ptree arg_type = get_type_for_arg(*rule, "*");
	vector<expression_ref> arguments;
	for(const auto& child: model_rep)
	    arguments.push_back( get_model_as(arg_type, child.second, scope) );
	return (E,get_list(arguments));
    }
    else if (not generate_function)
    {
	for(int i=0;i<call.size();i++)
	{
	    string arg_name = array_index(call,i).get_value<string>();
	    ptree arg_tree = get_arg(*rule, arg_name);
	    if (arg_tree.get("no_apply",false)) continue;

	    ptree arg_type = arg_tree.get_child("arg_type");
	    expression_ref arg = get_model_as(arg_type, model_rep.get_child(arg_name), scope);
	    E = (E,arg);
	}
	return E;
    }

    auto Prefix = lambda_expression( constructor("Distributions.Prefix",2) );
    auto Log = lambda_expression( constructor("Distributions.Log",2) );

    // 2. Apply the arguments listed in the call : 'f call.name1 call.name2 call.name3'
    //    There could be fewer of these than the rule arguments.
    for(int i=0;i<call.size();i++)
    {
	string call_arg_name = array_index(call,i).get_value<string>();
	E = (E,dummy("arg_" + call_arg_name));
    }

    // 3. Return the function call: 'return (f call.name1 call.name2 call.name3)'
    E = (dummy("Prelude.return"),E);

    // 4. Peform the rule arguments 'Prefix "arg_name" (arg_+arg_name) >>= (\arg_name -> (Log "arg_name" arg_name) << E)'
    for(int i=0;i<args.size();i++)
    {
	auto argi = array_index(args,i);

	// No need to perform or log lambda arguments.
	if (argi.get("no_apply",false)) continue;

	string arg_name = array_index(argi,0).get_value<string>();
	ptree arg_tree = get_arg(*rule, arg_name);
	ptree arg_type = arg_tree.get_child("arg_type");

	// E = Log "arg_name" arg_name >> E
	if (should_log(model_rep, arg_name))
	{
	    auto log_action = (Log,arg_name,dummy("arg_"+arg_name));
	    E = (dummy("Prelude.>>"),log_action,E);
	}

	// Apply arguments if necessary
	expression_ref action = dummy("xarg_"+arg_name);
	auto applied_args = argi.get_child_optional("applied_args");
	if (applied_args)
	    action = apply_args(action, *applied_args);

	// Prefix "arg_name" (arg_+arg_name)
	if (not no_log) action = (Prefix, arg_name, action);

	// E = 'action <<=
	E = (dummy("Prelude.>>="), action, lambda_quantify(dummy("arg_"+arg_name), E));
    }

    if (not no_log) E = (Prefix, name, E);

    for(int j=args.size()-1;j>=0;j--)
    {
	auto argi = array_index(args,j);
	string arg_name = array_index(argi,0).get_value<string>();
	if (argi.get("no_apply",false))
	    E = lambda_quantify(dummy("arg_"+arg_name),E); // These args are not performed.
	else
	    E = lambda_quantify(dummy("xarg_"+arg_name),E);
    }

//	std::cerr<<E<<"\n";

    for(int i=0;i<args.size();i++)
    {
//	    std::cerr<<show(array_index(args,i))<<"\n";
	string arg_name = array_index(args,i).get<string>("arg_name");
	ptree arg_tree = get_arg(*rule, arg_name);
	if (arg_tree.get("no_apply",false)) continue;

	ptree arg_type = arg_tree.get_child("arg_type");
	expression_ref arg = get_model_as(arg_type, model_rep.get_child(arg_name), extend_scope(*rule, i, scope));
	E = (E,arg);
    }

    return E;
}


/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param model_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
model_t get_model(const ptree& type, const ptree& model_rep)
{
    // --------- Convert model to MultiMixtureModel ------------//
    expression_ref full_model = get_model_as(type, model_rep, {});

    if (log_verbose)
	std::cout<<"full_model = "<<full_model<<std::endl;

    return {model_rep, type, full_model};
}

model_t get_model(const string& type, const string& model)
{
    auto required_type = parse_type(type);
    auto model_rep = parse(model);
//    std::cout<<"model1 = "<<show(model_rep)<<std::endl;

    auto p = translate_model(required_type, model_rep);
    model_rep = p.first;
    auto equations = p.second;
    substitute(equations, model_rep);
    substitute(equations, required_type);
    if (log_verbose)
    {
	std::cout<<"model = "<<unparse(model_rep)<<std::endl;
	std::cout<<"type = "<<unparse_type(required_type)<<std::endl;
	std::cout<<show(equations)<<std::endl;
    }
    return get_model(required_type, model_rep);
}
