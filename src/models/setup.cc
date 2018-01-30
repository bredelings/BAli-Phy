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
#include <boost/optional.hpp>
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

using boost::optional;
using std::string;
using std::pair;
using std::set;
using std::vector;
using std::valarray;
using boost::program_options::variables_map;
using boost::shared_ptr;

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
	return "x3[TN]";
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

bool is_loggable_function(const Rules& R, const string& name)
{
    auto rule = R.get_rule_for_func(name);
    if (not rule) return false;
    return not rule->get("no_log",false);
}

bool is_unlogged_random(const Rules& R, const ptree& model)
{
    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    // 1. If this function is random, then yes.
    if (name == "Sample") return true;

    // 2. If this function is loggable then any random children have already been logged.
    if (is_loggable_function(R, name)) return false;

    // 3. Otherwise check if children are random and unlogged
    for(const auto& p: model)
	if (is_unlogged_random(R, p.second))
	    return true;

    return false;
}

bool should_log(const Rules& R, const ptree& model, const string& arg_name)
{
    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    if (not is_loggable_function(R, name)) return false;

    auto arg = model.get_child(arg_name);

    if (is_unlogged_random(R, arg))
	return true;
    else
	return false;
}

expression_ref arg_to_apply(const ptree& expression)
{
    if (expression.is_a<int>())
	return expression.get_value<int>();
    else if (expression.is_a<double>())
	return expression.get_value<double>();
    else if (expression.is_a<bool>())
	return expression.get_value<bool>();
    else if (is_constant(expression) and expression.is_a<string>())
	return expression.get_value<string>();
	    
    expression_ref E;
    string top = expression.get_value<string>();
    if (top.find('.') == string::npos)
	E = dummy(string("arg_")+top);
    else
	E = dummy(top);

    for(auto& arg: expression)
	E = {E, arg_to_apply(arg.second)};

    return E;
}

expression_ref apply_args(expression_ref action, const ptree& applied_args)
{
    return {action, arg_to_apply(applied_args)};
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

void require_type(const ptree& E, const ptree& required_type, const string& type2, const Rules& rules)
{
    if (not unify(ptree(type2), required_type))
	throw myexception()<<"Expected type '"<<unparse_type(required_type)<<"' but '"<<unparse(E, rules)<<"' of type '"<<unparse_type(type2)<<"'";
}

expression_ref get_constant_model(const ptree& required_type, const ptree& model_rep, const Rules& rules)
{
    // 1. If its an integer constant
    if (model_rep.is_a<int>())
    {
	if (required_type.get_value<string>() == "Double")
	    return {dummy("Prelude.return"), (double)model_rep};

	require_type(model_rep, required_type, "Int", rules);

	return {dummy("Prelude.return"), (int)model_rep};
    }

    // 2. If its an integer constant
    if (model_rep.is_a<double>())
    {
	require_type(model_rep, required_type, "Double", rules);

	return {dummy("Prelude.return"), (double)model_rep};
    }

    // 3. If its a bool constant
    if (model_rep.is_a<bool>())
    {
	require_type(model_rep, required_type, "Bool", rules);

	return {dummy("Prelude.return"), (bool)model_rep};
    }

    string name = model_rep.get_value<string>();

    // 4. If its a string constant
    if (name.size() > 2 and name[0] == '"' and name.back() == '"')
    {
	if (model_rep.size() != 0)
	    throw myexception()<<"An string constant cannot have arguments!";

	require_type(model_rep, required_type, "String", rules);

	return {dummy("Prelude.return"), name.substr(1,name.size()-2)};
    }

    return {};
}

expression_ref get_variable_model(const ptree& E, const set<string>& scope)
{
    if (E.size() or not E.is_a<string>()) return {};

    auto name = E.get_value<string>();
    if (scope.count(name))
	return {dummy("Prelude.return"),dummy(string("arg_") + name)};
    else
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

expression_ref get_model_as(const Rules& R, const ptree& required_type, const ptree& model_rep, const set<string>& scope)
{
    //  std::cout<<"model = "<<model<<std::endl;
    //  auto result = parse(model);
    //  std::cout<<result.get_value<string>()<<"\n";
    //  write_info(std::cout, result);
    //  std::cout<<std::endl;
    //  ptree model_rep = parse(model);

    // 1. Complain on empty expressions
    if (model_rep.empty() and model_rep.value_is_empty())
	throw myexception()<<"Can't construct type '"<<unparse_type(required_type)<<"' from empty description!";

    // 2. Handle constant expressions
    if (auto constant = get_constant_model(required_type, model_rep, R)) return constant;

    // 3. Handle variables
    if (auto variable = get_variable_model(model_rep, scope)) return variable;

    // 4. Now we have a function -- get the rule
    auto name = model_rep.get_value<string>();

    auto rule = R.get_rule_for_func(name);
    if (name == "let")
    {
	// The problem with this is that the order is wrong.
	string var_name = model_rep[1].first;

	Rule arg_var;
	arg_var.push_back({"arg_name",ptree(var_name)});
	arg_var.push_back({"arg_type",ptree("a")});

	Rule arg_body;;
	arg_body.push_back({"arg_name","body"});
	arg_body.push_back({"arg_type",ptree("b")});

	Rule args;
	args.push_back({"",arg_body});
	args.push_back({"",arg_var});
	    
	Rule call("Prelude.id");
	call.push_back({"",ptree("body")});

	Rule r;
	r.push_back({"name",ptree("let")});
	r.push_back({"constraints",ptree()});
	r.push_back({"result_type",ptree("b")});
	r.push_back({"args", args});
	r.push_back({"call", call});

	rule = r;
    }

    if (not rule) throw myexception()<<"No rule for '"<<name<<"'";
    if (not rule->count("call")) throw myexception()<<"No call for '"<<name<<"'";
	
    // 5. Extract parts of the rule
    bool generate_function = rule->get("generate_function",true);
    bool perform_function = rule->get("perform",false);
    bool no_log = rule->get("no_log",false);
    ptree call = rule->get_child("call");
    ptree args = rule->get_child("args");
    
    if (not is_qualified_symbol(call.get_value<string>()) and not is_haskell_builtin_con_name(call.get_value<string>()))
	throw myexception()<<"For rule '"<<name<<"', function '"<<call.get_value<string>()<<"' must be a qualified symbol or a builtin constructor like '(,)', but it is neither!";
    expression_ref E = dummy(call.get_value<string>());

    // This means (i) don't perform the arguments first and (ii) don't add "return" to the result.
    if (not generate_function)
    {
	for(int i=0;i<call.size();i++)
	{
	    string arg_name = array_index(call,i).get_value<string>();
	    ptree arg_tree = get_arg(*rule, arg_name);
	    ptree arg_type = arg_tree.get_child("arg_type");
	    expression_ref arg = get_model_as(R, arg_type, model_rep.get_child(arg_name), scope);
	    E = {E,arg};
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
	E = {E, dummy("arg_" + call_arg_name)};
    }

    // 3. Return the function call: 'return (f call.name1 call.name2 call.name3)'
    if (not perform_function)
	E = {dummy("Prelude.return"),E};

    // 4. Peform the rule arguments 'Prefix "arg_name" (arg_+arg_name) >>= (\arg_name -> (Log "arg_name" arg_name) << E)'
    int i=0;
    for(;i<args.size();i++)
    {
	auto argi = array_index(args,i);

	if (argi.get("no_apply",false)) break;
	string arg_name = argi.get_child("arg_name").get_value<string>();
	ptree arg_tree = get_arg(*rule, arg_name);
	ptree arg_type = arg_tree.get_child("arg_type");
	expression_ref arg = get_model_as(R, arg_type, model_rep.get_child(arg_name), extend_scope(*rule, i, scope));

	// Apply arguments if necessary
	auto applied_args = argi.get_child_optional("applied_args");
	if (applied_args)
	    arg = apply_args(arg, *applied_args);

	auto log_name = name + ":" + arg_name;
	// Prefix "arg_name" (arg_+arg_name)
	if (not no_log) arg = {Prefix, log_name, arg};

	// Wrap the argument in its appropriate Alphabet type
	if (auto alphabet_expression = argi.get_child_optional("alphabet"))
	{
	    auto alphabet_scope = extend_scope(*rule, i, scope);
	    ptree alphabet_type = get_fresh_type_var(alphabet_scope);
	    alphabet_scope.insert(alphabet_type);
	    auto A = get_model_as(R, alphabet_type, *alphabet_expression, alphabet_scope);
	    arg = {dummy("Distributions.set_alphabet"),A,arg};
	}

	// E = Log "arg_name" arg_name >> E
	if (should_log(R, model_rep, arg_name))
	{
	    expression_ref log_action = {Log, log_name, dummy("arg_"+arg_name)};
	    E = {dummy("Prelude.>>"), log_action, E};
	}

	// E = 'arg <<= (\arg_name -> E)
	E = {dummy("Prelude.>>="), arg, lambda_quantify(dummy("arg_"+arg_name), E)};
    }

    for(;i<args.size();i++)
    {
	// E = (\arg_name -> E)
	auto argi = array_index(args,i);
	string arg_name = argi.get_child("arg_name").get_value<string>();
	E = lambda_quantify(dummy("arg_"+arg_name), E);
	continue;
    }

    return E;
}


/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param model_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
model_t get_model(const Rules& R, const ptree& type, const std::set<term_t>& constraints, const ptree& model_rep, const set<string>& scope)
{
    // --------- Convert model to MultiMixtureModel ------------//
    expression_ref full_model = get_model_as(R, type, model_rep, scope);

    if (log_verbose >= 2)
	std::cout<<"full_model = "<<full_model<<std::endl;

    return {model_rep, type, constraints, full_model};
}

model_t get_model(const Rules& R, const string& type, const string& model, const vector<pair<string,string>>& scope)
{
    auto required_type = parse_type(type);
    auto model_rep = parse(R, model);
//    std::cout<<"model1 = "<<show(model_rep)<<std::endl;

    vector<pair<string,ptree>> typed_scope;
    for(auto& x: scope)
	typed_scope.push_back({x.first, parse_type(x.second)});
    auto p = translate_model(R, required_type, model_rep, typed_scope);

    model_rep = p.first;
    auto equations = p.second;
    substitute(equations, model_rep);
    substitute(equations, required_type);
    if (log_verbose >= 1)
    {
	std::cout<<"model = "<<unparse(model_rep, R)<<std::endl;
	std::cout<<"type = "<<unparse_type(required_type)<<std::endl;
	std::cout<<"equations: "<<show(equations)<<std::endl;
	std::cout<<"structure = "<<show(model_rep)<<std::endl;
	std::cout<<std::endl;
    }

    set<string> names_in_scope;
    for(auto& x: scope)
	names_in_scope.insert(x.first);
    return get_model(R, required_type, equations.get_constraints(), model_rep, names_in_scope);
}
