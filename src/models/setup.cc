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
#include "computation/module.H"
#include "computation/expression/expression.H"
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/tuple.H"
#include "computation/expression/list.H"
#include "computation/operations.H"
#include "model.H"

using boost::optional;
using std::string;
using std::pair;
using std::set;
using std::map;
using std::vector;
using std::valarray;
using boost::program_options::variables_map;
using boost::shared_ptr;

string model_t::show(const Rules& rules, bool top) const
{
    if (top)
	return show_model(extract_value(description), rules);
    else
	return unparse(extract_value(description), rules);
}

string model_t::show_pretty(const Rules& rules, bool top) const
{
    auto p = pretty_model_t(description);
    return p.show(rules, not top);
}

string model_t::show_main(const Rules& rules, bool top) const
{
    auto p = pretty_model_t(description);
    return p.show_main(rules, top);
}

string model_t::show_extracted(const Rules& rules) const
{
    auto p = pretty_model_t(description);
    return p.show_extracted(rules);
}

model_t::model_t(const ptree& d, const ptree&t, const std::set<term_t>& c, const expression_ref& e)
    :description(d), type(t), constraints(c), expression(e)
{
}

void to_json(json& j, const pretty_model_t& m)
{
    j["main"] = unparse_annotated(m.main);
    json extracted = json::array();
    for(int i=0;i<m.terms.size();i++)
    {
	json p = json::array();
	p[0] = m.term_names[i];
	p[1] = m.terms[i];
	extracted.push_back(p);
    }
    j["extracted"] = extracted;
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
	return "tn93";
    else if (dynamic_cast<const AminoAcidsWithStop*>(&a))
	return "";
    else if (dynamic_cast<const AminoAcids*>(&a))
	return "lg08";
    else if (dynamic_cast<const Codons*>(&a))
	return "gy94";
    else if (dynamic_cast<const Triplets*>(&a))
	return "x3[tn93_sym]";
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
    if (name == "let") return true;

    if (name == "function") return true;

    if (not rule) return false;
    return not rule->get("no_log",false);
}

enum class var_type_t {unknown=0,constant,random,lambda};

typedef map<string,var_type_t> names_in_scope_t;

bool is_random(const ptree& model, const names_in_scope_t& scope)
{
    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    // 1. If this function is random, then yes.
    if (name == "sample") return true;

    // 2. If this is a random variable, then yes.
    if (not model.size() and model.is_a<string>())
	if (scope.count(name) and scope.at(name) == var_type_t::random) return true;

    // 3. Otherwise check if children are random and unlogged
    for(const auto& p: model)
	if (is_random(p.second, scope))
	    return true;

    return false;
}

bool is_unlogged_random(const Rules& R, const ptree& model, const names_in_scope_t& scope)
{
    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    // 1. If this function is random, then yes.
    if (name == "sample") return true;

    // 2. If this is a random variable, then yes.
    if (not model.size() and model.is_a<string>())
	if (scope.count(name) and scope.at(name) == var_type_t::random) return true;

    // 3. If this function is loggable then any random children have already been logged.
    if (is_loggable_function(R, name)) return false;

    // 4. Otherwise check if children are random and unlogged
    for(const auto& p: model)
	if (is_unlogged_random(R, p.second, scope))
	    return true;

    return false;
}

bool should_log(const Rules& R, const ptree& model, const string& arg_name, const names_in_scope_t& scope)
{
    auto name = model.get_value<string>();

    if (is_constant(model)) return false;

    if (not is_loggable_function(R, name)) return false;

    auto arg = model.get_child(arg_name);

    if (is_unlogged_random(R, arg, scope))
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
	E = var(string("arg_")+top);
    else
	E = var(top);

    for(auto& arg: expression)
	E = {E, arg_to_apply(arg.second)};

    return E;
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

names_in_scope_t extend_scope(names_in_scope_t scope, const string& var, var_type_t t)
{
    if (scope.count(var))
	scope.erase(var);
    scope.insert({var, t});
    return scope;
}

names_in_scope_t extend_scope(const ptree& rule, int skip, const names_in_scope_t& scope)
{
    auto scope2 = scope;
    int i=0;
    for(const auto& arg: rule.get_child("args"))
    {
	i++;
	if (i < skip) continue;

	const auto& argument = arg.second;
	string arg_name = argument.get<string>("arg_name");

	scope2.erase(arg_name);
	scope2.insert({arg_name,var_type_t::unknown});
    }

    return scope2;
}

int get_index_for_arg_name(const ptree& rule, const string& arg_name)
{
    ptree args = rule.get_child("args");
    for(int i=0; i < args.size(); i++)
    {
	auto argi = array_index(args,i);
	if (arg_name == argi.get_child("arg_name").get_value<string>())
	    return i;
    }
    throw myexception()<<"No arg named '"<<arg_name<<"'";
}

pair<expression_ref,set<string>> get_model_as(const Rules& R, const ptree& model_rep, const names_in_scope_t& scope);

expression_ref parse_constant(const ptree& model)
{
    if (model.value_is_empty())
	throw myexception()<<"parse_constant( ): got a null value!";

    if (model.is_a<int>()) return (int)model;
    if (model.is_a<double>()) return (double)model;
    if (model.is_a<bool>()) return (bool)model;
    string name = model.get_value<string>();
    if (name.size() > 2 and name[0] == '"' and name.back() =='"') return name.substr(1,name.size()-2);
    return {};
}

expression_ref get_constant_model(const ptree& model_rep)
{
    if (expression_ref C = parse_constant(model_rep))
    {
	if (model_rep.size() != 0) throw myexception()<<"An constant cannot have arguments!\n  '"<<model_rep.show()<<"'";
	return {var("Prelude.return"), Tuple(C,List())};
    }
    else
	return {};
}

optional<pair<expression_ref,set<string>>> get_variable_model(const ptree& E, const names_in_scope_t& scope)
{
    if (E.size() or not E.is_a<string>()) return {};

    auto name = E.get_value<string>();

    // 1. If the name is not in scope then we are done.
    if (not scope.count(name)) return boost::none;

    auto x = var(string("arg_")+name);
    expression_ref V;
    set<string> lambda_vars;

    // 2. If the name is a lambda var, then we need to quantify it, and put it into the list of free lambda vars
    if (scope.at(name) == var_type_t::lambda)
    {
	V = lambda_quantify(x,x);
	lambda_vars.insert(name);
    }
    // 3. Otherwise the expression is just the variable itself
    else
	V = x;

    // 4. Construct the logging tuple and return it in order to allow this action to be performed.
    V = {var("Prelude.return"),Tuple(V,List())};

    // 5. We need an extra level of {} to allow constructing the optional.
    return {{V,lambda_vars}};
}


optional<pair<expression_ref,set<string>>> get_model_let(const Rules& R, const ptree& model_rep, const names_in_scope_t& scope)
{
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a let, then we are done.
    if (name != "let") return boost::none;

    string var_name = model_rep[0].first;
    ptree var_exp = model_rep[0].second;
    ptree body_exp = model_rep[1].second;

    auto var_type = is_random(var_exp, scope)?var_type_t::random : var_type_t::unknown;

    // 1. Perform the body with var_name in scope
    auto p = get_model_as(R, body_exp, extend_scope(scope, var_name, var_type));
    expression_ref E = p.first;

    // 2. Perform the variable expression
    {
	auto arg_p = get_model_as(R, var_exp, scope);
	expression_ref arg = arg_p.first;
	if (arg_p.second.size())
	    throw myexception()<<"You cannot let-bind a variable to an expression with a function-variable";

	// E = 'arg <<= (\pair_var_name -> let {arg_name=fst pair_var_name} in E)
	E = lambda_quantify(var("pair_arg_"+var_name), let_expression({{var("arg_"+var_name),{var("Prelude.fst"),var("pair_arg_"+var_name)}}},E));

	E = {var("Prelude.>>="), arg, E};
    }

    return {{E, p.second}};
}

optional<pair<expression_ref,set<string>>> get_model_lambda(const Rules& R, const ptree& model_rep, const names_in_scope_t& scope)
{
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a lambda, then we are done.
    if (name != "function") return boost::none;

    // 2. Get the variable name and the body from the model
    string var_name = model_rep[0].first;
    var x("arg_" + var_name);
    var pair_arg_body("pair_arg_body");
    ptree body_exp = model_rep[1].second;

    // 3. Parse the body with the lambda variable in scope, and find the free variables.
    auto body_E = get_model_as(R, body_exp, extend_scope(scope, var_name, var_type_t::lambda));
    expression_ref body = body_E.first;
    auto lambda_vars = body_E.second;

    // E = E x l1 l2 l3
    expression_ref E = {var("Prelude.fst"),pair_arg_body};
    for(auto& vname: lambda_vars)
	E = {E,var("arg_"+vname)};

    // E = \x -> E
    E = lambda_quantify(x, E);
    
    // Remove x from the lambda vars.
    if (lambda_vars.count(var_name)) lambda_vars.erase(var_name);

    // E = \l1 l2 l3 -> E
    for(auto& vname: std::reverse(lambda_vars))
	E = lambda_quantify(var("arg_"+vname),E);

    // E = return $ (E,snd pair_arg_body)
    E = {var("Prelude.return"),Tuple(E,{var("Prelude.snd"),var("pair_arg_body")})};

    // E = do {pair_arg_body <- body ; E}
    E = {var("Prelude.>>="),body,lambda_quantify(pair_arg_body,E)};


    // In summary, we have
    //E = do
    //      pair_arg_body <- body_action
    //      return $ (\l1 l2 l3 -> \x -> ((fst pair_air_body) x l1 l2 l3) , snd pair_arg_body)
    return {{E, lambda_vars}};
}


pair<expression_ref,set<string>> get_model_as(const Rules& R, const ptree& model_rep, const names_in_scope_t& scope)
{
    //  std::cout<<"model = "<<model<<std::endl;
    //  auto result = parse(model);
    //  std::cout<<result.get_value<string>()<<"\n";
    //  write_info(std::cout, result);
    //  std::cout<<std::endl;
    //  ptree model_rep = parse(model);

    // 1. Complain on empty expressions
    if (model_rep.empty() and model_rep.value_is_empty())
	throw myexception()<<"Can't construct model from from empty description!";

    // 2. Handle constant expressions
    else if (auto constant = get_constant_model(model_rep))
	return {constant,{}};

    // 3. Handle variables
    else if (auto variable = get_variable_model(model_rep, scope))
	return *variable;

    // 4. Let expressions
    else if (auto let = get_model_let(R, model_rep, scope))
	return *let;

    // 5. Let expressions
    else if (auto func = get_model_lambda(R, model_rep, scope))
	return *func;

    auto name = model_rep.get_value<string>();

    // 5. Now we have a function -- get the rule
    auto rule = R.get_rule_for_func(name);
    if (not rule) throw myexception()<<"No rule for '"<<name<<"'";
    if (not rule->count("call")) throw myexception()<<"No call for '"<<name<<"'";
	
    // 6. Extract parts of the rule
    bool perform_function = rule->get("perform",false);
    ptree call = rule->get_child("call");
    ptree args = rule->get_child("args");
    
    if (not is_qualified_symbol(call.get_value<string>()) and not is_haskell_builtin_con_name(call.get_value<string>()))
	throw myexception()<<"For rule '"<<name<<"', function '"<<call.get_value<string>()<<"' must be a qualified symbol or a builtin constructor like '(,)', but it is neither!";
    expression_ref E = var(call.get_value<string>());

    // 7. Parse models for arguments to figure out which free lambda variables they contain
    vector<expression_ref> arg_models;
    vector<set<string>> arg_lambda_vars;
    set<string> lambda_vars;
    for(int i=0;i<args.size();i++)
    {
	auto argi = array_index(args,i);

	string arg_name = argi.get_child("arg_name").get_value<string>();
	auto m = get_model_as(R, model_rep.get_child(arg_name), extend_scope(*rule, i, scope));
	if (perform_function and m.second.size())
	    throw myexception()<<"Argument '"<<arg_name<<"' of '"<<name<<"' contains a lambda variable: not allowed!";

	arg_models.push_back(m.first);
	arg_lambda_vars.push_back(m.second);
	add(lambda_vars, m.second);
    }

    // 8a. Apply the arguments listed in the call : 'f call.name1 call.name2 call.name3'
    //    There could be fewer of these than the rule arguments.
    //    Also each arg needs to have its own lambda vars applied to it.
    for(int i=0;i<call.size();i++)
    {
	string call_arg_name = array_index(call,i).get_value<string>();
	// check that arg_name is a valid argument
	get_arg(*rule, call_arg_name);
	expression_ref arg = var("arg_" + call_arg_name);

	// Apply the free lambda variables to arg before using it.
	int index = get_index_for_arg_name(*rule, call_arg_name);
	for(auto& vname: arg_lambda_vars[index])
	    arg = {arg, var("arg_"+vname)};

	E = {E, arg};
    }
    // 8b. Return a lambda function 
    for(auto& vname: std::reverse(lambda_vars))
	E = lambda_quantify(var("arg_"+vname),E);

    // 9. Compute loggers
    expression_ref loggers = List();
    for(int i=0;i<args.size();i++)
    {
	auto argi = array_index(args,i);

	string arg_name = argi.get_child("arg_name").get_value<string>();

	auto log_name = name + ":" + arg_name;

	bool do_log = arg_lambda_vars[i].empty() and should_log(R, model_rep, arg_name, scope);
	loggers = {var("Distributions.add_logger"),loggers,log_name,var("pair_arg_"+arg_name),do_log};
    }

    // 10. Return the function call: 'return (f call.name1 call.name2 call.name3)'
    expression_ref Return = var("Prelude.return");
    if (not perform_function)
	E = {Return,E};

    E = {var("Prelude.>>="), E, lambda_quantify(var("result"),{Return,Tuple(var("result"),loggers)})};

    // 11. Peform the rule arguments 'Prefix "arg_name" (arg_+arg_name) >>= (\arg_name -> (Log "arg_name" arg_name) << E)'
    for(int i=0;i<args.size();i++)
    {
	auto argi = array_index(args,i);

	string arg_name = argi.get_child("arg_name").get_value<string>();
	expression_ref arg = arg_models[i];

	auto log_name = name + ":" + arg_name;

	// Wrap the argument in its appropriate Alphabet type
	if (auto alphabet_expression = argi.get_child_optional("alphabet"))
	{
	    auto alphabet_scope = extend_scope(*rule, i, scope);
//	    ptree alphabet_type = get_fresh_type_var(alphabet_scope);
//	    alphabet_scope.insert(alphabet_type);
	    auto A_pair = get_model_as(R, *alphabet_expression, alphabet_scope);
	    if (A_pair.second.size())
		throw myexception()<<"An alphabet cannot depend on a lambda variable!";
	    auto& A = A_pair.first;
	    arg = {var("Distributions.set_alphabet"),A,arg};
	}

	// E = 'arg <<= (\arg_name_pair -> let {arg_name=fst arg_name_pair} in E)
	E = lambda_quantify(var("pair_arg_"+arg_name), let_expression({{var("arg_"+arg_name),{var("Prelude.fst"),var("pair_arg_"+arg_name)}}},E));

	E = {var("Prelude.>>="), arg, E};
    }

    return {E, lambda_vars};
}


/// \brief Constrict a substitution::MultiModel for a specific alphabet
///
/// \param model_name The name of the substitution model.
/// \param a The alphabet.
/// \param frequencies The initial letter frequencies in the model.
///
model_t get_model(const Rules& R, const ptree& type, const std::set<term_t>& constraints, const ptree& model_rep, const names_in_scope_t& scope)
{
    // --------- Convert model to MultiMixtureModel ------------//
    expression_ref full_model = get_model_as(R, extract_value(model_rep), scope).first;

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

    model_rep = extract_value(p.first);
    auto equations = p.second;
    substitute(equations, model_rep);
    substitute(equations, required_type);
    if (log_verbose >= 1)
    {
	std::cout<<"model = "<<unparse(model_rep, R)<<std::endl;
	std::cout<<"type = "<<unparse_type(required_type)<<std::endl;
	std::cout<<"equations: "<<show(equations)<<std::endl;
	std::cout<<"structure = "<<show(model_rep)<<std::endl;
	std::cout<<"annotated structure = "<<show(p.first)<<std::endl;
	std::cout<<"pretty:\n"<<pretty_model_t(p.first).show(R)<<std::endl;
	std::cout<<std::endl;
    }

    names_in_scope_t names_in_scope;
    for(auto& x: scope)
	names_in_scope.insert({x.first,var_type_t::unknown});
    return get_model(R, required_type, equations.get_constraints(), p.first, names_in_scope);
}

// Some things, like log, exp, add, sub, etc. don't really have named arguments.
//    For these things exp[~normal[0,1]] remains exp[~normal[0,1]]
// Some things, like normal, gamma, etc. have named arguments.
//    For these things, we pull out random arguments, so that normal[~normal[0,1],1] becomes
//      normal[mu,1]: normal:mu ~ norma[0,1]
// Some things like tn, hky, etc. are considered to have named parameters.
//    For these things, we pull out all arguments that are numbers.
//    So, tn93[1,~log_normal[0,1]] becomes tn93 ; tn:kappa_pur=1 , tn:kappa_pyr ~ lognormal[0,1]

bool do_extract(const ptree& func, const ptree& arg)
{
    // 1. Don't extract arguments to e.g. log[], add[], sub[], etc.
    //    This is supposed to indicate things who arguments don't really have names?
    if (func.get("no_log",false)) return false;

    string func_name = func.get_child("value").get_value<string>();
    // 1b. Don't pull anything out of "let"
    if (func_name == "let") return false;
    // 1c. Don't pull anything out of lambdas
    if (func_name == "function") return false;

    auto arg_value = arg.get_child("value");
    string arg_type = unparse_type(arg.get_child("type"));

    // 2. Extract non-random things that are not models.
    if (func.get<string>("extract","none") == "all")
    {
	if (arg_type == "Int" or arg_type == "Double" or arg_type == "LogDouble")
	    return true;
	if (arg_type == "List[Double]" or arg_type == "List[Pair[String,Double]]")
	    return true;
    }

    if (not is_constant(arg_value))
    {
	auto arg_name = arg_value.get_value<string>();

	// 3. Pull out random arguments
	if (arg_name == "sample") return true;
    }

    return false;
}

// E = {type: T,value:{arg1:E,...argn:E}}

// This only extracts from the top level...

vector<pair<string, ptree>> extract_terms(ptree& m)
{
    // move value's children out of the structure
    ptree& value = m.get_child("value");

    vector<pair<string,ptree>> extracted;
    vector<pair<string,ptree>> extracted_top;
    // Walk each argument and determine if it should be pulled out
    for(auto& x: value)
    {
	string name = value.get_value<string>() + ":" + x.first;

	// If we should pull out the argument then do so
	if (do_extract(m, x.second))
	{
	    ptree extracted_value;
	    std::swap(x.second, extracted_value);
	    extracted_top.push_back({name, extracted_value});
	}
	// Otherwise look into the argument's value and try to pull things out
	else if (not x.second.is_null()) // for function[x=null,body=E]
	{
	    auto e = extract_terms(x.second);

	    for(auto& et: e)
		extracted.push_back({name+"/"+et.first, std::move(et.second)});
	}
    }
    std::move(extracted_top.begin(), extracted_top.end(), std::back_inserter(extracted));
    return extracted;
}

#include "startup/help.hh"

string pretty_model_t::show_extracted(const Rules& R) const
{
    const int indent = 4;

    string output;

    for(int i=0; i<terms.size(); i++)
    {
	string t = string(indent,' ') + term_names[i] + " ";
	string value = terms[i].show(R, false);
	output += "\n" + t + indent_and_wrap(0, t.size() + 2, 10000, value);
    }
    return output;
}

string pretty_model_t::show_main(const Rules& R, bool top) const
{
    if (top)
	return unparse(extract_value(main), R);
    else
	return show_model(extract_value(main), R);
}

string pretty_model_t::show(const Rules& R, bool top) const
{
    return show_main(R,top) + show_extracted(R);
}

pretty_model_t::pretty_model_t(const ptree& m)
    :main(m)
{
    // 1. Extract terms
    for(auto& x: extract_terms(main))
    {
	term_names.push_back(x.first);
	terms.push_back(x.second);
    }

    // 2. Fix up names
    for(auto& name: term_names)
	name = translate_structures(name);

    term_names = short_parameter_names(term_names);
}
