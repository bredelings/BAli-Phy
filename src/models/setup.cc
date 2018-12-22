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

using std::optional;
using std::string;
using std::pair;
using std::set;
using std::map;
using std::vector;
using std::valarray;
using boost::program_options::variables_map;
using boost::shared_ptr;

expression_ref fst  = var("Data.Tuple.fst");
expression_ref snd  = var("Data.Tuple.snd");
expression_ref bind = var("Compiler.Base.>>=");
expression_ref do_return = var("Compiler.Base.return");

string model_t::show(bool top) const
{
    if (top)
	return show_model_annotated(description);
    else
	return unparse_annotated(description);
}

string model_t::show_pretty(bool top) const
{
    auto p = pretty_model_t(description);
    return p.show(not top);
}

string model_t::show_main(bool top) const
{
    auto p = pretty_model_t(description);
    return p.show_main(top);
}

string model_t::show_extracted() const
{
    auto p = pretty_model_t(description);
    return p.show_extracted();
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
    else if (dynamic_cast<const Doublets*>(&a))
	return "tn93_sym+x2_sym+f";
    else if (dynamic_cast<const Triplets*>(&a))
	return "tn93+x3";
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

struct var_info_t
{
    var x;
    bool is_random = false;
    bool depends_on_lambda = false;
    var_info_t(const var& v, bool r=false, bool l=false)
	:x(v),is_random(r),depends_on_lambda(l)
    { }
};

typedef map<string,var_info_t> names_in_scope_t;

bool is_random(const ptree& model, const names_in_scope_t& scope)
{
    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    // 1. If this function is random, then yes.
    if (name == "sample") return true;

    // 2. If this is a random variable, then yes.
    if (not model.size() and model.is_a<string>())
	if (scope.count(name) and scope.at(name).is_random) return true;

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
	if (scope.count(name) and scope.at(name).is_random) return true;

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

names_in_scope_t extend_scope(names_in_scope_t scope, const string& var, const var_info_t& var_info)
{
    if (scope.count(var))
	scope.erase(var);
    scope.insert({var, var_info});
    return scope;
}

// When processing arg i, we should only have args i+1 and after in scope.
names_in_scope_t extend_scope(const ptree& rule, int skip, const names_in_scope_t& scope)
{
    auto scope2 = scope;
    int i=0;
    for(const auto& arg: rule.get_child("args"))
    {
	if (i++ <= skip) continue;

	const auto& argument = arg.second;
	auto arg_name = argument.get<string>("arg_name");
	auto ref_name = "@"+arg_name;
	var x("arg_var_"+arg_name);

	scope2.erase(ref_name);
	scope2.insert({ref_name,{x,false,false}});
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
	return {do_return, Tuple(C,List())};
    }
    else
	return {};
}

optional<pair<expression_ref,set<string>>> get_variable_model(const ptree& E, const names_in_scope_t& scope)
{
    if (E.size() or not E.is_a<string>()) return {};

    auto name = E.get_value<string>();

    // 1. If the name is not in scope then we are done.
    if (not scope.count(name)) return {};

    var x = scope.at(name).x;

    expression_ref V;
    set<string> lambda_vars;

    // 2. If the name is a lambda var, then we need to quantify it, and put it into the list of free lambda vars
    if (scope.at(name).depends_on_lambda)
    {
	V = lambda_quantify(x,x);
	lambda_vars.insert(name);
    }
    // 3. Otherwise the expression is just the variable itself
    else
	V = x;

    // 4. Construct the logging tuple and return it in order to allow this action to be performed.
    V = {do_return, Tuple(V,List())};

    // 5. We need an extra level of {} to allow constructing the optional.
    return {{V,lambda_vars}};
}


/*
 *
 * do
 *   pair_var <- var_body
 *   let var_name = fst pair_var
 *   pair_body <- let_body
 *   return (fst pair_body, [("let:var",(Nothing,[(var_name,pair_x)])),("let:body",(Nothing,snd pair_body))])
 */
optional<pair<expression_ref,set<string>>> get_model_let(const Rules& R, const ptree& model_rep, const names_in_scope_t& scope)
{
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a let, then we are done.
    if (name != "let") return {};

    string var_name = model_rep[0].first;
    ptree var_exp = model_rep[0].second;
    ptree body_exp = model_rep[1].second;

    var pair_x("pair_var_"+var_name);
    var x("var_"+var_name);

    var pair_body("pair_body");
    var_info_t var_info(x,is_random(var_exp, scope));

    // 1. Perform the body with var_name in scope
    auto p = get_model_as(R, body_exp, extend_scope(scope, var_name, var_info));
    expression_ref let_body = p.first;

    // FIXME: we currently prohibit var_exp from containing any lambda-variables, so we don't need to check if it has them.
    bool do_log = is_unlogged_random(R, var_exp, scope);
    expression_ref var_loggers = List();
    var_loggers = {var("Distributions.add_logger"), var_loggers, var_name, pair_x, do_log};

    expression_ref loggers = List();
    var Nothing("Data.Maybe.Nothing");
    loggers = {var("Distributions.add_logger"),loggers,"let:body",Tuple(Nothing,{snd,pair_body}),false};
    loggers = {var("Distributions.add_logger"),loggers,"let:var",Tuple(Nothing,var_loggers),false};

    // E = return (fst pair_body, loggers)
    expression_ref E = {do_return,Tuple({fst,pair_body},loggers)};

    // E = do {pair_body <- let_body ; E}
    E = {bind, let_body, lambda_quantify(pair_body,E)};

    // 2. Perform the variable expression
    {
	auto arg_p = get_model_as(R, var_exp, scope);
	expression_ref arg = arg_p.first;
	if (arg_p.second.size())
	    throw myexception()<<"You cannot let-bind a variable to an expression with a function-variable";

	// E = let arg_name=fst pair_var_name; E
	E = let_expression({{x,{fst,pair_x}}},E);

	// E = do {pair_x <- arg ; E}
	E = {bind, arg, lambda_quantify(pair_x,E)};
    }

    return {{E, p.second}};
}

optional<pair<expression_ref,set<string>>> get_model_lambda(const Rules& R, const ptree& model_rep, const names_in_scope_t& scope)
{
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a lambda, then we are done.
    if (name != "function") return {};

    // 2. Get the variable name and the body from the model
    string var_name = model_rep[0].first;
    var x("lambda_var_" + var_name);
    var pair_arg_body("pair_arg_body");
    ptree body_exp = model_rep[1].second;

    // 3. Parse the body with the lambda variable in scope, and find the free variables.
    var_info_t var_info(x,false,true);
    auto body_scope = extend_scope(scope, var_name, var_info);
    auto body_E = get_model_as(R, body_exp, body_scope);
    expression_ref body = body_E.first;
    auto lambda_vars = body_E.second;

    // E = E x l1 l2 l3
    expression_ref E = {fst,pair_arg_body};
    for(auto& vname: lambda_vars)
	E = {E, body_scope.at(vname).x};

    // E = \x -> E
    E = lambda_quantify(x, E);
    
    // Remove x from the lambda vars.
    if (lambda_vars.count(var_name)) lambda_vars.erase(var_name);

    // E = \l1 l2 l3 -> E
    for(auto& vname: std::reverse(lambda_vars))
	E = lambda_quantify(scope.at(vname).x,E);

    // E = return $ (E,snd pair_arg_body)
    E = {do_return, Tuple(E,{snd,var("pair_arg_body")})};

    // E = do {pair_arg_body <- body ; E}
    E = {bind,body,lambda_quantify(pair_arg_body,E)};


    // In summary, we have
    //E = do
    //      pair_arg_body <- body_action
    //      return $ (\l1 l2 l3 -> \x -> ((fst pair_air_body) x l1 l2 l3) , snd pair_arg_body)
    return {{E, lambda_vars}};
}

expression_ref make_call(const ptree& call)
{
    if (call.is_null())
	throw myexception()<<"Can't construct expression from null value:\n"<<call.show()<<"\n";
    if (not call.empty() and not call.has_value<string>())
	throw myexception()<<"Call should not have arguments:\n"<<call.show()<<"\n";

    if (call.is_a<bool>())
	return {bool(call)};
    else if (call.is_a<int>())
	return {int(call)};
    else if (call.is_a<double>())
	return {double(call)};
    assert(call.has_value<string>());
    auto name = call.get_value<string>();
    expression_ref E;

    if (is_qualified_symbol(name) or is_haskell_varsym(name) or is_haskell_consym(name) or is_haskell_builtin_con_name(name))
	E = var(name);
    else
	E = var("arg_var_"+name);

    for(auto& pair: call)
	E = {E,make_call(pair.second)};

    return E;
}


// NOTE: To some extent, we construct the expression in the reverse order in which it is performed.
pair<expression_ref,set<string>> get_model_function(const Rules& R, const ptree& model_rep, const names_in_scope_t& scope)
{
    auto name = model_rep.get_value<string>();

    // 1. Get the rule for the function
    auto rule = R.get_rule_for_func(name);
    if (not rule) throw myexception()<<"No rule for '"<<name<<"'";
    if (not rule->count("call")) throw myexception()<<"No call for '"<<name<<"'";
	
    bool perform_function = rule->get("perform",false);
    ptree call = rule->get_child("call");
    ptree args = rule->get_child("args");
    
    if (not is_qualified_symbol(call.get_value<string>()) and not is_haskell_builtin_con_name(call.get_value<string>()))
	throw myexception()<<"For rule '"<<name<<"', function '"<<call.get_value<string>()<<"' must be a qualified symbol or a builtin constructor like '(,)', but it is neither!";

    // 2. Construct the call expression
    expression_ref E = make_call(call);

    // 3a. Parse models for arguments to figure out which free lambda variables they contain
    // 3b. ALSO, let-bind arg_var_<name> for any arguments that are (i) not performed and (ii) depend on a lambda variable.
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

	if (not arg_lambda_vars[i].empty())
	{
	    var x("arg_var_"+arg_name);
	    var pair_x("pair_arg_var_"+arg_name);

	    // Apply the free lambda variables to arg result before using it.
	    expression_ref F = {fst, pair_x};
	    for(auto& vname: arg_lambda_vars[i])
		F = {F, scope.at(vname).x};

	    E = let_expression({{x,F}},E);
	}
    }

    // 4. Return a lambda function 
    for(auto& vname: std::reverse(lambda_vars))
	E = lambda_quantify(scope.at(vname).x, E);

    // 5. Compute loggers
    expression_ref loggers = List();
    for(int i=0;i<args.size();i++)
    {
	auto argi = array_index(args,i);

	string arg_name = argi.get_child("arg_name").get_value<string>();

	auto log_name = name + ":" + arg_name;

	bool do_log = arg_lambda_vars[i].empty() and should_log(R, model_rep, arg_name, scope);
	loggers = {var("Distributions.add_logger"),loggers,log_name,var("pair_arg_var_"+arg_name),do_log};
    }

    // 6. Return the function call: 'return (f call.name1 call.name2 call.name3)'
    if (not perform_function)
	E = {do_return,E};

    E = {bind, E, lambda_quantify(var("result"),{do_return,Tuple(var("result"),loggers)})};

    // 7. Peform the rule arguments 'Prefix "arg_name" (arg_+arg_name) >>= (\arg_name -> (Log "arg_name" arg_name) << E)'
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
	    auto A_pair = get_model_as(R, *alphabet_expression, alphabet_scope);
	    if (A_pair.second.size())
		throw myexception()<<"An alphabet cannot depend on a lambda variable!";
	    auto& A = A_pair.first;
	    arg = {var("Distributions.set_alphabet"),A,arg};
	}

	// E = 'arg <<= (\arg_name_pair -> let {arg_name=fst arg_name_pair} in E)
	var pair_x("pair_arg_var_"+arg_name);
	if (arg_lambda_vars[i].empty())
	{
	    var x("arg_var_"+arg_name);
	    E = let_expression({{x,{fst,pair_x}}},E);
	}
	E = lambda_quantify(pair_x, E);

	E = {bind, arg, E};
    }

    return {E, lambda_vars};
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

    // 6. Functions
    return get_model_function(R, model_rep, scope);
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
	std::cout<<"model = "<<unparse_annotated(p.first)<<std::endl;
	std::cout<<"type = "<<unparse_type(required_type)<<std::endl;
	std::cout<<"equations: "<<show(equations)<<std::endl;
	std::cout<<"structure = "<<show(model_rep)<<std::endl;
	std::cout<<"annotated structure = "<<show(p.first)<<std::endl;
	std::cout<<"pretty:\n"<<pretty_model_t(p.first).show()<<std::endl;
	std::cout<<std::endl;
    }

    names_in_scope_t names_in_scope;
    for(auto& x: scope)
	names_in_scope.insert({x.first,var_info_t(var("var_"+x.first))});

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

// However, we would like gy94[pi=f1x4] to NOT pull out the pi, because f1x4 is ALSO a model.

bool annotated_term_is_model(const ptree& term)
{
    return term.get<string>("extract","none") == "all";
}

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

    // 2. If this is a model, then extract non-random things that are not models.
    if (annotated_term_is_model(func))
    {
	if (annotated_term_is_model(arg)) return false;

	// FIXME - It would be nice if we could universally return true here.
	//         But first, we need to handle - not extracting gamma::n
	//                                      - suppressing gamma::a = getAlphabet
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

#include "util/text.H"

string pretty_model_t::show_extracted() const
{
    const int indent = 4;

    string output;

    for(int i=0; i<terms.size(); i++)
    {
	string t = string(indent,' ') + term_names[i] + " ";
	string value = terms[i].show(false);
	output += "\n" + t + indent_and_wrap(0, t.size() + 2, 10000, value);
    }
    return output;
}

string pretty_model_t::show_main(bool top) const
{
    if (top)
	return unparse_annotated(main);
    else
	return show_model_annotated(main);
}

string pretty_model_t::show(bool top) const
{
    return show_main(top) + show_extracted();
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
