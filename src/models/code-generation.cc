#include "code-generation.H"

#include "parse.H"                         // for is_constant( )
#include "rules.H"                         // for Rules
#include "util/set.H"                      // for add, plus, minus
#include "util/log-level.H"                // for log_verbose
#include "util/graph.H"                    // for make_graph( )
#include "computation/haskell/haskell.H"   // for Hs::LExp
#include "computation/haskell/ids.H"       // for haskell_qid
#include "util/string/pred.H"              // for starts_with
#include "util/string/join.H"              // for join( )
#include "computation/expression/let.H"
#include "computation/expression/bool.H"
#include "computation/expression/apply.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/tuple.H"
#include "computation/expression/list.H"
#include "computation/expression/do_block.H"

using std::string;
using std::vector;
using std::optional;
using std::set;
using std::map;

bool is_loggable_function(const Rules& R, const string& name)
{
    auto rule = R.get_rule_for_func(name);

    // A random "let" has NOT necessarily been logged.
    // * let[k=random,k+2]         # OK    : we assume that if k is random, the whole thing is random.
    // * let[k=random,hky85[k]]    # Not OK: we assume that if k is random, the whole thing is random.
    // Probably we should track whether the result is an unlogged random in translation_result_t.
    if (name == "let") return false;

    if (name == "function") return true;

    if (not rule) return false;
    return not rule->get("no_log",false);
}

void perform_action_simplified(Stmts& block, const var& x, const var& log_x, bool is_referenced, expression_ref E, bool is_action, bool has_loggers)
{
    if (is_action)
    {
        if (not has_loggers)
            // x <- code
            block.perform(x, E);
        else
            // (x, log_x) <- code
            block.perform(Tuple(x,log_x), E);
    }
    else
    {
        if (has_loggers)
            block.let(Tuple(x,log_x), E);
        else if (is_referenced)
            block.let(x, E);
    }
}

void perform_action_simplified_(generated_code_t& block, const var& x, bool is_referenced, const generated_code_t& code)
{
    if (code.perform_function)
        block.stmts.perform(x,code.E);
    else if (is_referenced or code.is_action())
        block.stmts.let(x, code.E);
}

void use_block(translation_result_t& block, const var& log_x, const translation_result_t& code, const string& name)
{
    add(block.imports, code.imports);
    add(block.lambda_vars, code.lambda_vars);
    add(block.haskell_vars, code.haskell_vars);
    add(block.code.used_states, code.code.used_states);
    add(block.code.free_vars, code.code.free_vars);

    for(auto& stmt: code.code.stmts)
        block.code.stmts.push_back(stmt);

    if (code.code.has_loggers())
        block.code.log_sub(name, log_x, code.code.loggers);
}

void perform_action_simplified(translation_result_t& block, const var& x, const var& log_x, bool is_referenced, const translation_result_t& code, const string& name)
{
    use_block(block, log_x, code, name);
    perform_action_simplified_(block.code, x, is_referenced, code.code);
}

bool is_loggable_type(const type_t& type)
{
    auto [head,args] = get_type_apps(type);

    if (head == "String") return true;

    else if (head == "Int") return true;

    else if (head == "Double") return true;

    else if (head == "DiscreteDistribution")
    {
        if (args.size() != 1) return false;

        return is_loggable_type(args[0]);
    }

    else if (head == "List")
    {
        if (args.size() != 1) return false;

        return is_loggable_type(args[0]);
    }

    else if (head == "Tuple")
    {
        for(auto& arg: args)
            if (not is_loggable_type(arg)) return false;
        return true;
    }

    return false;
}

expression_ref simplify_intToDouble(const expression_ref& E)
{
    if (is_apply_exp(E) and E.size() == 2)
    {
        if (E.sub()[0].is_a<var>() and E.sub()[0].as_<var>().name == "intToDouble" and E.sub()[1].is_int())
        {
            int i = E.sub()[1].as_int();
            return double(i);
        }
    }

    return E;
}

optional<string> get_func_name(const ptree& model)
{
    auto value = model.get_child("value");
    if (not value.has_value<string>())
        return {};

    auto func_name = value.get_value<string>();

    if (func_name == "function")
        return get_func_name(value[1].second);

    if (is_qualified_symbol(func_name))
        func_name = get_unqualified_name(func_name);

    if (is_haskell_id(func_name))
        return func_name;
    else
        return {};
}

expression_ref generated_code_t::add_arguments(const expression_ref& F, const std::map<std::string,expression_ref>& state_values) const
{
    auto E = F;
    for(auto& state: used_states)
	E = {E, state_values.at(state)};
    for(auto& [_,x]: free_vars)
	E = {E, x};
    return E;
}

bool CodeGenState::is_random(const ptree& model_) const
{
    auto model = model_.get_child("value");

    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    // 1. If this function is random, then yes.
    if (name == "sample") return true;

    // 2. If this is a random variable, then yes.
    if (not model.size() and model.is_a<string>())
        if (identifiers.count(name) and identifiers.at(name).is_random) return true;

    // 3. Otherwise check if children are random and unlogged
    for(const auto& p: model)
        if (is_random(p.second))
            return true;

    return false;
}

bool CodeGenState::is_unlogged_random(const ptree& model_) const
{
    auto model = model_.get_child("value");

    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    // 1. If this function is random, then yes.
    if (name == "sample") return true;

    // Don't treat let-declared random variables as unlogged.
    // 2. If this is a random variable, then yes.
    // if (not model.size() and model.is_a<string>())
    //    if (scope.identifiers.count(name) and scope.identifiers.at(name).is_random) return true;

    // 3. If this function is loggable then any random children have already been logged.
    if (is_loggable_function(*R, name)) return false;

    // 4. Otherwise check if children are random and unlogged
    for(const auto& [_,child]: model)
        if (is_unlogged_random(child))
            return true;

    return false;
}

bool CodeGenState::should_log(const ptree& model_, const string& arg_name) const
{
    auto model = model_.get_child("value");

    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    if (not is_loggable_function(*R, name)) return false;

    auto arg = model.get_child(arg_name);

    return is_unlogged_random(arg);
}

CodeGenState CodeGenState::extend_scope(const string& var, const var_info_t& var_info) const
{
    auto scope = *this;
    return scope.extend_modify_scope(var, var_info);
}

CodeGenState& CodeGenState::extend_modify_scope(const string& var, const var_info_t& var_info)
{
    if (identifiers.count(var))
        identifiers.erase(var);
    identifiers.insert({var, var_info});
    haskell_vars.insert(var_info.x);
    return *this;
}

int get_index_for_arg_name(const ptree& rule, const string& arg_name)
{
    ptree args = rule.get_child("args");
    for(int i=0; i < args.size(); i++)
    {
        auto argi = array_index(args,i);
        if (arg_name == argi.get_child("name").get_value<string>())
            return i;
    }
    throw myexception()<<"No arg named '"<<arg_name<<"'";
}

expression_ref parse_constant(const ptree& model)
{
    if (model.value_is_empty())
        throw myexception()<<"parse_constant( ): got a null value!";

    if (model.is_a<int>()) return (int)model;
    if (model.is_a<double>()) return (double)model;
    if (model.is_a<bool>()) return (bool)model;
    string name = model.get_value<string>();
    if (name.size() > 2 and name[0] == '"' and name.back() =='"') return String(name.substr(1,name.size()-2));
    return {};
}

optional<translation_result_t> get_constant_model(const ptree& model)
{
    auto model_rep = model.get_child("value");
    if (expression_ref C = parse_constant(model_rep))
    {
        if (model_rep.size() != 0) throw myexception()<<"An constant cannot have arguments!\n  '"<<model_rep.show()<<"'";
        translation_result_t result;
        result.code.E = C;
        return result;
    }
    else
        return {};
}

optional<translation_result_t> CodeGenState::get_variable_model(const ptree& model) const
{
    auto model_rep = model.get_child("value");

    if (not model_rep.has_value<string>()) return {};

    auto name = model_rep.get_value<string>();

    // 1. Translate the default arg or variable
    translation_result_t result;
    if (not name.empty() and name[0] == '@')
    {
	// Handle references to other arguments in default_value and alphabet
        name = name.substr(1);
        if (not arg_env)
            throw myexception()<<"Looking up argument '"<<name<<"' in an empty environment!";

        auto& env = *arg_env;
        if (not env.code_for_arg.count(name))
            throw myexception()<<env.func<<"."<<env.arg<<": can't find argument '"<<name<<"' referenced in default_value or alphabet";

        expression_ref V = env.code_for_arg.at(name);

        result.code.E = V;
    }
    else if (identifiers.count(name))
    {
	var_info_t var_info = identifiers.at(name);

	if (var_info.depends_on_lambda)
	    result.lambda_vars = {name};
	result.code.free_vars.insert({name, var_info.x});

	result.code.E = var_info.x;
    }
    else
	return {};

    auto scope2 = *this;

    // 2. Handle argument arguments
    int i=0;
    for(auto& [arg_name, arg]: model_rep)
    {
	string var_name = name + "_" + std::to_string(i+1);
	string log_name = name + ":" + std::to_string(i+1);

	auto arg_model = scope2.get_model_as(arg);
	auto arg_code = arg_model.code;

	// Avoid re-using any haskell vars
	add(scope2.haskell_vars, arg_model.haskell_vars);

	// (x, logger) <- arg
	var x = scope2.get_var(var_name);
	var log_x = scope2.get_var("log_" + var_name);

	auto type = arg.get_child("type");
        bool do_log = is_unlogged_random(arg) and is_loggable_type(type) and arg_model.lambda_vars.empty();

	// Emit x <- or x= fo the variable, or prepare to substitute for it
	use_block(result, log_x, arg_model, log_name);
	expression_ref applied_arg = arg_code.E;
	if (arg_code.perform_function)
	{
	    applied_arg = log_x;
	    result.code.stmts.perform(x, arg_code.E);
	    assert(arg_model.lambda_vars.empty());
	}
	else if (do_log and not is_var(arg_code.E))
	{
	    applied_arg = log_x;
	    result.code.stmts.let(x, arg_code.E);
	    assert(arg_model.lambda_vars.empty());
	}


	// Log the value if we are saving it.
	if (do_log) result.code.log_value(log_name, applied_arg, type);
	
	// Make the call expression
	result.code.E = {result.code.E, applied_arg};

	i++;
    }

    result.code.E = simplify_intToDouble(result.code.E);

    add(result.haskell_vars, scope2.haskell_vars);

    return result;
}


/*
 *
 * do
 *   pair_var <- var_body
 *   let var_name = fst pair_var
 *   pair_body <- let_body
 *   return (fst pair_body, [("let:var",(Nothing,[(var_name,pair_x)])),("let:body",(Nothing,snd pair_body))])
 */
optional<translation_result_t> CodeGenState::get_model_let(const ptree& model) const
{
    auto scope2 = *this;

    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a let, then we are done.
    if (name != "let") return {};

    auto [var_name , var_exp ] = model_rep[0];
    auto [body_name, body_exp] = model_rep[1];

    var x = scope2.get_var(var_name);
    var log_x = scope2.get_var("log_" + var_name);
    bool x_is_random = is_random(var_exp);
    var_info_t var_info(x, x_is_random);

    var body = scope2.get_var("body");
    var log_body = scope2.get_var("log_body");

    // 1. Perform the variable expression
    auto arg_result = get_model_as(var_exp);

    if (arg_result.lambda_vars.size())
        var_info.depends_on_lambda = true;

    // 2. Perform the body with var_name in scope
    auto body_result = scope2.extend_scope(var_name, var_info).get_model_as(body_exp);

    // 3. Construct code.
    translation_result_t result;
    result.haskell_vars = scope2.haskell_vars;

    // (x, log_x) <- arg_result
    perform_action_simplified(result, x, log_x, true, arg_result, var_name);
    auto type = var_exp.get_child("type");
    if (x_is_random and is_loggable_type(type))
        result.code.log_value(var_name, x, type);

    // body_result
    use_block(result, log_body, body_result, "body");
    result.code.E = body_result.code.E;

    result.code.free_vars.erase(var_name);

    return result;
}

/*
 *
 * do
 *   pair_var1 <- var1_body
 *   let var1_name = fst pair_var1
 *   loggers = [("var_name",(Nothing,[(var_name,pair_x)]))]
 */
translation_result_t CodeGenState::get_model_decls(const ptree& model)
{
    translation_result_t result;

    for(auto& [var_name, var_exp]: model)
    {
	var x = get_var(var_name);
	var log_x = get_var("log_" + var_name);
	bool x_is_random = is_random(var_exp);
	var_info_t var_info(x, x_is_random);

	// 1. Perform the variable expression
	auto arg_result = get_model_as(var_exp);

	if (arg_result.lambda_vars.size())
	    var_info.depends_on_lambda = true;

	// 3. Construct code.
	add(result.haskell_vars, haskell_vars);

	// (x, log_x) <- arg_result
	perform_action_simplified(result, x, log_x, true, arg_result, var_name);
	auto type = var_exp.get_child("type");
	if (x_is_random and is_loggable_type(type))
	    result.code.log_value(var_name, x, type);

	// 4. Put x into the scope for the next decl.
	extend_modify_scope(var_name, var_info);
    }
    return result;
}

expression_ref eta_reduce(expression_ref E)
{
    while(is_lambda_exp(E) and E.sub()[0].is_a<var>())
    {
        auto& x    = E.sub()[0].as_<var>();
        auto& body = E.sub()[1];

        if (is_apply_exp(body) and body.sub().back() == x)
        {
	    expression_ref E2;
            // ($) f x  ==> f
            if (body.size() == 2)
                E2 = body.sub()[0];
            // ($) f y x ==> ($) f y
            else
            {
                // This is the simple case, where we can just pop an argument off the end of the list.
                assert(body.size() > 2);
                object_ptr<expression> body2 = body.as_expression().clone();
                body2->sub.pop_back();
                E2 = body2;
            }
	    if (get_free_indices(E2).count(x))
		break;
	    else
		E = E2;
        }
        else
            break;
    }
    return E;
}

set<string> find_vars_in_pattern(const ptree& pattern0)
{
    auto pattern = pattern0.get_child("value");

    if (is_nontype_variable(pattern))
        return {string(pattern)};
    else if (is_tuple(pattern))
    {
        set<string> vars;
        for(auto& [_,sub_pattern]: pattern)
        {
            auto slot_vars = find_vars_in_pattern(sub_pattern);
            for(auto& var_name: slot_vars)
            {
                assert(not vars.count(var_name));
                vars.insert(var_name);
            }
        }
        return vars;
    }
    else
        std::abort();
}


optional<translation_result_t> CodeGenState::get_model_lambda(const ptree& model) const
{
    auto scope2 = *this;

    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a lambda, then we are done.
    if (name != "function") return {};

    // 2. Get the variable name and the body from the model
    auto pattern = model_rep[0].second;
    auto var_names = find_vars_in_pattern(pattern);
    auto body = scope2.get_var("lbody");
    auto log_body = scope2.get_var("log_lbody");

    ptree body_model = model_rep[1].second;

    // We don't have to worry about avoiding any haskell variables that correspond
    // to scripting language variables with names that are lambda vars.
    for(auto& var_name: var_names)
    {
	if (identifiers.count(var_name))
	{
	    auto x = identifiers.at(var_name).x;
	    scope2.haskell_vars.erase(x);
	}
    }

    // 3. Parse the body with the lambda variable in scope, and find the free variables.
    for(auto& var_name: var_names)
    {
        auto x = scope2.get_var(var_name);
        var_info_t var_info(x,false,true);
        scope2.extend_modify_scope(var_name, var_info);
    }
    auto body_result = scope2.get_model_as(body_model);

    // 4. Remove pattern variables from the lambda vars.
    for(auto& var_name: var_names)
        if (body_result.lambda_vars.count(var_name))
            body_result.lambda_vars.erase(var_name);

    // 5. Add the lambda in front of the expression
    auto pattern2 = scope2.get_model_as(pattern);
    body_result.code.E = lambda_quantify(pattern2.code.E, body_result.code.E);

    // 6. Now eta-reduce E.  If E == (\x -> body x), we will get just E == body
    body_result.code.E = eta_reduce(body_result.code.E);
    for(auto& var_name: var_names)
	body_result.code.free_vars.erase(var_name);

    return body_result;
}

expression_ref make_call(const ptree& call, const map<string,expression_ref>& simple_args)
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

    // Process arguments;
    vector<expression_ref> args;
    for(int i=0;i<call.size();i++)
	args.push_back(make_call(call[i].second, simple_args));

    // Process expression
    if (name == "List")
    {
	vector<Hs::LExp> located_args;
	for(auto& arg: args)
	    located_args.push_back({noloc,arg});
	E = Hs::List(located_args);
    }
    else if (name == "Tuple")
    {
	vector<Hs::LExp> located_args;
	for(auto& arg: args)
	    located_args.push_back({noloc,arg});
	E = Hs::Tuple(located_args);
    }
    else
    {
	// FIXME! Here is where we are assuming that unqualified ids are arg_var_NAME variables.
	if (name[0] == '@')
	{
	    name = name.substr(1);
	    try
	    {
		E = simple_args.at(name);
	    }
	    catch(...)
	    {
		throw myexception()<<"cannot find argument '"<<name<<"'";
	    }
	}
	else
	    E = var(name);

	for(int i=0;i<call.size();i++)
	{
	    if (i == call.size()-1 and call[i].second == "@submodel")
		E = {var("+>"), args[i], E};
	    else
		E = {E, args[i]};
	}
    }	

    return E;
}

vector<bool> get_args_referenced(const vector<string>& arg_names, const vector<set<string>>& used_args_for_arg)
{
    vector<bool> referenced(arg_names.size(), false);
    for(int i=0;i<arg_names.size();i++)
        for(int j=0;j<arg_names.size();j++)
        {
            if (used_args_for_arg[i].count(arg_names[j]))
            {
                referenced[j] = true;
                if (log_verbose > 2)
                    std::cerr<<arg_names[i]<<" references "<<arg_names[j]<<"\n";
            }
        }
    return referenced;
}

vector<int> get_args_order(const vector<string>& arg_names, const vector<set<string>> used_args_for_arg)
{
    const int N = arg_names.size();

    // 1. Construct the directed reference graph
    auto G = make_graph(N, [&](int i, int j) { return used_args_for_arg[i].count(arg_names[j]); });

    // 2. Complain about loops
    for(auto& loop_component: get_loop_components(G))
    {
        if (loop_component.size() == 1)
        {
            int i = loop_component[0];
            throw myexception()<<"default argument for '"<<arg_names[i]<<"' references itself!";
        }
        else
        {
            vector<string> loop_names;
            for(int i: loop_component)
                loop_names.push_back(arg_names[i]);
            throw myexception()<<"Must specify at least one of: "<<join(loop_names,',');
        }
    }
    
    auto order = topo_sort(G);
//    std::reverse(order.begin(), order.end());
    return order;
}


// NOTE: To some extent, we construct the expression in the reverse order in which it is performed.
optional<translation_result_t> CodeGenState::get_model_list(const ptree& model) const
{
    auto scope2 = *this;
    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a lambda, then we are done.
    if (name != "List") return {};

    int N = model_rep.size();

    translation_result_t result;

    // 2. Construct the names of the haskell variables for the arguments.
    vector<expression_ref> argument_environment(N);
    for(int i=0; i<N; i++)
    {
        // 3a. Compute vars for element
        string var_name = "_"+std::to_string(i+1);
        string log_name = "["+std::to_string(i+1)+"]";

        auto x = scope2.get_var(var_name);
        argument_environment[i] = x;

        // 3b. Generate code for the list element
        auto element = array_index(model_rep, i);
        auto element_result = scope2.get_model_as(element);

        // 3c. Avoid re-using any haskell vars.
        add(scope2.haskell_vars, element_result.haskell_vars);

        // 3d. Include stmts and dependencies
        auto log_x = scope2.get_var("log"+var_name);
        use_block(result, log_x, element_result, log_name);

        // 3e. Maybe emit code for the element.
	auto type = element.get_child("type");
        bool do_log = is_unlogged_random(element) and is_loggable_type(type);
        if (element_result.code.perform_function)
            result.code.stmts.perform(x, element_result.code.E);
        if (do_log and not is_var(element_result.code.E))
            result.code.stmts.let(x, element_result.code.E);
        else
            argument_environment[i] = element_result.code.E;

        // 3f. Maybe log the element.
        if (do_log)
            result.code.log_value(log_name, argument_environment[i], type);
    }

    // 4. Compute the call expression.
    result.code.E = get_list(argument_environment);

    // 5. Make sure not to re-use any vars adding do this code.
    add(result.haskell_vars, scope2.haskell_vars);

    return result;
}

// NOTE: To some extent, we construct the expression in the reverse order in which it is performed.
optional<translation_result_t> CodeGenState::get_model_tuple(const ptree& model) const
{
    auto scope2 = *this;
    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a tuple, then we are done.
    if (name != "Tuple") return {};

    int N = model_rep.size();

    translation_result_t result;

    // 2. Construct the names of the haskell variables for the arguments.
    vector<expression_ref> argument_environment(N);
    for(int i=0; i<N; i++)
    {
        // 3a. Compute vars for element
        string var_name = "_"+std::to_string(i+1);
        string log_name = "["+std::to_string(i+1)+"]";

        auto x = scope2.get_var(var_name);
        argument_environment[i] = x;

        // 3b. Generate code for the list element
        auto element = array_index(model_rep, i);
        auto element_result = scope2.get_model_as(element);

        // 3c. Avoid re-using any haskell vars.
        add(scope2.haskell_vars, element_result.haskell_vars);

        // 3d. Include stmts and dependencies
        auto log_x = scope2.get_var("log"+var_name);
        use_block(result, log_x, element_result, log_name);

        // 3e. Maybe emit code for the element.
	auto type = element.get_child("type");
        bool do_log = is_unlogged_random(element) and is_loggable_type(type);
        if (element_result.code.perform_function)
            result.code.stmts.perform(x, element_result.code.E);
        if (do_log and not is_var(element_result.code.E))
            result.code.stmts.let(x, element_result.code.E);
        else
            argument_environment[i] = element_result.code.E;

        // 3f. Maybe log the element.
        if (do_log)
            result.code.log_value(log_name, argument_environment[i], type);
    }

    // 4. Compute the call expression.
    result.code.E = get_tuple(argument_environment);

    // 5. Make sure not to re-use any vars adding do this code.
    add(result.haskell_vars, scope2.haskell_vars);

    return result;
}

// NOTE: To some extent, we construct the expression in the reverse order in which it is performed.
translation_result_t CodeGenState::get_model_function(const ptree& model) const
{
    auto scope2 = *this;
    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    translation_result_t result;

    // 1. Get the rule for the function
    auto rule = R->get_rule_for_func(name);
    if (not rule) throw myexception()<<"No rule for '"<<name<<"'";
    if (not rule->count("call")) throw myexception()<<"No call for '"<<name<<"'";
    if (auto rule_imports = rule->get_child_optional("import"))
    {
        for(auto& [_, mod]: *rule_imports)
            result.imports.insert(mod.get_value<string>());
    }

    result.code.perform_function = rule->get("perform",false);
    ptree call = rule->get_child("call");
    ptree args = rule->get_child("args");

    if (not is_haskell_qid(call.get_value<string>()) and
        not is_haskell_qsym(call.get_value<string>()) and
        not is_haskell_builtin_con_name(call.get_value<string>()) and
        not starts_with(call.get_value<string>(),"@"))
        throw myexception()<<"For rule '"<<name<<"', function '"<<call.get_value<string>()<<"' doesn't seem to be a valid haskell id or a valid argument reference.";

    // 2. Construct the names of the haskell variables for the arguments.
    vector<string> arg_names(args.size());
    map<string,expression_ref> argument_environment;
    vector<var> arg_vars;
    vector<var> log_vars;
    vector<set<string>> used_args_for_arg(args.size());
    for(int i=0;i<args.size();i++)
    {
        auto argi = array_index(args,i);
        arg_names[i] = argi.get_child("name").get_value<string>();

        auto arg = model_rep.get_child(arg_names[i]);
        bool is_default_value = arg.get_child("is_default_value").get_value<bool>();

        // We only count references from the argument if its a default value.
        if (is_default_value)
            used_args_for_arg[i] = get_used_args(arg);
        // However, the alphabet always references the current arguments.
        if (auto alphabet_exp = arg.get_child_optional("alphabet"))
            add(used_args_for_arg[i], get_used_args(*alphabet_exp));

        auto var_name = arg_names[i];
        if (var_name == "submodel")
        {
            auto arg = model_rep.get_child(arg_names[i]);
            if (auto func_name = get_func_name(arg))
                var_name = (*func_name)+"_model";
        }
        arg_vars.push_back(scope2.get_var(var_name));
        log_vars.push_back(scope2.get_var("log_"+var_name));

        argument_environment[arg_names[i]] = arg_vars.back();
    }

    // 3.. Figure out which args are referenced from other args
    auto arg_referenced = get_args_referenced(arg_names, used_args_for_arg);

    auto arg_order = get_args_order(arg_names, used_args_for_arg);

    // 4. Construct the alphabet for each argument, if there is one.
    vector<translation_result_t> arg_models(args.size());
    vector<set<string>> arg_lambda_vars;
    vector<string> log_names(args.size());

    // FIXME! There might be some problem where we reference alphabet vars like a_3
    //        before we define them, in situations where we don't substitute.
    for(int i: arg_order)
    {
        log_names[i] = name + ":" + arg_names[i];

        auto arg = model_rep.get_child(arg_names[i]);

        optional<var> alphabet;
        if (auto alphabet_expression = arg.get_child_optional("alphabet"))
        {
            string var_name = "alpha";
            if (alphabet_expression->get_child("value").has_value<string>() and alphabet_expression->get_child("value").get_value<string>() == "getNucleotides")
                var_name = "nucs";
            auto x = scope2.get_var(var_name);
            auto log_x = scope2.get_var("log_"+arg_names[i]+"_alpha");

            auto alphabet_scope = scope2;
            alphabet_scope.arg_env = {{name,arg_names[i],argument_environment}};
            auto alphabet_result = alphabet_scope.get_model_as(*alphabet_expression);
            if (alphabet_result.lambda_vars.size())
                throw myexception()<<"An alphabet cannot depend on a lambda variable!";

            assert(not alphabet_result.code.has_loggers());
            assert(not alphabet_result.code.perform_function);
            use_block(result, log_x, alphabet_result, log_names[i]+":alphabet");

            if (is_var(alphabet_result.code.E))
                alphabet = alphabet_result.code.E.as_<var>();
            else
            {
                alphabet = x;
                result.code.stmts.let(x, alphabet_result.code.E);
            }
        }

        bool is_default_value = arg.get_child("is_default_value").get_value<bool>();

        auto scope3 = scope2;
        if (is_default_value)
            scope3.arg_env = {{name,arg_names[i],argument_environment}};
        if (alphabet)
            scope3.set_state("alphabet", *alphabet);

        arg = model_rep.get_child(arg_names[i]);
        arg_models[i] = scope3.get_model_as(arg);

        // Move this to generate()
        if (result.code.perform_function and arg_models[i].lambda_vars.size())
            throw myexception()<<"Argument '"<<arg_names[i]<<"' of '"<<name<<"' contains a lambda variable: not allowed!";

        // Avoid re-using any haskell vars.
        add(scope2.haskell_vars, arg_models[i].haskell_vars);

        // (x, logger) <- arg
        auto x = arg_vars[i];
        auto log_x = log_vars[i];

	auto type = arg.get_child("type");
        bool do_log = should_log(model, arg_names[i]) and is_loggable_type(type) and arg_models[i].lambda_vars.empty();

        // 6b. Emit x <- or x = for the variable, or prepare to substitute it.
        use_block(result, log_x, arg_models[i], log_names[i]);
        if (arg_models[i].code.perform_function)
            result.code.stmts.perform(x, arg_models[i].code.E);
        else if ((arg_referenced[i] or do_log) and not is_var(arg_models[i].code.E))
        {
            result.code.stmts.let(x, arg_models[i].code.E);
        }
        else // Substitute for the expression
        {
            // FIXME: This assumes that the argument occurs in the call at most once!
            argument_environment[arg_names[i]] = arg_models[i].code.E;

            // NOTE: if arg_models[i].lambda_vars isn't empty, then we need to note this in the argument_environment
            // so that @arg references are known to depend on lambda vars.
            // MAYBE: change code_for_arg< > to map<string,var_info_t>?

            // NOTE: anything that references a lambda variable has to be in code.E, not code.stmts!
        }

        // 6c. Write the logger for the variable.
        if (do_log)
	{
            result.code.log_value(log_names[i], argument_environment[arg_names[i]], type);
	}
    }

    if (auto computed = rule->get_child_optional("computed"))
    {
	for(auto& [_,x]: *computed)
	{
	    // A. Generate a unique haskell name for the computed variable
	    auto x_name = x.get_child("name").get_value<string>();
	    auto x_log_name = name + ":" + x_name;
	    auto x_var = scope2.get_var(x_name);

	    // B. Each computed variable can only reference earlier computed variables.
	    auto& value = x.get_child("value");
	    auto x_type = ptree("unknown_type");
	    result.code.stmts.let(x_var, make_call(value, argument_environment));

	    // C. Log the computed variable
	    result.code.log_value(x_log_name, x_var, x_type);

            // D. Put this var into the argument environment
	    argument_environment[x_name] = x_var;
	}
    }

    // 7. Compute the call expression.
    try
    {
        result.code.E = make_call(call, argument_environment);

        result.code.E = simplify_intToDouble(result.code.E);
    }
    catch(myexception& err)
    {
        err.prepend("In call for function '"+name+"': ");
        throw;
    }

    // 8. Make sure not to re-use any vars adding do this code.
    add(result.haskell_vars, scope2.haskell_vars);

    return result;
}

// NOTE: To some extent, we construct the expression in the reverse order in which it is performed.
optional<translation_result_t> CodeGenState::get_model_state(const ptree& model) const
{
    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    optional<string> state_name;
    if (name == "get_state")
    {
        // How do we access the child here?
        auto arg = model_rep[0].second;
        state_name = arg.get_child("value");
    }

    if (state_name)
    {
        if (state.count(*state_name))
        {
            auto x = state.at(*state_name);
            translation_result_t result;
            result.code.E = x;
            result.code.used_states = {*state_name};
            return result;
        }
        else
            throw myexception()<<"No state '"<<*state_name<<"'!";
    }
    else
        return {};
}

translation_result_t CodeGenState::get_model_as(const ptree& model_rep) const
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
        return *constant;

    // 3. Handle variables
    else if (auto variable = get_variable_model(model_rep))
        return *variable;

    // 4. Let expressions
    else if (auto let = get_model_let(model_rep))
        return *let;

    // 5. Lambda expressions
    else if (auto lambda = get_model_lambda(model_rep))
        return *lambda;

    // 6. get_state[state] expressions.
    else if (auto list = get_model_list(model_rep))
        return *list;

    // 7. get_state[state] expressions.
    else if (auto list = get_model_tuple(model_rep))
        return *list;

    // 8. get_state[state] expressions.
    else if (auto state = get_model_state(model_rep))
        return *state;

    // 9. Functions
    return get_model_function(model_rep);
}

