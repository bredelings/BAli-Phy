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
#include "util/string/join.H"
#include "util/string/pred.H"
#include "util/set.H"
#include "util/range.H"
#include "util/myexception.H"
#include "models/rules.H"
#include "models/parse.H"
#include "models/translate.H"
#include "models/path.H"
#include "computation/module.H"
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/bool.H"
#include "computation/expression/apply.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/tuple.H"
#include "computation/expression/list.H"
#include "computation/expression/do_block.H"
#include "computation/operations.H"
#include "model.H"

extern int log_verbose;

using std::optional;
using std::string;
using std::pair;
using std::tuple;
using std::set;
using std::map;
using std::vector;
using std::valarray;
using std::shared_ptr;
using boost::program_options::variables_map;

expression_ref do_return = var("return");

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

model_t::model_t(const ptree& d, const set<string>& i, const ptree&t, const std::set<term_t>& c, const expression_ref& e)
    :description(d), imports(i), type(t), constraints(c), expression(e)
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

struct arg_env_t
{
    string func; // context: calling func:arg
    string arg;
    map<string,expression_ref> code_for_arg;

    arg_env_t(const string& f, const string& a, const map<string,expression_ref>& c):
        func(f),
        arg(a),
        code_for_arg(c)
    { };
};

struct name_scope_t
{
    map<string,var_info_t> identifiers;
    set<var> vars;
    optional<arg_env_t> arg_env;

    var get_var(string name)
    {
        if (name.empty() or not std::islower(name[0]))
            name = "_"+name;
        var x(name);
        for(int i=2; vars.count(x); i++)
            x = var(name+"_"+std::to_string(i));
        vars.insert(x);
        return x;
    }
};

bool is_random(const ptree& model_, const name_scope_t& scope)
{
    auto model = model_.get_child("value");

    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    // 1. If this function is random, then yes.
    if (name == "sample") return true;

    // 2. If this is a random variable, then yes.
    if (not model.size() and model.is_a<string>())
        if (scope.identifiers.count(name) and scope.identifiers.at(name).is_random) return true;

    // 3. Otherwise check if children are random and unlogged
    for(const auto& p: model)
        if (is_random(p.second, scope))
            return true;

    return false;
}

bool is_unlogged_random(const Rules& R, const ptree& model_, const name_scope_t& scope)
{
    auto model = model_.get_child("value");

    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

    // 1. If this function is random, then yes.
    if (name == "sample") return true;

    // 2. If this is a random variable, then yes.
    if (not model.size() and model.is_a<string>())
        if (scope.identifiers.count(name) and scope.identifiers.at(name).is_random) return true;

    // 3. If this function is loggable then any random children have already been logged.
    if (is_loggable_function(R, name)) return false;

    // 4. Otherwise check if children are random and unlogged
    for(const auto& p: model)
        if (is_unlogged_random(R, p.second, scope))
            return true;

    return false;
}

bool should_log(const Rules& R, const ptree& model_, const string& arg_name, const name_scope_t& scope)
{
    auto model = model_.get_child("value");

    auto name = model.get_value<string>();

    if (is_constant(model)) return false;

    if (not is_loggable_function(R, name)) return false;

    auto arg = model.get_child(arg_name);

    if (is_unlogged_random(R, arg, scope))
        return true;
    else
        return false;
}

name_scope_t extend_scope(name_scope_t scope, const string& var, const var_info_t& var_info)
{
    if (scope.identifiers.count(var))
        scope.identifiers.erase(var);
    scope.identifiers.insert({var, var_info});
    scope.vars.insert(var_info.x);
    return scope;
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

tuple<expression_ref,set<string>,set<string>,set<string>,bool> get_model_as(const Rules& R, const ptree& model_rep, const name_scope_t& scope);

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

expression_ref get_constant_model(const ptree& model)
{
    auto model_rep = model.get_child("value");
    if (expression_ref C = parse_constant(model_rep))
    {
        if (model_rep.size() != 0) throw myexception()<<"An constant cannot have arguments!\n  '"<<model_rep.show()<<"'";
        return {do_return, Tuple(C,List())};
    }
    else
        return {};
}

optional<tuple<expression_ref,set<string>,set<string>,set<string>,bool>> get_variable_model(const ptree& model, const name_scope_t& scope)
{
    auto E = model.get_child("value");

    if (E.size() or not E.is_a<string>()) return {};

    auto name = E.get_value<string>();

    // 0. Handle references to other arguments in default_value and alphabet
    if (not name.empty() and name[0] == '@')
    {
        name = name.substr(1);
        if (not scope.arg_env)
            throw myexception()<<"Looking up argument '"<<name<<"' in an empty environment!";

        auto& env = *scope.arg_env;
        if (not env.code_for_arg.count(name))
            throw myexception()<<env.func<<"."<<env.arg<<": can't find argument '"<<name<<"' referenced in default_value or alphabet";

        expression_ref V = env.code_for_arg.at(name);
        V = {do_return, Tuple(V,List())};

        return {{V, {}, {}, {name}, false}};
    }

    // 1. If the name is not in scope then we are done.
    if (not scope.identifiers.count(name)) return {};

    var x = scope.identifiers.at(name).x;

    expression_ref V;
    set<string> lambda_vars;


    // 2. If the name is a lambda var, then we need to quantify it, and put it into the list of free lambda vars
    if (scope.identifiers.at(name).depends_on_lambda)
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
    return {{V,{},lambda_vars,{},false}};
}


/*
 *
 * do
 *   pair_var <- var_body
 *   let var_name = fst pair_var
 *   pair_body <- let_body
 *   return (fst pair_body, [("let:var",(Nothing,[(var_name,pair_x)])),("let:body",(Nothing,snd pair_body))])
 */
optional<tuple<expression_ref,set<string>,set<string>,set<string>,bool>> get_model_let(const Rules& R, const ptree& model, const name_scope_t& scope)
{
    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();
    set<string> imports;

    // 1. If the phrase is not a let, then we are done.
    if (name != "let") return {};

    auto [var_name , var_exp ] = model_rep[0];
    auto [body_name, body_exp] = model_rep[1];

    var x("var_"+var_name);
    var arg_loggers("loggers_"+var_name);

    var body_var("let_body_var");
    var body_loggers("let_body_loggers");
    var_info_t var_info(x,is_random(var_exp, scope));

    set<string> free_vars;

    do_block code;

    // 1. Perform the variable expression
    auto [arg, arg_imports, arg_vars, arg_free_vars, any_arg_loggers] = get_model_as(R, var_exp, scope);
    add(imports, arg_imports);
    add(free_vars, arg_free_vars);

    if (arg_vars.size())
        throw myexception()<<"You cannot let-bind a variable to an expression with a function-variable";

    // (var_NAME, loggers_NAME) <- arg
    code.perform(Tuple(x,arg_loggers), arg);

    // 2. Perform the body with var_name in scope
    auto [let_body, let_body_imports, let_body_lambda_vars, let_body_free_vars, any_body_loggers] = get_model_as(R, body_exp, extend_scope(scope, var_name, var_info));
    add(imports, let_body_imports);
    add(free_vars, let_body_free_vars);

    free_vars.erase(var_name);

    // (let_body_var, let_body_logers) <- let_body
    code.perform(Tuple(body_var, body_loggers), let_body);

    // FIXME: we currently prohibit var_exp from containing any lambda-variables, so we don't need to check if it has them.
    bool do_log = is_unlogged_random(R, var_exp, scope);
    expression_ref var_loggers = List();
    var_loggers = {var("add_logger"), var_loggers, String(var_name), Tuple(x,arg_loggers), make_Bool(do_log)};

    expression_ref loggers = List();
    var Nothing("Nothing");
    loggers = {var("add_logger"),loggers,String("let:body"),Tuple(Nothing,body_loggers), make_Bool(false)};
    loggers = {var("add_logger"),loggers,String("let:var"),Tuple(Nothing,var_loggers), make_Bool(false)};

    // return (let_body_var, loggers)
    code.finish_return(Tuple(body_var,loggers));

    return {{code.get_expression(), imports, let_body_lambda_vars, free_vars, any_body_loggers or any_arg_loggers}};
}

optional<tuple<expression_ref,set<string>, set<string>,set<string>,bool>> get_model_lambda(const Rules& R, const ptree& model, const name_scope_t& scope)
{
    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a lambda, then we are done.
    if (name != "function") return {};

    // 2. Get the variable name and the body from the model
    string var_name = model_rep[0].first;
    var x("lambda_var_" + var_name);

    var body_var("arg_lambda_body");
    var body_loggers("log_lambda_body");
    ptree body_exp = model_rep[1].second;

    // 3. Parse the body with the lambda variable in scope, and find the free variables.
    var_info_t var_info(x,false,true);
    auto body_scope = extend_scope(scope, var_name, var_info);
    auto [body, body_imports, lambda_vars, lambda_free_vars, any_loggers] = get_model_as(R, body_exp, body_scope);
    auto free_vars = lambda_free_vars;

    // E = E x l1 l2 l3
    expression_ref E = body_var;
    for(auto& vname: lambda_vars)
        E = {E, body_scope.identifiers.at(vname).x};

    // E = \x -> E
    E = lambda_quantify(x, E);
    
    // Remove x from the lambda vars.
    if (lambda_vars.count(var_name)) lambda_vars.erase(var_name);
    if (free_vars.count(var_name)) free_vars.erase(var_name);

    // E = \l1 l2 l3 -> E
    for(auto& vname: std::reverse(lambda_vars))
        E = lambda_quantify(scope.identifiers.at(vname).x,E);

    do_block code;

    // (body_var, body_loggers) <- body
    code.perform(Tuple(body_var,body_loggers), body);

    // return $ (E,body_loggers)
    code.finish_return( Tuple(E, body_loggers) );

    // In summary, we have
    //E = do
    //      (body_var,body_loggers) <- body_action
    //      return $ (\l1 l2 l3 -> \x -> (body_var x l1 l2 l3) , body_loggers)
    return {{code.get_expression(), body_imports, lambda_vars, free_vars, true}};
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

    for(auto& pair: call)
        E = {E,make_call(pair.second, simple_args)};

    return E;
}

// @ return (X,[])
expression_ref is_simple_return_stmt(const expression_ref& E)
{
    if (E.size() != 2) return {};

    if (not is_apply_exp(E)) return {};

    if (is_var(E.sub()[0]) and E.sub()[0].as_<var>().name == "return")
    {
        auto arg = E.sub()[1];

        if (arg.size() != 2) return {};

        if (not has_constructor(arg,tuple_head(2).name())) return {};

        auto logger = arg.sub()[1];

        if (logger == List())
            return arg.sub()[0];
    }
    return {};
}

// do { return (X,[]) }
expression_ref is_simple_return(const expression_ref& E)
{
    if (auto x = is_simple_return_stmt(E))
        return x;

    if (not E.is_a<do_block>()) return {};

    auto& stmts = E.as_<do_block>().get_stmts();

    if (stmts.size() != 1) return {};

    if (not stmts[0].is_a<SimpleQual>()) return {};

    return is_simple_return_stmt(stmts[0].as_<SimpleQual>().exp);
}

//do { PAT <- X ; return (PAT,[]) }
expression_ref is_simple_action_return(const expression_ref& E)
{
    if (not E.is_a<do_block>()) return {};

    auto& stmts = E.as_<do_block>().get_stmts();

    if (stmts.size() != 2) return {};

    if (not stmts[0].is_a<PatQual>()) return {};

    auto& pattern = stmts[0].as_<PatQual>().bindpat;
    auto& action  = stmts[0].as_<PatQual>().exp;

    if (not stmts[1].is_a<SimpleQual>()) return {};

    auto return_pat = is_simple_return_stmt(stmts[1].as_<SimpleQual>().exp);

    if (not return_pat) return {};

    if (return_pat == pattern)
        return action;
    else
        return {};
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


// NOTE: To some extent, we construct the expression in the reverse order in which it is performed.
tuple<expression_ref, set<string>, set<string>, set<string>, bool> get_model_function(const Rules& R, const ptree& model, const name_scope_t& scope)
{
    auto scope2 = scope;
    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();
    set<string> imports;

    // 1. Get the rule for the function
    auto rule = R.get_rule_for_func(name);
    if (not rule) throw myexception()<<"No rule for '"<<name<<"'";
    if (not rule->count("call")) throw myexception()<<"No call for '"<<name<<"'";
    if (auto rule_imports = rule->get_child_optional("import"))
    {
        for(auto& [_, mod]: *rule_imports)
            imports.insert(mod.get_value<string>());
    }

    bool perform_function = rule->get("perform",false);
    ptree call = rule->get_child("call");
    ptree args = rule->get_child("args");
    
    if (not is_haskell_qid(call.get_value<string>()) and
        not is_haskell_qsym(call.get_value<string>()) and
        not is_haskell_builtin_con_name(call.get_value<string>()) and
        not starts_with(call.get_value<string>(),"@"))
        throw myexception()<<"For rule '"<<name<<"', function '"<<call.get_value<string>()<<"' doesn't seem to be a valid haskell id or a valid argument reference.";

    // 4. Construct the call expression
    map<string,expression_ref> argument_environment;
    vector<var> arg_vars;
    vector<var> log_vars;
    vector<var> arg_func_vars;
    for(int i=0;i<args.size();i++)
    {
        auto argi = array_index(args,i);
        string arg_name = argi.get_child("arg_name").get_value<string>();

        auto var_name = arg_name;
        arg_vars.push_back(scope2.get_var(var_name));
        log_vars.push_back(scope2.get_var("log_"+var_name));
        arg_func_vars.push_back(scope2.get_var(var_name+"_func"));

        argument_environment[arg_name] = arg_vars.back();
    }

    // 2. Parse models for arguments to figure out which free lambda variables they contain
    vector<expression_ref> arg_models;
    vector<set<string>> arg_lambda_vars;
    vector<set<string>> arg_free_vars(args.size());
    vector<expression_ref> simple_value;
    vector<expression_ref> simple_action;
    set<string> lambda_vars;
    set<string> free_vars;
    vector<bool> arg_loggers;
    vector<bool> arg_referenced(args.size(), false);
    bool any_loggers = false;
    for(int i=0;i<args.size();i++)
    {
        auto argi = array_index(args,i);
        string arg_name = argi.get_child("arg_name").get_value<string>();
        auto arg = model_rep.get_child(arg_name);
        bool is_default_value = arg.get_child("is_default_value").get_value<bool>();
        if (is_default_value)
            scope2.arg_env = {{name,arg_name,argument_environment}};

        auto [m, arg_imports, vars, used_args, any_arg_loggers] = get_model_as(R, model_rep.get_child(arg_name), scope2);

        if (is_default_value)
            arg_free_vars[i] = used_args;
        else
            add(free_vars, used_args);

        add(imports, arg_imports);

        if (perform_function and vars.size())
            throw myexception()<<"Argument '"<<arg_name<<"' of '"<<name<<"' contains a lambda variable: not allowed!";

        arg_loggers.push_back(any_arg_loggers);
        any_loggers = any_loggers or any_arg_loggers;
        arg_models.push_back(m);
        arg_lambda_vars.push_back(vars);
        add(lambda_vars, vars);

        // Wrap the argument in its appropriate Alphabet type
        if (auto alphabet_expression = argi.get_child_optional("alphabet"))
        {
            auto alphabet_scope = scope2;
            alphabet_scope.arg_env = {{name,arg_name,argument_environment}};
            auto [A, alphabet_imports, alphabet_lambda_vars, alphabet_free_vars, any_alphabet_loggers] = get_model_as(R, valueize(*alphabet_expression), alphabet_scope);
            if (lambda_vars.size())
                throw myexception()<<"An alphabet cannot depend on a lambda variable!";
            add(arg_free_vars[i], alphabet_free_vars);
            add(imports, alphabet_imports);
            arg_models.back() = {var("set_alphabet"),A,arg_models.back()};
            any_loggers = any_loggers or any_alphabet_loggers;
        }


        for(int j=i+1;j<args.size();j++)
        {
            auto argj = array_index(args,j);

            string arg_name_j = argj.get_child("arg_name").get_value<string>();
            if (arg_free_vars[i].count(arg_name_j))
            {
                arg_referenced[j] = true;
                if (log_verbose > 1)
                    std::cerr<<"Arg "<<name<<":"<<arg_name<<" references "<<name<<":"<<arg_name_j<<"\n";
            }
        }

        simple_value.push_back(simplify_intToDouble(is_simple_return(arg_models.back())));
        simple_action.push_back(is_simple_action_return(arg_models.back()));
    }

    do_block code;

    // 3. Peform the rule arguments in reverse order
    for(int i=args.size()-1; i>=0 ;i--)
    {
        expression_ref arg = arg_models[i];

        // If there are no lambda vars used, then we can just place the result into scope directly, without applying anything to it.
        auto x = arg_vars[i];
        auto logger = log_vars[i];
        if (arg_lambda_vars[i].empty())
        {
            if (simple_value[i])
            {
                // (arg_var_NAME, arg_logger_NAME) <- return (X,[])
                if (not arg_referenced[i])
                    assert(not arg_loggers[i]);
                else
                    code.let(x,simple_value[i]);
            }
            else if (simple_action[i])
            {
                // (arg_var_NAME, arg_logger_NAME) <- do {PAT <- action; return (PAT,[]}
                // => arg_var_NAME <- action
                assert(not arg_loggers[i]);
                code.perform(x, simple_action[i]);
            }
            else
            {
                // (arg_var_NAME, arg_logger_NAME) <- arg
                code.perform(Tuple(x,logger), arg);
            }
        }
        else
        {
            // (arg_var_NAME, arg_logger_NAME) <- arg
            code.perform(Tuple(arg_func_vars[i],logger), arg);
        }
    }

    // 4. Construct the call expression
    for(int i=0;i<args.size();i++)
    {
        auto argi = array_index(args,i);
        string arg_name = argi.get_child("arg_name").get_value<string>();
        if (simple_value[i] and not arg_referenced[i] and arg_lambda_vars[i].empty())
            argument_environment[arg_name] = simple_value[i];
    }
    expression_ref E;
    try
    {
        E = make_call(call, argument_environment);
    }
    catch(myexception& E)
    {
        E.prepend("In call for function '"+name+"': ");
        throw;
    }

    // 5. let-bind arg_var_<name> for any arguments that are (i) not performed and (ii) depend on a lambda variable.
    for(int i=0;i<args.size();i++)
    {
        auto argi = array_index(args,i);
        string arg_name = argi.get_child("arg_name").get_value<string>();

        if (not arg_lambda_vars[i].empty())
        {
            auto x_func = arg_func_vars[i];
            auto x = arg_vars[i];

            // Apply the free lambda variables to arg result before using it.
            expression_ref F = x_func;
            for(auto& vname: arg_lambda_vars[i])
                F = {F, scope.identifiers.at(vname).x};

            E = let_expression({{x,F}},E);
        }
    }

    // 6. Return a lambda function
    for(auto& vname: std::reverse(lambda_vars))
        E = lambda_quantify(scope.identifiers.at(vname).x, E);

    // 7. Compute loggers
    vector<expression_ref> logger_bits;
    for(int i=0;i<args.size();i++)
    {
        auto argi = array_index(args,i);

        string arg_name = argi.get_child("arg_name").get_value<string>();

        auto log_name = name + ":" + arg_name;

        expression_ref x_ret = (arg_lambda_vars[i].empty()) ? arg_vars[i] : arg_func_vars[i];
        expression_ref logger = (arg_loggers[i])?log_vars[i]:List();

        bool do_log = arg_lambda_vars[i].empty() ? should_log(R, model, arg_name, scope) : false;
        any_loggers = any_loggers or do_log;

        if (arg_lambda_vars[i].empty() and simple_value[i] and not arg_referenced[i])
        {
            // Under these circumstances, we don't output `let arg_var = simple_value[i]`,
            // we just substitute simple_value[i] where arg_var would go.
            //
            // FIXME - if simple_value[i] is not atomic, like f x, then we duplicate work by
            // definining a let.
            x_ret = simple_value[i];
        }

        expression_ref logger_bit;
        if (do_log and not arg_loggers[i])                      // value, but no sub-loggers
            logger_bit = {var("%=%"),String(log_name),x_ret};
        else if (arg_loggers[i] and not do_log)                 // sub-loggers, but no value
            logger_bit = {var("%>%"),String(log_name),logger};
        else if (arg_loggers[i] and do_log)                     // value and sub-loggers
            logger_bit = {var("%=>%"),Tuple(x_ret,logger)};
        else                                                    // no value and no sub-loggers: don't emit a logger at all
            continue;
        logger_bits.push_back(logger_bit);
    }
    expression_ref loggers = get_list(logger_bits);

    // 8. Return the function call: 'return (f call.name1 call.name2 call.name3)'
    if (not perform_function)
    {
        code.finish_return( Tuple(E,loggers) );
    }
    else
    {
        // result <- E
        code.perform( var("result"), E );
        // return (result, loggers)
        code.finish_return( Tuple(var("result"),loggers) );
    }

    return {code.get_expression(), imports, lambda_vars, free_vars, any_loggers};
}

tuple<expression_ref, set<string>, set<string>, set<string>, bool> get_model_as(const Rules& R, const ptree& model_rep, const name_scope_t& scope)
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
        return {constant, {}, {}, {}, false};

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
model_t get_model(const Rules& R, const ptree& type, const std::set<term_t>& constraints, const ptree& model_rep, const name_scope_t& scope)
{
    // --------- Convert model to MultiMixtureModel ------------//
    auto [full_model, imports, _1, _2, _3] = get_model_as(R, model_rep, scope);

    if (log_verbose >= 2)
        std::cout<<"full_model = "<<full_model<<std::endl;

    return {model_rep, imports, type, constraints, full_model};
}

model_t get_model(const Rules& R, const string& type, const string& model_string, const vector<pair<string,string>>& scope)
{
    auto required_type = parse_type(type);
    auto model_rep = parse(R, model_string);
//    std::cout<<"model1 = "<<show(model_rep)<<std::endl;

    vector<pair<string,ptree>> typed_scope;
    for(auto& [name,type]: scope)
        typed_scope.push_back({name, parse_type(type)});
    auto [model, equations] = translate_model(R, required_type, model_rep, typed_scope);

    model_rep = extract_value(model);

    substitute(equations, model_rep);
    substitute(equations, required_type);
    if (log_verbose >= 1)
    {
        std::cout<<"model = "<<unparse_annotated(model)<<std::endl;
        std::cout<<"type = "<<unparse_type(required_type)<<std::endl;
        std::cout<<"equations: "<<show(equations)<<std::endl;
        std::cout<<"structure = "<<show(model_rep)<<std::endl;
        std::cout<<"annotated structure = "<<show(model)<<std::endl;
        std::cout<<"pretty:\n"<<pretty_model_t(model).show()<<std::endl;
        std::cout<<std::endl;
    }

    name_scope_t names_in_scope;
    for(auto& [name,type]: scope)
        names_in_scope.identifiers.insert({name, var_info_t(var("var_"+name))});

    return get_model(R, required_type, equations.get_constraints(), model, names_in_scope);
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
    for(auto& [arg_name,arg_value]: value)
    {
        string name = value.get_value<string>() + ":" + arg_name;

        // If we should pull out the argument then do so
        if (do_extract(m, arg_value))
        {
            ptree extracted_value;
            std::swap(arg_value, extracted_value);
            extracted_top.push_back({name, extracted_value});
        }
        // Otherwise look into the argument's value and try to pull things out
        else if (not arg_value.is_null()) // for function[x=null,body=E]
        {
            for(auto& [sub_name,sub_term]: extract_terms(arg_value))
                extracted.push_back({name+"/"+sub_name, std::move(sub_term)});
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
    for(auto& [name,term]: extract_terms(main))
    {
        term_names.push_back(name);
        terms.push_back(term);
    }

    // 2. Fix up names
    for(auto& name: term_names)
        name = translate_structures(name);

    term_names = short_parameter_names(term_names);
}
