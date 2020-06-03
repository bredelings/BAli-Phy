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

#include "util/graph.H"

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

model_t::model_t(const ptree& d, const set<string>& i, const ptree&t, const std::set<term_t>& s, const generated_code_t& c)
    :description(d), imports(i), type(t), constraints(s), code(c)
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

struct translation_result_t
{
    generated_code_t code;
    set<string> imports;
    set<string> lambda_vars;
    set<string> used_args;
};

translation_result_t get_model_as(const Rules& R, const ptree& model_rep, const name_scope_t& scope);

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
        return translation_result_t{{C,false,false},{},{},{}};
    }
    else
        return {};
}

optional<translation_result_t> get_variable_model(const ptree& model, const name_scope_t& scope)
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

        translation_result_t result = {{V,false,false}, {}, {}, {name}};
        return result;
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

    // 4. We need an extra level of {} to allow constructing the optional.
    translation_result_t result = {{V,false,false}, {}, lambda_vars, {}};
    return result;
}


void maybe_log(vector<expression_ref>& loggers,
               const string& name,
               const expression_ref& value,
               const expression_ref& subloggers)
{
    expression_ref logger_bit;
    if (value)
        loggers.push_back({var("%=%"),String(name),value});
    if (subloggers)
        loggers.push_back({var("%>%"),String(name),subloggers});
}


void perform_action_simplified(do_block& block, const var& x, const var& log_x, bool is_referenced, const generated_code_t& code)
{
    if (code.is_action)
    {
        if (not code.has_loggers)
            // x <- code
            block.perform(x, code.E);
        else
            // (x, log_x) <- code
            block.perform(Tuple(x,log_x), code.E);
    }
    else
    {
        if (code.has_loggers)
            block.let(Tuple(x,log_x), code.E);
        else if (is_referenced)
            block.let(x, code.E);
    }
}

/*
 *
 * do
 *   pair_var <- var_body
 *   let var_name = fst pair_var
 *   pair_body <- let_body
 *   return (fst pair_body, [("let:var",(Nothing,[(var_name,pair_x)])),("let:body",(Nothing,snd pair_body))])
 */
optional<translation_result_t> get_model_let(const Rules& R, const ptree& model, const name_scope_t& scope)
{
    auto scope2 = scope;

    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a let, then we are done.
    if (name != "let") return {};

    auto [var_name , var_exp ] = model_rep[0];
    auto [body_name, body_exp] = model_rep[1];

    var x = scope2.get_var(var_name);
    var log_x = scope2.get_var("log_" + var_name);
    bool x_is_random = is_random(var_exp, scope);
    var_info_t var_info(x, x_is_random);

    var body = scope2.get_var("body");
    var log_body = scope2.get_var("log_body");

    // 1. Perform the variable expression
    auto arg_result = get_model_as(R, var_exp, scope);

    if (arg_result.lambda_vars.size())
        throw myexception()<<"You cannot let-bind a variable to an expression with a function-variable";

    // 2. Perform the body with var_name in scope
    auto body_result = get_model_as(R, body_exp, extend_scope(scope, var_name, var_info));

    auto result = body_result;
    add(result.imports, arg_result.imports);
    add(result.used_args, arg_result.used_args);
    result.code.has_loggers = body_result.code.has_loggers or arg_result.code.has_loggers or x_is_random;

    // 3. Construct loggers
    vector<expression_ref> logger_bits;
    {
        expression_ref value = x_is_random ? x : expression_ref{};
        expression_ref subloggers = arg_result.code.has_loggers ? log_x : expression_ref{};
        maybe_log(logger_bits, var_name, value, subloggers);
    }
    {
        expression_ref subloggers = body_result.code.has_loggers ? log_body : expression_ref{};
        maybe_log(logger_bits, "body", {}, subloggers);
    }
    expression_ref loggers = get_list(logger_bits);

    // 4. Construct code.

    do_block code;

    // (x, log_x) <- E
    perform_action_simplified(code, x, log_x, true, arg_result.code);

    // (body, log_body) <- body
    perform_action_simplified(code, body, log_body, true, body_result.code);

    // return (body, loggers)
    if (body_result.code.is_action)
    {
        if (body_result.code.has_loggers)
            code.finish_return(Tuple(body,loggers));
        else
            code.finish_return(body);
    }
    else
    {
        if (body_result.code.has_loggers)
            code.finish(Tuple(body,loggers));
        else
            code.finish(body);
    }

    result.code.E = code.get_expression();

    return result;
}

expression_ref eta_reduce(expression_ref E)
{
    while(is_lambda_exp(E))
    {
        auto& x    = E.sub()[0].as_<var>();
        auto& body = E.sub()[1];

        if (is_apply_exp(body) and body.sub().back() == x)
        {
            // ($) f x  ==> f
            if (body.size() == 2)
                E = body.sub()[0];
            // ($) f y x ==> ($) f y
            else
            {
                // This is the simple case, where we can just pop an argument off the end of the list.
                assert(body.size() > 2);
                object_ptr<expression> body2 = body.as_expression().clone();
                body2->sub.pop_back();
                E = body2;
            }
        }
        else
            break;
    }
    return E;
}

optional<translation_result_t> get_model_lambda(const Rules& R, const ptree& model, const name_scope_t& scope)
{
    auto scope2 = scope;

    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    // 1. If the phrase is not a lambda, then we are done.
    if (name != "function") return {};

    // 2. Get the variable name and the body from the model
    string var_name = model_rep[0].first;
    auto x = scope2.get_var(var_name);
    auto body = scope2.get_var("lbody");
    auto log_body = scope2.get_var("log_lbody");

    ptree body_model = model_rep[1].second;

    // 3. Parse the body with the lambda variable in scope, and find the free variables.
    var_info_t var_info(x,false,true);
    auto body_scope = extend_scope(scope2, var_name, var_info);
    auto body_result = get_model_as(R, body_model, body_scope);

    // FIXME! If we can ensure that arguments are applied with most-nested first, then
    // we can avoid the complicated code in both #4a,b,d,e and #5b.
    // All this code is just to handle the possibility that arguments are in a different order.

    // 4. Make the lambda expression:

    // 4a. Apply argments: E = E x l1 l2 l3
    expression_ref E = body;
    for(auto& vname: body_result.lambda_vars)
        E = {E, body_scope.identifiers.at(vname).x};

    // 4b. First quantify with x: E = \x -> E
    E = lambda_quantify(x, E);
    
    // 4c. Remove x from the lambda vars.
    if (body_result.lambda_vars.count(var_name)) body_result.lambda_vars.erase(var_name);

    // 4d. Then quantify with all the other vars.
    // E = \l1 l2 l3 -> E
    for(auto& vname: std::reverse(body_result.lambda_vars))
        E = lambda_quantify(scope2.identifiers.at(vname).x,E);

    // Here E = (\l1 l2 l3 -> \x -> (body x l1 l2 l3))

    // 4e. Now eta-reduce E.  If E == (\x -> body x), we will get just E == body
    E = eta_reduce(E);

    // 5. If E is the same as body var, we can just return the previous values unchanged,
    //    except that we have removed x from the lambda_vars.
    if (E == body)
    {
        return body_result;
    }
    else
    {
        do_block code;

        // (body, log_body) <- body_exp
        perform_action_simplified(code, body, log_body, true, body_result.code);

        // return $ (E, log_body)
        if (body_result.code.has_loggers)
            code.finish_return( Tuple(E, log_body) );
        else
            code.finish_return( E );

        body_result.code.E = code.get_expression();
        // In summary, we have
        //E = do
        //      (body,log_body) <- body_action
        //      return $ (\l1 l2 l3 -> \x -> (body x l1 l2 l3) , log_body)
        return body_result;
    }
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

    for(int i=0;i<call.size();i++)
    {
        if (i == call.size()-1 and call[i].second == "@submodel")
            E = {var("&"), make_call(call[i].second, simple_args),E};
        else
            E = {E, make_call(call[i].second, simple_args)};
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
                if (log_verbose > 1)
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
translation_result_t get_model_function(const Rules& R, const ptree& model, const name_scope_t& scope)
{
    auto scope2 = scope;
    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();

    translation_result_t result;

    // 1. Get the rule for the function
    auto rule = R.get_rule_for_func(name);
    if (not rule) throw myexception()<<"No rule for '"<<name<<"'";
    if (not rule->count("call")) throw myexception()<<"No call for '"<<name<<"'";
    if (auto rule_imports = rule->get_child_optional("import"))
    {
        for(auto& [_, mod]: *rule_imports)
            result.imports.insert(mod.get_value<string>());
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
        if (var_name == "submodel")
        {
            auto arg = model_rep.get_child(arg_name);
            if (auto func_name = get_func_name(arg))
                var_name = (*func_name)+"_model";
        }
        arg_vars.push_back(scope2.get_var(var_name));
        log_vars.push_back(scope2.get_var("log_"+var_name));
        arg_func_vars.push_back(scope2.get_var(var_name+"_func"));

        argument_environment[arg_name] = arg_vars.back();
    }

    // 2. Parse models for arguments to figure out which free lambda variables they contain
    vector<generated_code_t> arg_models;
    vector<set<string>> arg_lambda_vars;
    vector<set<string>> used_args_for_arg(args.size());
    vector<string> arg_names(args.size());
    vector<bool> arg_loggers;

    for(int i=0;i<args.size();i++)
    {
        auto argi = array_index(args,i);
        arg_names[i] = argi.get_child("arg_name").get_value<string>();
        auto arg = model_rep.get_child(arg_names[i]);
        bool is_default_value = arg.get_child("is_default_value").get_value<bool>();
        if (is_default_value)
            scope2.arg_env = {{name,arg_names[i],argument_environment}};

        auto arg_result = get_model_as(R, model_rep.get_child(arg_names[i]), scope2);

        if (is_default_value)
            used_args_for_arg[i] = arg_result.used_args;
        else
            add(result.used_args, arg_result.used_args);

        if (perform_function and arg_result.lambda_vars.size())
            throw myexception()<<"Argument '"<<arg_names[i]<<"' of '"<<name<<"' contains a lambda variable: not allowed!";

        arg_loggers.push_back(arg_result.code.has_loggers);
        result.code.has_loggers = result.code.has_loggers or arg_result.code.has_loggers;
        result.code.is_action = result.code.is_action or arg_result.code.is_action;
        arg_models.push_back(arg_result.code);
        arg_lambda_vars.push_back(arg_result.lambda_vars);
        add(result.lambda_vars, arg_result.lambda_vars);
        add(result.imports, arg_result.imports);

        // Wrap the argument in its appropriate Alphabet type
        if (auto alphabet_expression = argi.get_child_optional("alphabet"))
        {
            auto alphabet_scope = scope2;
            alphabet_scope.arg_env = {{name,arg_names[i],argument_environment}};
            auto alphabet_result = get_model_as(R, valueize(*alphabet_expression), alphabet_scope);
            if (alphabet_result.lambda_vars.size())
                throw myexception()<<"An alphabet cannot depend on a lambda variable!";
            assert(not alphabet_result.code.has_loggers);
            assert(not alphabet_result.code.is_action);
            add(used_args_for_arg[i], alphabet_result.used_args);
            add(result.imports, alphabet_result.imports);
            arg_models.back().E = {var("set_alphabet'"), alphabet_result.code.E, arg_models.back().E};
        }
    }

    // 2. Figure out which args are referenced from other args
    //    FIXME!  We should start with just args referenced by the call expression.
    //    We could also count HOW MANY times each arg is referenced by the call expression.
    auto arg_referenced = get_args_referenced(arg_names, used_args_for_arg);

    auto arg_order = get_args_order(arg_names, used_args_for_arg);

    do_block code;

    // 3. Peform the rule arguments in reverse order
    for(int i: arg_order)
    {
        // (x, logger) <- arg
        auto x = arg_vars[i];
        auto log_x = log_vars[i];
        auto arg = arg_models[i];
        auto x_func = arg_func_vars[i];

        // If there are no lambda vars used, then we can just place the result into scope directly, without applying anything to it.
        bool need_to_emit = arg_referenced[i] or arg.has_loggers;
        if (arg_lambda_vars[i].empty())
            perform_action_simplified(code, x,      log_x, need_to_emit, arg);
        else
            perform_action_simplified(code, x_func, log_x, true, arg);

    }

    // 4. Construct the call expression
    for(int i=0;i<args.size();i++)
    {
        auto argi = array_index(args,i);
        string arg_name = argi.get_child("arg_name").get_value<string>();
        auto arg = arg_models[i];

        bool can_substitute = not arg.is_action and not arg.has_loggers;
        if (can_substitute and not arg_referenced[i] and arg_lambda_vars[i].empty())
            argument_environment[arg_name] = arg.E;
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
    for(auto& vname: std::reverse(result.lambda_vars))
        E = lambda_quantify(scope.identifiers.at(vname).x, E);

    // 7. Compute loggers
    vector<expression_ref> logger_bits;
    for(int i=0;i<args.size();i++)
    {
        auto argi = array_index(args,i);

        string arg_name = argi.get_child("arg_name").get_value<string>();

        auto log_name = name + ":" + arg_name;

        bool do_log = arg_lambda_vars[i].empty() ? should_log(R, model, arg_name, scope) : false;

        expression_ref value  = do_log                    ? arg_vars[i] : expression_ref{};
        expression_ref logger = arg_models[i].has_loggers ? log_vars[i] : expression_ref{};

        result.code.has_loggers = result.code.has_loggers or do_log;

        auto can_substitute = not arg_models[i].is_action and not arg_models[i].has_loggers;
        if (arg_lambda_vars[i].empty() and can_substitute and not arg_referenced[i])
        {
            // Under these circumstances, we don't output `let arg_var = simple_value[i]`,
            // we just substitute simple_value[i] where arg_var would go.
            //
            // FIXME - if simple_value[i] is not atomic (like f x) then we duplicate work
            // then we duplicate work by substituting:
            value = arg_models[i].E;
        }

        maybe_log(logger_bits, log_name, value, logger);
    }
    expression_ref loggers = get_list(logger_bits);

    // 8. Return the function call: 'return (f call.name1 call.name2 call.name3)'
    if (not perform_function)
    {
        if (result.code.has_loggers)
            E = Tuple(E,loggers);
        if (result.code.is_action)
            E = {do_return, E};
        if (not code.empty())
            E = code.finish(E);
    }
    else
    {
        result.code.is_action = true;
        if (result.code.has_loggers)
        {
            // result <- E
            code.perform( var("result"), E );
            // return (result, loggers)
            E = code.finish_return( Tuple(var("result"),loggers) );
        }
        else if (not code.empty())
            E = code.finish( E );
    }

    result.code.E = simplify_intToDouble(E);
    return result;
}

translation_result_t get_model_as(const Rules& R, const ptree& model_rep, const name_scope_t& scope)
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
    auto [full_model, imports, _1, _2] = get_model_as(R, model_rep, scope);

    if (log_verbose >= 2)
        std::cout<<"full_model = "<<full_model.E<<std::endl;

    return model_t{model_rep, imports, type, constraints, full_model};
}

model_t get_model(const Rules& R, const string& type, const string& model_string, const vector<pair<string,string>>& scope)
{
    auto required_type = parse_type(type);
    auto model_rep = parse(R, model_string);
//    std::cout<<"model1 = "<<show(model_rep)<<std::endl;

    map<string,ptree> typed_scope;
    for(auto& [name,type]: scope)
        typed_scope.insert({name, parse_type(type)});
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
