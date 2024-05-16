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
#include <sstream>
#include <boost/program_options.hpp>

#include "util/graph.H"

#include "models/setup.H"
#include "sequence/doublets.H"
#include "sequence/codons.H"
#include "util/string/join.H"
#include "util/string/pred.H"
#include "util/set.H"
#include "util/range.H"
#include "util/myexception.H"
#include "models/rules.H"
#include "models/parse.H"
#include "models/typecheck.H"
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
#include "computation/haskell/ids.H"

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

bool is_loggable_type(const type_t& type)
{
    if (type.get_value<string>() == "String") return true;

    else if (type.get_value<string>() == "Int") return true;

    else if (type.get_value<string>() == "Double") return true;

    else if (type.get_value<string>() == "List")
    {
        if (type.size() != 1) return false;

        return is_loggable_type(type[0].second);
    }

    else if (type.get_value<string>() == "Tuple")
    {
        for(auto& [_,type2]: type)
            if (not is_loggable_type(type2)) return false;
        return true;
    }

    return false;
}


json::object convert_to_json(const pretty_model_t& m)
{
    json::object j;
    j["main"] = unparse_annotated(m.main);
    json::array extracted;
    for(int i=0;i<m.terms.size();i++)
    {
	json::array p(2);
        p[0] = json::string(m.term_names[i]);
        p[1] = convert_to_json(m.terms[i]);
        extracted.push_back(p);
    }
    j["extracted"] = extracted;
    return j;
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

string print_equals_function(expression_ref E)
{
    std::ostringstream result;
    while(is_lambda_exp(E))
    {
        auto x = E.sub()[0];
        E = E.sub()[1];
        result<<" "<<x;
    }
    result<<" = "<<E;
    return result.str();
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
        return "tn93_sym+>x2_sym+>f";
    else if (dynamic_cast<const Triplets*>(&a))
        return "tn93+>x3";
    else
        return "";
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

    // A random "let" has NOT necessarily been logged.
    // * let[k=random,k+2]         # OK    : we assume that if k is random, the whole thing is random.
    // * let[k=random,hky85[k]]    # Not OK: we assume that if k is random, the whole thing is random.
    // Probably we should track whether the result is an unlogged random in translation_result_t.
    if (name == "let") return false;

    if (name == "function") return true;

    if (not rule) return false;
    return not rule->get("no_log",false);
}

// See simplify(json& j) in models/model.cc and simplify(ptree&) models/in path.cc

void simplify(Loggers& loggers)
{
    // 1. Second, determine if we have subloggers where we can lift children out and append them to the prefix.
    std::vector<bool> lift_children(loggers.size(), false);
    for(int i=0;i<loggers.size();i++)
    {
        auto& l = loggers[i];
        if (auto lsub = l.as<LogSub>())
        {
            lift_children[i] = true;
            for(auto& ll: lsub->loggers)
            {
                if (auto llsub = ll.as<LogSub>())
                {
                    if (llsub->prefix.size() > 0 and llsub->prefix[0] == '[')
                        continue;
                }
                lift_children[i] = false;
                break;
            }
        }
    }

    /// 2. Move the children
    Loggers loggers3;
    for(int i=0; i<loggers.size(); i++)
    {
        auto& l = loggers[i];
        if (not lift_children[i])
            loggers3.push_back(std::move(l));
        else
        {
            auto lsub = l.as<LogSub>();
            for(auto& ll: lsub->loggers)
            {
                const auto& llsub = ll.as<LogSub>();
                llsub->prefix = lsub->prefix + llsub->prefix;
                loggers3.push_back(std::move(ll));
            }
        }
    }
    std::swap(loggers, loggers3);

    // 3. First we simplify all the level below this one.
    for(auto& l: loggers)
    {
        if (auto lsub = l.as<LogSub>())
            simplify(lsub->loggers);
    }

    // 4. In order to move child-level names up to the top level, we have to avoid
    //   a. clashing with the same name at the top level
    //   b. clashing with the same name in a sibling.
    // We therefore count which names at these levels occur twice and avoid them.

    // NOTE: In theory we could have subloggers with internal names/prefixes that overlap with the
    // external prefix of another logger.  But if that logger is going away, then its external
    // prefix will disappear and the conflict isn't real.  However, we do prevent merging
    // subloggers that have names that conflict with the (external) prefix of another logger.
    // EXAMPLE: If we have a situation like {I1/S1, S2/I1} then this approach won't simplify to {S1,I1}

    std::multiset<string> names;
    for(auto& l: loggers)
    {
        names.insert(l->get_name());
        if (auto lsub = l.as<LogSub>())
            for(auto& ll: lsub->loggers)
                names.insert(ll->get_name());
    }

    // 5. If none of the names in an entry occur twice, then we can move all then
    //    names in that entry up to the top level.
    vector<bool> move_children(loggers.size());
    for(int i=0; i<loggers.size(); i++)
    {
        auto l = loggers[i].as<LogSub>();
        if (not l) continue;

        bool ok = true;
        for(auto& ll: l->loggers)
        {
            if (names.count(ll->get_name()) > 1)
            {
                ok = false;
                break;
            }
        }
        if (ok)
            move_children[i] = true;
    }

    /// 6. Move the children
    Loggers loggers2;
    for(int i=0; i<loggers.size(); i++)
    {
        auto& l = loggers[i];
        if (not move_children[i])
            loggers2.push_back(std::move(l));
        else
        {
            for(auto& ll: l.as<LogSub>()->loggers)
                loggers2.push_back(std::move(ll));
        }
    }
    std::swap(loggers, loggers2);
}


expression_ref generate_loggers(do_block& code, const Loggers& loggers)
{
    vector<expression_ref> simple_loggers;
    for(auto& l: loggers)
    {
        if (auto lsub = l.as<LogSub>())
        {
            auto log_x = lsub->log_var;
            auto logger_list = generate_loggers(code,lsub->loggers);
            code.let(log_x,logger_list);
            simple_loggers.push_back({var("%>%"),String(lsub->prefix),log_x});
        }
        else if (auto lvalue = l.as<LogValue>())
            simple_loggers.push_back({var("%=%"),String(lvalue->name),lvalue->value});
        else
            std::abort();
    }
    return get_list(simple_loggers);
}


expression_ref generated_code_t::generate() const
{
    do_block code(stmts);

    auto loggers2 = loggers;
    simplify(loggers2);
    auto L = generate_loggers(code,loggers2);

    auto R = simplify_intToDouble(E);

    if (not perform_function)
    {
        if (has_loggers())
        {
            // FIXME: technically, we should make sure that "result" and "loggers" are unique names
            // FIXME: it would be nice to use e.g. tn93_model = ... instead of just result = ....
            //        see:  var_name = (*func_name)+"_model";  for naming "submodel" arguments.
            code.let(var("result"),R);
            code.let(var("loggers"),L);
            R = Tuple(var("result"),var("loggers"));
        }
        // If there are let stmts, we could return let{decls} in R
        if (not code.empty())
            R = code.finish_return(R);
    }
    else
    {
        if (has_loggers())
        {
            // result <- E
            code.perform( var("result"), R );
            // return (result, loggers)
            R = code.finish_return( Tuple(var("result"),L) );
        }
        else if (not code.empty())
            R = code.finish( R );
    }

    // if is_action() is false, we should not have a do_block()
    assert(is_action() or not R.is_a<do_block>());

    for(int i=lambda_vars.size()-1; i>=0; i--)
        R = lambda_quantify(lambda_vars[i],R);

    return R;
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
    map<string, var> state;

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

    void set_state(const string& name, const var& x)
    {
        assert(vars.count(x));
        if (state.count(name))
            state.erase(name);
        state.insert({name,x});
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

    // Don't treat let-declared random variables as unlogged.
    // 2. If this is a random variable, then yes.
    // if (not model.size() and model.is_a<string>())
    //    if (scope.identifiers.count(name) and scope.identifiers.at(name).is_random) return true;

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

    if (is_constant(model)) return false;

    auto name = model.get_value<string>();

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

void extend_modify_scope(name_scope_t& scope, const string& var, const var_info_t& var_info)
{
    if (scope.identifiers.count(var))
        scope.identifiers.erase(var);
    scope.identifiers.insert({var, var_info});
    scope.vars.insert(var_info.x);
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
    set<var> vars;
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
        translation_result_t result;
        result.code.E = C;
        return result;
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

        translation_result_t result;
        result.code.E = V;
        return result;
    }

    // 1. If the name is not in scope then we are done.
    if (not scope.identifiers.count(name)) return {};

    var_info_t var_info = scope.identifiers.at(name);

    translation_result_t result;
    result.code.E = var_info.x;
    if (var_info.depends_on_lambda)
        result.lambda_vars = {name};
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
    add(block.vars, code.vars);
    add(block.code.used_states, code.code.used_states);

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

void generated_code_t::log_value(const string& name, const expression_ref& value)
{
    loggers.push_back(LogValue(name, value));
}

void generated_code_t::log_sub(const string& name, const var& log_var, const Loggers& ls)
{
    loggers.push_back(LogSub(name, log_var, ls));
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
        var_info.depends_on_lambda = true;

    // 2. Perform the body with var_name in scope
    auto body_result = get_model_as(R, body_exp, extend_scope(scope2, var_name, var_info));

    // 3. Construct code.

    translation_result_t result;
    result.vars = scope2.vars;

    // (x, log_x) <- arg_result
    perform_action_simplified(result, x, log_x, true, arg_result, var_name);
    if (x_is_random and is_loggable_type(var_exp.get_child("type")))
        result.code.log_value(var_name, x);

    // body_result
    use_block(result, log_body, body_result, "body");
    result.code.E = body_result.code.E;

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


optional<translation_result_t> get_model_lambda(const Rules& R, const ptree& model, const name_scope_t& scope)
{
    auto scope2 = scope;

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

    // 3. Parse the body with the lambda variable in scope, and find the free variables.

    for(auto& var_name: var_names)
    {
        auto x = scope2.get_var(var_name);
        var_info_t var_info(x,false,true);
        extend_modify_scope(scope2, var_name, var_info);
    }
    auto body_result = get_model_as(R, body_model, scope2);

    // 4. Remove pattern variables from the lambda vars.
    for(auto& var_name: var_names)
        if (body_result.lambda_vars.count(var_name))
            body_result.lambda_vars.erase(var_name);

    // 5. Add the lambda in front of the expression
    auto pattern2 = get_model_as(R, pattern, scope2);
    body_result.code.E = lambda_quantify(pattern2.code.E, body_result.code.E);

    // 6. Now eta-reduce E.  If E == (\x -> body x), we will get just E == body
    body_result.code.E = eta_reduce(body_result.code.E);

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
            E = {var("+>"), make_call(call[i].second, simple_args),E};
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
optional<translation_result_t> get_model_list(const Rules& R, const ptree& model, const name_scope_t& scope)
{
    auto scope2 = scope;
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
        auto element_result = get_model_as(R, element, scope2);

        // 3c. Avoid re-using any haskell vars.
        add(scope2.vars, element_result.vars);

        // 3d. Include stmts and dependencies
        auto log_x = scope2.get_var("log"+var_name);
        use_block(result, log_x, element_result, log_name);

        // 3e. Maybe emit code for the element.
        bool do_log = is_unlogged_random(R, element, scope) and is_loggable_type(element.get_child("type"));
        if (element_result.code.perform_function)
            result.code.stmts.perform(x, element_result.code.E);
        if (do_log and not is_var(element_result.code.E))
            result.code.stmts.let(x, element_result.code.E);
        else
            argument_environment[i] = element_result.code.E;

        // 3f. Maybe log the element.
        if (do_log)
            result.code.log_value(log_name, argument_environment[i]);
    }

    // 4. Compute the call expression.
    result.code.E = get_list(argument_environment);

    // 5. Make sure not to re-use any vars adding do this code.
    add(result.vars, scope2.vars);

    return result;
}

// NOTE: To some extent, we construct the expression in the reverse order in which it is performed.
optional<translation_result_t> get_model_tuple(const Rules& R, const ptree& model, const name_scope_t& scope)
{
    auto scope2 = scope;
    auto model_rep = model.get_child("value");
    auto name = model_rep.get_value<string>();


    // 1. If the phrase is not a lambda, then we are done.
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
        auto element_result = get_model_as(R, element, scope2);

        // 3c. Avoid re-using any haskell vars.
        add(scope2.vars, element_result.vars);

        // 3d. Include stmts and dependencies
        auto log_x = scope2.get_var("log"+var_name);
        use_block(result, log_x, element_result, log_name);

        // 3e. Maybe emit code for the element.
        bool do_log = is_unlogged_random(R, element, scope) and is_loggable_type(element.get_child("type"));
        if (element_result.code.perform_function)
            result.code.stmts.perform(x, element_result.code.E);
        if (do_log and not is_var(element_result.code.E))
            result.code.stmts.let(x, element_result.code.E);
        else
            argument_environment[i] = element_result.code.E;

        // 3f. Maybe log the element.
        if (do_log)
            result.code.log_value(log_name, argument_environment[i]);
    }

    // 4. Compute the call expression.
    result.code.E = get_tuple(argument_environment);

    // 5. Make sure not to re-use any vars adding do this code.
    add(result.vars, scope2.vars);

    return result;
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
        arg_names[i] = argi.get_child("arg_name").get_value<string>();

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
            auto alphabet_result = get_model_as(R, *alphabet_expression, alphabet_scope);
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
        arg_models[i] = get_model_as(R, arg, scope3);

        // Move this to generate()
        if (result.code.perform_function and arg_models[i].lambda_vars.size())
            throw myexception()<<"Argument '"<<arg_names[i]<<"' of '"<<name<<"' contains a lambda variable: not allowed!";

        // Avoid re-using any haskell vars.
        add(scope2.vars, arg_models[i].vars);

        // (x, logger) <- arg
        auto x = arg_vars[i];
        auto log_x = log_vars[i];

        bool do_log = should_log(R, model, arg_names[i], scope) and is_loggable_type(arg.get_child("type")) and arg_models[i].lambda_vars.empty();

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
            result.code.log_value(log_names[i], argument_environment[arg_names[i]]);
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
	    result.code.stmts.let(x_var, make_call(value, argument_environment));

	    // C. Log the computed variable
	    result.code.log_value(x_log_name, x_var);

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
    add(result.vars, scope2.vars);

    return result;
}

// NOTE: To some extent, we construct the expression in the reverse order in which it is performed.
optional<translation_result_t> get_model_state(const Rules&, const ptree& model, const name_scope_t& scope)
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
        if (scope.state.count(*state_name))
        {
            auto x = scope.state.at(*state_name);
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

    // 5. Lambda expressions
    else if (auto func = get_model_lambda(R, model_rep, scope))
        return *func;

    // 6. get_state[state] expressions.
    else if (auto list = get_model_list(R, model_rep, scope))
        return *list;

    // 7. get_state[state] expressions.
    else if (auto list = get_model_tuple(R, model_rep, scope))
        return *list;

    // 8. get_state[state] expressions.
    else if (auto state = get_model_state(R, model_rep, scope))
        return *state;

    // 9. Functions
    return get_model_function(R, model_rep, scope);
}

void substitute_annotated(const equations& equations, ptree& model)
{
    auto& type = model.get_child("type");
    substitute(equations, type);

    for(auto& [key, value]: model.get_child("value"))
        substitute_annotated(equations, value);
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
    auto [code, imports, _1, _2] = get_model_as(R, model_rep, scope);

    if (log_verbose >= 3)
        std::cout<<"full_model = "<<code.print()<<std::endl;

    for(const string& state_name: code.used_states)
        code.lambda_vars.push_back( scope.state.at(state_name) );

    return model_t{model_rep, imports, type, constraints, code};
}

model_t get_model(const Rules& R, const string& type, const string& model_string, const string& what,
                  const vector<pair<string,string>>& scope,
                  const map<string,pair<string,string>>& state)
{
    auto required_type = parse_type(type);
    auto model_rep = parse(R, model_string, what);
//    std::cout<<"model1 = "<<show(model_rep)<<std::endl;

    map<string,ptree> typed_scope;
    for(auto& [name,type]: scope)
        typed_scope.insert({name, parse_type(type)});
    map<string,ptree> typed_state;
    for(auto& [state_name,p]: state)
    {
        auto& [_, var_type_string] = p;
        typed_state.insert({state_name,parse_type(var_type_string)});
    }
    auto [model, equations] = typecheck_and_annotate_model(R, required_type, model_rep, typed_scope, typed_state);

    model_rep = extract_value(model);

    substitute(equations, model_rep);
    substitute(equations, required_type);
    substitute_annotated(equations, model);
    if (log_verbose >= 2)
    {
        std::cout<<"model = "<<unparse_annotated(model)<<std::endl;
        std::cout<<"type = "<<unparse_type(required_type)<<std::endl;
        std::cout<<"equations: "<<show(equations)<<std::endl;
        std::cout<<"structure = "<<show(model_rep)<<std::endl;
        std::cout<<"annotated structure = "<<show(model)<<std::endl;
        std::cout<<"pretty:\n"<<pretty_model_t(model).show()<<std::endl;
        std::cout<<std::endl;
    }

    vector<var> lambda_vars;

    name_scope_t names_in_scope;
    for(auto& [name,type]: scope)
    {
        auto x = names_in_scope.get_var(name);
        names_in_scope.identifiers.insert({name, var_info_t(x)});
    }

    for(auto& [state_name,p]: state)
    {
        auto& [var_name, _] = p;
        auto x = names_in_scope.get_var(var_name);
        names_in_scope.set_state(state_name,x);
        lambda_vars.push_back(x);
    }

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
    if (term.get<string>("extract","none") == "all") return true;

    auto value = term.get_child("value");

    if (value.has_value<string>() and value.get_value<string>() == "List")
    {
        for(auto& [arg_name,arg_value]: term.get_child("value"))
            if (annotated_term_is_model(arg_value)) return true;
    }

    return false;
}

// Don't extract terms that
// * contain function variables
// * don't extract gamma::n if its an integer
// Suppress gamma::a = getAlphabet
// Some terms seem to have types like 'var5' so they always fail the check for
//   extractable types.


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
    // 1d. Don't pull anything out of lambdas
    if (func_name == "List") return false;
    // 1e. Don't pull anything out of lambdas
    if (func_name == "Tuple") return false;

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
        if (arg_type == "List<Double>" or arg_type == "List<(String,Double)>")
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
    if (value.has_value<string>() and value.get_value<string>() == "let")
    {
        assert(value.size() == 2);

        string var_name = value[0].first;
        ptree tmp1;
        std::swap(tmp1,value[0].second);
        ptree tmp2;
        std::swap(tmp2,value[1].second);
        auto extracted = extract_terms(tmp2);
        extracted.insert(extracted.begin(),{var_name,tmp1});
        std::swap(tmp2,m);
        return extracted;
    }

    vector<pair<string,ptree>> extracted_top;
    int i=0;
    // Walk each argument and determine if it should be pulled out
    for(auto& [arg_name,arg_value]: value)
    {
        auto func = value.get_value<string>();
        string name = func + ":" + arg_name;
        if (func == "List" or func == "Tuple")
        {
            name = "["+std::to_string(++i)+"]";
        }

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
            {
                auto sup_name = name + "/" + sub_name;
                // Fuse subscripts like [1] into the name.
                if (sub_name.size() and sub_name[0] == '[')
                    sup_name = name + sub_name;
                extracted.emplace_back(sup_name, std::move(sub_term));
            }
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

    term_names = short_parameter_names(term_names);
}
