#ifndef CODE_GEN_H
#define CODE_GEN_H

#include <string>                                   // for string
#include <map>                                      // for map
#include <set>                                      // for set
#include <vector>                                   // for vector
#include <optional>                                 // for optional
#include <utility>                                  // for pair
#include "util/ptree.H"                             // for ptree
#include "models/typecheck.H"
#include "computation/expression/var.H"             // for var
#include "computation/expression/do_block.H"        // for Stmts

#include "logger.H"

class Rules;

struct generated_code_t
{
    std::vector<var> lambda_vars;

    Stmts stmts;
    expression_ref E;
    Loggers loggers;
    bool perform_function = false;

    // Which states were actually used.
    std::set<std::string> used_states;
    std::map<std::string,var> free_vars;

    // Under these circumstances, we will return a monadic action
    bool is_action() const {return perform_function or not stmts.empty();}
    bool has_loggers() const {return not loggers.empty();}

    expression_ref generate() const;

    void log_value(const std::string&, expression_ref, const ptree&);
    void log_sub(const std::string&, const var&, const Loggers&);

    std::string print() const {return generate().print();}
};

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
    std::string func; // context: calling func:arg
    std::string arg;
    std::map<std::string,expression_ref> code_for_arg;

    arg_env_t(const std::string& f, const std::string& a, const std::map<std::string,expression_ref>& c):
        func(f),
        arg(a),
        code_for_arg(c)
    { };
};

struct translation_result_t
{
    generated_code_t code;
    std::set<std::string> imports;
    std::set<std::string> lambda_vars;
    std::set<var> haskell_vars;
};

struct CodeGenState
{
    const Rules* R;
    std::map<std::string,var_info_t> identifiers;
    std::set<var> haskell_vars;
    std::optional<arg_env_t> arg_env;
    std::map<std::string, var> state;

    bool is_random(const ptree&) const;
    bool is_unlogged_random(const ptree& model) const;
    bool should_log(const ptree& model_, const std::string& arg_name) const;

    var get_var(std::string name)
    {
        if (name.empty() or not std::islower(name[0]))
            name = "_"+name;
        var x(name);
        for(int i=2; haskell_vars.count(x); i++)
            x = var(name+"_"+std::to_string(i));
        haskell_vars.insert(x);
        return x;
    }

    void set_state(const std::string& name, const var& x)
    {
        assert(haskell_vars.count(x));
        if (state.count(name))
            state.erase(name);
        state.insert({name,x});
    }

    CodeGenState extend_scope(const std::string& var, const var_info_t& var_info) const;
    CodeGenState& extend_modify_scope(const std::string& var, const var_info_t& var_info);

    translation_result_t get_model_decls(const ptree& model);
    std::optional<translation_result_t> get_variable_model(const ptree& model) const;
    std::optional<translation_result_t> get_model_let(const ptree& model) const;
    std::optional<translation_result_t> get_model_lambda(const ptree& model) const;
    std::optional<translation_result_t> get_model_list(const ptree& model) const;
    std::optional<translation_result_t> get_model_tuple(const ptree& model) const;
    std::optional<translation_result_t> get_model_state(const ptree& model) const;
    translation_result_t get_model_function(const ptree& model) const;
    translation_result_t get_model_as(const ptree& model_rep) const;

    CodeGenState(const Rules& r):R(&r) {}
};

expression_ref simplify_intToDouble(const expression_ref& E);
void perform_action_simplified(Stmts& block, const var& x, const var& log_x, bool is_referenced, expression_ref E, bool is_action, bool has_loggers);
void perform_action_simplified(generated_code_t& block, const var& x, const var& log_x, bool is_referenced, const generated_code_t& code);

#endif
