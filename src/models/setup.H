#ifndef SMODEL_SETUP_H
#define SMODEL_SETUP_H
#include <set>                                      // for set
#include <string>                                   // for string
#include <utility>                                  // for pair
#include <vector>                                   // for vector
#include "computation/expression/expression_ref.H"  // for expression_ref
#include "computation/expression/do_block.H"        // for do_block
#include "sequence/alphabet.H"                      // for alphabet
#include "unification.H"                            // for term_t, less
#include "util/json.hh"                             // for json
#include "util/ptree.H"                             // for ptree
class Rules;

class pretty_model_t
{
public:
    ptree main;
    std::vector<std::string> term_names;
    std::vector<pretty_model_t> terms;

    std::string show(bool = true) const;
    std::string show_main(bool = true) const;
    std::string show_extracted() const;

    pretty_model_t(const ptree& m);
};

struct generated_code_t
{
    std::vector<var> lambda_vars;

    Stmts stmts;
    expression_ref E;
    std::vector<expression_ref> loggers;
    bool perform_function = false;

    // Which states were actually used.
    std::set<std::string> used_states;

    // Under these circumstances, we will return a monadic action
    bool is_action() const {return perform_function or not stmts.empty();}
    bool has_loggers() const {return not loggers.empty();}

    expression_ref generate() const;

    expression_ref generate_top() const;

    void log_value(const std::string&, const expression_ref&);
    void log_sub(const std::string&, const expression_ref&);

    std::string print() const {return generate_top().print();}
};

void perform_action_simplified(Stmts& block, const var& x, const var& log_x, bool is_referenced, expression_ref E, bool is_action, bool has_loggers);
void perform_action_simplified(generated_code_t& block, const var& x, const var& log_x, bool is_referenced, const generated_code_t& code);

void maybe_log(std::vector<expression_ref>& logger_bits,
               const std::string& name,
               const expression_ref& value,
               const expression_ref& subloggers);

class model_t
{
    ptree description;
public:
    std::set<std::string> imports;
    ptree type;
    std::set<term_t> constraints;
    generated_code_t code;

    pretty_model_t pretty_model() const {return {description};};

    std::string show(bool = true) const;

    std::string show_pretty(bool = true) const;
    std::string show_main(bool = true) const;
    std::string show_extracted() const;

    model_t() = default;
    model_t(const ptree&,const std::set<std::string>&, const ptree&, const std::set<term_t>&, const generated_code_t&);
};

void to_json(json& j, const pretty_model_t& m);

model_t get_model(const Rules& R, const std::string& type, const std::string& model,
                  const std::vector<std::pair<std::string,std::string>>& = {},
                  const std::map<std::string,std::pair<std::string,std::string>>& = {}
    );

std::string default_markov_model(const alphabet& a);
#endif
