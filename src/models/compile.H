#ifndef MODELS_COMPILE_H
#define MODELS_COMPILE_H
#include <set>                                      // for set
#include <string>                                   // for string
#include <utility>                                  // for pair
#include <variant>                                  // for variant
#include <vector>                                   // for vector
#include "computation/expression/expression_ref.H"  // for expression_ref
#include "sequence/alphabet.H"                      // for alphabet
#include "unification.H"                            // for type_t
#include "util/json.hh"                             // for json
#include "models/typecheck.H"
#include "models/code-generation.H"

class Rules;

class pretty_model_t
{
public:
    CM::TypedExpr main;
    std::vector<std::string> term_names;
    std::vector<pretty_model_t> terms;

    std::string show(bool = true) const;
    std::string show_main(bool = true) const;
    std::string show_extracted() const;

    pretty_model_t(const CM::TypedExpr& m);
};

bool annotated_term_is_model(const CM::TypedExpr&);
bool bound(const CM::TypedExpr&, const std::set<std::string>&);
bool do_extract(const CM::TypedExpr&, const CM::TypedExpr&, const std::set<std::string>&);
std::vector<std::pair<std::string, CM::TypedExpr>> extract_terms(CM::TypedExpr&, const std::set<std::string>&);

void maybe_log(std::vector<expression_ref>& logger_bits,
               const std::string& name,
               const expression_ref& value,
               const expression_ref& subloggers);

class model_t
{
    using description_t = std::variant<std::monostate, CM::TypedExpr, CM::TypedDecls>;

    description_t description;

    const CM::TypedExpr& expression_description() const;
public:
    std::set<std::string> imports;
    type_t type;
    std::set<type_t> constraints;
    generated_code_t code;
    std::set<std::string> used_args;

    bool empty() const {return std::holds_alternative<std::monostate>(description);}
    pretty_model_t pretty_model() const {return {expression_description()};};

    std::string show(bool = true) const;

    std::string show_pretty(bool = true) const;
    std::string show_main(bool = true) const;
    std::string show_extracted() const;

    model_t() = default;
    model_t(CM::TypedExpr,const std::set<std::string>&, const type_t&, const std::set<type_t>&, const generated_code_t&);
    model_t(CM::TypedDecls,const std::set<std::string>&, const type_t&, const std::set<type_t>&, const generated_code_t&);
};

model_t compile_model(const Rules& R,
		      const TypecheckingState& TC, CodeGenState code_gen_state,
		      type_t required_type,
		      const std::string& model,
		      const std::string& what,
		      const std::vector<std::pair<std::string,type_t>>& = {},
		      const std::map<std::string,std::pair<std::string,type_t>>& = {}
    );

model_t compile_decls(const Rules& R,
		      TypecheckingState& TC, CodeGenState& code_gen_state,
		      const std::string& decls,
		      const std::vector<std::pair<std::string,type_t>>& scope,
		      const std::map<std::string,std::pair<std::string,type_t>>& state);

json::object convert_to_json(const pretty_model_t& m);

TypecheckingState makeTypechecker(const Rules& R,
				  const std::vector<std::pair<std::string,type_t>>& scope,
				  const std::map<std::string,std::pair<std::string,type_t>>& state);

std::string print_equals_function(expression_ref E);

std::string default_markov_model(const alphabet& a);
#endif
