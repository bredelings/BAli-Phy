#include <vector>
#include <set>
#include "rules.H"
#include "util/myexception.H"
#include "util/string/join.H"
#include "util/io.H"
#include "util/json.hh"
#include "models/compile.H"
#include "models/haskell-binding-contexts.H"
#include "models/haskell-type-to-model-type.H"
#include "models/parse.H"
#include "models/driver.hh" // for parse_expression( )
#include "models/rule-call-inference.H"

#include "computation/module.H"
#include "computation/typecheck/typecheck.H"

#include <memory>

using std::vector;
using std::set;
using std::map;
using std::string;
using std::optional;
namespace fs = std::filesystem;

RuleModelExpr parse_rule_template_expr(const string& text, const string& what);

namespace
{

struct RawRule
{
    string name;
    json::object fields;
};

struct RuleSignature
{
    ParsedType result_type;
    vector<RuleConstraint> constraints;
    map<string, ParsedType> arg_types;
    optional<RuleHaskellSignature> haskell_signature;
    optional<RuleHaskellCallAnalysis> haskell_call_analysis;
};

enum class RuleSignatureMode
{
    Explicit,
    Inferred
};

// Looks up a raw JSON field without creating it, so validation can distinguish
// absent fields from fields with the wrong type.
const json::value* maybe_field(const json::object& object, const string& key)
{
    return object.if_contains(key);
}

// Reads a required JSON string field and reports the rule currently being
// loaded in type errors.
string required_string(const json::object& object, const string& key, const string& rule_name)
{
    auto value = maybe_field(object, key);
    if (not value or not value->is_string())
        throw myexception()<<"In rules for '"<<rule_name<<"', "<<key<<" is not a string.";
    return string(value->as_string());
}

// Reads an optional JSON string field and rejects non-string values, matching
// the strict checks used for required string fields.
optional<string> optional_string(const json::object& object, const string& key, const string& rule_name)
{
    auto value = maybe_field(object, key);
    if (not value)
        return {};
    if (not value->is_string())
        throw myexception()<<"In rules for '"<<rule_name<<"', "<<key<<" is not a string.";
    return string(value->as_string());
}

// Reads a boolean field with a default while rejecting non-boolean JSON values.
bool optional_bool(const json::object& object, const string& key, bool default_value, const string& rule_name)
{
    auto value = maybe_field(object, key);
    if (not value)
        return default_value;
    if (not value->is_bool())
        throw myexception()<<"In rules for '"<<rule_name<<"', "<<key<<" is not a boolean.";
    return value->as_bool();
}

// Reads an optional array field and produces rule-qualified error messages.
const json::array* optional_array(const json::object& object, const string& key, const string& rule_name)
{
    auto value = maybe_field(object, key);
    if (not value)
        return nullptr;
    if (not value->is_array())
        throw myexception()<<"In rule for "<<rule_name<<": \""<<key<<"\" must be an array";
    return &value->as_array();
}

const json::array* optional_args_array(const json::object& object, const string& rule_name)
{
    auto value = maybe_field(object, "args");
    if (not value)
        throw myexception()<<"In rule for "<<rule_name<<": \"args\" is missing";
    if (value->is_array())
        return &value->as_array();
    throw myexception()<<"In rule for "<<rule_name<<": \"args\" must be an array";
}

// Checks the raw constraints field before signature-mode validation so malformed
// JSON keeps the existing field-specific error instead of becoming a mixed-mode error.
void validate_constraints_field(const json::object& object, const string& rule_name)
{
    auto value = maybe_field(object, "constraints");
    if (value and not value->is_array())
        throw myexception()<<"In rule for "<<rule_name<<": \"constraints\" must be an array";
}

// Classifies a raw binding signature without converting type strings, enforcing
// the explicit/inferred split before partially typed rules can enter conversion.
RuleSignatureMode classify_signature_mode(const json::object& object, const string& rule_name)
{
    validate_constraints_field(object, rule_name);

    bool has_result_type = maybe_field(object, "result_type");
    bool has_constraints = maybe_field(object, "constraints");
    bool any_arg_has_type = false;
    bool any_arg_omits_type = false;
    vector<string> args_with_type;
    vector<string> args_without_type;

    auto args = optional_args_array(object, rule_name);
    for(auto& value: *args)
    {
        if (not value.is_object())
            throw myexception()<<"In rule for "<<rule_name<<": entry in \"args\" is not an object";
        const auto& arg_object = value.as_object();
        auto arg_name = optional_string(arg_object, "name", rule_name).value_or("<unnamed>");
        if (maybe_field(arg_object, "type"))
        {
            any_arg_has_type = true;
            args_with_type.push_back(arg_name);
        }
        else
        {
            any_arg_omits_type = true;
            args_without_type.push_back(arg_name);
        }
    }

    if (has_result_type and not any_arg_omits_type)
        return RuleSignatureMode::Explicit;

    if (not has_result_type and not any_arg_has_type and not has_constraints)
        return RuleSignatureMode::Inferred;

    myexception error;
    error<<"In rule for "<<rule_name<<": mixed signature mode; ";
    if (has_result_type)
        error<<"\"result_type\" is present";
    else
        error<<"\"result_type\" is absent";
    if (has_constraints)
        error<<", \"constraints\" is present";
    if (not args_with_type.empty())
        error<<", args with \"type\": "<<join(args_with_type, ", ");
    if (not args_without_type.empty())
        error<<", args without \"type\": "<<join(args_without_type, ", ");
    error<<". Use either a full explicit signature or omit all signature fields for inferred mode.";
    throw error;
}

// Reads an optional array of strings from raw rule JSON.
vector<string> string_array(const json::object& object, const string& key, const string& rule_name)
{
    vector<string> result;
    if (auto array = optional_array(object, key, rule_name))
    {
        for(auto& value: *array)
        {
            if (not value.is_string())
                throw myexception()<<"In rule for "<<rule_name<<": entry in \""<<key<<"\" is not a string";
            result.push_back(string(value.as_string()));
        }
    }
    return result;
}

// Reads an optional array of strings into a set for module imports.
std::set<string> import_set(const json::object& object, const string& rule_name)
{
    auto imports = string_array(object, "import", rule_name);
    return {imports.begin(), imports.end()};
}

// Converts raw type constraint strings into native command-line model types.
vector<RuleConstraint> parse_constraints(const json::object& object, const string& rule_name)
{
    vector<RuleConstraint> constraints;
    auto value = maybe_field(object, "constraints");
    if (not value)
        return constraints;
    if (not value->is_array())
        throw myexception()<<"In rule for "<<rule_name<<": \"constraints\" must be an array";

    for(auto& constraint: value->as_array())
    {
        if (not constraint.is_string())
            throw myexception()<<"In rule for "<<rule_name<<": entry in \"constraints\" is not a string";
        constraints.push_back(parse_type(string(constraint.as_string())));
    }
    return constraints;
}

// Parses the complete JSON signature for explicit-mode rules into concrete
// command-line model types.
RuleSignature parse_explicit_signature(const json::object& object, const string& rule_name)
{
    RuleSignature signature;
    signature.result_type = parse_type(required_string(object, "result_type", rule_name));
    signature.constraints = parse_constraints(object, rule_name);

    auto args = optional_args_array(object, rule_name);
    for(auto& value: *args)
    {
        if (not value.is_object())
            throw myexception()<<"In rule for "<<rule_name<<": entry in \"args\" is not an object";
        const auto& arg_object = value.as_object();
        auto arg_name = required_string(arg_object, "name", rule_name);
        signature.arg_types.insert({arg_name, parse_type(required_string(arg_object, "type", rule_name))});
    }
    return signature;
}

// Builds the narrow request object used by Haskell call-template signature
// inference, without constructing a partially initialized Rule.
RuleCallAnalysisInput make_rule_call_analysis_input(const RawRule& raw_rule)
{
    RuleCallAnalysisInput input;
    const auto& name = raw_rule.name;
    const auto& fields = raw_rule.fields;
    input.name = name;
    input.call = parse_rule_template_expr(required_string(fields, "call", name), name + ": call");
    input.imports = import_set(fields, name);

    auto args = optional_args_array(fields, name);
    for(auto& x: *args)
    {
        if (not x.is_object())
            throw myexception()<<"In rule for "<<name<<": entry in \"args\" is not an object";
        const auto& arg_object = x.as_object();
        RuleCallAnalysisArg arg;
        arg.name = required_string(arg_object, "name", name);
        arg.default_value_source = optional_string(arg_object, "default_value", name);
        arg.alphabet_source = optional_string(arg_object, "alphabet", name);
        input.args.push_back(std::move(arg));
    }
    return input;
}

// Copies the semantic inference result into the durable Rule signature shape
// used outside the inference helper.
RuleHaskellSignature make_rule_haskell_signature(const InferredRuleSignature& inferred)
{
    return RuleHaskellSignature{inferred.quantified_vars, inferred.result_type, inferred.arg_types, inferred.constraints};
}

// Converts best-effort Haskell call analysis into durable Rule data, dropping
// the lowered Haskell expression because it belongs to the transient module.
RuleHaskellCallAnalysis make_rule_haskell_call_analysis(RuleCallAnalysis analysis)
{
    RuleHaskellCallAnalysis result;
    result.resolved_symbols = std::move(analysis.resolved_symbols);
    result.referenced_args = std::move(analysis.referenced_args);
    if (analysis.signature)
        result.inferred_call_signature = make_rule_haskell_signature(*analysis.signature);
    result.context_error = {};
    result.resolution_error = std::move(analysis.resolution_error);
    result.inference_error = std::move(analysis.inference_error);
    return result;
}

// Records that explicit Haskell call analysis could not create the import
// context, while leaving the explicit JSON signature usable.
RuleHaskellCallAnalysis make_rule_haskell_call_context_error(std::string error)
{
    RuleHaskellCallAnalysis result;
    result.context_error = std::move(error);
    return result;
}

// Converts an inferred semantic Haskell signature into the concrete model
// signature stored on Rule objects, retaining the semantic signature as source data.
RuleSignature bridge_inferred_signature(const RuleCallAnalysisInput& input, const InferredRuleSignature& inferred)
{
    HaskellTypeBridgeState bridge_state;
    seed_haskell_type_bridge_vars(bridge_state, inferred.quantified_vars);
    RuleSignature signature;
    signature.haskell_signature = make_rule_haskell_signature(inferred);
    signature.result_type = bridge_haskell_type_to_model_type(inferred.result_type, bridge_state);

    for(const auto& arg: input.args)
    {
        auto type = inferred.arg_types.find(arg.name);
        if (type == inferred.arg_types.end())
            throw myexception()<<"In rule for "<<input.name<<": no inferred type for argument '"<<arg.name<<"'";
        signature.arg_types.insert({arg.name, bridge_haskell_type_to_model_type(type->second, bridge_state)});
    }
    // Compatibility output: inferred Haskell predicates are retained above, while
    // the current model typechecker still consumes CM::Type constraints.
    for(const auto& constraint: inferred.constraints)
        signature.constraints.push_back(bridge_haskell_constraint_to_model_constraint(constraint, bridge_state));
    return signature;
}

// Probes explicit-rule import sets independently so one plugin/import failure
// can be recorded without disabling analysis for unrelated explicit rules.
std::map<std::set<std::string>, std::string> explicit_context_errors_for(const std::shared_ptr<module_loader>& loader, const std::map<std::set<std::string>, BindingImportSet>& import_sets)
{
    std::map<std::set<std::string>, std::string> errors;
    for(const auto& [key, imports]: import_sets)
    {
        try
        {
            (void)HaskellBindingContexts::build(loader, {imports});
        }
        catch(const std::exception& e)
        {
            errors.insert({key, e.what()});
        }
    }
    return errors;
}

// Parses citation metadata into a native shape used by the help renderer.
RuleCitation parse_citation(const json::value& value, const string& rule_name)
{
    RuleCitation citation;
    if (value.is_string())
    {
        citation.text = string(value.as_string());
        return citation;
    }
    if (not value.is_object())
        throw myexception()<<"In rule for "<<rule_name<<": \"citation\" must be a string or an object";

    const auto& object = value.as_object();
    citation.title = optional_string(object, "title", rule_name);
    citation.year = optional_string(object, "year", rule_name);

    if (auto authors = optional_array(object, "author", rule_name))
        for(auto& author_value: *authors)
        {
            if (not author_value.is_object())
                throw myexception()<<"In rule for "<<rule_name<<": entry in \"author\" is not an object";
            if (auto name = optional_string(author_value.as_object(), "name", rule_name))
                citation.authors.push_back({*name});
        }

    if (auto identifiers = optional_array(object, "identifier", rule_name))
        for(auto& identifier_value: *identifiers)
        {
            if (not identifier_value.is_object())
                throw myexception()<<"In rule for "<<rule_name<<": entry in \"identifier\" is not an object";
            const auto& identifier = identifier_value.as_object();
            auto type = optional_string(identifier, "type", rule_name);
            auto id = optional_string(identifier, "id", rule_name);
            if (type and id)
                citation.identifiers.push_back({*type, *id});
        }

    if (auto links = optional_array(object, "link", rule_name))
        for(auto& link_value: *links)
        {
            if (not link_value.is_object())
                throw myexception()<<"In rule for "<<rule_name<<": entry in \"link\" is not an object";
            const auto& link = link_value.as_object();
            if (auto url = optional_string(link, "url", rule_name))
                citation.links.push_back({*url, optional_string(link, "anchor", rule_name)});
        }

    return citation;
}

// Loads one binding JSON file, injects its directory-derived category, and
// returns raw rule data for the normal rule conversion step.
RawRule load_rule_json(const fs::path& path, const fs::path& rel_path)
{
    checked_ifstream infile(path, "function file");

    try {
        json::value j;
        infile>>j;
        if (not j.is_object())
            throw myexception()<<"Top-level JSON value is not an object.";

        auto rule = j.as_object();
        json::array category;
        for(const auto& s: rel_path)
            category.push_back(json::string(s.string()));
        rule["category"] = category;

        auto name = required_string(rule, "name", path.string());
        return {name, std::move(rule)};
    }
    catch (const std::exception& e)
    {
        throw myexception()<<"Error parsing JSON function description "<<path<<"\n:  "<<e.what();
    }
    catch (...)
    {
        throw myexception()<<"Error parsing JSON function description "<<path<<"\n";
    }
}

}

// TODO: reject HKY+HKY -- reduce constraints.
//       reject HKY model with amino acid data.
//       reject M3[LG,F1x4]

// TODO: decrease memory usage for pairwise alignments.
//       - make pairwise alignments represent the relevant bits without translation (e.g. make convert_to_bits a no-op)
//       - make pairwise contain two dynamic_bitsets

// TODO: decrease memory usage for HMM::bitmask_t from 64 bits to 8 bits.

// TODO: try implementing a horseshoe model for f and s

// TODO: complain when setting calculator=SEV and imodel!=none

// TODO: devirtualize pool::allocate

// TODO: implement iteration-based color for 3D MDS plot.

// TODO: reduce memory:
//       - toss identifiers, and make them not heads.
//       - further reduce memory for pairwise alignments.

// TODO: move some things into a computation/machine directory
//       separate parsing and model-creation code.
//       move parsing code into computation directory

// TODO: speed up likelihood code by caching/indexing on (node,index)
//       - caching based on (node,index) means that we'd have to expose (node,index), which is odd.
//       - can we internally cache the edges_before_edge computation?
//       - how about making branch lookup constant, and changing the prev/next fields?

// QUESTION: how do we think about indexing on the root instead of just tracking the execution graph?
//           how does this relate to factorial n = case n or n' => factorial n'?

// TODO: full laziness transformation.  <- WE ARE HERE.
//       do simplification before and after full laziness
//       case-merging
//       generate un-optimized code for cases (e.g. def_function)
//       handle x@(y:ys) and guards in case statements.
//       when a depends on b, fully simplify b before trying to simplify a. (split modules into a LIST of topdecls)
//       unpack_cstring (AFTER splitting up dependences in modules)
//       allow reasonable exporting, to break up SModel.hs into submodules that are exported from it.
//       READ SANTOS THESIS.
//       Q: why is floating-inwards necessary?  This seems to substitute for more intelligent analyses...
//       get optimization examples from the thesis?
//       allow case a+x of v -> E to put v EITHER into a closure OR on a stack.

// TODO: add covarion and CAT10 and CAT20 models.

//--- Up to here, just do it.

// TODO: fix compilation with recent boost.

// TODO: move logging, scale_factor, prefixing, etc. out of models.

// TODO: make a function that generates a JSON object (or property tree) object in order to log things.
//       - maybe make things like frequencies be implemented as a Map [(String,Double)]
//       - maybe just make a specialized logger and/or reader that treats a [String] and a [Double] as map from String->Double.
//         * make an automatic conversion rule so we can supply e.g. {A:0.1,T:0.2,C:0.3,G:0.4} and get the numbers in the right order,
//           and also a way of logging them in the right order?
//       - can we handle e.g. {A:log[0.1],T:1.0-Log[0.1]} ?

// TODO: Add a "scope" construct to haskell to handle things that shouldn't be moved out of scope?

// TODO: make sub-partitions: split 0/1 partitions under Mk or Mkv into sub-partitions where alphabet size = character size.

// HARD TODO: missing complete genes? (or document why not).

// HARD TODO: --help implicit value?
//   Also, say which parameter and function if we have an argument type mismatch.

// TODO: * Add 01 alphabets.
//       * HARD: Allow loading 01234 character data -- probably requires sub-partitions.
//       * Condition on columns not being invariant.
//         - this only makes sense for constant alignments.
//         - perhaps each column should kind of be a separate partition.
//         - this isn't SEQUENCE data.
//         - but manuscripts ARE sequences
//         - should we handle insertions & deletions that way?
//       * Q: how to add the extra term?

// TODO: Q: Relatedly, how much slowdown does the alignment prior multiplication tree cause?

// TODO: change Scale and *T (branch lengths) into an array, and log them that way.

// TODO: rewrite tree reader/writer functions to use lambdas.

// ? TODO: clean up loggers.{H,C} to use lambda functions
// ? TODO: clean up transition kernels to use lambda functions?
// TODO: find some way to run under the prior?
// TODO: rewrite frequencies_prior..

// Parses binding model expressions through the command-line model parser and
// normalizes positional arguments over CmdModel.
RuleModelExpr parse_rule_model_expr(const Rules& R, const string& text, const string& what)
{
    return parse_model_expr(R, text, what);
}

// Compatibility boundary: rule templates still share the model parser syntax.
// Remove when templates get their own parser or spelling.
RuleModelExpr parse_rule_template_expr(const string& text, const string& what)
{
    return parse_expression(text, what);
}

/* NOTE: convert_rule parses and processes strings.  It converts:

   - "result_type" -> type
   - "arg_type" -> type
   - "constraints" -> type

   - "call" -> expression
   - "default_value" -> expression -> fill in default values
   - "alphabet" -> expression -> fill in default values
   - "computed" -> expression
*/
Rule convert_rule(const Rules& R, const RawRule& raw_rule, const RuleSignature& signature)
{
    Rule rule;
    const auto& fields = raw_rule.fields;
    const auto& name = raw_rule.name;
    rule.name = name;

    rule.result_type = signature.result_type;
    rule.constraints = signature.constraints;
    rule.haskell_signature = signature.haskell_signature;
    rule.haskell_call_analysis = signature.haskell_call_analysis;

    {
        rule.call = parse_rule_template_expr(required_string(fields, "call", name), name + ": call");
    }

    if (auto args = optional_args_array(fields, name))
    {
        for(auto& x: *args)
        {
            if (not x.is_object())
                throw myexception()<<"In rule for "<<name<<": entry in \"args\" is not an object";
            const auto& arg_object = x.as_object();
            string arg_name = required_string(arg_object, "name", name);
            RuleArg arg;
            arg.name = arg_name;

            auto arg_type = signature.arg_types.find(arg_name);
            if (arg_type == signature.arg_types.end())
                throw myexception()<<"In rule for "<<name<<": no resolved type for argument '"<<arg_name<<"'";
            arg.type = arg_type->second;

            if (auto default_value = optional_string(arg_object, "default_value", name))
                arg.default_value = parse_rule_model_expr(R, *default_value, name + ": default value for '"+arg_name+"'");

            if (auto alphabet = optional_string(arg_object, "alphabet", name))
                arg.alphabet = parse_rule_model_expr(R, *alphabet, name + ": alphabet for '"+arg_name+"'");

            arg.description = optional_string(arg_object, "description", name);

            rule.args.push_back(std::move(arg));
        }
    }

    // Handle optional element "computed".
    if (auto computed = optional_array(fields, "computed", name))
    {
	for(auto& x: *computed)
	{
            if (not x.is_object())
                throw myexception()<<"In rule for "<<name<<": entry in \"computed\" is not an object";
            const auto& computed_object = x.as_object();
            ComputedRule c;
            c.name = required_string(computed_object, "name", name);
	    c.value = parse_rule_template_expr(required_string(computed_object, "value", name), name + ": computed value for '"+c.name+"'");
            rule.computed.push_back(std::move(c));
	}
    }

    rule.imports = import_set(fields, name);
    rule.no_log = optional_bool(fields, "no_log", false, name);
    rule.perform = optional_bool(fields, "perform", false, name);
    rule.extract = optional_string(fields, "extract", name);

    rule.synonyms = string_array(fields, "synonyms", name);
    rule.deprecated_synonyms = string_array(fields, "deprecated-synonyms", name);

    rule.docs.title = optional_string(fields, "title", name);
    rule.docs.description = optional_string(fields, "description", name);
    rule.docs.examples = string_array(fields, "examples", name);
    rule.docs.see = string_array(fields, "see", name);
    if (auto citation = maybe_field(fields, "citation"))
        rule.docs.citation = parse_citation(*citation, name);
    rule.docs.category = string_array(fields, "category", name);

    return rule;
}

// Builds a lightweight rule entry so default-value parsing can resolve local
// rule names before the full rule conversion pass has run.
Rule make_rule_stub(const RawRule& raw_rule)
{
    Rule rule;
    const auto& name = raw_rule.name;
    const auto& fields = raw_rule.fields;
    rule.name = name;
    if (auto args = optional_args_array(fields, name))
    {
        for(auto& x: *args)
        {
            if (not x.is_object())
                throw myexception()<<"In rule for "<<name<<": entry in \"args\" is not an object";
            RuleArg arg;
            arg.name = required_string(x.as_object(), "name", name);
            rule.args.push_back(std::move(arg));
        }
    }
    return rule;
}

// Resolves explicit JSON signatures and inferred Haskell signatures into one
// map keyed by rule name for the later conversion pass.
std::map<std::string, RuleSignature> resolve_rule_signatures(const map<std::string, RawRule>& raw_rules, const std::shared_ptr<module_loader>& loader)
{
    std::map<std::string, RuleSignature> signatures;
    std::map<std::string, RuleCallAnalysisInput> explicit_inputs;
    std::map<std::string, RuleCallAnalysisInput> inferred_inputs;
    vector<BindingImportSet> inferred_import_sets;

    for(auto& [name, raw_rule]: raw_rules)
    {
        auto mode = classify_signature_mode(raw_rule.fields, raw_rule.name);
        auto input = make_rule_call_analysis_input(raw_rule);
        if (mode == RuleSignatureMode::Explicit)
        {
            signatures.insert({name, parse_explicit_signature(raw_rule.fields, raw_rule.name)});
            explicit_inputs.insert({name, std::move(input)});
        }
        else
        {
            inferred_import_sets.push_back({input.imports});
            inferred_inputs.insert({name, std::move(input)});
        }
    }

    if (not loader)
        throw myexception()<<"Inferred signature mode requires a Haskell module loader";

    std::map<std::set<std::string>, BindingImportSet> explicit_import_sets;
    for(auto& [name, input]: explicit_inputs)
    {
        auto normalized_imports = normalize_binding_imports({input.imports});
        explicit_import_sets.insert({normalized_imports.modules, BindingImportSet{input.imports}});
    }

    auto explicit_context_errors = explicit_context_errors_for(loader, explicit_import_sets);
    vector<BindingImportSet> explicit_good_imports;
    for(const auto& [key, imports]: explicit_import_sets)
        if (not explicit_context_errors.count(key))
            explicit_good_imports.push_back(imports);

    optional<HaskellBindingContexts> explicit_contexts;
    if (not explicit_good_imports.empty())
    {
        try
        {
            explicit_contexts = HaskellBindingContexts::build(loader, explicit_good_imports);
        }
        catch(const std::exception& e)
        {
            for(const auto& imports: explicit_good_imports)
                explicit_context_errors.insert({normalize_binding_imports(imports).modules, e.what()});
        }
    }

    for(auto& [name, input]: explicit_inputs)
    {
        auto normalized_imports = normalize_binding_imports({input.imports});
        const auto& import_key = normalized_imports.modules;
        if (auto error = explicit_context_errors.find(import_key); error != explicit_context_errors.end())
            signatures.at(name).haskell_call_analysis = make_rule_haskell_call_context_error(error->second);
        else
        {
            // Temporary limitation: broad explicit-rule loading records
            // resolution only until expected-type checking replaces raw inference.
            signatures.at(name).haskell_call_analysis = make_rule_haskell_call_analysis(analyze_rule_call_resolution(*explicit_contexts, input));
        }
    }

    if (inferred_inputs.empty())
        return signatures;

    auto contexts = HaskellBindingContexts::build(loader, inferred_import_sets);
    for(auto& [name, input]: inferred_inputs)
    {
        InferredRuleSignature inferred;
        try
        {
            inferred = infer_rule_call_signature(contexts, input);
        }
        catch(const std::exception& e)
        {
            throw myexception()<<"In rule for "<<input.name<<": Haskell signature inference failed: "<<e.what();
        }

        try
        {
            signatures.insert({name, bridge_inferred_signature(input, inferred)});
        }
        catch(const std::exception& e)
        {
            throw myexception()<<"In rule for "<<input.name<<": Haskell-to-model compatibility bridge failed: "<<e.what();
        }
    }
    return signatures;
}

const map<std::string, Rule>& Rules::get_rules() const
{
    return rules;
}

optional<Rule> Rules::get_rule_for_func(const string& s) const
{
    auto it = rules.find(s);
    if (it != rules.end())
	return it->second;

    if (auto syn = synonyms.find(s); syn != synonyms.end())
	return get_rule_for_func(syn->second);

    if (auto syn = deprecated_synonyms.find(s); syn != deprecated_synonyms.end())
	throw myexception()<<"I don't recognize '"<<s<<"'.  Perhaps you meant '"<<syn->second<<"'?";

    return {};
}

Rule Rules::require_rule_for_func(const string& s) const
{
    if (auto rule = get_rule_for_func(s))
	return *rule;
    else
	throw myexception()<<"No function '"<<s<<"'.";
}

std::optional<std::size_t> Rule::arg_index(const string& arg_name) const
{
    for(std::size_t i=0; i<args.size(); i++)
        if (args[i].name == arg_name)
            return i;
    return {};
}

const RuleArg* Rule::maybe_arg(const string& arg_name) const
{
    if (auto i = arg_index(arg_name))
        return &args[*i];
    return nullptr;
}

RuleArg* Rule::maybe_arg(const string& arg_name)
{
    if (auto i = arg_index(arg_name))
        return &args[*i];
    return nullptr;
}

const RuleArg& Rule::require_arg(const string& arg_name) const
{
    if (auto arg = maybe_arg(arg_name))
        return *arg;
    throw myexception()<<"Rule for function '"<<name<<"' has no argument '"<<arg_name<<"'";
}

string Rule::keyword_for_positional_arg(std::size_t i) const
{
    if (i >= args.size())
	throw myexception()<<"Trying to access positional arg "<<i+1<<" for '"<<name<<"', which only has "<<args.size()<<" positional arguments.";

    return args[i].name;
}

const RuleArg* maybe_get_arg(const Rule& rule, const string& arg_name)
{
    return rule.maybe_arg(arg_name);
}

const RuleArg& get_arg(const Rule& rule, const string& arg_name)
{
    return rule.require_arg(arg_name);
}

string get_keyword_for_positional_arg(const Rule& rule, int i)
{
    return rule.keyword_for_positional_arg(i);
}

CM::Type get_type_for_arg(const Rule& rule, const string& arg)
{
    return get_arg(rule,arg).type;
}

string show_paths(const vector<fs::path>& paths)
{
    vector<string> spaths;
    for(auto& path: paths)
	spaths.push_back(path.string());
    return join(spaths,":");
}

// Loads binding JSON files and resolves every rule signature before parsing
// defaults, using the required Haskell loader for signature resolution.
Rules::Rules(const vector<fs::path>& pl, const std::shared_ptr<module_loader>& loader)
{
    if (not loader)
        throw myexception()<<"Rules construction requires a Haskell module loader";

    std::map<std::string, RawRule> raw_rules;

    // 1. Only keep paths that have a /functions/ subdir
    for(auto& path: pl)
    {
	auto fpath = path / "bindings";
	if (fs::exists(fpath))
	    path_list.push_back(fpath);
    }

    // 2. Find all the files in the rules directory that end with '.json'
    for(auto& path: path_list)
    {
	assert(fs::exists(path));

	for(auto& dir_entry: fs::recursive_directory_iterator(path))
	{
	    auto abs_path = dir_entry.path();
	    if (abs_path.extension() == ".json" and abs_path.filename().string()[0] != '.')
	    {
		auto rel_path = fs::relative(dir_entry.path(), path);
		auto raw_rule = load_rule_json(abs_path, rel_path.parent_path());
		auto name = raw_rule.name;
		auto synonyms = string_array(raw_rule.fields, "synonyms", name);
		auto deprecated = string_array(raw_rule.fields, "deprecated-synonyms", name);

		if (raw_rules.count(name))
		    std::cerr<<"Warning: ignoring additional definition of function '"<<name<<"' from file '"<<abs_path<<"'\n";
		else
		    raw_rules[name] = std::move(raw_rule);

		for(auto& synonym: synonyms)
		    if (not raw_rules.count(synonym) and not this->synonyms.count(synonym))
			this->synonyms[synonym] = name;

		for(auto& synonym: deprecated)
		    if (not raw_rules.count(synonym) and not this->synonyms.count(synonym) and not deprecated_synonyms.count(synonym))
			deprecated_synonyms[synonym] = name;
	    }
	}
    }

    auto signatures = resolve_rule_signatures(raw_rules, loader);

    // 4. Seed the rules map so that parsing default values can still resolve
    // positional arguments for rules that have not been fully converted yet.
    for(auto& [name, raw_rule]: raw_rules)
	rules[name] = make_rule_stub(raw_rule);

    // 5. Convert the rules - FIXME: should we convert default args in a later step?
    for(auto& [name, raw_rule]: raw_rules)
	rules[name] = convert_rule(*this, raw_rule, signatures.at(name));
}
