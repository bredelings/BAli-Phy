#include <vector>
#include <set>
#include "rules.H"
#include "util/myexception.H"
#include "util/string/join.H"
#include "util/io.H"
#include "util/json.hh"
#include "models/compile.H"
#include "models/parse.H" // for is_constant( )
#include "models/driver.hh" // for parse_expression( )

#include "computation/module.H"
#include "computation/typecheck/typecheck.H"

using std::vector;
using std::set;
using std::map;
using std::string;
using std::optional;
namespace fs = std::filesystem;

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

vector<RuleConstraint> parse_constraints(const ptree& cc)
{
    vector<RuleConstraint> constraints;
    for(auto& c: cc)
    {
	string s = c.second.get_value<string>();
	constraints.push_back(parse_type(s));
    }
    return constraints;
}

ptree parse_value(ptree call)
{
    auto s = call.get_value<string>();
    if (s == "[]")
	return ptree("[]");
    else
	// parse_type still uses [].  its not just for types.
	return parse_expression(call.get_value<string>(),"call");
}


vector<string> get_string_array(const ptree& rule, const string& key, const string& rule_name)
{
    vector<string> result;
    if (auto p = rule.get_child_optional(key))
    {
        if (not p->value_is_empty())
            throw myexception()<<"In rule for "<<rule_name<<": \""<<key<<"\" must be an array";
        for(auto& [_, x]: *p)
        {
            if (not x.is_a<string>())
                throw myexception()<<"In rule for "<<rule_name<<": entry in \""<<key<<"\" is not a string";
            result.push_back(x.get_value<string>());
        }
    }
    return result;
}

std::set<string> get_imports(const ptree& rule, const string& rule_name)
{
    std::set<string> result;
    if (auto p = rule.get_child_optional("import"))
    {
        if (not p->value_is_empty())
            throw myexception()<<"In rule for "<<rule_name<<": \"import\" must be an array";
        for(auto& [_, x]: *p)
        {
            if (not x.is_a<string>())
                throw myexception()<<"In rule for "<<rule_name<<": entry in \"import\" is not a string";
            result.insert(x.get_value<string>());
        }
    }
    return result;
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
Rule convert_rule(const Rules& R, const string& name, const ptree& raw_rule)
{
    Rule rule;
    rule.name = name;

    {
	auto result_type = raw_rule.get_child("result_type");
        if (not result_type.has_value<string>())
            throw myexception()<<"In rules for '"<<name<<"', result_type is not a string.";
	rule.result_type = parse_type(result_type.get_value<string>());
    }

    {
	ptree constraints;
	if (auto constraints_ = raw_rule.get_child_optional("constraints"))
            constraints = *constraints_;
	rule.constraints = parse_constraints(constraints);
    }

    {
	auto call = raw_rule.get_child("call");
	rule.call = parse_value(call);
    }

    for(auto& [_,x]: raw_rule.get_child("args"))
    {
        string arg_name = x.get_child("name").get_value<string>();
        RuleArg arg;
        arg.name = arg_name;

	{
	    auto arg_type = x.get_child("type");

	    if (not arg_type.has_value<string>())
		throw myexception()<<"In rules for '"<<name<<"', type for argument '"<<arg_name<<"' is not a string.";

	    arg.type = parse_type(arg_type.get_value<string>());
	}

	if (auto default_value = x.get_child_optional("default_value"))
	{
	    if (not default_value->has_value<string>())
		throw myexception()<<"In rules for '"<<name<<"', default value for argument '"<<arg_name<<"' is not a string.";

	    arg.default_value = parse(R, default_value->get_value<string>(), name + ": default value for '"+arg_name+"'");
	}

	if (auto alphabet = x.get_child_optional("alphabet"))
	{
	    if (not alphabet->has_value<string>())
		throw myexception()<<"In rules for '"<<name<<"', alphabet for argument '"<<arg_name<<"' is not a string.";

	    arg.alphabet = parse(R, alphabet->get_value<string>(), name + ": alphabet for '"+arg_name+"'");
	}

        if (auto description = x.get_optional<string>("description"))
            arg.description = *description;

        rule.args.push_back(std::move(arg));
    }

    // Handle optional element "computed".
    if (auto computed = raw_rule.get_child_optional("computed"))
    {
	for(auto& [_,x]: *computed)
	{
            ComputedRule c;
            c.name = x.get_child("name").get_value<string>();
	    c.value = parse_value(x.get_child("value"));
            rule.computed.push_back(std::move(c));
	}
    }

    rule.imports = get_imports(raw_rule, name);
    rule.no_log = raw_rule.get("no_log", false);
    rule.perform = raw_rule.get("perform", false);
    if (auto extract = raw_rule.get_optional<string>("extract"))
        rule.extract = *extract;

    rule.synonyms = get_string_array(raw_rule, "synonyms", name);
    rule.deprecated_synonyms = get_string_array(raw_rule, "deprecated-synonyms", name);

    if (auto title = raw_rule.get_optional<string>("title"))
        rule.docs.title = *title;
    if (auto description = raw_rule.get_optional<string>("description"))
        rule.docs.description = *description;
    rule.docs.examples = get_string_array(raw_rule, "examples", name);
    rule.docs.see = get_string_array(raw_rule, "see", name);
    if (auto citation = raw_rule.get_child_optional("citation"))
        rule.docs.citation = *citation;
    if (auto category = raw_rule.get_child_optional("category"))
        for(auto& [_, x]: *category)
            rule.docs.category.push_back(x.get_value<string>());

    return rule;
}

Rule make_rule_stub(const string& name, const ptree& raw_rule)
{
    Rule rule;
    rule.name = name;
    for(auto& [_, x]: raw_rule.get_child("args"))
    {
        RuleArg arg;
        arg.name = x.get_child("name").get_value<string>();
        rule.args.push_back(std::move(arg));
    }
    return rule;
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

ptree get_type_for_arg(const Rule& rule, const string& arg)
{
    return get_arg(rule,arg).type;
}

/*
ptree get_type_for_arg(const string& func, const string& arg) const
{
    if (auto rule = get_rule_for_func(func))
	return ::get_type_for_arg(*rule, arg);
    else
	return ptree("?");
}
*/

ptree Rules::get_result_type(const ptree& model_rep) const
{
    if (model_rep.is_a<int>())
	return ptree("Int");
    else if (model_rep.is_a<double>())
	return ptree("Double");
    else if (model_rep.is_a<bool>())
	return ptree("Bool");
    else if (model_rep.is_a<string>() and is_constant(model_rep))
	return ptree("String");

    if (auto rule = get_rule_for_func(model_rep.get_value<string>()))
	return rule->result_type;
    else
	return ptree("?");
}


string show_paths(const vector<fs::path>& paths)
{
    vector<string> spaths;
    for(auto& path: paths)
	spaths.push_back(path.string());
    return join(spaths,":");
}

ptree json_to_ptree(const json::value& j)
{
    ptree p;
    switch(j.kind())
    {
    case json::kind::null:
	break;
	// j.is_boolean()
    case json::kind::bool_:
	p = ptree(j.as_bool());
	break;
    case json::kind::uint64:
	p = ptree((int)j.as_uint64());
	break;
    case json::kind::int64:
    p = ptree((int)j.as_int64());
	break;
    case json::kind::double_:
	p = ptree(j.as_double());
	break;
    case json::kind::string:
	p = ptree(string(j.as_string()));
	break;
	// j.is_array()
    case json::kind::array:
	for(auto& x: j.as_array())
	    p.push_back({"", json_to_ptree(x)});
	break;
    case json::kind::object:
	for(auto& [key,value]: j.as_object())
	    p.push_back({key, json_to_ptree(value)});
	break;
    default:
	break;
    }
    return p;
}

void Rules::add_rule_json(const fs::path& path, const fs::path& rel_path)
{
    checked_ifstream infile(path, "function file");

    ptree rule;
    try {
	json::value j;
	infile>>j;
	json::array category;
	for(const auto& s:rel_path)
	    category.push_back(json::string(s.string()));
	j.as_object()["category"] = category;
	rule = json_to_ptree(j);
    }
    catch (const std::exception& e)
    {
	throw myexception()<<"Error parsing JSON function description "<<path<<"\n:  "<<e.what();
    }
    catch (...)
    {
	throw myexception()<<"Error parsing JSON function description "<<path<<"\n";
    }

    string name = rule.get<string>("name");

    if (raw_rules.count(name))
	std::cerr<<"Warning: ignoring additional definition of function '"<<name<<"' from file '"<<path<<"'\n";
    else
	raw_rules[name] = rule;

    if (auto syn = rule.get_child_optional("synonyms"))
    {
	if (not syn->value_is_empty())
	    throw myexception()<<"In rule for "<<*rule.get_optional<string>("name")<<": \"synonyms\" must be an array";
    }

    if (auto syn = rule.get_child_optional("deprecated-synonyms"))
    {
	if (not syn->value_is_empty())
	    throw myexception()<<"In rule for "<<*rule.get_optional<string>("name")<<": \"deprecated-synonyms\" must be an array";
    }

    if (auto synonyms = rule.get_child_optional("synonyms"))
    {
	for(auto& synonym_pair: *synonyms)
	{
	    if (not synonym_pair.second.is_a<string>())
		throw myexception()<<"Synonym for rule '"<<name<<"' is not a string!";
	    auto synonym = (string)synonym_pair.second;
	    if (not raw_rules.count(synonym) and not this->synonyms.count(synonym))
		this->synonyms[synonym] = name;
	}
    }

    if (auto synonyms = rule.get_child_optional("deprecated-synonyms"))
    {
	for(auto& synonym_pair: *synonyms)
	{
	    if (not synonym_pair.second.is_a<string>())
		throw myexception()<<"Deprecated synonym for rule '"<<name<<"' is not a string!";
	    auto synonym = (string)synonym_pair.second;
	    if (not raw_rules.count(synonym) and not this->synonyms.count(synonym) and not deprecated_synonyms.count(synonym))
		deprecated_synonyms[synonym] = name;
	}
    }
}

Rules::Rules(const vector<fs::path>& pl)
{
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
		add_rule_json(abs_path, rel_path.parent_path());
	    }
	}
    }

    // 3. Seed the rules map so that parsing default values can still resolve
    // positional arguments for rules that have not been fully converted yet.
    for(auto& [name, raw_rule]: raw_rules)
	rules[name] = make_rule_stub(name, raw_rule);

    // 4. Convert the rules - FIXME: should we convert default args in a later step?
    for(auto& [name, raw_rule]: raw_rules)
	rules[name] = convert_rule(*this, name, raw_rule);
}
