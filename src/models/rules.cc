#include <vector>
#include <set>
#include "rules.H"
#include "myexception.H"
#include "util.H"
#include "io.H"
#include <boost/optional.hpp>
#include "util/json.hh"
#include "models/setup.H"

using std::vector;
using std::set;
using std::map;
using std::string;
using boost::optional;
namespace pt = boost::property_tree;
using pt::ptree;
namespace fs = boost::filesystem;

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
// TODO: change scale_means_only -> scale_all_branches

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

// TODO: allow addition/subtraction?
// ? TODO: clean up loggers.{H,C} to use lambda functions
// ? TODO: clean up transition kernels to use lambda functions?
// TODO: find some way to run under the prior?
// TODO: rewrite frequencies_prior..
// Q: fmutsel version of m3, etc.
// Q: how to share GTR between genes in gtr+gwf?

ptree parse(const Rules&, const string& s);
ptree parse_type(const string& s);

ptree parse_constraints(const ptree& cc)
{
    ptree constraints;
    for(auto& c: cc)
    {
	string s = c.second.get_value<string>();
	constraints.push_back({"", parse_type(s)});
    }
    return constraints;
}

ptree convert_rule(const Rules& R, Rule rule)
{

    {
	ptree& result_type = rule.get_child("result_type");
	result_type = parse_type(result_type.get_value<string>());
    }

    {
	if (not rule.get_child_optional("constraints"))
	    rule.push_back({"constraints",ptree()});
	ptree& constraints = rule.get_child("constraints");
	constraints = parse_constraints(constraints);
    }

    {
	ptree& call = rule.get_child("call");
	auto s = call.get_value<string>();
	if (s == "[]")
	    call = ptree("[]");
	else
	    call = parse_type(call.get_value<string>());
    }

    for(auto& arg_pair: rule.get_child("args"))
    {
	auto& x = arg_pair.second;

	{
	    ptree& arg_type = x.get_child("arg_type");
	    arg_type = parse_type(arg_type.get_value<string>());
	}

	if (auto default_value = x.get_child_optional("default_value"))
	{
	    (*default_value) = parse(R, default_value->get_value<string>());
	}

	if (auto applied_args= x.get_child_optional("applied_args"))
	{
	    (*applied_args) = parse_type(applied_args->get_value<string>());
	}
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
    if (it == rules.end())
	return boost::none;
    return it->second;
}

Rule Rules::require_rule_for_func(const string& s) const
{
    if (auto rule = get_rule_for_func(s))
	return *rule;
    else
	throw myexception()<<"No function '"<<s<<"'.";
}

ptree get_arg(const Rule& rule, const string& arg_name)
{
    for(const auto& arg: rule.get_child("args"))
	if (arg.second.get<string>("arg_name") == arg_name)
	    return arg.second;
    throw myexception()<<"Rule for function '"<<rule.get<string>("name")<<"' has no argument '"<<arg_name<<"'";
    // FIXME give info about function here?
}

string get_keyword_for_positional_arg(const Rule& rule, int i)
{
    const auto arguments = rule.get_child("args");
    auto name = rule.get_child("result_type").get_value<string>();
    if (i > arguments.size())
	throw myexception()<<"Trying to access positional arg "<<i+1<<" for '"<<name<<"', which only has "<<arguments.size()<<" positional arguments.";

    auto it = arguments.begin();
    for(int j=0;j<i;j++)
	it++;
	
    return it->second.get<string>("arg_name");
}

ptree get_type_for_arg(const Rule& rule, const string& arg)
{
    return get_arg(rule,arg).get_child("arg_type");
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

ptree Rules::get_result_type(const string& func) const
{
    if (can_be_converted_to<int>(func)) return ptree("Int");
    if (can_be_converted_to<double>(func)) return ptree("Double");

    if (auto rule = get_rule_for_func(func))
	return rule->get_child("result_type");
    else
	return ptree("?");
}

ptree Rules::get_result_type(const ptree& model_rep) const
{
    return get_result_type(model_rep.get_value<string>());
}


string show_paths(const vector<fs::path>& paths)
{
    vector<string> spaths;
    for(auto& path: paths)
	spaths.push_back(path.string());
    return join(spaths,":");
}

ptree json_to_ptree(const json& j)
{
    ptree p;
    switch(j.type())
    {
    case json::value_t::null:
	break;
	// j.is_boolean()
    case json::value_t::boolean:
    case json::value_t::number_integer:
	// j.is_number_unsigned()
    case json::value_t::number_unsigned:
    case json::value_t::number_float:
	p = ptree(j.dump());
	break;
    case json::value_t::string:
	p = ptree(j.get<string>());
	break;
	// j.is_array()
    case json::value_t::array:
	for(auto& x: j)
	    p.push_back({"", json_to_ptree(x)});
	break;
    case json::value_t::object:
	for(auto it: json::iterator_wrapper(j))
	    p.push_back({it.key(), json_to_ptree(it.value())});
	break;
    default:
	break;
    }
    return p;
}

void Rules::add_rule(const fs::path& path)
{
    checked_ifstream infile(path.string(),"function file");

    Rule rule;
    try {
	json j;
	infile>>j;
	rule = json_to_ptree(j);
    }
    catch (const std::exception& e)
    {
	throw myexception()<<"Error parsing JSON function description '"<<path.string()<<"'\n:  "<<e.what();
    }
    catch (...)
    {
	throw myexception()<<"Error parsing JSON function description '"<<path.string()<<"'\n";
    }

    string name = rule.get<string>("name");

    if (rules.count(name))
	std::cerr<<"Warning: ignoring additional definition of function '"<<name<<"' from file '"<<path<<"'";
    else
	rules[name] = rule;
}

Rules::Rules(const vector<fs::path>& pl)
{
    // 1. Only keep paths that have a /functions/ subdir
    for(auto& path: pl)
    {
	auto fpath = path / "functions";
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
	    if (abs_path.extension() == ".json")
		add_rule(abs_path);
	}
    }

    // 3. Convert the rules - FIXME: should we convert default args in a later step?
    for(auto& rule: rules)
	rule.second = convert_rule(*this, rule.second);
}
