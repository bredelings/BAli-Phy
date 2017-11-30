#include <vector>
#include <set>
#include "rules.H"
#include "myexception.H"
#include "util.H"
#include "io.H"
#include <boost/property_tree/json_parser.hpp>
#include <boost/optional.hpp>

using std::vector;
using std::set;
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

vector<Rule> get_rules(const Rules& R)
{
    return R;
}

optional<Rule> Rules::get_rule_for_func(const string& s) const
{
    for(const auto& rule: *this)
	if (rule.get<string>("name") == s)
	    return rule;
    return boost::none;
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


string get_function_name_from_rel_path(fs::path path)
{
    path.replace_extension();
    vector<string> xx;
    assert(path.is_relative());
    for(auto& x: path)
	xx.push_back(x.string());
    return join(xx,".");
}

fs::path get_rel_path_from_function_name(const std::string& s)
{
    fs::path path;
    for(auto& x: split(s,"."))
    {
	if (x.empty()) throw myexception ()<<"Empty element in rule name '"<<s<<"'";
	path = path / x;
    }
    return path;
}

optional<fs::path> Rules::get_path_for_function(const std::string& name) const
{
    if (name.empty())
	throw myexception()<<"Can't load rule for empty string!";

    auto rel_path = get_rel_path_from_function_name(name);

    for(auto& path: path_list)
    {
	assert(fs::exists(path));

	auto abs_path = path / rel_path;

	abs_path.replace_extension("json");
	
	if (fs::exists(abs_path))
	    return abs_path;
    }

    return boost::none;
}

string show_paths(const vector<fs::path>& paths)
{
    vector<string> spaths;
    for(auto& path: paths)
	spaths.push_back(path.string());
    return join(spaths,":");
}

Rule Rules::load_rule(const std::string& name) const
{
    auto path = get_path_for_function(name);
    if (not path)
	throw myexception()<<"can't find JSON definition file for function '"<<name<<"' in path '"<<show_paths(path_list)<<"'";

    checked_ifstream infile(path->string(),"function file");
    Rule rule;
    try {
	pt::read_json(infile, rule);
    }
    catch (...)
    {
	throw myexception()<<"Error parsing JSON function description '"<<path->string()<<"'\n";
    }

    // Complain if the file name doesn't descript the file name.
    if (rule.get<string>("name") != name)
	throw myexception()<<"Function file '"<<path->string()
			   <<"' contains function '"<<rule.get<string>("name")
			   <<"' instead of '"<<name<<"'";

    return rule;
}

void Rules::add_rule(const std::string& s)
{
    try{
	push_back(load_rule(s));
    }
    catch (std::exception& e)
    {
	std::cerr<<"Error loading rule for '"<<s<<"'\n";
    }
    catch (...) { }
}

vector<string> Rules::find_all_rules() const
{
    set<string> paths;
    for(auto& path: path_list)
    {
	assert(fs::exists(path));

	for(auto& dir_entry: fs::recursive_directory_iterator(path))
	{
	    auto rel_path = fs::relative(dir_entry.path(), path);

	    if (rel_path.extension() == ".json")
		paths.insert( get_function_name_from_rel_path(rel_path));
	}
    }
    vector<string> pathsv;
    pathsv.insert(pathsv.begin(), paths.begin(), paths.end());
    return pathsv;
}

Rules::Rules(const vector<fs::path>& pl)
{
    // 1. Add path/functions for each path in the list, if it exists
    for(auto& path: pl)
    {
	auto fpath = path / "functions";
	if (fs::exists(fpath))
	    path_list.push_back(fpath);
    }

    // 2. Load all the rules
    for(auto& rule_name : find_all_rules())
	add_rule(rule_name);
    
    // 3. Convert the rules - FIXME: should we convert default args in a later step?
    for(auto& rule: *this)
	rule = convert_rule(*this, rule);
}
