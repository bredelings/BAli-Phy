#include "startup/help.hh"

#include <boost/optional/optional_io.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/algorithm/string.hpp>
#include "startup/cmd_line.H"
#include "startup/paths.H"
#include "util.H"
#include "../io.H"
#include "setup.H"
#include "version.H"
#include "models/rules.H"
#include "models/parse.H"

using std::string;
using std::map;
using std::vector;
using std::cout;
using boost::optional;

namespace po = boost::program_options;
using po::variables_map;

namespace fs = boost::filesystem;

string get_topic_from_string(const string& s)
{
    string s2 = s;
    int index = s2.find_first_of("\r\n");
    if (index != string::npos)
	s2 = s2.substr(0,index);
    if (s2.empty() or s2[s2.size()-1] == '.')
	;
    else if (s2[s2.size()-1] == ':')
	s2[s2.size()-1] = '.';
    else
	s2.push_back('.');
    return s2;
}

string pad(const string& s, int n)
{
    if (s.size() < n)
	return s+string(n-s.size(),' ');
    return s;
}


void help_on_help(std::ostream& o, const map<string,string>& help)
{
    o<<"Help topics via --help=arg are available for:\n";
    o<<"  =simple              Command-line flags and a short description.\n";
    o<<"  =advanced            Info on advanced command-line flags.\n";
    o<<"  =expert              Info on expert flags - these might be broken!.\n\n";
    o<<"  =functions           A list of functions and result type.\n";
    o<<"  =<function name>     Function type, description, and argument names.\n\n";
    o<<"  =help                This list of topics.\n\n";
    for(auto& x: help)
	o<<"  ="<<pad(x.first,18)<<"  "<<get_topic_from_string(x.second)<<"\n";
    o<<"\n";
}

std::map<string,string> load_help_files(const std::vector<fs::path>& package_paths)
{
    map<string,string> help;

    for(auto& package_path: package_paths)
    {
	auto path = package_path / "help";

	if (fs::exists(path))
	    for(auto& dir_entry: fs::recursive_directory_iterator(path))
	    {
		auto abs_path = fs::canonical(dir_entry.path() );
		string topic = abs_path.stem().string();
		
		if (abs_path.extension() == ".txt" and not help.count(topic))
		    help[topic] = boost::trim_copy(read_file(abs_path.string(), "help file"));
	    }
    }

    return help;
}


string indent(int n, const string& lines)
{
    string spaces(n,' ');
    return spaces + boost::replace_all_copy(lines, "\n", "\n"+spaces);
}

const std::string ansi_plain("\033[0m");
const std::string ansi_under("\033[4m");
const std::string ansi_bold("\033[1m");
const std::string ansi_red("\033[1;31m");
const std::string ansi_green("\033[1;32m");
const std::string ansi_yellow("\033[1;33m");
const std::string ansi_cyan("\033[1;36m");

string bold(const string& line)
{
    return ansi_bold + line + ansi_plain;
}

string underline(const string& line)
{
    return ansi_under + line + ansi_plain;
}

string header(const string& text)
{
    return underline(text) + ":\n\n";
}

optional<string> get_authors(const Rule& rule)
{
    auto citation = rule.get_child_optional("citation");
    if (not citation)
	return boost::none;

    vector<string> authors;
    if (auto authors_ = citation->get_child_optional("author"))
	for(auto& author: *authors_)
	    if (auto name = author.second.get_optional<string>("name"))
	    {
		auto names = split(*name,", ");
		if (names.size() == 2)
		{
		    string ref = names[0];
		    if (not names[1].empty())
		    {
			names[1] = string(1,names[1][0]) + '.';
			if (authors.empty())
			    ref = names[0] + ", " + names[1];
			else
			    ref = names[1] + " " +names[0];
		    }
		    authors.push_back(ref);
		}
		else
		{
		    authors.push_back(*name);
		}
	    }

    if (authors.size())
    {
	if (authors.size() <= 2)
	    return join(authors," and ");
	else
	    return authors[0]+" et al";
    }
    return boost::none;
}


optional<string> get_citation(const Rule& rule, bool show_title)
{
    auto citation = rule.get_child_optional("citation");
    if (not citation)
	return boost::none;

    if (citation->is_a<string>())
	return citation->get_value<string>();

    vector<string> cite;
    auto title = citation->get_optional<string>("title");
    auto year = citation->get_optional<string>("year");
    auto authors = get_authors(rule);

    if (authors)
	cite.push_back(*authors);
    if (year)
	cite.push_back("("+*year+")");
    if (title and show_title)
	cite.push_back(*title);

    return join(cite," ");
}



optional<string> get_citation_doi(const Rule& rule)
{
    optional<string> url;

    // 1. Check if there is a citation field.
    auto citation = rule.get_child_optional("citation");
    if (not citation) return boost::none;

    // 2. Try to get the DOI
    if (auto identifiers = citation->get_child_optional("identifier"))
    {
	for(auto& identifier: *identifiers)
	{
	    auto type = identifier.second.get_child_optional("type");
	    if (not type or type->get_value<string>() != "doi") continue;

	    auto id = identifier.second.get_child_optional("id");
	    if (id)
		return id->get_value<string>();
	}
    }
    return boost::none;
}

optional<string> get_citation_url(const Rule& rule)
{
    // 1. Check if there is a citation field.
    auto citation = rule.get_child_optional("citation");
    if (not citation) return boost::none;

    // 2. Try to get the URL from the "link" field.
    if (auto links = citation->get_child_optional("link"))
    {
	for(auto& link: *links)
	{
	    auto url = link.second.get_child_optional("url");
	    if (not url) continue;

	    auto anchor = link.second.get_child_optional("anchor");
	    if (anchor)
		return url->get_value<string>()+"/#"+anchor->get_value<string>();
	    else
		return url->get_value<string>();
	}
	
    }

    if (auto doi = get_citation_doi(rule))
    {
	return "https://doi.org/"+*doi;
    }

    return boost::none;
}

string show_model_abbrev(ptree p)
{
    bool top_sample = false;
    if (p.get_value<string>() == "Sample")
    {
	top_sample = true;
	p = p.begin()->second;
    }

    string output = p.get_value<string>();
    if (p.size())
	output += "[..]";
    string connector = top_sample?"~ ":"= ";

    return connector + output;
}

string get_help_for_rule(const Rule& rule)
{
    std::ostringstream help;
    if (auto title = rule.get_optional<string>("title"))
	help<<bold(*title)<<std::endl<<std::endl;

    string name = rule.get<string>("name");
    string result_type = unparse_type(rule.get_child("result_type"));
    auto args = rule.get_child("args");
    vector<string> args_names_types;
    // Actually, we may have a problem here...
    if (auto constraints = rule.get_child_optional("constraints"))
    {
	vector<string> cs;
	for(auto& x: *constraints)
	    cs.push_back(unparse_type(x.second));
//		help<<join(cs,", ")<<" => ";
    }
    for(auto& argpair: args)
    {
	auto& arg = argpair.second;
	if (arg.get_child_optional("no_apply")) continue;
	string arg_name = arg.get<string>("arg_name");
	if (auto default_value = arg.get_child_optional("default_value"))
	{
	    string def = show_model(*default_value);
	    if (def.size() > 15)
		def = show_model_abbrev(*default_value);
	    arg_name += " " + def;
	}
	args_names_types.push_back(arg_name);
    }
    help<<header("Usage");
    help<<"   "<<bold(name);
    if (args_names_types.size()) help<<"["<<join(args_names_types,", ")<<"]\n\n";
    
    help<<header("Arguments");
    for(auto& argpair: args)
    {
	auto& arg = argpair.second;
	if (arg.get_child_optional("no_apply")) continue;

	// 1. arg: description
	optional<string> description = arg.get_optional<string>("description");
	help<<"   "<<arg.get<string>("arg_name")<<": "<<description<<".\n";

	// 2. type: type
	help<<"       type: "<<unparse_type(arg.get_child("arg_type"))<<"\n";

	// 3. default =/~ default
	if (auto default_value = arg.get_child_optional("default_value"))
	    help<<"       default "<<show_model(*default_value)<<"\n";

	help<<std::endl;
    }
    help<<header("Result type");
    help<<indent(3,result_type)<<"\n\n";

    if (auto description = rule.get_optional<string>("description"))
    {
	help<<header("Description");
	help<<indent(3, *description)<<std::endl<<std::endl;
    }

    if (auto examples = rule.get_child_optional("examples"))
    {
	help<<header("Examples");
	for(auto& x: *examples)
	{
	    help<<indent(3, x.second.get_value<string>())<<"\n\n";
	}
    }

    if (auto citation = get_citation(rule,false))
    {
	help<<header("Citation");
	help<<indent(3,*citation)<<std::endl;
	if (auto url = get_citation_url(rule))
	    help<<indent(3,*url)<<std::endl;
	help<<std::endl;
    }
    
    return help.str();
}


void show_help(const string& topic, const vector<fs::path>& package_paths)
{
    auto help = load_help_files(package_paths);
	
    if (topic == "help")
    {
	help_on_help(std::cout, help);
	return;
    }
    if (help.count(topic))
    {
	std::cout<<help[topic];
	std::cout<<std::endl;
	return;
    }

    Rules R(package_paths);
    if (topic == "functions")
    {
	for(auto& rule: R.get_rules())
	{
	    string name = rule.second.get_child("name").get_value<string>();
	    string result_type = unparse_type(rule.second.get_child("result_type"));
	    std::cout<<name<<" :: "<<result_type << std::endl;
	}
	return;
    }

    if (auto rule = R.get_rule_for_func(topic))
    {
	std::cout<<get_help_for_rule(*rule);
	return;
    }
    else
    {
	cout<<"Help topic '"<<topic<<"' not found.\n\n";
	help_on_help(cout,help);
	return;
    }
}
