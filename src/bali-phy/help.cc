#include "help.hh"

#include <regex>
#include <list>
#include <filesystem>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string.hpp>
#include "cmd_line.H"
#include "paths.H"
#include "util/string/join.H"
#include "util/string/split.H"
#include "util/string/pred.H"
#include "util/io.H"
#include "util/io/optional.H"
#include "version.H"
#include "models/rules.H"
#include "models/parse.H"
#include "util/text.H"
#include "util/ptree.H"

using std::string;
using std::list;
using std::map;
using std::vector;
using std::cout;
using std::optional;

namespace po = boost::program_options;
using po::variables_map;

namespace fs = std::filesystem;

string do_quotes(const string& line)
{
    return std::regex_replace(line,std::regex("([^\\\\]|^)`([^`]*)`"),string("$1")+black(highlight_bg("$2")).c_str());
}

string do_double_emph(string line)
{
    line = std::regex_replace(line,std::regex("([^\\\\]|^)__([^_ ][^_]*)__"),string("$1")+bold("$2").c_str());
    return std::regex_replace(line,std::regex("([^\\\\]|^)\\*\\*([^* ][^*]*)\\*\\*"),string("$1")+bold("$2").c_str());
}

string do_single_emph(string line)
{
    line = std::regex_replace(line,std::regex("([^\\\\]|^)\\*([^* ][^*]*)\\*"),string("$1")+underline("$2").c_str());
    return std::regex_replace(line,std::regex("([^\\\\]|^)_([^_ ][^_]*)_"),string("$1")+underline("$2").c_str());
}

string do_unescape(const string& line)
{
    return std::regex_replace(line,std::regex("\\\\([`_*])"),"$1");
}

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

vector<string> get_path(fs::path p)
{
    vector<string> v;
    assert(p.is_relative());
    for(const auto& pentry: p)
	v.push_back(pentry.string());
    if (v.size() and v.back().find('.') != string::npos)
	v.back() = v.back().substr(0,v.back().rfind('.'));
    return v;
}

string indent_and_wrap_citation(int indent, int extra_indent, int width, const string& text)
{
    if (text.empty()) return text;

    list<string> wrapped_lines;

    auto lines = get_lines(text);
    assert(not lines.empty());

    for(auto& line: lines)
	wrapped_lines.push_back(indent_and_wrap(indent, indent+extra_indent, width, line));

    return join(wrapped_lines, '\n');
}


string header(const string& text)
{
    return underline(text) + ":\n\n";
}

optional<string> get_authors(const Rule& rule)
{
    if (not rule.docs.citation)
	return {};
    const auto& citation = *rule.docs.citation;

    vector<string> authors;
    for(auto& author: citation.authors)
    {
        auto names = split(author.name,", ");
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
            authors.push_back(author.name);
        }
    }

    if (authors.size())
    {
	if (authors.size() <= 2)
	    return join(authors," and ");
	else
	    return authors[0]+" et al";
    }
    return {};
}


optional<string> get_citation(const Rule& rule, bool show_title)
{
    if (not rule.docs.citation)
	return {};
    const auto& citation = *rule.docs.citation;

    if (citation.text)
	return *citation.text;

    vector<string> cite;
    auto authors = get_authors(rule);

    if (authors)
	cite.push_back(*authors);
    if (citation.year)
	cite.push_back("("+*citation.year+")");

    if (authors or citation.year)
    {
	string x = join(cite, " ");
	cite.clear();
	cite.push_back(x);
    }
    if (citation.title and show_title)
	cite.push_back(*citation.title);

    return join(cite,"\n");
}



optional<string> get_citation_id(const Rule& rule, const string& idtype)
{
    // 1. Check if there is a citation field.
    if (not rule.docs.citation) return {};
    const auto& citation = *rule.docs.citation;

    // 2. Try to get the DOI
    for(auto& identifier: citation.identifiers)
    {
        if (identifier.type != idtype) continue;

        return identifier.id;
    }
    return {};
}

optional<string> get_citation_url(const Rule& rule)
{
    // 1. Check if there is a citation field.
    if (not rule.docs.citation) return {};
    const auto& citation = *rule.docs.citation;

    // 2. Try to get the URL from the "link" field.
    for(auto& link: citation.links)
    {
        if (link.anchor)
            return link.url + "/#" + *link.anchor;
        else
            return link.url;
    }

    if (auto doi = get_citation_id(rule,"doi"))
    {
	return "https://doi.org/"+*doi;
    }
    else if (auto pmcid = get_citation_id(rule,"pmcid"))
    {
	return "https://www.ncbi.nlm.nih.gov/pmc/articles/"+*pmcid;
    }
    else if (auto pmid = get_citation_id(rule,"pmid"))
    {
	return "https://www.ncbi.nlm.nih.gov/pubmed/"+*pmid;
    }

    return {};
}

string get_help_for_rule(const Rule& rule)
{
    std::ostringstream help;
    if (auto title = rule.docs.title)
	help<<bold(*title)<<std::endl<<std::endl;

    string name = rule.name;
    string result_type = unparse_type(rule.result_type);
    vector<string> args_names_types;
    // Actually, we may have a problem here...
    if (not rule.constraints.empty())
    {
	vector<string> cs;
	for(auto& x: rule.constraints)
	    cs.push_back(unparse_type(x));
//		help<<join(cs,", ")<<" => ";
    }
    for(auto& arg: rule.args)
    {
	string arg_name = arg.name;
	string arg_type = unparse_type(arg.type);
	args_names_types.push_back(blue(arg_name) + bold(": ") + red(arg_type));
    }
    help<<header("Usage");
    help<<"   "<<bold(name);
    if (args_names_types.size()) help<<bold("(")<<join(args_names_types,bold(", "))<<bold(")");
    help<<" "<<bold("->")<<" "<<red(result_type);
    help<<"\n\n";

    if (not rule.synonyms.empty())
    {
	vector<string> syn;
	for(auto& x: rule.synonyms)
	    syn.push_back(x);
	help<<header("Synonyms");
	help<<indent_and_wrap(3, terminal_width(),join(syn,", "))<<"\n\n";
    }

    if (not rule.args.empty())
	help<<header("Arguments");
    for(auto& arg: rule.args)
    {
	// 1. arg: description
	auto description = arg.description.value_or("");
	help<<"   "<<blue(arg.name)<<": "<<description<<".\n";

	// 2. default =/~ default
	if (arg.default_value)
	    help<<"       default "<<show_model(*arg.default_value)<<"\n";

	help<<std::endl;
    }

    if (auto description = rule.docs.description)
    {
	help<<header("Description");
        auto text = indent_and_wrap_pars(3, terminal_width(), *description);
//        text = do_quotes(text);
//        text = do_double_emph(text);
//        text = do_single_emph(text);
//        text = do_unescape(text);
	help<<text<<std::endl<<std::endl;
    }

    if (not rule.docs.examples.empty())
    {
	help<<header("Examples");
	for(auto& x: rule.docs.examples)
	{
	    help<<indent(3, x)<<"\n\n";
	}
    }

    if (auto citation = get_citation(rule,true))
    {
	help<<header("Citation");
	help<<indent_and_wrap_citation(3,3,terminal_width(),*citation)<<std::endl;
	if (auto url = get_citation_url(rule))
	    help<<indent(3,*url)<<std::endl;
	help<<std::endl;
    }
    
    if (not rule.docs.see.empty())
    {
	vector<string> see;
	for(auto& x: rule.docs.see)
	    see.push_back(bold(x));
	help<<header("See also");
	help<<indent_and_wrap(3, terminal_width(),join(see,", "))<<"\n\n";
    }

    return help.str();
}

string pseudo_markdown(const string& text)
{
    std::ostringstream marked;
    auto lines = split(text,'\n');

    // If the text ends with a '\n', we shouldn't add an extra line.
    if (lines.size() and lines.back().empty())
	lines.pop_back();

    for(auto& line: lines)
    {
	bool header = false;
	if (starts_with(line,"# "))
	{
	    header = true;
	    line = line.substr(2);
	}
	line = do_quotes(line);
	line = do_double_emph(line);
	line = do_single_emph(line);
	line = do_unescape(line);
	if (header)
	    line = bold(line);
	marked<<line<<"\n";
    }

    return marked.str();
}

const ptree* find(const string& key0, const ptree& p)
{
    for(auto& [key,value]: p.children())
    {
	if (key == key0) return &value;
	if (auto found = find(key0, value))
	    return found;
    }
    return {};
}

vector<string> get_subtopics(const ptree& p)
{
    vector<string> subtopics;
    for(auto [name,value]: p.children())
    {
	if (value.children().size()) name += "/";
	subtopics.push_back(name);
    }
    return subtopics;
}

ptree load_help_files(const std::vector<fs::path>& package_paths)
{
    ptree help;

    // 1. Load help from Markdown files in the help/ directory
    for(auto& package_path: package_paths)
    {
	auto path = package_path / "help";

	if (fs::exists(path))
	    for(auto& dir_entry: fs::recursive_directory_iterator(path))
	    {
		auto abs_path = fs::canonical(dir_entry.path());
		if (not fs::is_directory(abs_path) and abs_path.extension() == ".txt")
		{
		    string content = boost::trim_copy(read_file(abs_path, "help file"));

		    auto rel_path = fs::relative(dir_entry.path(), path);
		    content = pseudo_markdown(content)+"\n";
		    help.make_path(get_path(rel_path)).put_value(content);
		}
	    }
    }

    // 2. Load help from JSON files in the bindings/ directory
    Rules R(package_paths);
    for(auto& [_,rule]: R.get_rules())
    {
	vector<string> category = rule.docs.category;
	category.push_back(rule.name);
	string text = get_help_for_rule(rule);
	help.make_path(category).put_value(text);
    }

    return help;
}

void help_topics(std::ostream& o, const ptree& help)
{
    auto subtopics = get_subtopics(help);
    
    o<<"To see help on one of the following topics, run `bali-phy help "<<underline("topic")<<"`\n\n";
    o<<show_options(subtopics);
    o<<"\n";
}

void help_topics(std::ostream& o, const std::vector<fs::path>& package_paths)
{
    help_topics(o, load_help_files(package_paths));
}

void show_help(const string& topic, const vector<fs::path>& package_paths)
{
    // 1. Load help from Markdown files (in help/) and JSON files (in bindings/)
    auto help = load_help_files(package_paths);
	
    // 3. Show a top-level overview of categories
    if (topic == "topics")
	help_topics(std::cout, help);
    else if (auto found = find(topic, help))
    {
	auto subtopics = get_subtopics(*found);
	if (not found->value_is_empty())
	{
	    std::cout<<found->get_value<string>();
	    if (subtopics.size())
		std::cout<<"\n";
	}
	if (subtopics.size())
	{
	    std::cout<<bold("Subtopics")<<":\n\n";
	    std::cout<<show_options(subtopics);
	}
	return;
    }
    else
    {
	cout<<"Help topic '"<<topic<<"' not found.\n\n";
	help_topics(cout,help);
	return;
    }
}
