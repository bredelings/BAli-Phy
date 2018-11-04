#include <regex>
#include <iterator>
#include "computation/loader.H"
#include "computation/module.H"
#include "computation/operations.H"
#include <boost/filesystem/operations.hpp>
#include "expression/expression.H"
#include "expression/lambda.H"
#include "startup/paths.H"
#include "parser/driver.hh"

#include "io.H"
#include "parser/desugar.H"

namespace fs = boost::filesystem;

using std::string;
using std::vector;
using std::set;
using std::map;

#if defined _MSC_VER || defined __MINGW32__ || defined __CYGWIN__
const string plugin_extension = ".dll";
#else
const string plugin_extension = ".so";
#endif

bool module_loader::try_add_plugin_path(const string& path)
{
    if (fs::exists(path))
    {
	plugins_path.push_back(path);
	return true;
    }
    else
	return false;
}

fs::path get_relative_path_from_haskell_id(const string& modid)
{
    vector<string> path = get_haskell_identifier_path(modid);
    fs::path file_path = path[0];
    for(int i=1;i<path.size();i++)
	file_path /= path[i];
    return file_path;
}

string module_loader::find_module(const string& modid) const
{
    try
    {
	fs::path path = get_relative_path_from_haskell_id(modid);
	path.replace_extension(".hs");
    
	fs::path filename = find_file_in_path(plugins_path, "modules"/path );
	return filename.string();
    }
    catch (myexception& e)
    {
	e.prepend("Loading module '"+modid+"': ");
	throw;
    }
}

//{-# LANGUAGE NoImplicitPrelude #-}
static std::regex language_option_re("^\\s*\\{-#\\s+LANGUAGE\\s+(.*[^\\s])\\s+#-\\}");

set<string> language_options(string& mod)
{
    set<string> options;

    auto s = mod.c_str();
    int pos = 0;

    std::cmatch m;
    while(std::regex_search(s+pos, m, language_option_re))
    {
	// 1. Get the text from the pragma {-# LANGUAGE ___text___ #-}
	auto o = m[1].str();

	// 2. Split the text on commas and whitespace, and put the items into `options`.
	// tokenization (non-matched fragments)
	std::regex sep_re("\\s*,\\s*"); // whitespace
	std::copy( std::sregex_token_iterator(o.begin(), o.end(), sep_re, -1),
		   std::sregex_token_iterator(),
		   std::inserter(options, options.end()));

	pos += m.length();
    }
    mod = mod.substr(pos);
    return options;
}

Module module_loader::load_module_from_file(const string& filename) const
{
    if (not modules.count(filename))
    {
	try
	{
	    string file_contents = read_file(filename,"module");

	    auto lang_options = language_options(file_contents);

	    auto m = parse_module_file(file_contents, filename);

	    if (dump_parsed)
		std::cout<<m<<std::endl;

	    modules.insert( {filename, Module(m, lang_options)} );
	}
	catch (myexception& e)
	{
	    e.prepend("Loading module from file '"+filename+"':\n  ");
	    throw;
	}
    }

    return modules.at(filename);
}

Module module_loader::load_module(const string& module_name) const
{
    string filename = find_module(module_name);
    Module M = load_module_from_file(filename);
    if (M.name != module_name)
	throw myexception()<<"Loading module file '"<<filename<<"'\n  Expected module '"<<module_name<<"'\n  Found module    '"<<M.name<<"'";
    return M;
}

module_loader::module_loader(const vector<fs::path>& path_list)
{
    for(auto& path: path_list)
	try_add_plugin_path(path.string());
}

#include <dlfcn.h>

expression_ref load_builtin_(const string& filename, const string& symbol_name, int n, const string& fname)
{
    // If not, then I think its treated as being already in WHNF, and not evaluated.
    if (n < 1) throw myexception()<<"A builtin must have at least 1 argument";

    // load the library
    void* library = dlopen(filename.c_str(), RTLD_LAZY);
    if (not library)
	throw myexception() << "Cannot load library: " << dlerror();

    // reset errors
    dlerror();
    
    // load the symbols
    void* fn =  dlsym(library, symbol_name.c_str());
    const char* dlsym_error = dlerror();
    if (dlsym_error)
	throw myexception() << "Cannot load symbol for builtin '"<<fname<<"' from file '"<<filename<<": " << dlsym_error;
    
    // Create the operation
    Operation O(n, (operation_fn)fn, fname);

    // Create the function body from it.
    return lambda_expression(O);
}

expression_ref load_builtin(const string& symbol_name, const string& filename, int n, const string& function_name)
{
    return load_builtin_(filename, symbol_name, n, function_name);
}

string module_loader::find_plugin(const string& plugin_name) const
{
    fs::path filepath = find_file_in_path(plugins_path, plugin_name + plugin_extension);
    return filepath.string();
}

expression_ref load_builtin(const module_loader& L, const string& symbol_name, const string& plugin_name, int n, const string& function_name)
{
    // Presumably on windows we don't need to search separately for ".DLL", since the FS isn't case sensitive.
    string filename = L.find_plugin(plugin_name);
    return load_builtin(symbol_name, filename, n, function_name);
}

