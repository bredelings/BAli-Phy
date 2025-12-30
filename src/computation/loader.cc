#include <regex>
#include <iterator>
#include "computation/loader.H"
#include "computation/module.H"
#include "computation/operations.H"
#include "expression/lambda.H"
#include "util/file-paths.H"
#include "parser/driver.hh"
#include "haskell/ids.H"
#include "haskell/extensions.H"
#include "core/func.H"

#include "util/io.H"
#include <boost/compute/detail/sha1.hpp>
#include <range/v3/all.hpp>

namespace fs = std::filesystem;

using std::string;
using std::vector;
using std::set;
using std::map;
using std::tuple;
using std::optional;
using std::shared_ptr;

#if defined _MSC_VER || defined __MINGW32__ || defined __CYGWIN__
const string plugin_extension = ".dll";
#else
const string plugin_extension = ".so";
#endif

bool module_loader::try_add_plugin_path(const fs::path& filepath)
{
    if (fs::exists(filepath))
    {
	plugins_path.push_back(filepath);
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

fs::path module_loader::find_module(const string& modid) const
{
    try
    {
	fs::path path = get_relative_path_from_haskell_id(modid);
	path.replace_extension(".hs");
    
	return find_file_in_path(plugins_path, "haskell"/path );
    }
    catch (myexception& e)
    {
	e.prepend("Loading module '"+modid+"': ");
	throw;
    }
}

optional<fs::path> module_loader::cache_path_for_module(const string& modid) const
{
    if (not cache_path) return {};

    fs::path path = get_relative_path_from_haskell_id(modid);
    path.replace_extension(".hs.mod");

    return *cache_path / path;
}

optional<fs::path> module_loader::find_cached_module(const string& modid) const
{
    if (auto path = cache_path_for_module(modid); path and fs::exists(*path) and fs::is_regular_file(*path))
	return *path;
    else
	return {};
}

//{-# LANGUAGE NoImplicitPrelude #-}
static std::regex language_option_re("^\\s*\\{-#\\s+LANGUAGE\\s+(.*[^\\s])\\s+#-\\}");

LanguageExtensions language_extensions(const string& filename, string& mod)
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


    LanguageExtensions lang_exts;
    vector<Message> messages;
    for(auto& option: options)
    {
        std::optional<yy::location> loc;
        auto note = lang_exts.set_option(option);
        if (note)
            messages.push_back({ErrorMsg, loc, {*note}});
    }
    show_messages({filename, mod}, std::cerr, messages);
    exit_on_error(messages);

    mod = mod.substr(pos);
    return lang_exts;
}

fs::path pretty_module_path(const fs::path& filepath)
{
    fs::path hpath;
    // if there's a path element called "haskell", split after that.
    for(const auto& p: filepath)
    {
        hpath /= p;
        if (p == "haskell")
            return filepath.lexically_relative(hpath);
    }
    return filepath.filename();
}

shared_ptr<Module> module_loader::load_module_from_file(const fs::path& filename) const
{
    if (not modules.count(filename.string()))
    {
	try
	{
            auto fname = std::make_shared<string>( pretty_module_path(filename).string() );

	    string file_contents = read_file(filename.string(), "module");

	    auto lang_exts = language_extensions(*fname, file_contents);

	    auto m = parse_module_file(file_contents, *fname, lang_exts);

	    if (dump_parsed)
		std::cout<<m.print()<<std::endl;

            string short_file_name = filename.filename().string();

            auto M = std::make_shared<Module>(Module(m, lang_exts, {short_file_name, file_contents}));
            // Save a reference to the string that we allocated, so we can clean it up later.
            M->filename = fname;
            modules.insert( {filename.string(), M} );
	}
	catch (myexception& e)
	{
	    e.prepend("Loading module from file '"+filename.string()+"':\n  ");
	    throw;
	}
    }

    // Return a COPY of the cached module.
    return std::make_shared<Module>( *modules.at(filename.string()) );
}

shared_ptr<Module> module_loader::load_module(const string& module_name) const
{
    auto filename = find_module(module_name);
    auto M = load_module_from_file(filename);
    if (M->name != module_name)
	throw myexception()<<"Loading module file "<<filename<<"\n  Expected module '"<<module_name<<"'\n  Found module    '"<<M->name<<"'";
    return M;
}

module_loader::module_loader(const optional<fs::path>& cp, const vector<fs::path>& path_list)
    :cache_path(cp)
{
    for(auto& path: path_list)
	try_add_plugin_path(path.string());
}

fs::path module_loader::find_plugin(const string& plugin_name) const
{
    return find_file_in_path(plugins_path, plugin_name + plugin_extension);
}

#include <dlfcn.h>

std::string function_prefix(const std::string& call_conv)
{
    if (call_conv == "bpcall")
        return "builtin_function_";
    else if (call_conv == "trcall")
        return "builtin_function_";
    else if (call_conv == "ecall")
        return "simple_function_";
    else
        throw myexception()<<"Calling convention '"<<call_conv<<"' not recognized";
}

void* load_builtin_(const fs::path& filename, const string& raw_symbol_name, const std::string& call_conv)
{
    const string builtin_prefix = function_prefix(call_conv);

    string symbol_name = builtin_prefix + raw_symbol_name;

    // load the library
    void* library = dlopen(filename.string().c_str(), RTLD_LAZY);
    if (not library)
	throw myexception() << "Cannot load library: " << dlerror();

    // reset errors
    dlerror();

    // load the symbols
    void* fn =  dlsym(library, symbol_name.c_str());
    const char* dlsym_error = dlerror();
    if (dlsym_error)
	throw myexception() << "Cannot load symbol for builtin '"<<raw_symbol_name<<"' from file "<<filename<<": " << dlsym_error;

    return fn;
}

void* module_loader::load_builtin_ptr(const string& plugin_name, const string& symbol_name, const string& call_conv) const
{
    // Presumably on windows we don't need to search separately for ".DLL", since the FS isn't case sensitive.
    auto filename = find_plugin(plugin_name);

    auto op = tuple<string,string>(plugin_name, symbol_name);
    if (not cached_builtins.count(op))
	cached_builtins.insert({op, load_builtin_(filename, symbol_name, call_conv)});

    return cached_builtins.at(op);
}

Core2::Exp<> module_loader::load_builtin(const string& plugin_name, const string& symbol_name, const string& call_conv, int n) const
{
    assert(not call_conv.empty());
    auto fn = load_builtin_ptr(plugin_name, symbol_name, call_conv);

    auto args = make_vars<>(n);
    auto args_exp = args | ranges::to<vector<Core2::Exp<>>>;

    Core2::Exp<> body = Core2::BuiltinOp<>(plugin_name, symbol_name, call_conv, args_exp, fn);
    return lambda_quantify(args, body);
}

