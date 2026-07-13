#include <algorithm>
#include <chrono>
#include <fstream>
#include <regex>
#include <iterator>
#include "computation/loader.H"
#include "computation/module.H"
#include "computation/operations.H"
#include "computation/core/ast.H"
#include "util/file-paths.H"
#include "parser/driver.hh"
#include "haskell/ids.H"
#include "haskell/extensions.H"
#include "core/func.H"

#include "util/io.H"
#include "util/log-level.H"
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

        if (user_source_root)
        {
            auto user_module = *user_source_root / path;
            if (fs::exists(user_module))
                return user_module;
        }

        if (auto file = check_file_in_path(plugins_path, "haskell"/path))
            return *file;

        auto user_source_root_string = user_source_root ? user_source_root->string() : string();
        throw myexception()<<"Couldn't find file "<<path<<" in user source root '"<<user_source_root_string
                           <<"' or file "<<("haskell"/path)<<" in package path '"<<show_path(plugins_path)<<"'";
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

vector<fs::path> path_components(const fs::path& path)
{
    vector<fs::path> components;
    for(const auto& part: path)
        components.push_back(part);
    return components;
}

fs::path module_source_path_for_file(const fs::path& filename, const string& module_name)
{
    // The user source root is the directory that module-name imports are
    // resolved relative to.  For a simple entry file like Main.hs, this is the
    // file's parent directory, so sibling imports such as Helper resolve.
    //
    // If the entry file is already laid out according to its module name, e.g.
    // src/Foo/Bar.hs containing module Foo.Bar, then the root should be src so
    // imports such as Foo.Baz resolve to src/Foo/Baz.hs.
    auto module_path = get_relative_path_from_haskell_id(module_name);
    module_path.replace_extension(".hs");

    auto source_path = filename.parent_path();
    if (source_path.empty())
        source_path = ".";

    // If the file name does not even match the module leaf name, fall back to
    // the file's parent directory.
    if (filename.filename() != module_path.filename())
        return source_path;

    auto source_parts = path_components(source_path);
    auto module_parts = path_components(module_path.parent_path());

    if (module_parts.size() > source_parts.size())
        return source_path;

    // Strip the module directory suffix from the file's parent directory.
    // src/Foo/Bar.hs + module Foo.Bar has source_path src/Foo and module
    // parent Foo, yielding root src.
    auto suffix_start = source_parts.size() - module_parts.size();
    for(size_t i=0;i<module_parts.size();i++)
        if (source_parts[suffix_start + i] != module_parts[i])
            return source_path;

    fs::path root;
    for(size_t i=0;i<suffix_start;i++)
        root /= source_parts[i];

    return root.empty() ? fs::path(".") : root;
}

void module_loader::set_user_source_root_for_file(const fs::path& filename, const string& module_name)
{
    auto root = module_source_path_for_file(filename, module_name);
    if (fs::exists(root))
        user_source_root = root;
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

// Select this executable's module cache, removing legacy artifacts and all
// but the four most recently selected executable caches.
static optional<fs::path> prepare_module_cache(const fs::path& cache_root)
{
    static const std::regex cache_key_re("^[0-9a-f]{16}$");

    try
    {
        fs::create_directories(cache_root);

        vector<fs::path> legacy_directories;
        for(const auto& entry: fs::directory_iterator(cache_root))
        {
            auto name = entry.path().filename().string();
            if (fs::is_directory(entry.symlink_status()) and
                std::regex_match(name, cache_key_re))
                continue;

            if (fs::is_regular_file(entry.symlink_status()) and
                entry.path().extension() == ".mod")
            {
                fs::remove(entry.path());
                continue;
            }

            if (not fs::is_directory(entry.symlink_status())) continue;
            legacy_directories.push_back(entry.path());
            for(const auto& legacy: fs::recursive_directory_iterator(entry.path()))
            {
                if (fs::is_directory(legacy.symlink_status()))
                    legacy_directories.push_back(legacy.path());
                else if (fs::is_regular_file(legacy.symlink_status()) and
                         legacy.path().extension() == ".mod")
                    fs::remove(legacy.path());
            }
        }

        std::sort(legacy_directories.begin(), legacy_directories.end(),
                  // Remove child directories before parents without disturbing
                  // a directory that still contains unrelated cache data.
                  [](const fs::path& left, const fs::path& right)
                  {
                      return std::distance(left.begin(), left.end()) >
                             std::distance(right.begin(), right.end());
                  });
        for(const auto& directory: legacy_directories)
        {
            std::error_code error;
            fs::remove(directory, error);
        }
    }
    catch(const fs::filesystem_error& e)
    {
        if (log_verbose >= 2)
            std::cerr<<"Failure removing legacy compiled-module cache files: "
                     <<e.what()<<"\n";
    }

    try
    {
        auto current = cache_root / module_loader::executable_hash();
        fs::create_directories(current);

        auto marker = current / ".last-used";
        std::ofstream marker_file(marker, std::ios::app);
        if (not marker_file)
            throw fs::filesystem_error("Could not touch compiled-module cache marker",
                                       marker, std::make_error_code(std::errc::io_error));
        marker_file.close();
        fs::last_write_time(marker, fs::file_time_type::clock::now());

        vector<std::pair<fs::file_time_type, fs::path>> caches;
        for(const auto& entry: fs::directory_iterator(cache_root))
        {
            auto name = entry.path().filename().string();
            auto last_used = entry.path() / ".last-used";
            if (fs::is_directory(entry.symlink_status()) and
                std::regex_match(name, cache_key_re) and
                fs::is_regular_file(last_used))
                caches.push_back({fs::last_write_time(last_used), entry.path()});
        }
        std::sort(caches.begin(), caches.end());

        auto excess = caches.size() > 4 ? caches.size() - 4 : 0;
        for(const auto& [_, path]: caches)
        {
            if (excess == 0) break;
            if (path == current) continue;

            std::error_code error;
            fs::remove_all(path, error);
            if (error)
            {
                if (log_verbose >= 2)
                    std::cerr<<"Failure removing old compiled-module cache '"
                             <<path<<"': "<<error.message()<<"\n";
            }
            else
                --excess;
        }

        return current;
    }
    catch(const fs::filesystem_error& e)
    {
        if (log_verbose >= 2)
            std::cerr<<"Failure preparing compiled-module cache: "<<e.what()<<"\n";
        return {};
    }
}

module_loader::module_loader(const optional<fs::path>& cp, const vector<fs::path>& path_list)
{
    if (cp) cache_path = prepare_module_cache(*cp);

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
