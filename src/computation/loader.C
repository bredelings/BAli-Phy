#include "computation/loader.H"
#include "computation/expression.H"
#include "computation/module.H"
#include "computation/operations.H"
#include "computation/graph_register.H"
#include <boost/filesystem/operations.hpp>

#include "io.H"
#include "parser/desugar.H"
#include "parser/AST.H"

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

expression_ref module_loader::read_module_from_file(const string& filename) const
{
  try
  {
    if (not modules.count(filename))
    {
      string file_contents = read_file(filename,"module");
      modules[filename] = parse_module_file(file_contents);
    }

    return modules[filename];
  }
  catch (myexception& e)
  {
    e.prepend("Loading module from file '"+filename+"':\n  ");
    throw e;
  }
}

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

fs::path find_file_in_path(const vector<string>& paths, const fs::path& file_path)
{
  for(const string& prefix: paths)
  {
    fs::path filename = prefix;
    filename /= file_path;
    if (not fs::exists(filename)) continue;
    return filename;
  }
  throw myexception()<<"Couldn't find file '"<<file_path.string()<<"' in path '"<<join(paths,':')<<"'";
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
    throw e;
  }
}

Module module_loader::load_module_from_file(const string& filename) const
{
  try
  {
    expression_ref module = read_module_from_file(filename);

    Module M(module);
    
    return M;
  }
  catch (myexception& e)
  {
    e.prepend("Loading module from file '"+filename+"':\n  ");
    throw e;
  }
}

vector<Module> load_modules(const module_loader& L, const vector<string>& module_names)
{
  vector<Module> P;
  for(const string& name: module_names)
    P.push_back(L.load_module(name));
  return P;
}

vector<Module> load_modules(const module_loader& L, const set<string>& module_names)
{
  vector<Module> P;
  for(const string& name: module_names)
    P.push_back(L.load_module(name));
  return P;
}

Module module_loader::load_module(const string& module_name) const
{
  string filename = find_module(module_name);
  Module M = load_module_from_file(filename);
  if (M.name != module_name)
    throw myexception()<<"Loading module file '"<<filename<<"'\n  Expected module '"<<module_name<<"'\n  Found module    '"<<M.name<<"'";
  return M;
}

#include <dlfcn.h>

typedef closure (*operation_fn)(OperationArgs&);

struct OperationFn: public Operation
{
  operation_fn perform_operation;
  string name_;

  OperationFn* clone() const {return new OperationFn(*this);}
  closure operator()(OperationArgs& Args) const {return perform_operation(Args);}

  std::string name() const {return name_;}

  OperationFn(void* fn, int n, const string& fname)
    :Operation(n),perform_operation((operation_fn)fn),name_(fname) 
  { }
};

expression_ref load_builtin_(const string& filename, const string& symbol_name, int n, const string& fname)
{
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
  OperationFn O(fn, n, fname);

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

