#include "computation/loader.H"
#include "computation/expression.H"
#include "computation/module.H"
#include "computation/operations.H"
#include "computation/graph_register.H"
#include "mytypes.H"
#include <boost/filesystem/operations.hpp>

#include "io.H"
#include "parser/desugar.H"
#include "parser/AST.H"

namespace fs = boost::filesystem;

using std::string;
using std::vector;
using std::set;
using std::map;

#if defined _MSC_VER || defined __MINGW32__
  const string plugin_extension = ".dll";
#else
  const string plugin_extension = ".so";
#endif

void make_Prelude(Module& P)
{
  // See http://www.haskell.org/onlinereport/standard-prelude.html


  // [ We could do this as two nested fmaps, instead. ]
  // [ We could factor out to_double(v2), and 1.0/to_double(v2)

  // FIXME - we have an problem with types here.  This will only work for Int, as-is.
  //  P += "{enumFromThen x y = ... }";
  //  P += "{enumFromThenTo x y z = ... }";

  //--------------------------------------- listFromVectorInt ----------------------------------------//
  P.def_function("getVectorIntElement", lambda_expression( BuiltinGetVectorIndexOp<int,Int>() ) ); 

  //--------------------------------------- listFromString ----------------------------------------//
  P.def_function("getStringElement", lambda_expression( BuiltinGetStringIndexOp() ) ); 
  P.def_function("sizeOfString", lambda_expression( StringSizeOp() ) );

  //--------------------------------------- listFromVectorVectorInt ----------------------------------------//
  P.def_function("getVectorVectorIntElement", lambda_expression( BuiltinGetVectorIndexOp<Vector<int>,Vector<int>>() ) ); 

  //--------------------------------------- listFromVectorVectorInt ----------------------------------------//
  P.def_function("getVectorvectorIntElement", lambda_expression( BuiltinGetVectorIndexOp<vector<int>,Vector<int>>() ) ); 

  //--------------------------------------- listToVectorInt ---------------------------------------//

  P.def_function("builtinNewVectorInt", lambda_expression( BuiltinNewVectorOp<int>() ) ); 
  P.def_function("builtinSetVectorIndexInt", lambda_expression( BuiltinSetVectorIndexOp<int,Int>() ) ); 

  //--------------------------------------- listToString ---------------------------------------//

  P.def_function("builtinNewString", lambda_expression( BuiltinNewStringOp() ) ); 
  P.def_function("builtinSetStringIndexInt", lambda_expression( BuiltinSetStringIndexOp() ) );

  //--------------------------------------- listToVectorDouble ---------------------------------------//

  P.def_function("builtinNewVectorDouble", lambda_expression( BuiltinNewVectorOp<double>() ) ); 
  P.def_function("builtinSetVectorIndexDouble", lambda_expression( BuiltinSetVectorIndexOp<double,Double>() ) ); 

  //--------------------------------------- listToVectorMatrix ---------------------------------------//

  P.def_function("builtinNewVectorMatrix", lambda_expression( BuiltinNewVectorOp<Matrix>() ) ); 
  P.def_function("builtinSetVectorIndexMatrix", lambda_expression( BuiltinSetVectorIndexOp<Matrix,MatrixObject>() ) ); 
}

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

    fs::path filename = find_file_in_path(modules_path, path );
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
    
    if (M.name == "Prelude")
      make_Prelude(M);

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
  return load_module_from_file(find_module(module_name));
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

expression_ref load_builtin(const module_loader& L, const string& symbol_name, const string& plugin_name, int n, const string& function_name)
{
  // Presumably on windows we don't need to search separate for ".DLL", since the FS isn't case sensitive.
  fs::path filepath = find_file_in_path(L.builtins_path, plugin_name + plugin_extension);
  return load_builtin(symbol_name, filepath.string(), n, function_name);
}

