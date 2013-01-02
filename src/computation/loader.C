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

using std::vector;
using std::string;

/* \todo: List of things to do to clean up programs.
 *
 * See list in parser/desugar.C
 *
 * 1. [DONE] Remove true/false in favor of True/False.
 * 2. [DONE] Convert strings to [Char]
 * 3. Update probability functions to separate the family from the probability object.
 *    3a. Construct the ExpOf transform to make logNormal, logGamma, etc.
 *    3b. Choose kernels based on the range, not based on the distribution name.
 * 4. [DONE] Convert Defs to use the machine.
 * 5. [DONE] SYNTAX: replace a ~ b ( c ) with a ~ b
 * 6. [DONE] SYNTAX: external a ~ b [To not declare all parameters]
 *      6a. [DONE] SYNTAX: data a ~ b [Don't treat a as a parameter at all!]
 * 7. [DONE] Allow defs in BUGS files.
 * 8. Rationalize Model_Notes, formula_expression_ref, and program?
 *    - [DONE] Make Model_Notes into a Program with notes added?
 *    - [DONE] Could we parse a BUGS file in to a Model_Notes?
 *    - Stop allowing the creation of parameters that aren't in modules!
 *    - Create formula_expression_ref's as a list of modules w/ notes.
 *      + Perhaps turn imports into submodules (that are prefixed with the main module) if they have parameters.
 *      + Perhaps when prefixing a module, we don't want to prefix its FUNCTIONS,
 *        just its parameters and notes?
 *      + Basically, a f.e.r. should be defined by a single module w/ imports, and a 'main' routine?
 *        - How does this relate to simply bringing in the notes attached to parameters?
 *        - How does this attempt to give parameters default names relate to the more general approach?
 *    - Eliminate C++ operators on formula_expression_ref -> use parser instead.
 *    - Eliminate C++ operators on expression_ref -> use parser instead.
 * 9. Try to rewrite e.g. M8b into a BUGS module.
 * 10. Add default values and Bounds to distributions.
 *    - [DONE] Add Bounds to distributions.
 *    - Ah, but how do we add default values to distributions that return random structures?
 *      10a. Make performing an MCMC move into an IO operation that is performed in the machine.
 *      10b. MCMC on a structure can then call MCMC on the child elements.
 *      10c. By examining the distribution of the structure, the parent MCMC can determine the distribution
 *            -- and thus the bounds -- for the MCMC on the parts.
 *      10d. We need a way to refer to dynamically created parameters -- and it can't be the static parameter name.
 *      10e. We *could* arrange for unique name for parameters inside a machine, so that we wouldn't need to
 *           refer to parameters by their address.
 *    - Well, do we want to supply Bounds for structure ELEMENTS?  Uh-oh -- we might!
 * 11. [DONE] Convert all of distribution-operations.H to the parser.
 * 12. [DONE] Remove arity argument to def_function.
 * 13. Rationalize Programs, Modules.
 *     13a. [DONE] Allow loading stuff from files.
 *     13b. [DONE] Allow importing, desugaring, and thus resolving symbols after modules are (jointly) loaded into the machine.
 *     13c. Remove any earlier attempts at importing.
 * 14. [DONE] Process imports
 *     + [DONE] 14a. Process mutually dependent modules.
 *     + [DONE] 15b. Note that clashing declarations are allowed if the symbol is unreferenced!
 * 15. Handle 'where' clauses (e.g. in "alt")
 * 16. Handle guards clauses (e.g. in gdrhs, and gdpat)
 *     + I *think* that guards cannot fail in a way that makes the rule fail and move to the next rule.
 *     + If so, the guards can be processed as a special RHS.
 * 17. Compute the entire probability expression, instead of adding pieces incrementally.
 * 18. Make Context load an entire program, instead of adding pieces incrementally.
 *     19. Move the Program from Context to reg_heap.
 * 20. [DONE] Load builtins from a file.
 *     20a. Convert builtins to new framework.
 * 21. [DONE] Add computed loggers.
 *     (This will allow us to e.g. select min/max functions for logging.)
 * 22. [DONE] Add function to clean up fully resolved symbols to make things look nicer.
 * 23. Print expressions with fixity.
 */


Module make_Prelude()
{
  // See http://www.haskell.org/onlinereport/standard-prelude.html
  Module P("Prelude");

  P.def_function("intToDouble", lambda_expression( Conversion<int,double>() ) ); 
  P.def_function("mkArray", lambda_expression( MkArray() ) ); 
  P.def_function("negate", lambda_expression( Negate() ) );
  P.def_function("exp", lambda_expression( Exp_Op() ) );
  P.def_function("!", lambda_expression( GetIndex() ) );
  P.def_function("getAddress", lambda_expression( Get_Address() ) );

  P.def_constructor("True",0);
  P.def_constructor("False",0);
  P.def_constructor("Just",1);
  P.def_constructor("Nothing",0);

  P.def_constructor("IOAction1",2);
  P.def_constructor("IOAction2",3);
  P.def_constructor("IOAction3",4);
  P.def_constructor("IOAction4",5);

  P.def_constructor("IOReturn",1);
  P.def_constructor("IOAndPass",2);
  P.def_constructor("IOAnd",2);

  // FIXME? IOAction 0 doesn't work, because we don't get a separate cell for each application... to nothing.
  //        Current approach: supply dummy arguments to such a builtin that are not used.

  //------------------------------------------------------------------------------------------------//


  // Is this right?

  P.def_function("*", lambda_expression( Multiply() ) );
  P.def_function("/", lambda_expression( Divide() ) );
  P.def_function("+", lambda_expression( Add() ) ); 
  P.def_function("-", lambda_expression( Minus() ) );

  // this needs to be added as a constructor expression
  // ":" is builtin, but has precedence 5 and right fixity.

  P.def_function("==", lambda_expression( Equals() ) );
  P.def_function("/=", lambda_expression( NotEquals() ) );
  P.def_function("<", lambda_expression( LessThan() ) );
  P.def_function(">", lambda_expression( GreaterThan() ) );
  
  // [ We could do this as two nested fmaps, instead. ]
  // [ We could factor out to_double(v2), and 1.0/to_double(v2)

  // FIXME - we have an problem with types here.  This will only work for Int, as-is.
  //  P += "{enumFromThen x y = ... }";
  //  P += "{enumFromThenTo x y z = ... }";

  P.def_function("doubleToLogDouble", lambda_expression( Conversion<double,log_double_t>() ) );

  P.def_function("sizeOfVectorUnsigned", lambda_expression( VectorSizeOp<unsigned>() ) );
  //--------------------------------------- listFromVectorInt ----------------------------------------//
  P.def_function("getVectorIntElement", lambda_expression( BuiltinGetVectorIndexOp<int,Int>() ) ); 
  P.def_function("sizeOfVectorInt", lambda_expression( VectorSizeOp<int>() ) );

  //--------------------------------------- listFromString ----------------------------------------//
  P.def_function("getStringElement", lambda_expression( BuiltinGetStringIndexOp() ) ); 
  P.def_function("sizeOfString", lambda_expression( StringSizeOp() ) );


  //--------------------------------------- listFromVectorVectorInt ----------------------------------------//
  P.def_function("getVectorVectorIntElement", lambda_expression( BuiltinGetVectorIndexOp<Vector<int>,Vector<int>>() ) ); 
  P.def_function("sizeOfVectorVectorInt", lambda_expression( VectorSizeOp<Vector<int>>() ) );

  //--------------------------------------- listFromVectorVectorInt ----------------------------------------//
  P.def_function("getVectorvectorIntElement", lambda_expression( BuiltinGetVectorIndexOp<vector<int>,Vector<int>>() ) ); 
  P.def_function("sizeOfVectorvectorInt", lambda_expression( VectorSizeOp<vector<int>>() ) );

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

  return P;
}

const Module& get_Prelude()
{
  static const Module P = make_Prelude();
  return P;
}

Module Distribution_Functions(const vector<string>&);
Module Range_Functions(const vector<string>&);
Module SModel_Functions(const vector<string>&);
Module PopGen_Functions(const vector<string>&);


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

fs::path get_relative_path(const vector<string>& v)
{
  fs::path file_path = v[0];
  for(int i=1;i<v.size();i++)
    file_path /= v[i];
  return file_path;
}

expression_ref load_module_from_file(const vector<string>& module_root_paths, const string& modid)
{
  vector<string> file_path = get_haskell_identifier_path(modid);
  file_path.back() += ".hs";

  try
  {
    fs::path filename = find_file_in_path(module_root_paths, get_relative_path(file_path) );
    string file_contents = read_file(filename.string(),"module");
    expression_ref module = parse_bugs_file(file_contents);
    return module;
  }
  catch (myexception& e)
  {
    e.prepend("Loading modules: ");
    throw e;
  }
}

Module load_module(const vector<string>& modules_path, const string& modid)
{
  Module M(modid);
  if (modid == "Prelude")
    M = get_Prelude();
  else if (modid == "Distributions")
    M = Distribution_Functions(modules_path);
  else if (modid == "Range")
    M = Range_Functions(modules_path);
  else if (modid == "SModel")
    M = SModel_Functions(modules_path);
  else if (modid == "PopGen")
    M = PopGen_Functions(modules_path);

  expression_ref module = load_module_from_file(modules_path,modid);

  M += module;

  if (M.name != modid)
    throw myexception()<<"Module file '"<<modid<<".hs' contains different module '"<<M.name<<"'";

  return M;
}

vector<Module> load_modules(const vector<string>& modules_path, const vector<string>& module_names)
{
  vector<Module> P;
  for(const string& name: module_names)
    P.push_back(load_module(modules_path,name));
  return P;
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

expression_ref load_builtin(const vector<string>& builtins_path, const string& filename, int n, const string& fname)
{
  // \todo:windows Make this depend on the operating system
  const string extension = ".so";

  fs::path filepath = find_file_in_path(builtins_path, filename + extension);
  return load_builtin(filepath.string(), n, fname);
}

expression_ref load_builtin(const string& filename, int n, const string& fname)
{
    // load the triangle library
  void* library = dlopen(filename.c_str(), RTLD_LAZY);
  if (not library)
    throw myexception() << "Cannot load library: " << dlerror();

  // reset errors
  dlerror();
    
  // load the symbols
  void* fn =  dlsym(library, "builtin_function");
  const char* dlsym_error = dlerror();
  if (dlsym_error)
    throw myexception() << "Cannot load symbol create: " << dlsym_error;
    
  // Create the operation
  OperationFn O(fn, n, fname);

  // Create the function body from it.
  return lambda_expression(O);
}
