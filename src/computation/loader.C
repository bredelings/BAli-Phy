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

/*
 * DONE:
 *
 * 1. [DONE] Allow defining constructors in files.
 * 2. [DONE] Convert strings to [Char]
 * 3. [DONE] Update probability functions to separate the family from the probability object.
 *    3a. [DONE] Construct the ExpOf transform to make logNormal, logGamma, etc.
 *    3b. [DONE] Choose kernels based on the range, not based on the distribution name.
 * 4. [DONE] Convert Defs to use the machine.
 * 5. [DONE] SYNTAX: replace a ~ b ( c ) with a ~ b
 * 6. [DONE] SYNTAX: external a ~ b [To not declare all parameters]
 *      6a. [DONE] SYNTAX: data a ~ b [Don't treat a as a parameter at all!]
 * 7. [DONE] Allow defs in BUGS files.
 * 8. [DONE] Rationalize C++ operations on expression_ref and formula_expression_ref
 *    - [DONE] Eliminate C++ operators on formula_expression_ref -> use parser instead.
 *    - [DONE] Eliminate C++ operators on expression_ref -> use parser instead.
 *    - [DONE] Make (f,g) only create an apply expression, but NOT substitute.
 *    - [DONE] Make f+g simply append g to f->sub.
 *    - [DONE] Make f*g substitute into f.
 * 9. [DONE] Convert all of distribution-operations.H to the parser.
 * 10. [DONE] Remove arity argument to def_function.
 * 11. [DONE] Process imports
 *     + [DONE] 11a. Process mutually dependent modules.
 *     + [DONE] 11b. Note that clashing declarations are allowed if the symbol is unreferenced!
 * 12. [DONE] Add function to clean up fully resolved symbols to make things look nicer.
 * 13. [DONE] Replace recalc indices with trigers.
 * 14. [DONE] Allow the creation, destruction, initialization, ranges, and MCMC of unnamed parameters.
 * 15. [DONE] Allow printing ints w/o decimal points.
 * 16. [DONE] Make a model-creation monad.
 * 17. [DONE] Eliminate formula_expression_ref.
 * 18. [DONE] Eliminate all notes.
 * 19. [DONE] Add Lexer => allows comments.
 * 20. [DONE] Eliminate module renaming.
 * 21. [DONE] Allow distributions on structures with variable components.
 * 22. [DONE] Name pieces of structures intelligently -- e.g. piA instead of pi!!0
 * 23. [DONE] Allow model files to create models where dimension of parameter depeonds on argument to model.
 * 24. [DONE] Allow creation of parameters in their own namespace.
 */

/* \todo: List of things to do to clean up programs.
 *
 * See list in parser/desugar.C
 * See list in models/parameters.C 
 * See list in models/module.C
 *
 * 1. Efficiently recalculate the probability when only a few densities change.
 *    - Will this require signals? (Signals might also help us to recalculate mu*t to see if anything changed.)
 *    - This will allow us to avoid maintaining a Markov blanket.
 * 2. Make sure we don't read alignments with ^@ characters in the sequences!
 *
 * 3. Eliminate the need for set_branch_mean_tricky( )
 *    - Implement setting D!b only when the change is large enough.
 * 4. Rewrite multi-case code to take patterns in terms of expression_ref's that might be seen from the parser.
 *     + Allows moving towards 16 incrementally.
 * 5. Handle 'where' clauses (e.g. in "alt")
 * 6. Handle guards clauses (e.g. in gdrhs, and gdpat)
 *     + I *think* that guards cannot fail in a way that makes the rule fail and move to the next rule.
 *       (The 'otherwise' rule might be special, though??)
 *     + If failure of all guards leads to failure, then can the guards can be processed as a special RHS?
 *     6b. Handle as-patterns.
 *        case v of x@pat -> body => case v of pat -> (\x->body) v
 *     6c. Handle irrefutable patterns that use ~.
 *        case v of ~h:t -> body
 *     6d. What does it mean (if its true) that irrefutable bindings are only irrefutable at the top level?
 * 7. Compute the entire probability expression at once, instead of adding pieces incrementally.
 * 8. Make Context load an entire program, instead of adding pieces incrementally.
 * 9. Move the Program from Context to reg_heap.

 * 10. Allow fixing parameters. (e.g. to test the branch-site model under ML)
 * 11. How to specify default priors if model creation is an IO operation?
 * 12. Optimizations
 *     - Perform applications if expression is used only once?
 *     - Remove let bindings for unused variables?
 *     - Merge let bidings with identical bodies?
 *     - Simplify some case expressions based on knowledge of let-bound variable?
 * 13. Print out simpler names than Test.i for parameter i.
 *     - I think parameters are in a separate namespace?
 *     - Perhaps put a '*' on the beginning of the name when comparing with the Haskell namespace?
 * 14. Eliminate make_Prelude.
 */


void make_Prelude(Module& P)
{
  // See http://www.haskell.org/onlinereport/standard-prelude.html

  // FIXME? IOAction 0 doesn't work, because we don't get a separate cell for each application... to nothing.
  //        Current approach: supply dummy arguments to such a builtin that are not used.

  //------------------------------------------------------------------------------------------------//


  // [ We could do this as two nested fmaps, instead. ]
  // [ We could factor out to_double(v2), and 1.0/to_double(v2)

  // FIXME - we have an problem with types here.  This will only work for Int, as-is.
  //  P += "{enumFromThen x y = ... }";
  //  P += "{enumFromThenTo x y z = ... }";

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

