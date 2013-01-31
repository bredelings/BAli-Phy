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

/* \todo: List of things to do to clean up programs.
 *
 * See list in parser/desugar.C
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
 * 8. Rationalize Model_Notes, formula_expression_ref, and program?
 *    - I note that a "model" compresses a complex expression into Model.main.
 *    - [DONE] Add Model_Notes to Module.
 *    - Stop allowing the creation of parameters that aren't in modules?
 *    - Define some (all?) things in a Module instead of a formula_expression_ref.
 *    - [DONE] Allow formula_expression_ref's to import things using import notes and import_submodel notes.
 *    - [DONE] Allow creating formula_expression_ref's from modules => refer to Module.main, and import the Module.
 *      + Perhaps when prefixing a module, we don't want to prefix its FUNCTIONS,
 *        just its parameters and notes?
 *      + No, we do want to prefix its functions, because they might refer to parameters.
 *      + Basically, a f.e.r. should be a set of definitions (w/ imports) and a 'main' routine?
 *        - How does this relate to simply bringing in the notes attached to parameters?
 *        - How does this attempt to give parameters default names relate to the more general approach?
 *      + Problem: code generation can currently be done, by passing numbers of model names as arguments to models.
 *                 These arguments are used to generate a different model.
 *                 But models define in a module file can only take function arguments, not arguments that affect the code
 *                   in the module.
 *    - Eliminate parameter definition notes in formula_expression_ref?
 *    - [DONE] Eliminate C++ operators on formula_expression_ref -> use parser instead.
 *    - [DONE] Eliminate C++ operators on expression_ref -> use parser instead.
 *    - [DONE] Make (f,g) only create an apply expression, but NOT substitute.
 *    - [DONE] Make f+g simply append g to f->sub.
 *    - [DONE] Make f*g substitute into f.
 * 9. Try to rewrite models into BUGS modules/models.
 *    - M8b? [ Issue - how many sub-categories, though? ]
 *    - branch-site [ Issue - how to specify F3x4, HKY?  Issue - how to specify number of conservation categories? ]
 *    - branch-site [ Issue - how to make the weight on "neutral" not *depend* on the number of conservation categories? ]
 *    - Problems: 
 *      + [PROBLEM] Allowing the dimension of distributions to depend on arguments.
 *      + Frequency defaults - how can we specify these?
 *      + Submodel parameters - these need separate parsing to coerce them to the right type.
 *        - But is that a problem?
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
 *      10f. Issue: if we have a structure of parameters, that's OK.  But what if we have a CHANGEABLE
 *                  structure, of changeable parameters!  Is that OK?
 *    - Well, do we want to supply Bounds for structure ELEMENTS?  Uh-oh -- we might!
 * 11. [DONE] Convert all of distribution-operations.H to the parser.
 * 12. [DONE] Remove arity argument to def_function.
 * 13. Rationalize Programs, Modules.
 *     13a. [DONE] Allow loading stuff from files.
 *     13b. [DONE] Allow importing, desugaring, and thus resolving symbols after modules are (jointly) loaded into the machine.
 *     13c. Remove any earlier attempts at importing.
 * 14. [DONE] Process imports
 *     + [DONE] 14a. Process mutually dependent modules.
 *     + [DONE] 14b. Note that clashing declarations are allowed if the symbol is unreferenced!
 * 15. Handle 'where' clauses (e.g. in "alt")
 * 16. Handle guards clauses (e.g. in gdrhs, and gdpat)
 *     + I *think* that guards cannot fail in a way that makes the rule fail and move to the next rule.
 *       (The 'otherwise' rule might be special, though??)
 *     + If failure of all guards leads to failure, then can the guards can be processed as a special RHS?
 *     16b. Handle as-patterns.
 *        case v of x@pat -> body => case v of pat -> (\x->body) v
 *     16c. Handle irrefutable patterns that use ~.
 *        case v of ~h:t -> body
 *     16d. What does it mean (if its true) that irrefutable bindings are only irrefutable at the top level?
 * 17. Compute the entire probability expression at once, instead of adding pieces incrementally.
 * 18. Make Context load an entire program, instead of adding pieces incrementally.
 *     19. Move the Program from Context to reg_heap.
 * 20. [DONE] Load builtins from a file.
 *     20a. Convert builtins to new framework.
 * 21. [DONE] Add computed loggers.
 *     (This will allow us to e.g. select min/max functions for logging.)
 *     21a. [DONE] Find a haskell expression to log [p1,p2,p3] based on the order of [q1,q2,q3]
 * 22. [DONE] Add function to clean up fully resolved symbols to make things look nicer.
 * 23. (?) Print expressions with fixity.
 * 24. Allow the creation, destruction, initialization, ranges, and MCMC of unnamed parameters.
 *     24a. Perhaps the letter frequencies would be a good first example of this.
 *     24b. The different categories in the branch-site model would be a good second example.
 *     24c. Allow each parameter to be uniquely identified by a number that is not its location.
 *     24d. The parameter will encode this number in its expression, even if it doesn't have a name.
 *     24e. We can then have an IO operation to change the parameter value.
 *     24f. This will allow us to set default values for vector parameters.
 *     24g. But how will we scan the variable parameter list and propose new values?
 *     24h. How will we determine the bounds of these parameters?
 * 25. Allow model files to create models with a variable number of parameters => depends on 24.
 * 26. (?) Allow model files to create models where an argument is the name of another model file.
 * 27. Allow fixing parameters. (e.g. to test the branch-site model under ML)
 * 28. Optimizations
 *     - Perform applications if expression is used only once?
 *     - Remove let bindings for unused variables?
 *     - Merge let bidings with identical bodies?
 *     - Simplify some case expressions based on knowledge of let-bound variable?
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

  P.def_function("sqrt", lambda_expression( Sqrt_Op() ) );

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

void Distribution_Functions(Module&);
void Range_Functions(Module&);
void SModel_Functions(Module&);
void PopGen_Functions(Module&);

expression_ref read_module_from_file(const string& filename)
{
  try
  {
    string file_contents = read_file(filename,"module");
    expression_ref module = parse_bugs_file(file_contents);
    return module;
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

string find_module(const module_loader& L, const string& modid)
{
  try
  {
    fs::path path = get_relative_path_from_haskell_id(modid);
    path.replace_extension(".hs");

    fs::path filename = find_file_in_path(L.modules_path, path );
    return filename.string();
  }
  catch (myexception& e)
  {
    e.prepend("Loading module '"+modid+"': ");
    throw e;
  }
}

Module load_module(const string& filename)
{
  try
  {
    expression_ref module = read_module_from_file(filename);

    Module M(module);
    
    if (M.name == "Prelude")
      make_Prelude(M);
    else if (M.name == "Distributions")
      Distribution_Functions(M);
    else if (M.name == "Range")
      Range_Functions(M);
    else if (M.name == "SModel")
      SModel_Functions(M);
    else if (M.name == "PopGen")
      PopGen_Functions(M);

    return M;
  }
  catch (myexception& e)
  {
    e.prepend("Loading module from file '"+filename+"':\n  ");
    throw e;
  }
}

Module load_module(const module_loader& L, const string& modid)
{
  return load_module(find_module(L, modid));
}

Module load_and_rename_module(const module_loader& L, const string& modid1, const string& modid2)
{
  try
  {
    expression_ref module = read_module_from_file(find_module(L, modid1));

    Module M1(module);

    if (M1.name != modid1)
      throw myexception()<<"Module file '"<<modid1<<".hs' contains different module '"<<M1.name<<"'";

    module = rename_module(module, modid1, modid2);

    Module M2(module);

    return M2;
  }
  catch (myexception& e)
  {
    e.prepend("Loading module '"+modid1+"' as '"+modid2+":\n  ");
    throw e;
  }
}

vector<Module> load_modules(const module_loader& L, const vector<string>& module_names)
{
  vector<Module> P;
  for(const string& name: module_names)
    P.push_back(load_module(L,name));
  return P;
}

vector<Module> load_modules(const module_loader& L, const set<string>& module_names)
{
  vector<Module> P;
  for(const string& name: module_names)
    P.push_back(load_module(L,name));
  return P;
}

vector<Module> load_and_rename_modules(const module_loader& L, const map<string,string>& module_names)
{
  vector<Module> P;
  for(const auto& x: module_names)
    P.push_back(load_and_rename_module(L, x.first, x.second));
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

expression_ref load_builtin(const module_loader& L, const string& filename, int n, const string& fname)
{
  // \todo:windows Make this depend on the operating system
  const string extension = ".so";

  fs::path filepath = find_file_in_path(L.builtins_path, filename + extension);
  return load_builtin(filepath.string(), n, fname);
}

expression_ref load_builtin(const string& filename, int n, const string& fname)
{
  // load the library
  void* library = dlopen(filename.c_str(), RTLD_LAZY);
  if (not library)
    throw myexception() << "Cannot load library: " << dlerror();

  // reset errors
  dlerror();
    
  const string prefix = "builtin_function_";
  string symbol_name = prefix + fs::path(filename).stem().string();

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
