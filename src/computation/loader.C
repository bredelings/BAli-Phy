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
 */

/* \todo: List of things to do to clean up programs.
 *
 * See list in parser/desugar.C
 * See list in models/parameters.C 
 * See list in models/module.C
 *
 * -3. Merging Model::add_submodel(Model_Notes&) and add_submodel(Context&, vector<expression_ref>&)
 *     - Distribution notes need to be present BEFORE parameters are added, because they determine the structure.
 *     - Distribution notes need to be added AFTER parameters, since they set prior_index for the parameter.
 *     - This is related to setting prior_index in both process_note( ) and add_probability_expression( ).
 *
 * -1. Make a model-creation monad.  This could allow us to modify the model after creating it, thus
 *    allowing the specification of default parameters.
 *
 *    - Our primary goal here is to eliminate formula_expression_ref in favor of writing 
 *      readable Haskell model descriptions.
 *    - Model descriptions probably need to be functions, since they need to take arguments.
 *    - These arguments could then affect  e.g. the dimension of distributions and the
 *       dimension of random lists created by the model.
 *
 * 0. Fix the parser to give meaningful error messages, instead of just saying that the entire body doesn't parse.
 *    - This is related to adding a lexer.  Which would also allow comments in the code.
 *
 * 1. Efficiently recalculate the probability when only a few densities change.
 *    - Will this require signals? (Signals might also help us to recalculate mu*t to see if anything changed.)
 *    - This will allow us to avoid maintaining a Markov blanket.
 * 2. (?) Rewrite module loading routines to load modules 1-at-a-time.
 * 3. Make sure we don't read alignments with ^@ characters in the sequences!
 * 4. Eliminate duplication between Model::add_note( ) and add_probability_expression( )
 *    4a. Computing things all at once would be nice, and make balanced *-trees easy.
 *    4b. Doing things incrementally is more monad-like, though.
 * 5. Allow distributions on structures with variable components
 *    5a. Make the computation of default values delayed, so that they can depend on parameters not yet set?
 *    5b. Perform MCMC on structures dynamically, inside the machine.
 *        - This move would walk the parameter, and apply sub-moves to its pieces.
 *          + This method would allow situations where parameters change size.
 *          + The method would also allow determine the bounds for slice moves dynamically.
 *        - We'd like to set the bounds dynamically, instead of only once, statically, before starting MCMC.
 *        - Hmm... If proposals depend on the bounds, that would make the proposals dynamic too!
 *    5c. Log structures with variable components intelligently.
 *        - How shall we name the pieces?  We want piA instead of pi!0.
 *
 * 7. Allow model files to create models where dimension of parameter depends on argument to model.

 * 8. Rationalize Model_Notes, formula_expression_ref, and program?
 *    - I note that a "model" compresses a complex expression into Model.main.
 *    - Remove Model_Notes from Module.
 *    - Allowing the creation of parameters in their own namespace, instead of having names in modules?
 *    - Define some (all?) things in a Module instead of a formula_expression_ref.
 *    - Eliminate import and import_submodel notes.
 *    - Eliminate parameter definition notes in formula_expression_ref?
 *    - [DONE] Allow creating formula_expression_ref's from modules => refer to Module.main, and import the Module.
 *    - The problem with a f.e.r. is that it doesn't have arguments you can substitute into.
 *    - Perhaps when prefixing a module, we don't want to prefix its FUNCTIONS,
 *      just its parameters and notes?
 *    - No, we do want to prefix its functions, because they might refer to parameters.
 *    - Problem:  Models need to be able to take arguments in order to affect distributions and their dimension.
 *        + Therefore, models should be functions!
 *        + Models might be implemented as functions of some internal random state.  We might want to log
 *          functions of the random state, and not the random state itself.  For example, w/ DPP, or a
 *          random tree of with the truncated DPP, etc.

 * 9. Try to rewrite models into BUGS modules/models.
 *    - M8b? [ Issue - how many sub-categories, though? ]
 *    - branch-site [ Issue - how to specify F3x4, HKY?  Issue - how to specify number of conservation categories? ]
 *    - branch-site [ Issue - how to make the weight on "neutral" not *depend* on the number of conservation categories? ]
 *    - Problems: 
 *      + [PROBLEM] Allowing the dimension of distributions to depend on arguments.
 *      + Frequency defaults - how can we specify these?
 *      + Submodel parameters in the M+M+M formulation - these need separate parsing to coerce them to the right type.
 *        - But is that a problem?

 * 10. Eliminate the need for set_branch_mean_tricky( )
 *    - Implement setting D!b only when the change is large enough.

 * 11. [SPEED] For bali-phy 5d.fasta --seed=0 --iter=1000
 *      + Split parameter names out of token roots?  Identifier names also? Program also?
 *      + Speed up remap_reg?  Make sure its inlined?  Make access(R).temp = R so we need no condition?
 *      - Spending 3.5% in evaluate_reg_to_object( ) to do nothing!
 *      - Spending 2% in case calling dynamic_cast from Object->compare( ).
 *      - Spending 1.3% in data_patition::copy()
 *      - Spending 1% in evaluate_reg_to_object() --- why?
 *      - Spending 1% in pop_temp_head()
 *      - Spending 1% in rs07_branch_HMM copying strings!
 *      - Spending 0.5% copying parameter names.
 *      + ------------------------------------------- +
 *      + Why is set_reg_value spending so much time in incremental_evaluate?
 *      + Spending 7.7% in _List_node_base::_M_transfer
 *        - accounts for 6.5% out of 11.81% of reg_heap::release_token( ).
 *      + Spending 1.7% deleting counted_base
 *      + ------------------------------------------- +
 *        + Also calling dynamic_cast from MH_move_iterate.  Total dynamic_cast about 5%.
 *      - MH_Move::iterate( ): 4.5% of CPU time spent checking way too many parameters to see if they are in range.
 *      - data_partition::[copy]: 3.7% total copying things.  For example, suba_index, even if it won't change.
 *      - trace_and_reclaim_unreachable( ) spends 5.1% out of 7.4% in operator new?
 *        + thus, perhaps 
 *      - (We are spending 20% of the time in operator new.)
 *      - We are spending 7% of the time in __ieee754_log_avx.
 *        - There should be a better way to multiply lots of doubles together while avoiding underflow.
 *      - Remove timer_stack things, in hopes that perf will supersed them.
 *      - push_temp_head( ) iterates over all 64 possible tokens??
 *        + Doesn't seem to actually take that much time, though!
 *      - 1% of CPU to spend on memory allocation from vector::vector in three_way_topology_sample?
 *      + We have a problem with needing to remap all heads - there are too many to scan!
 *      - [DONE?] Clear identifiers after loading programs -- Model::compile();
 *      - [DONE] Model::keys: 1% copying this every time we set a parameter.
 *      - [DONE] 3x speed-up by implementing C++ version of vector_from_list
 *      - [DONE] 15% speedup by eliminating roots() in favor of just scanning all the heads in token_roots.
 *      - [DONE] Vector into something that can be used like vector.

 * 13. Rationalize Programs, Modules.
 *     13a. [DONE] Allow loading stuff from files.
 *     13b. [DONE] Allow importing, desugaring, and thus resolving symbols after modules are (jointly) loaded into the machine.
 *     13c. Remove any earlier attempts at importing.
 * 14. Rewrite multi-case code to take patterns in terms of expression_ref's that might be seen from the parser.
 *     + Allows moving towards 16 incrementally.
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
 * 19. Move the Program from Context to reg_heap.
 * 20. [DONE] Load builtins from a file.
 *     20a. Convert builtins to new framework.
 * 21. [DONE] Add computed loggers.
 *     (This will allow us to e.g. select min/max functions for logging.)
 *     21a. [DONE] Find a haskell expression to log [p1,p2,p3] based on the order of [q1,q2,q3]
 * 23. (?) Print expressions with fixity.

 * 27. Allow fixing parameters. (e.g. to test the branch-site model under ML)
 * 28. Make model creation into an IO operation?
 *     - Hmm.... but how would we specify default priors, then?
 * 29. Optimizations
 *     - Perform applications if expression is used only once?
 *     - Remove let bindings for unused variables?
 *     - Merge let bidings with identical bodies?
 *     - Simplify some case expressions based on knowledge of let-bound variable?
 * 30. Can we also maintain parameter/distribution pairs?
 *     - This will allow us to determine the bounds on a parameter, for example.
 * 31. Print out simpler names than Test.i for parameter i.
 *     - I think parameters are in a separate namespace?
 *     - Perhaps put a '*' on the beginning of the name when comparing with the Haskell namespace?
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

