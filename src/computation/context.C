#include "computation/context.H"
#include "prelude.H"
#include "program.H"
#include "let-float.H"

using std::string;
using std::vector;
using std::map;
using std::pair;
using std::set;

using std::cerr;
using std::endl;

closure let_float(closure&& C)
{
  C.exp = let_float(expression_ref(C.exp));
  return C;
}

closure graph_normalize(closure&& C)
{
  C.exp = graph_normalize(expression_ref(C.exp));
  return C;
}

closure indexify(closure&& C)
{
  C.exp = indexify(expression_ref(C.exp));
  return C;
}

closure trim_normalize(closure&& C)
{
  C.exp = trim_normalize(expression_ref(C.exp));
  return C;
}

closure Fun_normalize(closure&& C)
{
  C.exp = Fun_normalize(expression_ref(C.exp));
  return C;
}

closure context::preprocess(const closure& C) const
{
  assert(let_float(C.exp)->print() == let_float(let_float(C.exp))->print());
  //  return trim_normalize( indexify( Fun_normalize( graph_normalize( let_float( translate_refs( closure(C) ) ) ) ) ) );
  return trim_normalize( indexify( graph_normalize( let_float( translate_refs( closure(C) ) ) ) ) );
}

string context::parameter_name(int i) const
{
  const closure& C = access(*parameters()[i]).C;
  if (object_ptr<const parameter> P = is_a<parameter>(C.exp))
  {
    return P->parameter_name;
  }
  throw myexception()<<"Parameter "<<i<<" is not a parameter: can't find name!";
}

reg_heap::root_t context::add_identifier(const string& name) const
{
  if (find_parameter(name) != -1)
    throw myexception()<<"Cannot add identifier '"<<name<<"': there is already a parameter with that name.";

  return memory->add_identifier_to_context(token,name);
}

int context::add_note(const expression_ref& E)
{
  return Model_Notes::add_note(resolve_refs(*P,E));
}


void context::rename_parameter(int i, const string& new_name)
{
  string old_name = parameter_name(i);

  int R = *parameters()[i];

  assert( access(R).changeable == true );
  set_C(R, parameter(new_name) );
}

bool context::reg_is_fully_up_to_date(int R) const
{
  if (not access(R).result) return false;

  const closure& result = access_result(R);

  // NOTE! result cannot be an index_var.
  const expression_ref& E = result.exp;

  // Therefore, if the result is atomic, then R is up-to-date.
  if (not E->size()) return true;

  // If the result is a lambda function, then R is up-to-date.
  if (not is_a<constructor>(E)) return true;

  // If we get here, this had better be a constructor!
  assert(is_a<constructor>(E));

  // Check each component that is a index_var to see if its out of date.
  for(int i=0;i<E->size();i++)
  {
    // assert_cast
    object_ptr<const index_var> V = assert_is_a<index_var>(E->sub[i]);
    int R2 = result.lookup_in_env( V->index );
    
    if (not reg_is_fully_up_to_date(R2)) return false;
  }

  // All the components must be fully up-to-date, so R is fully up-to-date.
  return true;
}

bool context::compute_expression_is_up_to_date(int index) const
{
  int& H = *heads()[index];

  return reg_is_fully_up_to_date(H);
}

// Is there a way to generalize the updating of reg_var elements of structures,
// when incremental evaluation walks a reg_var chain?

expression_ref context::full_evaluate(int& R) const
{
  R = incremental_evaluate(R);
  const closure& result = access_result(R);

  {
    // NOTE! result cannot be a index_var.
    
    // Therefore, if the result is atomic, then we are done.
    if (not result.exp->size()) return result.exp;

    // If the result is a lambda function, then we are done.
    // (a) if we are going to USE this, we should just call lazy evaluate! (which return a heap variable)
    // (b) if we are going to PRINT this, then we should probably normalize it more fully....?
    if (not is_a<constructor>(result.exp)) return result.exp;
  }

  // If the result is a structure, then evaluate its fields and substitute them.
  {
    object_ptr<expression> E ( result.exp->clone() );
    assert(is_a<constructor>(E));

    for(int i=0;i<E->size();i++)
    {
      object_ptr<const index_var> V = assert_is_a<index_var>(E->sub[i]);
      int R2 = result.lookup_in_env( V->index );

      E->sub[i] = full_evaluate(R2);
    }
    return E;
  }
}

/// Return the value of a particular index, computing it if necessary
closure context::lazy_evaluate(int index) const
{
  int& H = *heads()[index];

  H = incremental_evaluate(H);

  return access_result(H);
}

/// Return the value of a particular index, computing it if necessary
object_ref context::evaluate(int index) const
{
  int& H = *heads()[index];

  H = incremental_evaluate(H);

  return access_result(H).exp->head;
}

expression_ref context::evaluate_structure(int index) const
{
  int& H = *heads()[index];

  H = incremental_evaluate(H);

  return full_evaluate(H);
}

closure context::lazy_evaluate_expression_(closure&& C) const
{
  try {
    int R = *push_temp_head();
    set_C(R, std::move(C) );

    R = incremental_evaluate(R);
    const closure& result = access_result(R);
    
    pop_temp_head();
    return result;
  }
  catch (myexception& e)
  {
    pop_temp_head();
    throw e;
  }
}

object_ref context::evaluate_expression_(closure&& C) const
{
  return lazy_evaluate_expression_(std::move(C)).exp->head;
}

expression_ref context::evaluate_structure_expression_(closure&& C) const
{
  try {
    int R = *push_temp_head();
    set_C(R, std::move(C) );

    expression_ref result = full_evaluate(R);
    pop_temp_head();
    return result;
  }
  catch (myexception& e)
  {
    pop_temp_head();
    throw e;
  }
}

closure context::lazy_evaluate_expression(const expression_ref& E) const
{
  return lazy_evaluate_expression_( preprocess(E) );
}

object_ref context::evaluate_expression(const expression_ref& E) const
{
  return evaluate_expression_( preprocess(E) );
}

expression_ref context::evaluate_structure_expression(const expression_ref& E) const
{
  return evaluate_structure_expression_( preprocess(E) );
}

bool context::parameter_is_set(int index) const
{
  assert(index >= 0 and index < parameters().size());

  int P = *parameters()[index];

  if (not access(P).result and not access(P).call) return false;

  return true;
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
object_ref context::get_parameter_value(int index) const
{
  assert(index >= 0 and index < parameters().size());

  int P = *parameters()[index];

  if (not access(P).result)
  {
    // If there's no result AND there's no call, then the result simply hasn't be set, so return NULL.
    if (not access(P).call) return object_ref();

    // If the value needs to be computed (e.g. its a call expression) then compute it.
    incremental_evaluate(P);
  }

  return access_result(P).exp->head;
}

/// Get the value of a non-constant, non-computed index
object_ref context::get_parameter_value(const std::string& name) const
{
  int index = find_parameter(name);
  if (index == -1)
    throw myexception()<<"Cannot find parameter called '"<<name<<"'";

  return get_parameter_value(index);
}

void context::set_parameter_value(int index, const expression_ref& O)
{
  object_ref v = O->head;
  assert(not O->size());
  assert(not dynamic_pointer_cast<const index_var>(v));
  assert(not dynamic_pointer_cast<const reg_var>(v));
  assert(not dynamic_pointer_cast<const var>(v));
  set_parameter_value_(index, v);
}

void context::set_parameter_value_expression(int index, const expression_ref& O)
{
  if (O)
    set_parameter_value_(index, preprocess(O) );
  else
    set_parameter_value_(index, {} );
}

void context::set_parameter_value_(int index, closure&& C)
{
  int P = *parameters()[index];

  set_reg_value(P, std::move(C) );
}

void context::set_reg_value(int P, closure&& C)
{
  return memory->set_reg_value(P, std::move(C), token);
}

/// Update the value of a non-constant, non-computed index
void context::set_parameter_value(const std::string& var, const expression_ref& O)
{
  set_parameter_value(find_parameter(var), O);
}

int context::n_parameters() const
{
  return parameters().size();
}

int context::find_parameter(const string& s) const
{
  for(int i=0;i<n_parameters();i++)
    if (parameter_name(i) == s)
      return i;

  return -1;
}

int context::add_parameter(const string& name)
{
  if (not is_haskell_var_name(name))
    throw myexception()<<"Parameter name '"<<name<<"' is not a Haskell variable name";

  string module_name = get_module_name(name);
  string var_name = get_unqualified_name(name);
  string full_name = name;
  if (module_name == "" or module_name == P->module_name)
  {
    module_name = P->module_name;
    full_name = module_name + "." + var_name;
    P.modify()->declare_parameter(var_name);
  }
  else
  {
    // FIXME: Right now the main program has a name.  What if the parameter is in that specific module?
    Program M(get_module_name(name));
    M.declare_parameter(get_unqualified_name(name));
    P.modify()->import_module(M, false);
  }

  assert(full_name.size() != 0);
  assert(find_parameter(full_name) == -1);

  int index = n_parameters();

  root_t r = allocate_reg();
  parameters().push_back( r );

  access(*r).changeable = true;
  set_C(*r, parameter(full_name) );

  return index;
}

/// Add an expression that may be replaced by its reduced form
int context::add_compute_expression(const expression_ref& E)
{
  return add_compute_expression_( preprocess(E) );
}

/// Add an expression that may be replaced by its reduced form
int context::add_compute_expression_(closure&& C)
{
  root_t r = allocate_reg();
  set_C( *r, std::move(C) );

  heads().push_back( r );
  return heads().size() - 1;
}

/// Change the i-th compute expression to e
void context::set_compute_expression(int i, const expression_ref& E)
{
  set_compute_expression_( i, preprocess(E) );
}

/// Change the i-th compute expression to e
void context::set_compute_expression_(int i, closure&& C)
{
  root_t r2 = allocate_reg();
  set_C( *r2, std::move(C) );

  root_t r1 = heads()[i];
  heads()[i] = r2;
  pop_root(r1);
}


int context::n_expressions() const
{
  return heads().size();
}

expression_ref context::get_expression(int i) const
{
  int H = *heads()[i];
  return reg_var(H);
}

void context::pop_temp_head() const
{
  memory->pop_temp_head( token );
}

void context::alphabetize_parameters()
{
  vector<string> names;
  for(int i=0;i<n_parameters();i++)
    names.push_back( parameter_name(i) );

  vector<string> names2 = names;
  std::sort(names2.begin(), names2.end());

  vector<int> mapping = compute_mapping(names, names2);

  parameters() = apply_mapping(parameters(), mapping);
}

void context::collect_garbage() const
{
  memory->collect_garbage();
}

expression_ref context::translate_refs(const expression_ref& E, vector<int>& Env) const
{
  int reg = -1;

  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (object_ptr<const parameter> p = is_a<parameter>(E))
  {
    string qualified_name = P->lookup_symbol(p->parameter_name).name;

    int param_index = find_parameter(qualified_name);
    
    if (param_index == -1)
      throw myexception()<<"Can't translate undefined parameter '"<<qualified_name<<"' ('"<<p->parameter_name<<"') in expression!";

    reg = *parameters()[param_index];
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (object_ptr<const var> V = is_a<var>(E))
  {
    string qualified_name = P->lookup_symbol(V->name).name;
    auto loc = identifiers().find( qualified_name );
    if (loc == identifiers().end())
    {
      if (is_haskell_builtin_con_name(V->name))
      {
	symbol_info S = P->lookup_builtin_symbol(V->name);
	add_identifier(S.name);
      
	// get the root for each identifier
	loc = identifiers().find(S.name);
	assert(loc != identifiers().end());
	
	root_t r = loc->second;
	int R = *r;
	
	assert(R != -1);
	set_C(R, preprocess(S.body) );
      }
      else
	throw myexception()<<"Can't translate undefined identifier '"<<V->name<<"' in expression!";
    }

    reg = *(loc->second);
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (object_ptr<const reg_var> RV = is_a<reg_var>(E))
    reg = RV->target;

  if (reg != -1)
  {
    int index = Env.size();
    Env.insert(Env.begin(), reg);

    return new index_var(index);
  }

  // Other constants have no parts, and don't need to be translated
  if (not E->size()) return E;

  // Translate the parts of the expression
  object_ptr<expression> V ( new expression(*E) );
  for(int i=0;i<V->size();i++)
    V->sub[i] = translate_refs(V->sub[i], Env);

  return V;
}

closure context::translate_refs(closure&& C) const
{
  closure C2 = C;
  C2.exp = translate_refs(C2.exp, C2.Env);
  return C2;
}

const vector<string>& context::get_module_path() const
{
  return module_path_;
}

context& context::operator+=(const std::vector<string>& module_names)
{
  return operator+=(load_modules(get_module_path(), module_names));
}

context& context::operator+=(const std::vector<Program>& P2)
{
  // FIXME - this is really creating a combined program, not just importing aliases!
  // At this level, aliases should be overwritten with local function bodies.
  // Aliases are really undefined functions!

  // Import the symbols in P2 into our symbol table, and add aliases.
  for(const auto p: P2)
    P.modify()->import_module(p, false);

  // Give each identifier a pointer to an unused location
  for(const auto p: P2)
    for(const auto& s: p.get_symbols())
    {
      const symbol_info& S = s.second;
      
      if (S.scope != local_scope) continue;
      
      if (S.symbol_type != variable_symbol and S.symbol_type != constructor_symbol) continue;
      
      if (identifiers().count(S.name))
	throw myexception()<<"Trying to define symbol '"<<S.name<<"' that is already defined in module '"<<P->module_name<<"'";
      
      add_identifier(S.name);
    }

  // Use these locations to translate these identifiers, at the cost of up to 1 indirection per identifier.
  for(const auto p: P2)
    for(const auto& s: p.get_symbols())
    {
      const symbol_info& S = s.second;
      
      if (S.scope != local_scope) continue;
      
      if (S.symbol_type != variable_symbol and S.symbol_type != constructor_symbol) continue;
      
      // get the root for each identifier
      map<string, root_t>::iterator loc = identifiers().find(S.name);
      assert(loc != identifiers().end());
      root_t r = loc->second;
      int R = *r;
      
      expression_ref F = p.get_function(S.name);
      
      assert(R != -1);
      set_C(R, preprocess(F) );
    }

  return *this;
}

context& context::operator=(const context& C)
{
  memory->release_token(token);
  
  memory = C.memory;
  token = memory->copy_token(C.token);
  P = C.P;
  notes = C.notes;

  return *this;
}

context::context(const vector<string>& module_path)
  :context(module_path,{},vector<Program>{})
{  }

// FIXME - this should be shared with Model::add_submodel( ), but we need to call Model::add_notes( ).
// If a model were to HOOK (a) add_parameter, (b) add_note( ), and (c) set_parameter_value( )
//  then this routine could work on both.
vector<int> add_submodel(context& C, const vector<expression_ref>& N)
{
  vector<int> new_parameters;

  // 1. Find and the declared parameter names
  std::set<string> declared_parameter_names = find_declared_parameters(N);
  if (not includes(declared_parameter_names, find_named_parameters(N)))
    throw myexception()<<"Some parameter is referenced, but not declared, at model creation!";

  // 2. Add the declared parameters
  for(const auto& name: declared_parameter_names)
    if (C.find_parameter(name) == -1)
    {
      int index = C.add_parameter(name);
      new_parameters.push_back(index);
    }
    else
      throw myexception()<<"Submodel declares existing parameter '"<<name<<"'!";
  
  // 3. Add the notes from this model to the current model.
  for(const auto& n: N)
    C.add_note(n);
  
  // 4. Set default values for newly declared parameters
  for(int index: new_parameters)
    if (not C.parameter_is_set(index))
      C.set_parameter_value_expression(index, C.default_parameter_value(index));

  return new_parameters;
}

context::context(const vector<string>& module_path, const vector<expression_ref>& N)
  :context(module_path, N,vector<Program>{})
{ }

context::context(const vector<string>& module_path, const vector<expression_ref>& N, const vector<Program>& Ps)
  :memory(new reg_heap()),
   P(new Program("Main")),
   token(memory->get_unused_token()),
   module_path_(module_path)
{
  (*this) += {"Prelude"};
  (*this) += Ps;

  add_submodel(*this, N);
}

context::context(const vector<string>& module_path, const vector<expression_ref>& N, const vector<string>& module_names)
  :context(module_path,N,load_modules(module_path,module_names))
{ }

context::context(const context& C)
  :Model_Notes(C),
   memory(C.memory),
   P(C.P),
   token(memory->copy_token(C.token)),
   module_path_(C.module_path_)
{ }

context::~context()
{
  memory->release_token(token);
}

#include "probability/distribution-operations.H"
#include "computation/operations.H"

expression_ref context::default_parameter_value(int i) const
{
  expression_ref default_value = lambda_expression(constructor("DefaultValue",2));

  vector<expression_ref> results;
  expression_ref query = (default_value, parameter( parameter_name(i) ), match(0));
  int found = find_match_notes(query, results, 0);

  if (found != -1)
  {
    assert(results.size());
    expression_ref value = results[0];
    //    assert(find_match_notes(query, results, found+1) == -1);
    return value;
  }

  results.clear();
  expression_ref query2 = (distributed, parameter( parameter_name(i) ), match(0));
  int found2 = find_match_notes(query2, results, 0);

  if (found2 != -1)
  {
    expression_ref _ = dummy(-1);
    expression_ref dist = results[0];
    expression_ref value = case_expression(results[0],Tuple((prob_density,_,_,_,v1,_),v2),(v1,v2));
    return value;
  }

  return {};
}

reg_heap::root_t context::push_temp_head() const
{
  return memory->push_temp_head( token );
}

std::ostream& operator<<(std::ostream& o, const context& C)
{
  for(int index = 0;index < C.n_expressions(); index++)
  {
    o<<index<<" "<<C.get_expression(index);
    o<<"\n";
  }
  return o;
}

// note = (~ x D)
// Pr_i = case D of ((prob_density _ density_op quantile_op _ _), args) -> (density_op x args)

// Maybe we should always just compute a new probability expression from scratch?
// Then we would always know that our notes were consistent!
// TODO: Move to model.{H,C}?
int add_probability_expression(context& C)
{
  expression_ref query = (distributed, match(0), match(1));

  expression_ref Pr;

  // Check each expression in the Formula
  map<string,string> prior_expressions;
  for(int i=0;i<C.n_notes();i++)
  {
    vector<expression_ref> results; 

    // If its a probability expression, then...
    if (not find_match(query, C.get_note(i), results)) continue;

    // Extract the density operation
    expression_ref x = results[0];
    expression_ref D = results[1];

    expression_ref density = dummy(0);
    expression_ref args = dummy(1);
    expression_ref _ = dummy(-1);

    if (prior_expressions.count(x->print()))
      throw myexception()<<"Duplicate prior expression for variable '"<<x->print()<<"'";
    prior_expressions[x->print()] = D->print();
    
    // Create an expression for calculating the density of these random variables given their inputs
    expression_ref Pr_i = case_expression(D, Tuple((prob_density,_,density,_,_,_),args), (density, args, x));
    
    // Extend the probability expression to include this term also.
    // (FIXME: a balanced tree could save computation time)
    if (not Pr)
      Pr = Pr_i;
    else
      Pr = Pr_i * Pr;
  }

  // If this model has random variables... 
  if (Pr)
  {
    return C.add_compute_expression(Pr);
  }
  else
    return -1;
}

