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

reg_heap::root_t context::add_identifier(const string& name)
{
  if (find_parameter(name) != -1)
    throw myexception()<<"Cannot add identifier '"<<name<<"': there is already a parameter with that name.";

  return memory->add_identifier_to_context(token,name);
}

// FIXME! This will use multiple names for objects if they occur twice.
expression_ref resolve_refs(const Program& P, const expression_ref& E)
{
  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (object_ptr<const parameter> p = is_a<parameter>(E))
  {
    string qualified_name = P.lookup_symbol(p->parameter_name).name;

    return parameter(qualified_name);
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (object_ptr<const var> V = is_a<var>(E))
  {
    string qualified_name = P.lookup_symbol(V->name).name;

    return var(qualified_name);
  }

  // Other constants have no parts, and don't need to be resolved
  if (not E->size()) return E;

  // Resolve the parts of the expression
  object_ptr<expression> V ( new expression(*E) );
  for(int i=0;i<V->size();i++)
    V->sub[i] = resolve_refs(P, V->sub[i]);

  return V;
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
    // FIXME - change to is_a<lambda2>?
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

// FIXME - change argument to closure?
void context::set_parameter_value(int index, const expression_ref& O)
{
  object_ref v = O->head;
  assert(not O->size());
  assert(not dynamic_pointer_cast<const index_var>(v));
  assert(not dynamic_pointer_cast<const reg_var>(v));
  assert(not dynamic_pointer_cast<const var>(v));
  set_parameter_value_(index, v);
}

// FIXME - change argument to closure?
void context::set_parameter_value_expression(int index, const expression_ref& O)
{
  set_parameter_value_(index, preprocess(O) );
}

// FIXME - change argument to closure?
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

  P.modify()->declare_parameter(name);

  assert(name.size() != 0);
  assert(find_parameter(name) == -1);

  int index = n_parameters();

  root_t r = allocate_reg();
  parameters().push_back( r );

  access(*r).changeable = true;
  set_C(*r, parameter(name) );

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

// FIXME! This will use multiple names for objects if they occur twice.
expression_ref context::translate_refs(const expression_ref& E, vector<int>& Env) const
{
  int reg = -1;

  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (object_ptr<const parameter> P = is_a<parameter>(E))
  {
    int param_index = find_parameter(P->parameter_name);
    
    if (param_index == -1)
      throw myexception()<<"Can't translate undefined parameter '"<<P->parameter_name<<"' in expression!";

    reg = *parameters()[param_index];
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (object_ptr<const var> V = is_a<var>(E))
  {
    string qualified_name = P->lookup_symbol(V->name).name;
    auto loc = identifiers().find( qualified_name );
    if (loc == identifiers().end())
      throw myexception()<<"Can't translate undefined identifier '"<<V->name<<"' in expression!";

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

context& context::operator+=(const Program& P2)
{
  // FIXME - this is really creating a combined program, not just importing aliases!
  // At this level, aliases should be overwritten with local function bodies.
  // Aliases are really undefined functions!

  // Import the symbols in P2 into our symbol table, and add aliases.
  P.modify()->import_module(P2, P2.module_name, false);

  // Give each identifier a pointer to an unused location
  for(const auto& s: P2.get_symbols())
  {
    const symbol_info& S = s.second;

    if (S.scope != local_scope) continue;

    if (S.symbol_type != variable_symbol and S.symbol_type != constructor_symbol) continue;

    if (identifiers().count(S.name))
      throw myexception()<<"Trying to define symbol '"<<S.name<<"' that is already defined in module '"<<P->module_name<<"'";

    add_identifier(S.name);
  }

  // Use these locations to translate these identifiers, at the cost of up to 1 indirection per identifier.
  for(const auto& s: P2.get_symbols())
  {
    const symbol_info& S = s.second;

    if (S.scope != local_scope) continue;

    if (S.symbol_type != variable_symbol and S.symbol_type != constructor_symbol) continue;

    // get the root for each identifier
    map<string, root_t>::iterator loc = identifiers().find(S.name);
    assert(loc != identifiers().end());
    root_t r = loc->second;
    int R = *r;

    expression_ref F = P2.get_function(S.name);

    // (1) find free dummies (must be named)
    // (2) substitute for them!
    // That should work, although it is NOT the most efficient mechanism!

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
  notes     = C.notes;

  return *this;
}

context::context()
  :memory(new reg_heap()),
   P(new Program("Main")),
   token(memory->get_unused_token())
{ 
  (*this) += get_Prelude();
}

context::context(const vector<expression_ref>& N)
  :memory(new reg_heap()),
   P(new Program("Main")),
   token(memory->get_unused_token())
{
  (*this) += get_Prelude();

  // 1. Create the parameters
  std::set<string> names = find_declared_parameters(N);
  if (not includes(names, find_named_parameters(N)))
    throw myexception()<<"Some parameter is referenced, but not declared, at model creation!";
  
  for(const auto& name: names)
    add_parameter(name);

  // 2. Add the notes refering to the parameters.
  add_notes(N);

  // 3. Then set all default values.
  for(int i=0;i<n_parameters();i++)
    set_parameter_value(i, default_parameter_value(i));
}

context::context(const vector<expression_ref>& N, const Program& P1)
  :memory(new reg_heap()),
   P(new Program("Main")),
   token(memory->get_unused_token())
{
  (*this) += get_Prelude();
  (*this) += P1;

  // 1. Create the parameters
  std::set<string> names = find_declared_parameters(N);
  if (not includes(names, find_named_parameters(N)))
    throw myexception()<<"Some parameter is referenced, but not declared, at model creation!";
  
  for(const auto& name: names)
    add_parameter(name);

  // 2. Add the notes refering to the parameters.
  add_notes(N);

  // 3. Then set all default values.
  for(int i=0;i<n_parameters();i++)
    set_parameter_value(i, default_parameter_value(i));
}

context::context(const vector<expression_ref>& N, const Program& P1, const Program& P2)
  :memory(new reg_heap()),
   P(new Program("Main")),
   token(memory->get_unused_token())
{
  (*this) += get_Prelude();
  (*this) += P1;
  (*this) += P2;

  // 1. Create the parameters
  std::set<string> names = find_declared_parameters(N);
  if (not includes(names, find_named_parameters(N)))
    throw myexception()<<"Some parameter is referenced, but not declared, at model creation!";
  
  for(const auto& name: names)
    add_parameter(name);

  // 2. Add the notes refering to the parameters.
  add_notes(N);

  // 3. Then set all default values.
  for(int i=0;i<n_parameters();i++)
    set_parameter_value(i, default_parameter_value(i));
}

context::context(const context& C)
  :Model_Notes(C),
   memory(C.memory),
   P(C.P),
   token(memory->copy_token(C.token))
{ }

context::~context()
{
  memory->release_token(token);
}

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
  else
    return object_ref();
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

#include "probability/distribution-operations.H"
#include "computation/operations.H"

// note = (~ x D)
// Pr_i = case D of ((prob_density _ density_op quantile_op), args) -> (density_op x args)

// Maybe we should always just compute a new probability expression from scratch?
// Then we would always know that our notes were consistent!
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
    expression_ref Pr_i = case_expression(D, Tuple((prob_density,_,density,_),args), (density, args, x));
    
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

