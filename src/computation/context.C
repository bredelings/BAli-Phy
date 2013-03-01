#include "computation/context.H"
#include "computation/program.H"
#include "loader.H"
#include "module.H"
#include "let-float.H"
#include "parser/desugar.H"
#include "parser/AST.H"

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

closure resolve_refs(const vector<Module>& P, closure&& C)
{
  C.exp = resolve_refs(P, C.exp);
  return C;
}

closure context::preprocess(const closure& C) const
{
  assert(C.exp);
  assert(let_float(C.exp)->print() == let_float(let_float(C.exp))->print());
  //  return trim_normalize( indexify( Fun_normalize( graph_normalize( let_float( translate_refs( closure(C) ) ) ) ) ) );
  return trim_normalize( indexify( graph_normalize( let_float( translate_refs( resolve_refs(*P, closure(C) ) ) ) ) ) );
}

string context::parameter_name(int i) const
{
  return parameters()[i].first;
}

reg_heap::root_t context::add_identifier(const string& name) const
{
  if (find_parameter(name) != -1)
    throw myexception()<<"Cannot add identifier '"<<name<<"': there is already a parameter with that name.";

  return memory->add_identifier_to_context(token,name);
}

int context::add_note(const expression_ref& E)
{
  if (is_AST(E, "import_note"))
  {
    string modid = *E->sub[0].assert_is_a<String>();
    (*this) += modid;
  }
  else if (is_AST(E, "import_submodel_note"))
  {
    string modid1 = *E->sub[0].assert_is_a<String>();
    string modid2 = *E->sub[1].assert_is_a<String>();

    (*this) += pair<string,string>{modid1, modid2};
  }
  
  return Model_Notes::add_note( E );
}


void context::rename_parameter(int i, const string& new_name)
{
  parameters()[i].first = new_name;
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

closure context::lazy_evaluate_expression_(closure&& C, bool ec) const
{
  try {
    int R = *push_temp_head();
    set_C(R, std::move(C) );

    R = incremental_evaluate(R,ec);
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

object_ref context::evaluate_expression_(closure&& C,bool ec) const
{
  expression_ref result = lazy_evaluate_expression_(std::move(C),ec).exp;
#ifndef NDEBUG
  if (is_a<lambda2>(result))
    throw myexception()<<"Evaluating lambda as object: "<<result->print();
#endif
  return result->head;
}

closure context::lazy_evaluate_expression(const expression_ref& E, bool ec) const
{
  return lazy_evaluate_expression_( preprocess(E), ec);
}

object_ref context::evaluate_expression(const expression_ref& E,bool ec) const
{
  return evaluate_expression_( preprocess(E), ec);
}

object_ref context::perform_expression(const expression_ref& E,bool ec) const
{
  expression_ref E2 = (identifier("unsafePerformIO"),E);
  return evaluate_expression_( preprocess(E2), ec);
}

bool context::parameter_is_set(int index) const
{
  assert(index >= 0 and index < n_parameters());

  int P = find_parameter_modifiable_reg(index);

  if (not access(P).result and not access(P).call) return false;

  return true;
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
object_ref context::get_reg_value(int R) const
{
  if (not access(R).result)
  {
    // If there's no result AND there's no call, then the result simply hasn't be set, so return NULL.
    if (not access(R).call) return object_ref();

    // If the value needs to be computed (e.g. its a call expression) then compute it.
    incremental_evaluate(R);
  }

  return access_result(R).exp->head;
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
object_ref context::get_modifiable_value(int index) const
{
  int R = get_modifiable_reg(index);

  return get_reg_value(R);
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
object_ref context::get_parameter_value(int index) const
{
  int R = find_parameter_modifiable_reg(index);

  return get_reg_value(R);
}

/// Get the value of a non-constant, non-computed index
object_ref context::get_parameter_value(const std::string& name) const
{
  int index = find_parameter(name);
  if (index == -1)
    throw myexception()<<"Cannot find parameter called '"<<name<<"'";

  return get_parameter_value(index);
}

void context::set_modifiable_value_(int index, closure&& C)
{
  int R = get_modifiable_reg(index);

  set_reg_value(R, std::move(C) );
}

void context::set_modifiable_value(int index, const expression_ref& O)
{
  object_ref v = O->head;
  assert(not O->size());
  assert(not dynamic_pointer_cast<const index_var>(v));
  assert(not dynamic_pointer_cast<const reg_var>(v));
  assert(not dynamic_pointer_cast<const identifier>(v));
  set_modifiable_value_(index, v);
}

void context::set_parameter_value(int index, const expression_ref& O)
{
  object_ref v = O->head;
  assert(not O->size());
  assert(not dynamic_pointer_cast<const index_var>(v));
  assert(not dynamic_pointer_cast<const reg_var>(v));
  assert(not dynamic_pointer_cast<const identifier>(v));
  set_parameter_value_(index, v);
}

void context::set_parameter_value_expression(int index, const expression_ref& O)
{
  if (O)
  {
    expression_ref E = (identifier("set_parameter_value"), token, parameter(parameter_name(index)), O);

    perform_expression(E);
  }
  else
    set_parameter_value_(index, {} );
}

void context::set_parameter_value_(int index, closure&& C)
{
  int P = find_parameter_modifiable_reg(index);

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

int context::add_parameter(const string& full_name)
{
  if (not is_haskell_var_name(full_name))
    throw myexception()<<"Parameter name '"<<full_name<<"' is not a Haskell variable name";

  // 1. Determine module name and extension of parameter name.
  string module_name = get_module_name(full_name);
  string var_name = get_unqualified_name(full_name);
  if (module_name.empty())
    throw myexception()<<"Trying to add parameter with unqualified name '"<<full_name<<"'!";
  assert(module_name + "." + var_name == full_name);

  // 2. Add it to some module.
  bool found = false;
  for(auto& module: *P.modify())
    if (module.name == module_name)
    {
      module.declare_parameter(var_name);
      found = true;
    }

  if (not found)
  {
    Module module(module_name);
    module.declare_parameter(var_name);
    P.modify()->push_back(module);
    // FIXME:maybe-works - Do all other modules now need to import this??
  }

  assert(full_name.size() != 0);
  assert(find_parameter(full_name) == -1);

  int index = n_parameters();

  root_t r = allocate_reg();
  parameters().push_back( {full_name, r} );

  set_C(*r, preprocess( (identifier("unsafePerformIO"),(identifier("new_modifiable"),get_token()) ) ) );

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
    string qualified_name = p->parameter_name;
    assert(is_qualified_symbol(qualified_name));

    int param_index = find_parameter(qualified_name);
    
    if (param_index == -1)
      throw myexception()<<"Can't translate undefined parameter '"<<qualified_name<<"' ('"<<p->parameter_name<<"') in expression!";

    reg = get_parameter_reg(param_index);
  }

  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (object_ptr<const modifiable> m = is_a<modifiable>(E))
    reg = get_modifiable_reg(m->index);

  // Replace parameters with the appropriate reg_var: of value whatever
  if (object_ptr<const identifier> V = is_a<identifier>(E))
  {
    string qualified_name = V->name;
    assert(is_qualified_symbol(qualified_name) or is_haskell_builtin_con_name(qualified_name));
    auto loc = identifiers().find( qualified_name );
    if (loc == identifiers().end())
    {
      if (is_haskell_builtin_con_name(V->name))
      {
	symbol_info S = Module::lookup_builtin_symbol(V->name);
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

const module_loader& context::get_module_loader() const
{
  return loader;
}

const vector<string>& context::get_module_path() const
{
  return loader.modules_path;
}

const vector<string>& context::get_builtins_path() const
{
  return loader.builtins_path;
}

context& context::operator+=(const string& module_name)
{
  if (not contains_module(*P, module_name))
    (*this) += get_module_loader().load_module(module_name);

  return *this;
}

context& context::operator+=(const pair<string,string>& module_names)
{
  if (not contains_module(*P, module_names.second))
    (*this) += get_module_loader().load_and_rename_module(module_names.first, module_names.second);

  return *this;
}

context& context::operator+=(const vector<string>& module_names)
{
  for(const auto& name: module_names)
    (*this) += name;

  return *this;
}

expression_ref parameter_constructor(const std::string& name, const vector<expression_ref>& notes)
{
  for(const auto& note: notes)
  {
    if (not is_exactly(note, ":~") and not is_exactly(note,":=~")) continue;

    if (not note->sub[0]->equals(parameter(name))) continue;

    expression_ref dist = note->sub[1];


    return (identifier("structure_for_range"),(identifier("distRange"),dist));
  }
  return identifier("new_modifiable");
}

void context::allocate_identifiers_for_modules(const vector<string>& module_names)
{
  // 2. Give each identifier a pointer to an unused location; define parameter bodies.
  for(const auto& name: module_names)
  {
    const Module& M = get_module(*P, name);

    for(const auto& s: M.get_symbols())
    {
      const symbol_info& S = s.second;

      if (S.scope != local_scope) continue;

      if (S.symbol_type == variable_symbol or S.symbol_type == constructor_symbol)
      {
	if (identifiers().count(S.name))
	  throw myexception()<<"Trying to define symbol '"<<S.name<<"' that is already defined!";

	add_identifier(S.name);
      }
      else if (S.symbol_type == parameter_symbol)
      {
	assert(find_parameter(S.name) == -1);

	root_t r = allocate_reg();
	parameters().push_back( {S.name, r} );

	expression_ref E = (identifier("unsafePerformIO"),(parameter_constructor(S.name,get_notes()), get_token()) );
	set_C(*r, preprocess( E ) );
      }
    }
  }
      
  // 3. Use these locations to translate these identifiers, at the cost of up to 1 indirection per identifier.
  for(const auto& name: module_names)
  {
    const Module& M = get_module(*P, name);

    for(const auto& s: M.get_symbols())
    {
      const symbol_info& S = s.second;

      if (S.scope != local_scope) continue;

      if (S.symbol_type != variable_symbol and S.symbol_type != constructor_symbol) continue;

      // get the root for each identifier
      map<string, root_t>::iterator loc = identifiers().find(S.name);
      assert(loc != identifiers().end());
      root_t r = loc->second;
      int R = *r;

      expression_ref F = M.get_function(S.name);
      assert(F);

      assert(R != -1);
      set_C(R, preprocess(F) );
    }
  }
}

// \todo FIXME:cleanup If we can make this only happen once, we can assume old_module_names is empty.
context& context::operator+=(const Module& M)
{
  Program& PP = *P.modify();

  int first_note = n_notes();

  // Get module_names, but in a set<string>
  set<string> old_module_names = module_names_set(PP);

  // 1. Add the new modules to the program, add notes, perform imports, and resolve symbols.
  add(loader, PP, *this, M);

  // 2. Give each identifier a pointer to an unused location; define parameter bodies.
  vector<string> new_module_names;
  for(auto& module: PP)
    if (not old_module_names.count(module.name))
      new_module_names.push_back(module.name);

  allocate_identifiers_for_modules(new_module_names);

  set_default_values_from_notes(*this,first_note,n_notes());
  
  return *this;
}

// \todo FIXME:cleanup If we can make this only happen once, we can assume old_module_names is empty.
context& context::operator+=(const vector<Module>& Ms)
{
  for(const auto& M: Ms)
    (*this) += M;

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

context::context(const module_loader& L)
  :context(L,{},vector<Module>{})
{  }

// FIXME - this should be shared with Model::add_submodel( ), but we need to call Model::add_notes( ).
// If a model were to HOOK (a) add_parameter, (b) add_note( ), and (c) set_parameter_value( )
//  then this routine could work on both.
vector<int> add_submodel(context& C, const vector<expression_ref>& N)
{
  vector<int> new_parameters;

  int first_note = C.n_notes();

  // 3. Add the notes from this model to the current model.
  for(const auto& n: N)
    C.add_note(n);
  
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
  
  set_default_values_from_notes(C, first_note, C.n_notes());

  return new_parameters;
}

context::context(const module_loader& L, const vector<expression_ref>& N)
  :context(L, N,vector<Module>{})
{ }

context::context(const module_loader& L, const vector<expression_ref>& N, const vector<Module>& Ps)
  :memory(new reg_heap()),
   P(new Program),
   token(memory->get_unused_token()),
   loader(L)
{
  (*this) += "Prelude";
  (*this) += "Parameters";
  (*this) += Ps;

  add_submodel(*this, N);
}

context::context(const module_loader& L, const vector<expression_ref>& N, const vector<string>& module_names)
  :context(L,N)
{
  for(const auto& name: module_names)
    (*this) += name;
}

context::context(const context& C)
  :Model_Notes(C),
   memory(C.memory),
   P(C.P),
   token(memory->copy_token(C.token)),
   loader(C.loader)
{ }

context::~context()
{
  memory->release_token(token);
}

reg_heap::root_t context::push_temp_head() const
{
  return memory->push_temp_head( token );
}

int context::get_parameter_reg(int index) const
{
  assert(index >= 0 and index < n_parameters());

  return *parameters()[index].second;
}

int context::find_parameter_modifiable_reg(int index) const
{
  int R = get_parameter_reg(index);

  int R2 = incremental_evaluate(R, false);

  if (not is_modifiable(access(R2).C.exp))
    throw myexception()<<"Parameter is not a modifiable!  Instead its value is '"<<access(R2).C.exp<<"'";

 return R2;
}

int context::get_modifiable_reg(int index) const
{
  const pool<int>& p = modifiable_regs();

  if (not p.is_used(index))
    throw myexception()<<"Attempting to access non-existent modifiable #"<<index;

  return p[index];
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

// Maybe we should always just compute a new probability expression from scratch?
// Then we would always know that our notes were consistent!
// TODO: Move to model.{H,C}?
int add_probability_expression(context& C)
{
  expression_ref Pr;

  // Check each expression in the Formula
  map<string,string> prior_expressions;
  for(int i=0;i<C.n_notes();i++)
  {
    // If its a probability expression, then...
    if (not is_exactly(C.get_note(i), ":~") and not is_exactly(C.get_note(i), ":=~")) continue;

    // Extract the density operation
    expression_ref x = C.get_note(i)->sub[0];
    expression_ref D = C.get_note(i)->sub[1];

    if (prior_expressions.count(x->print()))
      throw myexception()<<"Duplicate prior expression for variable '"<<x->print()<<"'";
    prior_expressions[x->print()] = D->print();
    
    // Create an expression for calculating the density of these random variables given their inputs
    expression_ref Pr_i = (identifier("Distributions.density"),D,x);
    
    // Extend the probability expression to include this term also.
    // (FIXME: a balanced tree could save computation time)
    if (not Pr)
      Pr = Pr_i;
    else
      Pr = (identifier("*"),Pr_i, Pr);
  }

  // If this model has random variables... 
  if (Pr)
  {
    return C.add_compute_expression(Pr);
  }
  else
    return -1;
}

void set_default_values_from_notes(context& C, int b, int e)
{
  // Set default values from distributions
  for(int i=b;i<e;i++)
  {
    vector<expression_ref> results;
    expression_ref query = constructor(":~",2) + match(0) + match(1);

    if (find_match(query, C.get_note(i), results))
    {
      expression_ref parameter = results[0];
      expression_ref value = (identifier("distDefaultValue"),results[1]);
      C.perform_expression( (identifier("set_parameter_value"),C.get_token(),parameter,value) );
    }
  }

  // Set default values from DefaultValue notes
  for(int i=b;i<e;i++)
  {
    vector<expression_ref> results;
    expression_ref query = constructor("DefaultValue",2) + match(0) + match(1);

    if (find_match(query, C.get_note(i), results))
    {
      expression_ref parameter = results[0];
      expression_ref value = results[1];
      C.perform_expression( (identifier("set_parameter_value"),C.get_token(),parameter,value) );
    }
  }

}
