#include "computation/context.H"
#include "computation/graph_register.H"
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

closure resolve_refs(const vector<Module>& P, closure&& C)
{
  C.exp = resolve_refs(P, C.exp);
  return C;
}

void context::make_clean() const
{
  if (memory()->is_dirty(token))
    token = memory()->switch_to_child_token(token);
}

void context::make_terminal_token() const
{
  if (not memory()->children_of_token(token).empty())
    token = memory()->switch_to_child_token(token);

  assert(memory()->children_of_token(token).empty());
}

void context::make_root_tip() const
{
  if (memory()->degree_of_token(token) >= 2)
    token = memory()->switch_to_child_token(token);
  make_root_token();
}

void context::make_root_token() const
{
  memory_->reroot_at(token);
}

object_ptr<reg_heap>& context::memory() const {return memory_;}

std::vector<int>& context::heads() const {return memory()->get_heads();}

std::vector<std::pair<std::string,int>>& context::parameters() const {return memory()->get_parameters();}

std::map<std::string, int>& context::identifiers() const {return memory()->get_identifiers();}

const std::vector<int>& context::triggers() const {make_root_token();return memory()->triggers(token);}
      std::vector<int>& context::triggers()       {make_root_token();return memory()->triggers(token);}

reg& context::access(int i) const {return memory()->access(i);}

bool context::reg_has_call(int r) const 
{
  make_root_token();
  return memory()->reg_has_call(token,r);
}

bool context::reg_has_result(int r) const 
{
  make_root_token();
  return memory()->reg_has_result(token,r);
}

const closure& context::access_result_for_reg(int i) const
{
  make_root_token();
  return memory()->access_result_for_reg(token,i);
}

reg& context::operator[](int i) const {return memory()->access(i);}

void context::set_C(int R, closure&& c) const {memory()->set_C(R,std::move(c));}

int context::incremental_evaluate(int R) const 
{
  make_root_token();
  memory()->mark_completely_dirty(token);
  return memory()->incremental_evaluate(R,token);
}

int context::incremental_evaluate_unchangeable(int R) const 
{
  return memory()->incremental_evaluate(R,0);
}

int context::allocate() const {return memory()->allocate();}

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

int context::add_identifier(const string& name) const
{
  if (find_parameter(name) != -1)
    throw myexception()<<"Cannot add identifier '"<<name<<"': there is already a parameter with that name.";

  return memory()->add_identifier(name);
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
  if (not reg_has_result(R)) return false;

  const closure& result = access_result_for_reg(R);

  // NOTE! result cannot be an index_var.
  const expression_ref& E = result.exp;

  // Therefore, if the result is atomic, then R is up-to-date.
  if (not E->size()) return true;

  // If the result is a lambda function, then R is up-to-date.
  if (E->head->type() != constructor_type) return true;

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
  int H = heads()[index];

  return reg_is_fully_up_to_date(H);
}

/// Return the value of a particular index, computing it if necessary
closure context::lazy_evaluate(int index) const
{
  int& H = heads()[index];

  H = incremental_evaluate(H);

  return access_result_for_reg(H);
}

/// Return the value of a particular index, computing it if necessary
object_ref context::evaluate(int index) const
{
  int& H = heads()[index];

  H = incremental_evaluate(H);

  return access_result_for_reg(H).exp->head;
}

/// Return the value of a particular index, computing it if necessary
object_ref context::perform(int index) const
{
  int H = heads()[index];

  return perform_expression(reg_var(H));
}

closure context::lazy_evaluate_expression_(closure&& C, bool ec) const
{
  try {
    int R = push_temp_head();
    set_C(R, std::move(C) );

    if (ec)
      R = incremental_evaluate(R);
    else
      R = incremental_evaluate_unchangeable(R);
    const closure& result = access_result_for_reg(R);
    
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
  expression_ref E2 = (get_expression(perform_io_head),E);
  return evaluate_expression_( preprocess(E2), ec);
}

bool context::parameter_is_modifiable(int index) const
{
  int R = get_parameter_reg(index);

  int R2 = incremental_evaluate_unchangeable(R);

  return is_modifiable(access(R2).C.exp);
}


bool context::parameter_is_set(int index) const
{
  assert(index >= 0 and index < n_parameters());

  int P = find_parameter_modifiable_reg(index);

  if (not reg_has_result(P) and not reg_has_call(P)) return false;

  return true;
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
object_ref context::get_reg_value(int R) const
{
  make_root_token();
  if (not reg_has_result(R))
  {
    // If there's no result AND there's no call, then the result simply hasn't be set, so return NULL.
    if (not reg_has_call(R)) return object_ref();

    // If the value needs to be computed (e.g. its a call expression) then compute it.
    incremental_evaluate(R);
  }

  return access_result_for_reg(R).exp->head;
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
  make_terminal_token();

  make_clean();
  // FIXME - we can only change values on contexts that are not dirty!
  // BUT this is ultimately checked in the reg_heap itself.
  memory()->set_reg_value(P, std::move(C), token);
}

/// Update the value of a non-constant, non-computed index
void context::set_parameter_value(const std::string& var, const expression_ref& O)
{
  int i = find_parameter(var);
  if (i == -1)
    throw myexception()<<"Cannot find parameter called '"<<var<<"'";
    
  set_parameter_value(i, O);
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

void context::add_parameter_(const string& name)
{
  assert(find_parameter(name) == -1);

  int R = allocate();

  parameters().push_back( {name, R} );
}

int context::add_parameter(const string& full_name)
{
  if (not is_haskell_var_name(full_name))
    throw myexception()<<"Parameter name '"<<full_name<<"' is not a Haskell variable name";

  // 0. Check that we don't already have a parameter with that name
  for(int i=0;i<n_parameters();i++)
    if (parameter_name(i) == full_name)
      throw myexception()<<"A parameter with name '"<<full_name<<"' already exists - cannot add another one.";

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

  add_parameter_(full_name);

  int R = parameters().back().second;

  set_C(R, preprocess( (identifier("unsafePerformIO"),identifier("new_modifiable") ) ) );

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
  int R = allocate();

  heads().push_back( R);

  set_C( R, std::move(C) );

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
  int R = allocate();

  heads()[i] = R;

  set_C( R, std::move(C) );
}

/// Should the ith compute expression be re_evaluated when invalidated?
void context::set_re_evaluate(int i, bool b)
{
  int& R = heads()[i];
  R = incremental_evaluate(R);
  if (memory()->reg_is_changeable(R))
    access(R).re_evaluate = b;
}

int context::n_expressions() const
{
  return heads().size();
}

expression_ref context::get_expression(int i) const
{
  int H = heads()[i];
  return reg_var(H);
}

void context::pop_temp_head() const
{
  memory()->pop_temp_head();
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

void context::release_identifiers()
{
  memory()->release_identifiers();
}

void context::compile()
{
  release_identifiers();
}

void context::collect_garbage() const
{
  memory()->collect_garbage();
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
	
	int R = loc->second;
	
	assert(R != -1);
	set_C(R, preprocess(S.body) );
      }
      else
	throw myexception()<<"Can't translate undefined identifier '"<<V->name<<"' in expression!";
    }

    reg = loc->second;
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
    if (not is_exactly(note, ":~")) continue;

    expression_ref p = parameter(name);

    if (not note->sub[0]->equals(*p)) continue;

    expression_ref dist = note->sub[1];


    return (identifier("structure_for_dist"),dist);
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
	add_parameter_(S.name);
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

      if (S.symbol_type == parameter_symbol) continue; // we handle this in initialize_parameter_structures_from_notes( )

      if (S.symbol_type != variable_symbol and S.symbol_type != constructor_symbol) continue;

      // get the root for each identifier
      auto loc = identifiers().find(S.name);
      assert(loc != identifiers().end());
      int R = loc->second;

      expression_ref F = M.get_function(S.name);
      assert(F);

      assert(R != -1);
      set_C(R, preprocess(F) );
    }
  }
}

void context::initialize_parameter_structures_for_modules(const vector<string>& module_names)
{
  // 3. Use these locations to translate these identifiers, at the cost of up to 1 indirection per identifier.
  for(const auto& name: module_names)
  {
    const Module& M = get_module(*P, name);

    for(const auto& s: M.get_symbols())
    {
      const symbol_info& S = s.second;

      if (S.scope != local_scope) continue;

      if (S.symbol_type == parameter_symbol)
      {
	int R = get_parameter_reg( find_parameter(S.name) );
	expression_ref E = (identifier("unsafePerformIO"), parameter_constructor(S.name,get_notes()) );
	set_C(R, preprocess( E ) );
      }
    }
  }
}

// \todo FIXME:cleanup If we can make this only happen once, we can assume old_module_names is empty.
context& context::operator+=(const Module& M)
{
  Program& PP = *P.modify();

  int first_note = n_notes();

  int first_module = PP.size();

  // Get module_names, but in a set<string>
  set<string> old_module_names = module_names_set(PP);

  // 1. Add the new modules to the program, add notes, perform imports, and resolve symbols.
  add(loader, PP, M);

  // 2. Give each identifier a pointer to an unused location; define parameter bodies.
  vector<string> new_module_names;
  for(auto& module: PP)
    if (not old_module_names.count(module.name))
      new_module_names.push_back(module.name);

  allocate_identifiers_for_modules(new_module_names);

  // 3. Now that parameters are defined, add notes -- which can add priors for parameters.
  for(int i=first_module;i<PP.size();i++)
    add_notes(PP[i].get_notes());

  // 4. Create a structure containing modifiables for each parameter
  initialize_parameter_structures_for_modules(new_module_names);

  // 5. Finally, set default parameter values.
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
  memory_->release_token(token);
  
  memory_ = C.memory_;
  token = memory_->copy_token(C.token);
  perform_io_head = C.perform_io_head;
  P = C.P;
  notes = C.notes;

  return *this;
}

context::context(const module_loader& L)
  :context(L,vector<expression_ref>{},vector<Module>{})
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
  :memory_(new reg_heap()),
   P(new Program),
   token(memory_->get_unused_token()),
   loader(L)
{
  (*this) += "Prelude";
  (*this) += "Parameters";
  (*this) += "Distributions";
  (*this) += Ps;

  perform_io_head = add_compute_expression(identifier("unsafePerformIO"));

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
   memory_(C.memory_),
   P(C.P),
   token(memory_->copy_token(C.token)),
   perform_io_head(C.perform_io_head),
   loader(C.loader)
{ }

context::~context()
{
  memory_->release_token(token);
}

int context::push_temp_head() const
{
  return memory()->push_temp_head();
}

int context::get_parameter_reg(int index) const
{
  assert(index >= 0 and index < n_parameters());

  return parameters()[index].second;
}

int context::find_parameter_modifiable_reg(int index) const
{
  int R = get_parameter_reg(index);

  int R2 = incremental_evaluate_unchangeable(R);

  if (R != R2)
    parameters()[index].second = R2;

#ifndef NDEBUG
  if (not is_modifiable(access(R2).C.exp))
    throw myexception()<<"Parameter is not a modifiable!  Instead its value is '"<<access(R2).C.exp<<"'";
#endif

 return R2;
}

int context::get_modifiable_reg(int r) const
{
  assert(is_modifiable(access(r).C.exp));

  return r;
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
      int token = C.get_token();
      C.make_terminal_token();
      expression_ref value = (identifier("evaluate"),token,(identifier("distDefaultValue"),results[1]));
      C.perform_expression( (identifier("set_parameter_value_"),C.get_token(),parameter,value) );
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
      C.perform_expression( (identifier("set_parameter_value_"),C.get_token(),parameter,value) );
    }
  }

}
