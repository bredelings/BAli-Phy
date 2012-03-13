#include "computation/context.H"
#include "prelude.H"

using boost::shared_ptr;
using std::string;
using std::vector;
using std::map;
using std::pair;
using std::set;

using std::cerr;
using std::endl;

string context::parameter_name(int i) const
{
  expression_ref E = access(*parameters()[i]).E;
  if (shared_ptr<const parameter> P = dynamic_pointer_cast<const parameter>(E))
  {
    return P->parameter_name;
  }
  throw myexception()<<"Parameter "<<i<<" is not a parameter: can't find name!";
}

int context::add_note(const expression_ref& E)
{
  notes.push_back(E);
  return notes.size()-1;
}

reg_heap::root_t reg_heap::add_identifier_to_context(int t, const string& name)
{
  map<string,root_t>& identifiers = get_identifiers_for_context(t);

  // if there's already an 's', then complain
  if (identifiers.count(name))
    throw myexception()<<"Cannot add identifier '"<<name<<"': there is already an identifier with that name.";

  root_t r = allocate_reg();
  access(*r).owners.insert(t);
  identifiers[name] = r;
  return r;
}

reg_heap::root_t context::add_identifier(const string& name)
{
  if (find_parameter(name) != -1)
    throw myexception()<<"Cannot add identifier '"<<name<<"': there is already a parameter with that name.";

  return memory->add_identifier_to_context(token,name);
}

void context::rename_parameter(int i, const string& new_name)
{
  string old_name = parameter_name(i);

  int R = *parameters()[i];

  assert( access(R).changeable == true );
  set_E(R, parameter(new_name) );
}

bool context::compute_expression_is_up_to_date(int index) const
{
  int& H = *heads()[index];

  return (access(H).result);
}

// Is there a way to generalize the updating of reg_var elements of structures,
// when incremental evaluation walks a reg_var chain?

expression_ref context::full_evaluate(int& R) const
{
  R = incremental_evaluate(R);
  expression_ref result = access(R).result;

  {
    // If the result is atomic, then we are done.
    shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(result);
    if (not E) return result;

    // If the result is a lambda function, then we are done.
    // (a) if we are going to USE this, we should just call lazy evaluate! (which return a heap variable)
    // (b) if we are going to PRINT this, then we should probably normalize it more fully....?
    if (not dynamic_pointer_cast<const constructor>(E->sub[0])) return result;
  }

  // If the result is a structure, then evaluate its fields and substitute them.
  {
    shared_ptr<expression> E = dynamic_pointer_cast<expression>(result);
    assert(dynamic_pointer_cast<const constructor>(E->sub[0]));

    for(int i=1;i<E->size();i++)
    {
      shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(E->sub[i]);
      assert(RV);
      int R2 = RV->target;

      E->sub[i] = full_evaluate(R2);
    }
    return result;
  }
}

/// Return the value of a particular index, computing it if necessary
shared_ptr<const Object> context::lazy_evaluate(int index) const
{
  int& H = *heads()[index];

  H = incremental_evaluate(H);

  return access(H).result;
}

/// Return the value of a particular index, computing it if necessary
shared_ptr<const Object> context::evaluate(int index) const
{
  int& H = *heads()[index];

  H = incremental_evaluate(H);

  return full_evaluate(H);
}

shared_ptr<const Object> context::lazy_evaluate_expression(const expression_ref& E) const
{
  int R = *push_temp_head();
  set_E(R, let_float(graph_normalize(translate_refs(E)) ));

  try {
    R = incremental_evaluate(R);
    shared_ptr<const Object> result = access(R).result;
    
    pop_temp_head();
    return result;
  }
  catch (myexception& e)
  {
    pop_temp_head();
    throw e;
  }
}

shared_ptr<const Object> context::evaluate_expression(const expression_ref& E) const
{
  int R = *push_temp_head();
  set_E(R, let_float(graph_normalize(translate_refs(E)) ));

  try {
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

bool context::parameter_is_set(int index) const
{
  int P = *parameters()[index];

  if (not access(P).result and access(P).call == -1) return false;

  return true;
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
shared_ptr<const Object> context::get_parameter_value(int index) const
{
  int P = *parameters()[index];

  if (not access(P).result)
  {
    // If there's no result AND there's no call, then the result simply hasn't be set, so return NULL.
    if (access(P).call == -1) return shared_ptr<const Object>();

    // If the value needs to be computed (e.g. its a call expression) then compute it.
    incremental_evaluate(P);
  }

  return access(P).result;
}

/// Get the value of a non-constant, non-computed index
shared_ptr<const Object> context::get_parameter_value(const std::string& name) const
{
  int index = find_parameter(name);
  if (index == -1)
    throw myexception()<<"Cannot find parameter called '"<<name<<"'";

  return get_parameter_value(index);
}

void context::set_parameter_value(int index, const expression_ref& O)
{
  int P = *parameters()[index];

  set_reg_value(P, O);
}

/// Update the value of a non-constant, non-computed index
void context::set_reg_value(int P, const expression_ref& O)
{
  return memory->set_reg_value(P, translate_refs(O), token);
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
  assert(name.size() != 0);
  assert(find_parameter(name) == -1);

  int index = n_parameters();

  root_t r = allocate_reg();
  parameters().push_back( r );

  access(*r).changeable = true;
  set_E(*r, parameter(name) );

  set_parameter_value(index, default_parameter_value(index) );
  
  return index;
}

/// Add an expression that may be replaced by its reduced form
int context::add_compute_expression(const expression_ref& E)
{
  std::cerr<<"add: "<<E<<"\n";

  expression_ref T = let_float(graph_normalize(translate_refs(E) ));

  std::cerr<<"add [compiled]: "<<T<<"\n";

  root_t r;
  if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(T))
  {
    assert( includes(access(RV->target).owners, token) );
    
    r = push_root( RV->target );
  }
  else
  {
    r = allocate_reg();
    set_E( *r, T );
  }

  heads().push_back(r);
  return heads().size()-1;
}

int context::n_expressions() const
{
  return heads().size();
}

expression_ref context::get_expression(int i) const
{
  return access(*heads()[i]).E;
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

expression_ref context::translate_refs(const expression_ref& R) const
{
  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (shared_ptr<const parameter> P = dynamic_pointer_cast<const parameter>(R))
  {
    int param_index = find_parameter(P->parameter_name);
    
    if (param_index == -1)
      throw myexception()<<"Can't translate undefined parameter '"<<P->parameter_name<<"' in expression!";

    int param_location = *parameters()[param_index];

    return expression_ref(new reg_var(param_location) );
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (shared_ptr<const var> V = dynamic_pointer_cast<const var>(R))
  {
    map<string,root_t>::const_iterator loc = identifiers().find(V->name);
    if (loc == identifiers().end())
      throw myexception()<<"Can't translate undefined identifier '"<<V->name<<"' in expression!";

    int R = *(loc->second);

    return expression_ref(new reg_var(R) );
  }

  // Other constants have no parts, and don't need to be translated
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return R;

  // Translate the parts of the expression
  expression_ref R2 = R;
  shared_ptr<expression> V ( new expression(*E) );
  for(int i=0;i<V->size();i++)
    V->sub[i] = translate_refs(V->sub[i]);

  return shared_ptr<const expression>(V);
}

int context::find_match_notes(const expression_ref& query, std::vector<expression_ref>& results, int start) const
{
  assert(start >= 0);
  for(int i=start;i<n_notes();i++)
  {
    results.clear();
    if (find_match(query, get_note(i), results))
      return i;
  }
  return -1;
}

context& context::operator+=(const Def& D)
{
  Program P2;
  P2 += D;
  (*this) += P2;

  return *this;
}

context& context::operator+=(const Program& P2)
{
  // Give each identifier a pointer to an unused location
  foreach(D, P2.functions)
  {
    if (not identifiers().count(D->first))
      add_identifier(D->first);
  }

  // Use these locations to translate these identifiers, at the cost of up to 1 indirection per identifier.
  foreach(D, P2.functions)
  {
    // get the root for each identifier
    string name = D->first;
    map<string, root_t>::iterator loc = identifiers().find(name);
    assert(loc != identifiers().end());
    root_t r = loc->second;
    int R = *r;

    expression_ref F = P2.get_function(name);

    assert(R != -1);
    set_E(R, let_float(graph_normalize(translate_refs(F))));
  }

  (*P) += P2;
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
   P(new Program()),
   token(memory->get_unused_token())
{ 
  (*this) += Prelude;
}

context::context(const vector<expression_ref>& N)
  :memory(new reg_heap()),
   P(new Program()),
   token(memory->get_unused_token()),
   notes(N)
{
  (*this) += Prelude;

  std::set<string> names = find_named_parameters(notes);
  
  // Then set all default values.
  foreach(i,names)
    add_parameter(*i);
}

context::context(const context& C)
  :memory(C.memory),
   P(C.P),
   token(memory->copy_token(C.token)),
   notes(C.notes)
{ }

context::~context()
{
  memory->release_token(token);
}

shared_ptr<const Object> context::default_parameter_value(int i) const
{
  expression_ref default_value = lambda_expression(constructor("default_value",2));

  vector<expression_ref> results;
  expression_ref query = default_value( parameter( parameter_name(i) ) )(match(0));
  int found = find_match_notes(query, results, 0);

  if (found != -1)
  {
    assert(results.size());
    expression_ref value = results[0];
    //    assert(find_match_notes(query, results, found+1) == -1);
    return value;
  }
  else
    return shared_ptr<const Object>();
}

reg_heap::root_t context::push_temp_head() const
{
  return memory->push_temp_head( token );
}

boost::shared_ptr<context> prefix_formula(const std::string& prefix, const boost::shared_ptr<const context>& C)
{
  shared_ptr<context> C2(C->clone());
  // prefix the parameter names
  for(int i=0;i<C2->n_parameters();i++)
    C2->rename_parameter(i, prefix + "::" + C2->parameter_name(i));

  // Let's NOT prefix the variable names, since they could be "globally scoped", like fmap

  // prefix the names in the model
  C2->get_notes() = add_prefix(prefix, C2->get_notes());
  return C2;
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

#include "distribution-operations.H"
#include "computation/operations.H"

// note = (~ x D)
// Pr_i = case D of ((prob_density _ density_op), args) -> (density_op x args)

int add_probability_expression(context& C)
{
  expression_ref query = (distributed, _1,_2);

  typed_expression_ref<Log_Double> Pr;

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
    expression_ref Pr_i = case_expression(true, D, Tuple((prob_density,_,density),args), (density, x, args));
    
    // Extend the probability expression to include this term also.
    // (FIXME: a balanced tree could save computation time)
    if (not Pr)
      Pr = typed_expression_ref<Log_Double>(Pr_i);
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

vector<expression_ref> add_prefix(const string& prefix, const vector<expression_ref>& notes)
{
  vector<expression_ref> notes2(notes.size());
  for(int i=0;i<notes.size();i++)
    notes2[i] = add_prefix(prefix, notes[i]);
  return notes2;
}

vector<expression_ref> combine(const vector<expression_ref>& N1, const vector<expression_ref>& N2)
{
  vector<expression_ref> N3 = N1;
  N3.insert(N3.end(), N2.begin(), N2.end());
  return N3;
}

