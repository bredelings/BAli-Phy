#include <iostream>
#include "graph_register.H"
#include "operations.H"

using boost::shared_ptr;
using std::string;
using std::vector;
using std::map;
using std::pair;

/*
 * Separate NoteLists (which are just raw expressions) from the executable context.
 * + We only call "combine" on NoteLists.
 * + We only find_match_note( ) on NoteLists.
 * + We allow starting from any place in find_match_note( ).
 *
 */

void reg::clear()
{
  E = expression_ref();
  changeable = false;
  used_inputs.clear();
  outputs.clear();
  call_outputs.clear();
  call = -1;
  result = shared_ptr< shared_ptr<const Object> >(new shared_ptr<const Object>);
}

reg::reg()
 :changeable(false),
  result(new shared_ptr<const Object>),
  prev_reg(-1),
  next_reg(-1),
  state(none)
{}

int context::add_note(const expression_ref& E)
{
  notes.push_back(E);
  return notes.size()-1;
}

void context::set_used_input(int R1, int slot, int R2) const
{
  assert(R1 >= 0 and R1 < n_regs());
  assert(R2 >= 0 and R2 < n_regs());

  // fixme
  if (access(R1).used_inputs[slot] != -1) return;

  access(R1).used_inputs[slot] = R2;
  access(R2).outputs.insert(pair<int,int>(R1,slot));
}

void context::clear_used_input(int R, int slot) const
{
  int R2 = access(R).used_inputs[slot];
  if (R2 == -1) return;
  assert(R2 >= 0 and R2 < n_regs());

  // FIXME - we should treat different slots differently
  access(R2).outputs.erase(pair<int,int>(R,slot));
  access(R).used_inputs[slot] = -1;
}

void context::clear_used_inputs(int R) const
{
  for(int i=0;i<access(R).used_inputs.size();i++)
    clear_used_input(R,i);
}

void context::clear_used_inputs(int R, int S) const
{
  clear_used_inputs(R);
  access(R).used_inputs = vector<int>(S, -1);
}

void context::set_call(int R1, int R2) const
{
  assert(access(R1).call == -1);
  assert(not *access(R1).result);
  assert(R2 >= 0 and R2 < n_regs());

  access(R1).call = R2;
  access(R2).call_outputs.insert(R1);
}

void context::clear_call(int R) const
{
  int R2 = access(R).call;
  if (R2 == -1) return;
  assert(R2 >= 0 and R2 < n_regs());
  
  access(R).call = -1;
  access(R2).call_outputs.erase(R);
}

string context::parameter_name(int i) const
{
  expression_ref E = access(parameters[i]).E;
  if (shared_ptr<const parameter> P = dynamic_pointer_cast<const parameter>(E))
  {
    return P->parameter_name;
  }
  throw myexception()<<"Parameter "<<i<<" is not a parameter: can't find name!";
}

void context::add_variable(const string& name, int R)
{
  // if there's already an 's', then complain
  if (find_variable(name) != -1)
    throw myexception()<<"Cannot add variable '"<<name<<"': there is already a variable with that name.";

  if (find_parameter(name) != -1)
    throw myexception()<<"Cannot add variable '"<<name<<"': there is already a parameter with that name.";

  assert(access(R).state == reg::used);

  variables[name] = R;
}

void context::rename_variable(const string& s1, const string& s2)
{
  // zero-length names are not allowed
  assert(s1.size() != 0);
  assert(s2.size() != 0);

  // if there's already an 's2', then complain
  if (find_variable(s2) != -1)
    throw myexception()<<"Cannot rename variable '"<<s1<<"' to '"<<s2<<"': there is already a variable with that name.";

  if (find_parameter(s2) != -1)
    throw myexception()<<"Cannot rename variable '"<<s1<<"' to '"<<s2<<"': there is already a parameter with that name.";

  // Remove the old name -> reg mapping
  map<string,int>::iterator loc = variables.find(s1);
  assert(loc != variables.end());

  int R = loc->second;

  variables.erase(loc);

  assert(access(R).state == reg::used);

  variables[s2] = R;
}

void context::rename_parameter(int i, const string& new_name)
{
  string old_name = parameter_name(i);

  int R = parameters[i];

  assert( access(R).changeable == true );
  access(R).E = parameter(new_name);
}

int incremental_evaluate(const context&, int);

/// Return the value of a particular index, computing it if necessary
shared_ptr<const Object> context::evaluate(int index) const
{
  int R = incremental_evaluate(*this, heads[index]);
  // FIXME - I'd like to update heads[index] to be R if it changed.
  //         This probably requires updating the memory roots.
  return *access(R).result;
}

expression_ref graph_normalize(const context&, const expression_ref&);

shared_ptr<const Object> context::evaluate_expression(const expression_ref& E) const
{
  int R = allocate_stack_reg();
  access(R).E = graph_normalize(*this, translate_refs(E));

  incremental_evaluate(*this,R);
  shared_ptr<const Object> result = *access(R).result;
  pop_reg(R);

  return result;
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
shared_ptr<const Object> context::get_parameter_value(int index) const
{
  return access(heads[index]).E;
}

/// Get the value of a non-constant, non-computed index
shared_ptr<const Object> context::get_parameter_value(const std::string&) const
{
  return shared_ptr<const Object>();
}

void context::set_parameter_value(int index, const expression_ref& O)
{
  int P = parameters[index];

  set_reg_value(P, O);
}

void set_call_if_reg_result(const context& C, int R)
{
  if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(*C[R].result))
  {
    int R2 = RV->target;
    
    // clear the result slot
    (*C[R].result).reset();
    
    C.set_call(R,R2);
  }
}

/// Update the value of a non-constant, non-computed index
void context::set_reg_value(int P, const expression_ref& OO)
{
  expression_ref O = graph_normalize(*this, translate_refs(OO));

  assert(dynamic_pointer_cast<const parameter>(access(P).E));
  assert(access(P).result);
  assert(access(P).changeable);
  clear_call(P);

  if (not is_WHNF(O))
  {
    int R = allocate_stack_reg();
    access(R).E = O;
    O = expression_ref( new reg_var(R) );

    // The result value here cannot be shared.
    access(P).result = shared_ptr< shared_ptr< const Object> >(new shared_ptr< const Object >(O));
    pop_reg(R);
  }
  else
  {
    // The result value here cannot be shared.
    access(P).result = shared_ptr< shared_ptr< const Object> >(new shared_ptr< const Object >(O));
  }
  set_call_if_reg_result(*this, P);

  vector< int > NOT_known_value_unchanged;
  std::set< int > visited;

  // The index that we just altered cannot be known to be unchanged.
  NOT_known_value_unchanged.push_back(P);
  visited.insert(P);

  // For each reg R1 that cannot (w/o recomputing) be known to be unchanged...
  for(int i=0;i<NOT_known_value_unchanged.size();i++)
  {
    int R1 = NOT_known_value_unchanged[i];

    // ... consider each downstream index2 that has index1 in slot2 of its computation (possibly unused).
    foreach(j,access(R1).outputs)
    {
      int R2 = j->first;

      // This one already marked NOT known_value_unchanged
      if (visited.find(R2) != visited.end()) continue;

      // Since R2 is not known to have identical USED inputs ...
      // ... then it is not known to have identical outputs
      NOT_known_value_unchanged.push_back(R2);
      visited.insert(R2);

      // Since the computation may be different, it can't be shared.
      access(R2).result = shared_ptr< shared_ptr< const Object> >(new shared_ptr< const Object >);

      // Since the computation may be different, we don't know if the value has changed.
      (*access(R2).result).reset();
      clear_call(R2);
    }

    foreach(j,access(R1).call_outputs)
    {
      int R2 = *j;

      // This one already marked NOT known_value_unchanged
      if (visited.find(R2) != visited.end()) continue;

      // Since R2 is not known to have identical USED inputs ...
      // ... then it is not known to have identical outputs
      NOT_known_value_unchanged.push_back(R2);
      visited.insert(R2);

      // Since the computation may be different, it can't be shared.
      access(R2).result = shared_ptr< shared_ptr< const Object> >(new shared_ptr< const Object >);

      // Since the computation may be different, we don't know if the value has changed.
      (*access(R2).result).reset();
    }
  }
}

/// Update the value of a non-constant, non-computed index
void context::set_parameter_value(const std::string& var, const expression_ref& O)
{
  set_parameter_value(find_parameter(var), O);
}

int context::n_parameters() const
{
  return parameters.size();
}

int context::find_parameter(const string& s) const
{
  for(int i=0;i<n_parameters();i++)
    if (parameter_name(i) == s)
      return i;

    throw myexception()<<"Can't find parameter named '"<<s<<"'";
}

int context::n_variables() const
{
  return variables.size();
}

int context::find_variable(const string& s) const
{
  map<string,int>::const_iterator loc = variables.find(s);
  if (loc == variables.end())
    return -1;

  return loc->second;
}

int context::add_parameter(const string& name)
{
  assert(name.size() != 0);

  int index = n_parameters();

  int R = allocate_root_reg();
  parameters.push_back( R );

  access(R).changeable = true;
  access(R).E = parameter(name);

  return index;
}

int reg_heap::n_regs() const
{
  return memory.size();
}

int reg_heap::n_free_regs() const
{
  int here = first_free_reg;
  int count = 0;
  for(;here != -1;here = access(here).next_reg)
    count++;
  return count;
}

int reg_heap::n_used_regs() const
{
  int here = first_used_reg;
  int count = 0;
  for(;here != -1;here = access(here).next_reg)
    count++;
  return count;
}

/// Add an expression that may be replaced by its reduced form
int context::add_compute_expression(const expression_ref& E)
{
  std::cout<<"add: "<<E->print()<<"\n";

  expression_ref T = graph_normalize(*this, translate_refs(E) );

  int R = -1;
  if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(T))
    R = RV->target;
  else
  {
    R = allocate_root_reg();
    access(R).E = T;
  }

  heads.push_back(R);
  return heads.size()-1;
}

/// Add an expression that may be replaced by its reduced form
int context::add_compute_expression(const string& name, const expression_ref& E)
{
  int index = add_compute_expression( E );
  int R = heads[index];
  add_variable(name, R);
  return index;
}

int context::n_expressions() const
{
  return heads.size();
}

expression_ref context::get_expression(int i) const
{
  return access(heads[i]).E;
}

int reg_heap::add_reg_to_free_list(int r)
{
  access(r).state = reg::free;
  access(r).prev_reg = -1;
  access(r).next_reg = first_free_reg;
  if (first_free_reg != -1)
    access(first_free_reg).prev_reg = r;
  first_free_reg = r;
  return r;
}

int reg_heap::get_free_reg()
{
  int r = first_free_reg;
  if (r != -1)
  {
    assert(access(r).state == reg::free);
    first_free_reg = access(r).next_reg;
    access(r).prev_reg = -1;
    access(r).next_reg = -1;
    access(r).state = reg::none;
  }
  return r;
}

int reg_heap::add_reg_to_used_list(int r)
{
  access(r).state = reg::used;
  access(r).prev_reg = -1;
  access(r).next_reg = first_used_reg;
  if (first_used_reg != -1)
    access(first_used_reg).prev_reg = r;
  first_used_reg = r;
  return r;
}

void reg_heap::remove_reg_from_used_list(int r)
{
  int P = access(r).prev_reg;
  int N = access(r).next_reg;

  if (P == -1)
    first_used_reg = N;
  else
    access(P).next_reg = N;

  if (N == -1)
    ;
  else
    access(N).prev_reg = P;

  access(r).state = reg::none;
}

void reg_heap::reclaim_used_reg(int r)
{
  // FIXME - we need to carefully clear references to things that might reference us back.. don't we?
  access(r).clear();
  remove_reg_from_used_list(r);
  add_reg_to_free_list(r);
}

void reg_heap::push_reg(int R)
{
  stack_roots.push_back(R);
}

void reg_heap::pop_reg(int R)
{
  if (stack_roots.empty())
    throw myexception()<<"Popping reg "<<R<<" when the top reg on the stack is ";

  if (stack_roots.back() != R)
    throw myexception()<<"Popping reg "<<R<<" when the top reg on the stack is ";

  stack_roots.pop_back();
}

void reg_heap::expand_memory(int s)
{
  assert(n_regs() == n_used_regs() + n_free_regs());

  int k = memory.size();
  memory.resize(memory.size()+s);
  for(int i=k;i<memory.size();i++)
    add_reg_to_free_list(i);

  assert(n_regs() == n_used_regs() + n_free_regs());
}

int reg_heap::allocate_root_reg()
{
  int R = allocate_reg();
  roots.push_back(R);
  return R;
}

int reg_heap::allocate_stack_reg()
{
  int R = allocate_reg();
  stack_roots.push_back(R);
  return R;
}

int reg_heap::allocate_reg()
{
  assert(n_regs() == n_used_regs() + n_free_regs());

  int r = get_free_reg();

  // allocation failed
  if (r == -1)
  {
    collect_garbage();
    assert(n_used_regs() + n_free_regs() == n_regs());
    if (memory.size() < n_used_regs()*2+10)
      expand_memory(memory.size()*2+10);
    r = get_free_reg();
    assert(r != -1);
  }

  add_reg_to_used_list(r);

  assert(n_regs() == n_used_regs() + n_free_regs());
  assert(access(r).state == reg::used);
  return r;
}

int context::allocate_root_reg() const
{
  int r = memory.allocate_root_reg();
  // FIXME - this should be unnecessary
  access(r).clear();
  return r;
}

int context::allocate_stack_reg() const
{
  int r = memory.allocate_stack_reg();
  // FIXME - this should be unnecessary
  access(r).clear();
  return r;
}

void context::collect_garbage() const
{
  memory.collect_garbage();
}

void get_exp_refs(const expression_ref& R, vector<int>& refs)
{
  if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>( R ))
  {
    refs.push_back(RV->target);
  }
  else if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    for(int i=0;i<E->size();i++)
      get_exp_refs(E->sub[i],refs);
  }
}

vector<int> get_exp_refs(const expression_ref& R)
{
  vector<int> regs;
  get_exp_refs(R,regs);
  return regs;
}

vector<int> get_reg_refs(const reg& R)
{
  vector<int> refs;

  get_exp_refs(R.E, refs);

  if (R.call != -1)
    refs.push_back(R.call);

  for(int j=0;j<R.used_inputs.size();j++)
    if (R.used_inputs[j] != -1)
      assert(includes(refs, R.used_inputs[j]));

  return refs;
}

void reg_heap::collect_garbage()
{
  std::cout<<"***********Garbage Collection******************"<<std::endl;
  assert(n_regs() == n_used_regs() + n_free_regs());

  vector<int> scan;
  for(int i=0;i<roots.size();i++)
    scan.push_back(roots[i]);
  for(int i=0;i<stack_roots.size();i++)
    scan.push_back(stack_roots[i]);

  while (not scan.empty())
  {
    vector<int> next_scan;
    for(int i=0;i<scan.size();i++)
    {
      reg& R = access(scan[i]);
      assert(R.state != reg::free);
      if (R.state == reg::checked) continue;

      R.state = reg::checked;
      vector<int> used_in_R = get_reg_refs(R);
   
      next_scan.insert(next_scan.end(), used_in_R.begin(), used_in_R.end());
    }
    scan = next_scan;
  }

  int here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);
    int next = access(here).next_reg;
    if (R.state == reg::checked)
      R.state = reg::used;
    else 
      reclaim_used_reg(here);

    here = next;
  }
}

reg_heap::reg_heap()
  :first_free_reg(-1),
   first_used_reg(-1)
{ }

expression_ref context::translate_refs(const expression_ref& R) const
{
  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (shared_ptr<const parameter> P = dynamic_pointer_cast<const parameter>(R))
  {
    int param_index = find_parameter(P->parameter_name);
    
    if (param_index == -1)
      throw myexception()<<"Can't translate undefined parameter '"<<P->parameter_name<<"' in expression!";

    int param_location = parameters[param_index];

    return expression_ref(new reg_var(param_location) );
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (shared_ptr<const var> V = dynamic_pointer_cast<const var>(R))
  {
    map<string,int>::const_iterator loc = variables.find(V->name);
    if (loc == variables.end())
      throw myexception()<<"Can't translate undefined variable '"<<V->name<<"' in expression!";

    int R = loc->second;

    return expression_ref(new reg_var(R) );
  }

  // Other constants have no parts, and don't need to be translated
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return R;

  // Translate the parts of the expression
  expression_ref R2 = R;
  expression* V = new expression(*E);
  for(int i=0;i<V->size();i++)
    V->sub[i] = translate_refs(V->sub[i]);

  return V;
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

context::context()
{ }

shared_ptr<const Object> context::default_parameter_value(int i) const
{
  expression_ref default_value = lambda_expression(data_function("default_value",2));

  vector<expression_ref> results;
  expression_ref query = default_value( parameter_name(i) )(match(0));
  int found = find_match_notes(query, results, 0);

  if (found != -1)
  {
    assert(results.size());
    return results[0];
  }  
  else
    return shared_ptr<const Object>();
}

context::context(const vector<expression_ref>& N)
  :notes(N)
{ }

expression_ref graph_normalize(const context& C, const expression_ref& R)
{
  // 1. Var
  if (is_dummy(R))
    return R;
  
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);

  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E) return R;
  
  // 2. Lambda
  shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]);
  if (L)
  {
    assert(E->size() == 3);
    expression* V = new expression(*E);
    V->sub[2] = graph_normalize(C,E->sub[2]);

    if (V->sub[2] == E->sub[2])
      return R;
    else
      return V;
  }

  // 3. Application
  if (dynamic_pointer_cast<const Apply>(E->sub[0]))
  {
    assert(E->size() == 3);
    expression_ref f = graph_normalize(C,E->sub[1]);
    expression_ref x = graph_normalize(C,E->sub[2]);

    int var_index = get_safe_binder_index(R);
    expression_ref f_ = dummy(var_index++);
    expression_ref x_ = dummy(var_index++);

    if (is_dummy(x))
    { 
      return let_expression(f_, f, apply_expression(f_,x));
    }
    else
    {
      vector<expression_ref> vars;
      vector<expression_ref> bodies;

      vars.push_back(f_);
      vars.push_back(x_);

      bodies.push_back(f);
      bodies.push_back(x);

      return let_expression(vars, bodies, apply_expression(f_,x_));
    }
  }

  // 6. Case
  shared_ptr<const Case> IsCase = dynamic_pointer_cast<const Case>(E->sub[0]);
  if (IsCase)
  {
    expression* V = new expression(*E);

    V->sub[1] = graph_normalize(C,V->sub[1]);

    shared_ptr<expression> bodies = dynamic_pointer_cast<expression>(V->sub[2]);
    while(bodies)
    {
      assert(bodies->size() == 3);
      shared_ptr<expression> alternative = dynamic_pointer_cast<expression>(bodies->sub[1]);
      assert(alternative);
      alternative->sub[2] = graph_normalize(C,alternative->sub[2]);
      bodies = dynamic_pointer_cast<expression>(bodies->sub[2]);
    }
    
    if (is_dummy(V->sub[1]))
      return V;
    else
    {
      int var_index = get_safe_binder_index(R);
      expression_ref x = dummy(var_index);
      expression_ref obj = V->sub[1];
      V->sub[1] = x;

      return let_expression(x,obj,V);
    }
  }

  // FIXME! Handle operations.
  // Extend the stack handling to be able to work on more than one argument.
  // Currently there is no need to evaluate arguments before applying them to functions.
  // Can we avoid evaluating functions before calling an operation?
  // - well, we could make the operation throw an exception identifying which is the first argument that needs to be
  //   evaluated.
  // - then, we could put the operation on the stack and begin evaluating just that one argument.
  
  // 4. Constructor
  if (dynamic_pointer_cast<const Function>(E->sub[0]) or 
      dynamic_pointer_cast<const Operation>(E->sub[0]))
  {
    int var_index = get_safe_binder_index(R);

    expression* Con = new expression;
    Con->sub.push_back(E->sub[0]);

    // Actually we probably just need x[i] not to be free in E->sub[i]
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    for(int i=1;i<E->size();i++)
    {
      if (is_dummy(E->sub[i]))
      {
	Con->sub.push_back(E->sub[i]);
      }
      else
      {
	expression_ref var = dummy( var_index++ );
	Con->sub.push_back( var );
	vars.push_back( var );
	bodies.push_back( graph_normalize(C,E->sub[i]) );
      }
    }

    return let_expression(vars, bodies, Con);
  }

  // 5. Let 
  shared_ptr<const let_obj> Let = dynamic_pointer_cast<const let_obj>(E->sub[0]);
  if (Let)
  {
    expression* V = new expression(*E);

    shared_ptr<expression> bodies = dynamic_pointer_cast<expression>(V->sub[1]);
    while(bodies)
    {
      assert(bodies->size() == 3);
      shared_ptr<expression> let_group = dynamic_pointer_cast<expression>(bodies->sub[1]);
      assert(let_group);
      let_group->sub[2] = graph_normalize(C,let_group->sub[2]);
      bodies = dynamic_pointer_cast<expression>(bodies->sub[2]);
    }
    
    V->sub[2] = graph_normalize(C,V->sub[2]);

    return V;
  }

  std::cerr<<"I don't recognize expression '"+ R->print() + "'\n";
  return R;
}

int incremental_evaluate(const context&, int);

#include "computation.H"

struct RegOperationArgs: public OperationArgs
{
  shared_ptr<const expression> E;

  const int R;

  const context& C;

  boost::shared_ptr<const Object> reference(int slot) const
  {
    return E->sub[slot+1];
  }

  boost::shared_ptr<const Object> evaluate(int slot)
  {
    // Any slot that we are going to evaluate needs to point to another node
    shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>( reference(slot) );
    assert(RV);
    int R2 = RV->target;

    if (not C[R].used_inputs[slot] != -1)
    {
      // evaluate R
      R2 = incremental_evaluate(C, R2);

      // Adjust the reference, if it changed.
      if (R2 != RV->target)
	dynamic_pointer_cast<expression>(C[R].E)->sub[slot+1] = new reg_var(R2);

      // mark R2 used by R in the correct slot
      C.set_used_input(R, slot, R2);

      // If R2 -> result was changeable, then R -> result will be changeable as well.
      if (C[R2].changeable) 
	C[R].changeable = true;
    }

    return *(C[R2].result);
  }

  RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

  RegOperationArgs(int r, const context& c)
    :R(r),C(c)
  { 
    // The object we are evaluating had better be a class expression (with parts).
    E = dynamic_pointer_cast<const expression>(C[R].E);
    assert(E);

    C.clear_used_inputs(R, E->size()-1);
  }
};

expression_ref compact_graph_expression(const context& C, const expression_ref& R);

  /*
   * eval r: p[r] = E
   * 
   *   if p[r] = E => F then
   *      return r.
   *  
   *   else if p[r] = E has a call to p[s] then
   *      s = eval s
   *      p[r] = E => (result of p[s])
   *      if p[r] is not changeable
   *         r = s
   *      return r;
   *  
   *   else if E is WHNF then
   *      p[r] = E => E
   *
   *   else if E is a variable p[s] then
   *      p[r] = p[s] => p[s]
   *
   *   else if E is a parameter then the result must already be set
   *      p[r] = parameter => R
   *     
   *   else
   *      assert(E is not a parameter)
   *      reduce E -> (F,changable)         [operations + let expression]
   *      if E->F is unchangeable
   *         p[r] = F
   *         restart
   *      else
   *         if (F is a variable p[s])
   *            p[r] = E => p[s]
   *         else if (F is WHNF)
   *            p[r] = E => F
   *         else
   *            s = allocate new reg
   *            p[r] = E => p[s]
   *         end if
   *   end if
   *
   *   Cases at this point:
   *   - NOT p[r] = parameter => E
   *   - NOT p[r] = parameter => p[s]
   *   -     p[r] = E => E
   *   -     p[r] = p[s] => p[s]
   *   -     p[r] = E => p[s] @ changeable
   *   -     p[r] = E => F    @ changeable
   *   - NOT p[r] = E => p[s] 
   *     --> p[r] = p[s] / restart
   *   - NOT p[r] = E => F 
   *     --> p[r] = F    / restart
   *
   *   if p[r] = E => p[s] then
   *      if (not changeable)
   *         r = s
   *      else
   *         set_call(r,s)
   *         restart
   *
   *   Cases at this point:
   *
   *   - NOT p[r] = parameter => E
   *   - NOT p[r] = parameter => p[s]
   *   -     p[r] = E => E
   *   - NOT p[r] = p[s] => p[s]
   *     --> p[r] = p[s] -> call s
   *   - NOT p[r] = E => p[s] @ changeable
   *     --> p[r] = p[s] -> call s
   *   -     p[r] = E => F    @ changeable
   *   - NOT p[r] = E => p[s] 
   *     --> p[r] = p[s] / restart
   *   - NOT p[r] = E => F 
   *     --> p[r] = F    / restart
   */

/// Evaluate C[R] and return a reg containing the results that looks through unchangeable redirections = reg_var chains
int  incremental_evaluate(const context& C, int R)
{
  assert(C[R].state == reg::used);
  assert(R >= 0 and R < C.n_regs());
  assert(C[R].result);

  std::cout<<"Statement: "<<R<<":   "<<C[R].E->print()<<std::endl;
  while (not *C[R].result)
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;

    std::cout<<"   statement: "<<R<<":   "<<C[R].E->print()<<std::endl;
    // If we know what to call, then call it and use it to set the result
    if (C[R].call != -1)
    {
      // Evaluate C[S], looking through unchangeable redirections
      int S = incremental_evaluate(C, C[R].call);

      *(C[R].result) = *(C[S].result);

      if (not C[R].changeable)
	R = S;

      continue;
    }

    /*---------- Below here, there is no call, and no result. ------------*/

    // Check for WHNF *OR* heap variables
    else if (is_WHNF(C[R].E))
      *(C[R].result) = C[R].E;

    // A parameter has a result that is not computed by reducing an expression.
    //       The result must be set.  Therefore, complain if the result is missing.
    else if (shared_ptr<const parameter> p = dynamic_pointer_cast<const parameter>(C[R].E))
      throw myexception()<<"Parameter with no result?! (Changeable = "<<C[R].changeable<<")";

    
    // Reduction: let expression
    else if (parse_let_expression(C[R].E, vars, bodies, T))
    {
      vector<shared_ptr<reg_var> > new_reg_vars;
      for(int i=0;i<vars.size();i++)
	new_reg_vars.push_back( shared_ptr<reg_var>(new reg_var(C.allocate_stack_reg())) );
      
      // Substitute the new heap vars for the dummy vars in expression T and in the bodies
      for(int i=0;i<vars.size();i++) 
      {
	// if the body is already a reg_var, let's not add a new reg_var just to point to it!
	expression_ref replacement_reg_var = new_reg_vars[i]->clone();
	if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(bodies[i]))
	  replacement_reg_var = bodies[i];

	for(int j=0;j<vars.size();j++)
	  bodies[j] = substitute(bodies[j], vars[i], *replacement_reg_var);
	
	T = substitute(T, vars[i], *replacement_reg_var);
      }
      
      assert(not C[R].changeable);
      C[R].E = T;

      for(int i=0;i<vars.size();i++) 
	C[ new_reg_vars[i]->target ].E = bodies[i];

      for(int i=vars.size()-1; i>=0; i--) 
	C.pop_reg( new_reg_vars[i]->target );
      
      assert(C[R].call == -1);
      assert(not *C[R].result);
    }
    
    // 3. Reduction: Operation (includes @, case, +, etc.)
    else
    {
      shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(C[R].E);
      assert(E);
      
      shared_ptr<const Operation> O = dynamic_pointer_cast<const Operation>(E->sub[0]);
      assert(O);

      // Although the reg itself is not a parameter, it will stay changeable if it ever computes a changeable result.
      // Therefore, we cannot do "assert(not C[R].changeable);" here.

      RegOperationArgs Args(R, C);
      expression_ref result = (*O)(Args);
      if (not C[R].changeable)
      {
	// The old used_input slots are not invalid, which is OK since none of them are changeable.
	C[R].E = result;
	C.clear_used_inputs(R);
	assert(C[R].call == -1);
	assert(not *C[R].result);
      }
      else
      {
	// Check for WHNF *OR* heap variables
	if (is_WHNF(result))
	  *(C[R].result) = result;
	else {
	  int R2 = C.allocate_stack_reg();
	  C.access(R2).E = result;
	  *C[R].result = shared_ptr<const Object>(new reg_var(R2));
	  C.pop_reg(R2);
	}
      }
	
#ifndef NDEBUG
      //      std::cout<<"Executing statement: "<<compact_graph_expression(C,E)<<"\n";
      std::cout<<"Executing operation: "<<O<<"\n";
      std::cout<<"Result changeable: "<<C[R].changeable<<"\n\n";
#endif
    }

    // 4. We can't exit the loop with a reg_var in the result slot. Change to a call.
    set_call_if_reg_result(C,R);
  }

#ifndef NDEBUG
  //  std::cout<<"Result = "<<compact_graph_expression(*C[R].result)<<"\n";
  //  std::cout<<"Result changeable: "<<C[R].changeable<<"\n\n";
#endif

  assert(*C[R].result);
  assert(is_WHNF(*C[R].result));
  assert(not dynamic_pointer_cast<const reg_var>(*C[R].result));

  return R;
}

void discover_graph_vars(const context& C, const expression_ref& R, map< int, std::string>& names)
{
  if (shared_ptr<const reg_var> H = dynamic_pointer_cast<const reg_var>(R))
  {
    if (names.find(H->target) != names.end())
    {
      // back out, we've been through this node before.
    }

    if (names.find(H->target) == names.end())
    {
      int num = names.size()+1;
      // give this node a name and mark it visited
      names[H->target] = "p"+convertToString(num);

      discover_graph_vars(C, C[H->target].E, names);
    }

  }

  if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    for(int i=0;i<E->size();i++)
      discover_graph_vars(C, E->sub[i], names);
  }
}

expression_ref compact_graph_expression(const context& C, const expression_ref& R_)
{
  //  return R_;
  expression_ref R = R_;
  map< int, std::string> names;

  int var_index = get_safe_binder_index(R);

  discover_graph_vars(C, R,names);

  //  std::cout<<R<<std::endl;
  vector< expression_ref > replace;
  foreach(i,names)
  {
    replace.push_back( reg_var( i->first) );
    var_index = std::max(var_index, get_safe_binder_index(C[i->first].E) );
    //    std::cout<<"<"<<i->first->name<<"> = "<<i->first->E<<std::endl;
  }
  //  std::cout<<R<<std::endl;
  vector<expression_ref> vars;
  vector<expression_ref> bodies;
  foreach(i,names)
  {
    vars.push_back(dummy(var_index++));
    bodies.push_back( C[i->first].E );
  }

  for(int i=0;i<bodies.size();i++)
  {
    //    std::cout<<"------\n";
    //    std::cout<<replace[i]<<" -> "<<vars[i]<<":\n";
    for(int j=0;j<bodies.size();j++)
    {
      bodies[j] = substitute(bodies[j], replace[i], vars[i]);
      //      std::cout<<vars[j]<<" = "<<bodies[j]<<std::endl;
    }

    R = substitute(R, replace[i], vars[i]);
    //    std::cout<<"R = "<<R<<std::endl;
  }

  R = let_expression(vars, bodies, R);
  //  std::cout<<R<<std::endl;
  R = launchbury_unnormalize(R);
  //  std::cout<<"substituted = "<<launchbury_unnormalize(R)<<std::endl;
  return R;
}

void add_prefix(const string& prefix, vector<expression_ref>& notes)
{
  for(int i=0;i<notes.size();i++)
    add_prefix(prefix, notes[i]);
}

boost::shared_ptr<context> prefix_formula(const std::string& prefix, const boost::shared_ptr<const context>& C)
{
  shared_ptr<context> C2(C->clone());
  // prefix the parameter names
  for(int i=0;i<C2->n_parameters();i++)
    C2->rename_parameter(i, prefix + "::" + C2->parameter_name(i));

  // prefix the variable names
  for(int i=0;i<C2->n_parameters();i++)
    ;

  // prefix the names in the model
  add_prefix(prefix, C2->get_notes());
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
