#include "graph_register.H"
#include "operations.H"

using boost::shared_ptr;
using std::string;
using std::vector;
using std::map;

reg::reg():name(convertToString(this)),named(false),changeable(false) {}
reg::reg(const string& s):name(s),named(true),changeable(false) {}
reg::reg(const expression_ref& e):E(e),name(convertToString(this)),named(false),changeable(false) {}

int reg_machine::find_free_token() const
{
  int token=-1;
  for(int i=0;i<is_token_active.size();i++)
    if (not is_token_active[i]) {
      token = i;
      break;
    }
  
  return token;
}

int reg_machine::add_token()
{
  int token = is_token_active.size();
  is_token_active.push_back(false);
  return token;
}

int reg_machine::claim_token()
{
  int token = find_free_token();

  if (token == -1)
    token = add_token();

  is_token_active[token] = true;

  //  std::cerr<<"-> "<<countt(active)<<"/"<<active.size()<<std::endl;
  return token;
}

void reg_machine::copy_token(int token1,int token2)
{
}

void reg_machine::init_token(int token)
{
}

void reg_machine::release_token(int token)
{
  is_token_active[token] = false;
}

shared_ptr<const Object> incremental_evaluate(const context&, shared_ptr<reg>&);

/// Return the value of a particular index, computing it if necessary
shared_ptr<const Object> context::evaluate(int index) const
{
  return incremental_evaluate(*this,heads[index]);
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
shared_ptr<const Object> context::get_parameter_value(int index) const
{
  return heads[index]->E;
}

/// Get the value of a non-constant, non-computed index
shared_ptr<const Object> context::get_parameter_value(const std::string&) const
{
  return shared_ptr<const Object>();
}

/// Update the value of a non-constant, non-computed index
void context::set_parameter_value(int index, const expression_ref& O)
{
  assert(is_WHNF(O));

  shared_ptr<reg> P = parameters[index];
  assert(P->changeable);

  if (P->result)
    // FIXME - invalidation is not working yet.
    std::abort();
  else
    P->result = shared_ptr< shared_ptr< const Object> >(new shared_ptr<const Object>(O));
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
  int index = find_index(parameter_names, s);
  if (index == -1)
    throw myexception()<<"Can't find parameter named '"<<s<<"'";
  return index;
}

int context::add_parameter(const string& s)
{
  int index = n_parameters();

  shared_ptr<reg> R(new reg(parameter(s)));
  R->name = s;
  R->named = true;
  R->changeable = true;

  parameter_names.push_back(s);
  parameters.push_back( R );

  return index;
}

expression_ref graph_normalize(const expression_ref& R);

int context::add_expression(const expression_ref& E)
{
  shared_ptr<reg> R ( new reg );
  std::cout<<"add: "<<E->print()<<"\n";
  R->E = graph_normalize(E);
  heads.push_back(R);
  return heads.size()-1;
}

context& context::operator=(const context&C)
{
  return *this;
}

context::context()
  :machine(new reg_machine),
   token(machine->claim_token())
{
  machine->init_token(token);
}

context::context(const context& C)
  :machine(C.machine),
   token(machine->claim_token())
{
  machine->copy_token(token, C.token);
}

context::~context()
{
  machine->release_token(token);
}

expression_ref graph_normalize(const expression_ref& R)
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
    V->sub[2] = graph_normalize(E->sub[2]);

    if (V->sub[2] == E->sub[2])
      return R;
    else
      return V;
  }

  // 3. Application
  if (dynamic_pointer_cast<const Apply>(E->sub[0]))
  {
    assert(E->size() == 3);
    expression_ref f = graph_normalize(E->sub[1]);
    expression_ref x = graph_normalize(E->sub[2]);

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

    V->sub[1] = graph_normalize(V->sub[1]);

    shared_ptr<expression> bodies = dynamic_pointer_cast<expression>(V->sub[2]);
    while(bodies)
    {
      assert(bodies->size() == 3);
      shared_ptr<expression> alternative = dynamic_pointer_cast<expression>(bodies->sub[1]);
      assert(alternative);
      alternative->sub[2] = graph_normalize(alternative->sub[2]);
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

    expression* C = new expression;
    C->sub.push_back(E->sub[0]);

    // Actually we probably just need x[i] not to be free in E->sub[i]
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    for(int i=1;i<E->size();i++)
    {
      if (is_dummy(E->sub[i]))
      {
	C->sub.push_back(E->sub[i]);
      }
      else
      {
	expression_ref var = dummy( var_index++ );
	C->sub.push_back( var );
	vars.push_back( var );
	bodies.push_back( graph_normalize(E->sub[i]) );
      }
    }

    return let_expression(vars, bodies, C);
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
      let_group->sub[2] = graph_normalize(let_group->sub[2]);
      bodies = dynamic_pointer_cast<expression>(bodies->sub[2]);
    }
    
    V->sub[2] = graph_normalize(V->sub[2]);

    return V;
  }

  std::cerr<<"I don't recognize expression '"+ R->print() + "'\n";
  return R;
}

shared_ptr<const Object> incremental_evaluate(const context&, const shared_ptr<reg>&);

#include "computation.H"

struct RegOperationArgs: public OperationArgs
{
  shared_ptr<const expression> E;

  const shared_ptr<reg>& R;

  const context& C;

  bool changeable;

  boost::shared_ptr<const Object> reference(int slot) const
  {
    return E->sub[slot+1];
  }

  boost::shared_ptr<const Object> evaluate(int slot)
  {
    // Any slot that we are going to evaluate needs to point to another node
    shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>( reference(slot) );
    assert(RV);
    shared_ptr<reg> R2 = RV->target;

    if (not R->used_inputs[slot])
    {
      // do we really want to allow incremental_evaluate to modify R2, here?
      incremental_evaluate(C,R2);

      R->used_inputs[slot] = R2;

      R2->outputs.insert(R);

      if (R2->changeable) 
	changeable = true;
    }

    return *(R2->result);
  }

  RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

  RegOperationArgs(const shared_ptr<reg>& r, const context& c)
    :R(r),C(c),changeable(false)
  { 
    // The object we are evaluating had better be a class expression (with parts).
    E = dynamic_pointer_cast<const expression>(R->E);
    assert(E);

    R->used_inputs.resize(E->size()-1);
    for(int i=0;i<R->used_inputs.size();i++)
      R->used_inputs[i].reset();
  }
};

expression_ref compact_graph_expression(const expression_ref& R);

shared_ptr<const Object> incremental_evaluate(const context& C, shared_ptr<reg>& R)
{
  while (true)
  {
    // Create the (possibly shared) result slot if it doesn't exist.
    if (not R->result) R->result = shared_ptr< shared_ptr<const Object> >(new shared_ptr<const Object> );
    
    // Return the result if its available
    if (*(R->result)) break;

    // If we know what to call, then call it and use it to set the result
    if (R->call)
    {
      incremental_evaluate(C, R->call);

      // If the used_inputs weren't changeable, we would have replaced R->E with R->call.
      assert(R->changeable);
      
      *(R->result) = *(R->call->result);
      continue;
    }

    /*---------- Below here, there is no call, and no result. ------------*/

    // Compute the value of this result
    expression_ref control = R->E;
    std::cout<<R->name<<":control = "<<control<<"\n";
    std::cout<<R->name<<":control(c) = "<<compact_graph_expression(control)<<"\n";

    // If this expression cannot be reduced further, then just return it here.
    if (is_WHNF(R->E)) {
      *(R->result) = R->E;
      continue;
    }

    /*--------- III. a ---------*/
    
    // 1. Let expressions
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;
    if (parse_let_expression(control, vars, bodies, T))
    {
      vector<shared_ptr<reg_var> > new_reg_vars;
      for(int i=0;i<vars.size();i++)
      {
	shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(vars[i]);
	assert(D);
	if (D->name.size())
	  new_reg_vars.push_back( shared_ptr<reg_var>(new reg_var(D->name)) );
	else
	  new_reg_vars.push_back( shared_ptr<reg_var>(new reg_var) );
      }
      
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
      
      for(int i=0;i<vars.size();i++) 
      {
	new_reg_vars[i]->value() = bodies[i];
      }
      
      R->E = T;
      continue;
    }
    
    /// IIIb. A parameter -> Set the result according to the context
    if (shared_ptr<const parameter> p = dynamic_pointer_cast<const parameter>(control))
    {
      int index = C.find_parameter(p->parameter_name);
      if (not R->changeable)
      {
	R = C.parameters[index];
	continue;
      }
      else
	throw myexception()<<"Parameter with no result?!";
    }

    // 2. A free variable. This should never happen.
    shared_ptr<const dummy> D = dynamic_pointer_cast<const dummy>(control);
    assert(not D);
    
    shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(control);
    assert(E);
    
    // 3. An Operation (includes @, case, +, etc.)
    if (shared_ptr<const Operation> O = dynamic_pointer_cast<const Operation>(E->sub[0]))
    {
      RegOperationArgs Args(R, C);
      expression_ref result = (*O)(Args);
      if (Args.changeable)
      {
	R->changeable = true;
	if (is_WHNF(result))
	  *(R->result) = result;
	else
	{
	  if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(result))
	    R->call = RV->target;
	  else
	    R->call = shared_ptr<reg>(new reg(result));
	}
      }
      else
      {
	R->E = result;
      }
      std::cout<<"Executing statement: "<<compact_graph_expression(E)<<"\n";
      std::cout<<"Executing operation: "<<O<<"\n";
      std::cout<<"Result = "<<compact_graph_expression(result)<<"\n";
      std::cout<<"Result changeable: "<<Args.changeable<<"\n\n";
    }
  }

  assert(R->result);
  assert(*R->result);

  return *(R->result);
}

expression_ref incremental_evaluate(const context& C, const expression_ref& E)
{
  shared_ptr<reg> R(new reg);
  R->E = graph_normalize(E);

  expression_ref result = incremental_evaluate(C,R);
  result = compact_graph_expression(result);

  return result;
}


void discover_graph_vars(const expression_ref& R, map< shared_ptr<reg>, std::string>& names)
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

      discover_graph_vars(H->target->E, names);
    }

  }

  if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    for(int i=0;i<E->size();i++)
      discover_graph_vars(E->sub[i], names);
  }
}

expression_ref compact_graph_expression(const expression_ref& R_)
{
  expression_ref R = R_;
  map< shared_ptr<reg>, std::string> names;

  int var_index = get_safe_binder_index(R);

  discover_graph_vars(R,names);

  //  std::cout<<R<<std::endl;
  vector< expression_ref > replace;
  foreach(i,names)
  {
    replace.push_back( reg_var( i->first) );
    var_index = std::max(var_index, get_safe_binder_index(i->first->E) );
    //    std::cout<<"<"<<i->first->name<<"> = "<<i->first->E<<std::endl;
  }
  //  std::cout<<R<<std::endl;
  vector<expression_ref> vars;
  vector<expression_ref> bodies;
  foreach(i,names)
  {
    if (not i->first->named)
      vars.push_back(dummy(var_index++));
    else
      vars.push_back(dummy(i->first->name));
    bodies.push_back( i->first->E );
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


// + 
// apply 
// +-*/
// constructor
// case
// let












