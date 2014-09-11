#include <vector>
#include <set>
#include "let-float.H"

using std::vector;
using std::set;

// ?- Determine which let statements have bound vars (bound_indices) and which do not (unbound_indices).
//    (The ones with no bound vars can be floated.)
bool find_let_statements_with_bound_vars(const vector<expression_ref>& let_vars, const vector<expression_ref>& let_bodies,
					 const set<dummy>& bound,
					 vector<int>& bound_indices, vector<int>& unbound_indices)
{
  set<dummy> let_bound;
  for(int i=0;i<let_vars.size();i++)
    let_bound.insert(*assert_is_a<dummy>(let_vars[i]));

  // Find the set of bound variables that could be free in let_bodies
  set<dummy> visible_bound = bound;
  for(const auto& i: let_bound)
    visible_bound.erase(i);

  vector< set<dummy> > free_vars;
  for(int i=0;i<let_bodies.size();i++)
    free_vars.push_back( get_free_indices( let_bodies[i] ) );

  bound_indices.clear();
  unbound_indices.clear();
  for(int i=0;i<let_bodies.size();i++)
    unbound_indices.push_back(i);

  // Find the indices that are not bound (directly or indirectly) by the bound variables
  set<dummy> new_bound = visible_bound;
  while (not new_bound.empty())
  {
    set<dummy> new_bound_next;
    for(int i=unbound_indices.size()-1;i>=0;i--)
    {
      int index = unbound_indices[i];
      if (not intersection(free_vars[index], new_bound).empty())
      {
	new_bound_next.insert(*assert_is_a<dummy>(let_vars[index]));
	bound_indices.push_back(index);
	unbound_indices.erase( unbound_indices.begin() + i);
      }
    }
    new_bound = new_bound_next;
  }

  return (not unbound_indices.empty());
}

//question: is move_lets supposed to be called with empty vars?
//answer: yes, sometimes.

/// Given let vars=bodies in (<binder bound> (let E_vars=E_bodies in T)), 
///  move some of the E_vars=E_bodies up to vars=bodies.
expression_ref move_lets(bool scope, const expression_ref E, 
			 vector<expression_ref>& vars, vector<expression_ref>& bodies,
			 const set<dummy>& bound, const set<dummy>& free)
{
  assert(E);
  assert(vars.size() == bodies.size());

  vector<expression_ref> E_vars;
  vector<expression_ref> E_bodies;
  expression_ref E2 = E;

  if (not parse_let_expression(E, E_vars, E_bodies, E2))
    E2 = E;

  // Find the set of variables to avoid renaming over: free + bound + let-bound-just-above
  //    (Hmm... should the let-bound-just-above be in 'bound'?)
  set<dummy> avoid = free;
  for(int i=0;i<vars.size();i++)
  {
    dummy D = *assert_is_a<dummy>(vars[i]);
    avoid.insert(D);
    add(avoid, get_free_indices(bodies[i]));
  }
  add(avoid, get_free_indices(E));
  add(avoid, bound);

  int new_index = max_index(avoid) + 1;
    

  // Determine which of the let-statements in E we can float.
  vector<int> unbound_indices;
  vector<int> bound_indices;
  if (find_let_statements_with_bound_vars(E_vars, E_bodies, bound, bound_indices, unbound_indices))
  {
    // Adjust the new indices to avoid hitting any of the other let-binder-variables in E
    for(int i=0;i<E_vars.size();i++)
    {
      dummy D = *assert_is_a<dummy>(E_vars[i]);
      new_index = std::max(new_index, D.index + 1);
    }

    /******************** alpha-rename E -> EE ********************/
    
    object_ptr<expression> EE ( E.ptr()->clone() );                  // Make a copy of E that we can alpha-rename.
    
    for(int index: unbound_indices)
    {
      dummy D = *assert_is_a<dummy>(E_vars[index]);
      if (includes(avoid, D))
      {
	dummy D2(new_index++);
	assert(not includes(avoid,D2));
	alpha_rename(EE, D, D2);
	avoid.insert(D2);
      }
    }
  
    // Recompute E_vars and E_bodies from the alpha-renamed version of E
    parse_let_expression(EE, E_vars, E_bodies, E2);            
    
    /*********** move free lets to higher-level environment **********/
    for(int index: unbound_indices)
    {
#ifndef NDEBUG
      // Check that we aren't duplicating any variables in the higher-level environment
      //      for(int j=0;j<vars.size();j++)
      //	assert(not includes(vars, E_vars[index]));
#endif
      vars.push_back(E_vars[index]);
      bodies.push_back(E_bodies[index]);
    }

    // Construct the remainder expression
    vector<expression_ref> E_vars2;
    vector<expression_ref> E_bodies2;
    for(int i=0;i<bound_indices.size();i++)
    {
      int index = bound_indices[i];
      
      E_vars2.push_back(E_vars[index]);
      E_bodies2.push_back(E_bodies[index]);
    }

    E2 = let_expression(E_vars2, E_bodies2, E2);
  }
  // If nothing is moveable, then just return the original statement.
  else
    E2 = E;

  // We can't float this out because its bound, or because there's no bound to float it through.
  if ((not scope) or (not intersection(get_free_indices(E2), bound).empty()))
  {
    assert(E2);
    return E2;
  }

  // Since we only substitute reg_vars into dummy's (for let, lambda, and case) these are all OK.
  // change to is_reg_like
  if (is_reglike(E2))
  {
    assert(E2);
    return E2;
  }


  // If E2 is not bound, and its not a let-bound dummy, then create a new expression for it.
  dummy D2(new_index++);
  vars.push_back( D2 );
  bodies.push_back( E2 );
  return D2;
}

expression_ref move_lets(bool scope, const expression_ref E, 
			 vector<expression_ref>& vars, vector<expression_ref>& bodies,
			 const set<dummy>& bound)
{
  set<dummy> free;
  return move_lets(scope, E, vars, bodies, bound, free);
}

expression_ref move_lets(bool scope, const expression_ref E,
			 vector<expression_ref>& vars, vector<expression_ref>& bodies)
{
  set<dummy> bound;
  return move_lets(scope, E, vars, bodies, bound);
}

template <typename T>
bool operator==(const std::set<T>& S1, const std::set<T>& S2)
{
  return includes(S1,S2) and includes(S2,S2);
}

// When we let_float \x.\y.x, we should float out x, even though its a dummy

// However, if we have let {z=2} in \x.\y.z, we should not introduce a let dummy
// for z, because its already let bound.

expression_ref let_float(const expression_ref& E)
{
  // 0. NULL
  if (not E) return E;

  // 1. Dummy variable
  // 2. Literal constants.  Treat as 0-arg constructor.
  if (not E.size()) return E;
  
  set<dummy> free_in_E = get_free_indices(E);

  vector<expression_ref> vars;
  vector<expression_ref> patterns;
  vector<expression_ref> bodies;
  expression_ref T;
  expression_ref E2;

  // 3. Lambda expressions
  if (object_ptr<const lambda> L = is_a<lambda>(E))
  {
    // Find the new let-bound set.
    dummy D = *assert_is_a<dummy>(E.sub()[0]);

    // First float lets in sub-expressions
    expression_ref M = let_float(E.sub()[1]);

    // Determine the bound indices
    set<dummy> bound;
    bound.insert(D);

    // Move lets across the lambda
    M = move_lets(true, M, vars, bodies, bound, free_in_E);

    // Reassemble the expression
    E2 = let_expression(vars, bodies, lambda_quantify(D, M) );

    assert(free_in_E == get_free_indices(E2));
  }

  // 4. Case expressions
  else if (parse_case_expression(E,T,patterns,bodies))
  {
    vector<expression_ref> let_vars;
    vector<expression_ref> let_bodies;

    // First float out of case object (bound = {}, free = fv(E))
    T = let_float(T);
    T = move_lets(true, T, let_vars, let_bodies, set<dummy>(), free_in_E);

    for(int i=0;i<bodies.size();i++)
    {
      // Find the bound variables in the i-th constructor
      set<dummy> bound = get_free_indices(patterns[i]);

      // Second float out of the case alternative bodies (bound = fv(patterns[i]), free = fv(E))
      // (Note: free = fv(E) is a bit conservative.)
      bodies[i] = let_float(bodies[i]);
      bodies[i] = move_lets(true, bodies[i], let_vars, let_bodies, bound, free_in_E);
    }

    E2 = let_expression(let_vars, let_bodies, make_case_expression(T, patterns, bodies));

    assert(free_in_E == get_free_indices(E2));
  }

  // 5. Let expressions
  else if (parse_let_expression(E,vars,bodies,T))
  {
    // Return let_float(T) if T doesn't mention any of the newly let-bound variables
    set<dummy> bound_vars_let;
    for(int i=0;i<vars.size();i++)
      bound_vars_let.insert(*assert_is_a<dummy>(vars[i]));

    set<dummy> free_vars_T = get_free_indices(T);
    if (intersection(bound_vars_let, free_vars_T).empty()) 
      return let_float(T);

    // First float lets in sub-expressions
    T = let_float(T);
    for(int i=0;i<bodies.size();i++)
      bodies[i] = let_float(bodies[i]);

    // Move lets out of T and into vars
    T = move_lets(false, T, vars, bodies, set<dummy>(), free_in_E);

    // Move lets out of bodies and into vars
    for(int i=0;i<bodies.size();i++)
    {
      // Note that bodies[i] might refer to a different object, if bodies is resized during move_lets.
      expression_ref E = move_lets(false, bodies[i], vars, bodies, set<dummy>(), free_in_E);
      // Therefore ensure that bodies[i] refer to the ith element of bodies AFTER any possible resize.
      bodies[i] = E;
    }

    E2 = let_expression(vars,bodies,T);
  }

  // 6. Handle application, constructors, and operations.
  else if (object_ptr<const Operator> O =  is_a<Operator>(E))
  {
    // First float lets in sub-expressions
    object_ptr<expression> V ( E.ptr()->clone() );
    
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    
    // Move lets from arguments into (vars,bodies)
    for(int i=0;i<E.size();i++)
    {
      V->sub[i] = let_float(V->sub[i]);
      V->sub[i] = move_lets(true, V->sub[i], vars, bodies, set<dummy>(), free_in_E);
    }
      
    E2 = let_expression(vars, bodies, object_ptr<const expression>(V));

    assert(free_in_E == get_free_indices(E2));
  }
  else
    throw myexception()<<"let_float: I don't understand expression '"<<E<<"'";

#ifndef NDEBUG
  set<dummy> S2 = get_free_indices(E2);
  assert(free_in_E == S2);
#endif

  return E2;
}

