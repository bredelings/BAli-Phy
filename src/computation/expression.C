#include "expression.H"
#include "util.H"
#include "operation.H"
#include "operations.H"
#include <set>
#include <iterator>
#include <map>
#include "graph_register.H"

using std::vector;
using std::string;
using std::set;

using boost::dynamic_pointer_cast;

expression_ref indexed_let_expression(const vector<expression_ref>& bodies, const expression_ref& T)
{
  expression* E = new expression( let2_obj() );

  E->sub.push_back(T);

  for(const auto& body: bodies)
    E->sub.push_back(body);

  return E;
}

expression_ref let_expression(const vector<expression_ref>& vars, const vector<expression_ref>& bodies, const expression_ref& T)
{
  if (vars.size() == 0) return T;

  // merge with existing let expression...
  {
    vector<expression_ref> vars2;
    vector<expression_ref> bodies2;
    expression_ref T2;
    if (parse_let_expression(T, vars2, bodies2, T2))
    {
      vector<expression_ref> vars12 = vars;
      vector<expression_ref> bodies12 = bodies;
      vars12.insert(vars12.end(),vars2.begin(), vars2.end());
      bodies12.insert(bodies12.end(), bodies2.begin(), bodies2.end());
      return let_expression(vars12, bodies12, T2);
    }
  }

  expression* E = new expression( let_obj() );
  E->sub.push_back(T);

  for(int i=0;i<vars.size();i++)
  {
    E->sub.push_back(vars[i]);
    E->sub.push_back(bodies[i]);
  }

  return E;
}

expression_ref let_expression(const expression_ref& var, const expression_ref& body, const expression_ref& T)
{
  return let_expression(vector<expression_ref>{var}, vector<expression_ref>{body}, T);
}

//let [(x[i], bodies[i])] T
bool parse_let_expression(const expression_ref& E, vector<expression_ref>& vars, vector<expression_ref>& bodies, expression_ref& T)
{
  vars.clear();
  bodies.clear();

  if (not is_a<let_obj>(E)) return false;

  // There should be an odd number of arguments.
  assert(E->sub.size()%2 == 1);

  T = E->sub[0];
  const int L = (E->sub.size()-1)/2;
  for(int i=0;i<L;i++)
  {
    vars.push_back(E->sub[1+2*i]);
    bodies.push_back(E->sub[2+2*i]);
  }

  return true;
}

//let T bodies[i]
bool parse_indexed_let_expression(const expression_ref& E, vector<expression_ref>& bodies, expression_ref& T)
{
  bodies.clear();

  if (not is_a<let2_obj>(E)) return false;

  T = E->sub[0];
  const int L = E->sub.size();
  for(int i=1;i<L;i++)
    bodies.push_back(E->sub[i]);

  return true;
}

/// R = case T of {patterns[i] -> bodies[i]}
bool parse_case_expression(const expression_ref& E, expression_ref& T, vector<expression_ref>& patterns, vector<expression_ref>& bodies)
{
  patterns.clear();
  bodies.clear();

  if (not is_a<Case>(E)) return false;

  T = E->sub[0];
  const int L = (E->sub.size()-1)/2;
  for(int i=0;i<L;i++)
  {
    patterns.push_back(E->sub[1 + 2*i]);
    bodies.push_back(E->sub[2 + 2*i]);
  }

  return true;
}


bool is_tuple_name(const string& s)
{
  if (s.size() < 3) return false;
  return s == tuple_name(s.size()-1);
}

// How do I make constructor-specific methods of printing data expressions?
// Can I move to defining the print function using an expression?
string expression::print() const 
{
  string result;
  assert(head);

  // The head should not have parts.
  // assert(not is_a<expression>());

  //  if (false)
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;

    if (is_a<lambda2>())
    {
      result = sub[0]->print();
      if (sub[0]->is_a<lambda2>())
	result = "/\\" + result;
      else
	result = "/\\." + result;
      return result;
    }

    if (parse_let_expression(this, vars, bodies, T))
    {
      result = "let {";
      vector<string> parts;
      for(int i=0;i<vars.size();i++)
	parts.push_back(vars[i]->print() + " = " + bodies[i]->print());
      result += join(parts,',');
      result += "} in " + T->print();
      return result;
    }

    if (parse_indexed_let_expression(this, bodies, T))
    {
      result = "let {";
      result += join(bodies,", ");
      result += "} in " + T->print();
      return result;
    }

    if (object_ptr<const Trim> T = is_a<Trim>())
    {
      object_ptr<const Vector<int>> V = ::is_a<Vector<int>>(sub[0]);

      result = "Trim {"+join(V->t,",")+"} " + sub[1]->print();
      return result;
    }

    if (parse_case_expression(this, T, vars, bodies))
    {
      result = "case " + T->print() + " of {";
      vector<string> parts;
      for(int i=0;i<vars.size();i++)
	parts.push_back( vars[i]->print() + " -> " + bodies[i]->print() );
      result += join(parts,',');
      result += "}";
      return result;
    }
  }

  // Print the (unparenthesized) sub-expressions
  vector<string> args(1+size());
  args[0] = head->print();
  for(int i=0;i<size();i++)
    args[1+i] = sub[i]->print();

  vector<string> pargs = args;
  for(int i=1;i<pargs.size();i++)
  {
    if (not sub[i-1]->size()) continue;

    object_ptr<const Operator> O = ::is_a<Operator>(sub[i-1]);

    if (O and is_tuple_name(O->name()) and sub[i-1]->size() == O->n_args()) continue;

    pargs[i] = "(" + args[i] + ")";
  }
  
  if (object_ptr<const Operator> O = is_a<Operator>())
  {
    string O_name = O->name();
    if (dynamic_pointer_cast<const Apply>(O))
      O_name = " ";

    if (O->precedence() > -1 and size() == 2)
    {
      assert(O->n_args() == 2);
      if (sub[0]->size())
      {
	if (sub[0]->is_exactly(*O) and O->associativity()==assoc_left)
	  pargs[1] = args[1];
	else if (object_ptr<const Operator> O2 = ::is_a<Operator>(sub[0]))
	  if (O2->precedence() > O->precedence())
	    pargs[1] = args[1];
      }
      if (sub[1]->size())
      {
	if (sub[1]->is_exactly(*O) and O->associativity()==assoc_right)
	  pargs[2] = args[2];
	else if (object_ptr<const Operator> O2 = ::is_a<Operator>(sub[1]))
	  if (O2->precedence() > O->precedence())
	    pargs[2] = args[2];
      }
      return pargs[1] + O_name + pargs[2];
    }
    else if (is_tuple_name(O->name()) and size() == O->n_args())
    {
      // Should Tuple's parenthesis sub-expressions?
      vector<string> sub_names;
      for(int i=0;i<size();i++)
	sub_names.push_back( args[1+i] );
      return "(" + join(sub_names,", ") + ")";
    }
      
    return O->print_expression( pargs );
  }

  return print_operator_expression( pargs );
}

bool expression::is_exactly(const Object& O) const
{
  if (head->compare(O))
    return true;
  else
    return false;
}

tribool expression::operator==(const expression& E) const
{
  tribool same = true;
  {
    tribool b = is_exactly(*E.head);
    if (indeterminate(b))
      std::cerr<<"Warning: '"<<head<<"' and '"<<E.head<<"' are unsure if they are equal.\n\n";

    same = same and b;
    if (not same) return false;
  }

  for(int i=0;i<size();i++) 
  {
    tribool b = sub[i]->compare(*E.sub[i]);

    if (indeterminate(b))
      std::cerr<<"Warning: '"<<sub[i]<<"' and '"<<E.sub[i]<<"' are unsure if they are equal.\n\n";

    same = same and b;
    if (not same) return false;
  }

  return same;
}

tribool expression::compare(const Object& o) const 
{
  const expression* E = dynamic_cast<const expression*>(&o);
  if (not E) 
    return false;

  return operator==(*E);
}

expression::expression(const object_ref& H)
  :head(H)
{ 
  assert(not dynamic_pointer_cast<const expression>(H));
}

expression::expression(const object_ref& H, const std::vector< expression_ref >& S)
  :head(H),sub(S)
{ 
  assert(not dynamic_pointer_cast<const expression>(H));
}

tribool index_var::compare(const Object& o) const 
{
  const index_var* V = dynamic_cast<const index_var*>(&o);
  if (not V) 
    return false;

  return index == V->index;
}

string index_var::print() const 
{
  return string("%")+convertToString(index);
}

bool dummy::operator==(const dummy& d) const
{
  if (name.size())
    return name == d.name;
  else
    return index == d.index;
}

tribool dummy::compare(const Object& o) const 
{
  const dummy* D = dynamic_cast<const dummy*>(&o);
  if (not D) 
    return false;

  return (*this) == *D;
}

string Trim::print() const
{
  return "Trim";
}

tribool Trim::compare(const Object& o) const 
{
  const Trim* T = dynamic_cast<const Trim*>(&o);
  return T;
}

string dummy::print() const {
  if (name.size())
    return name;
  else if (index < 0)
    return "_";
  else
    return string("#")+convertToString(index);
}

bool dummy::operator<(const dummy& D) const 
{
  if (name.size() and not D.name.size())
      return true;

  if (not name.size() and D.name.size())
      return false;

  if (name.size())
    return name < D.name;
  else
    return index < D.index;
}

tribool match::compare(const Object& o) const 
{
  const match* E = dynamic_cast<const match*>(&o);
  if (not E) 
    return false;

  return index == E->index;
}

string match::print() const 
{
  if (index == -1) 
    return "_";
  else
    return string("_")+convertToString(index);
}

tribool let_obj::compare(const Object& o) const 
{
  const let_obj* T = dynamic_cast<const let_obj*>(&o);
  return T;
}

string let_obj::print() const 
{
  return "let";
}

tribool let2_obj::compare(const Object& o) const 
{
  const let2_obj* T = dynamic_cast<const let2_obj*>(&o);
  return T;
}

string let2_obj::print() const 
{
  return "let";
}


// How would we handle lambda expressions, here?
bool find_match(const expression_ref& pattern, const expression_ref& E, vector< expression_ref >& results)
{
  // If this is a match expression, then succeed, and store E as the result of the match
  object_ptr<const match> M = is_a<match>(pattern);
  if (M) 
  {
    if (M->index >= 0)
    {
      if (results.size() < M->index+1) results.resize(M->index+1);

      if (results[M->index]) throw myexception()<<"Match expression contains match index "<<M->index<<"' more than once!";

      results[M->index] = E;
    }

    return true;
  }

  // Expressions must have the same number of arguments
  if (pattern->size() != E->size()) return false;

  // Check if the heads are equal
  tribool b = same_head(pattern, E);
  assert(not indeterminate(b));
  if (not b) return false;

  // Sub-expressions must match
  for(int i=0;i<pattern->size();i++)
    if (not find_match(pattern->sub[i], E->sub[i], results))
      return false;

  return true;
}

tribool parameter::compare(const Object& o) const 
{
  const parameter* E = dynamic_cast<const parameter*>(&o);
  if (not E) 
    return false;

  return parameter_name == E->parameter_name;
}

string lambda::print() const {
  return "lambda";
}

tribool lambda::compare(const Object& o) const 
{
  return dynamic_cast<const lambda*>(&o);
}

string lambda2::print() const {
  return "/\\";
}

tribool lambda2::compare(const Object& o) const 
{
  return dynamic_cast<const lambda2*>(&o);
}

expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& R)
{
  return new expression(lambda(),{dummy, R});
}

expression_ref lambda_quantify(int dummy_index, const expression_ref& R)
{
  return lambda_quantify(dummy(dummy_index), R);
}

expression_ref lambda_expression(const Operator& O)
{
  int n = O.n_args();
  assert(n != -1);
  
  expression_ref R;
  if (n == 0)
    R = expression_ref(O.clone());
  else
  {
    expression* E = new expression(O);
    for(int i=0;i<n;i++)
      E->sub.push_back(expression_ref(dummy(i)));
    R = expression_ref(E);
  }
  
  for(int i=n-1;i>=0;i--) 
    R = lambda_quantify(i,R);
  
  return R;
}

tribool constructor::compare(const Object& o) const
{
  const constructor* E = dynamic_cast<const constructor*>(&o);
  if (not E) 
    return false;

  // Should we check that the arity matches also?

  return f_name == E->f_name;
}

constructor::constructor(const string& s, int n)
  :f_name(s), n_args_(n), assoc(assoc_none),prec(-1)
{
  
}

constructor left_assoc_constructor(const std::string& s,int prec)
{
  constructor f(s, 2);
  f.prec = prec;
  f.assoc = assoc_left;
  return f;
}

constructor right_assoc_constructor(const std::string& s,int prec)
{
  constructor f(s, 2);
  f.prec = prec;
  f.assoc = assoc_right;
  return f;
}

/// Do the substitutions match(i) -> replace[i] for all i where replace[i] is not null.
expression_ref substitute(const expression_ref& R, const vector<expression_ref>& replace)
{
  expression_ref R2 = R;
  for(int i=0;i<replace.size();i++)
    if (replace[i])
      R2 = substitute(R2, match(i), replace[i]);

  return R2;
}

expression_ref substitute(const expression_ref& R1, int dummy_index, const expression_ref& R2)
{
  return substitute(R1,dummy(dummy_index),R2);
}

template <typename T>
int find_index_backward(const vector<T>& v,const T& t)
{
  int L = v.size();
  for(int i=0;i<L;i++)
    if (v[L-i-1] == t)
      return i;
  return -1;
}

expression_ref make_indexed_lambda(const expression_ref& R)
{
  return new expression(lambda2(),{R});
}

/// Convert to using de Bruijn indices.
expression_ref indexify(const expression_ref& E, const vector<dummy>& variables)
{
  if (not E->size())
  {
    // Indexed Variable - This is assumed to be a free variable, so just shift it.
    if (object_ptr<const index_var> V = is_a<index_var>(E))
      return index_var(V->index + variables.size());

    // Variable
    else if (object_ptr<const dummy> D = is_a<dummy>(E))
    {
      assert(D->index != -1);

      int index = find_index_backward(variables, *D);
      if (index == -1)
	throw myexception()<<"Dummy '"<<D<<"' is apparently not bound variables in '"<<E<<"'?";
      else
	return object_ptr<const index_var>(new index_var(index));
    }
    // Constant
    else
      return E;
  }
  
  // Lambda expression - /\x.e
  if (object_ptr<const lambda> L = is_a<lambda>(E))
  {
    vector<dummy> variables2 = variables;
    variables2.push_back(*is_a<dummy>(E->sub[0]));
    return make_indexed_lambda( indexify(E->sub[1], variables2) );
  }

  // Let expression
  vector<expression_ref> vars;
  vector<expression_ref> bodies;
  expression_ref T;
  if (parse_let_expression(E, vars, bodies, T))
  {
    vector<dummy> variables2 = variables;
    for(const auto& var: vars)
      variables2.push_back(*is_a<dummy>(var));

    for(auto& body: bodies)
      body = indexify(body, variables2);

    T = indexify(T, variables2);

    return indexed_let_expression(bodies,T);
  }

  // case expression
  vector<expression_ref> patterns;
  if (parse_case_expression(E, T, patterns, bodies))
  {
    T = indexify(T, variables);

    for(int i=0;i<bodies.size();i++)
    {
      // Handle c[i] x[i][1..n] -> body[i]
      expression_ref& P = patterns[i];
      expression_ref& B = bodies[i];

      vector<dummy> variables2 = variables;
      for(int j=0;j<P->size();j++)
	variables2.push_back(*is_a<dummy>(P->sub[j]));

#ifndef NDEBUG
      if (object_ptr<const dummy> D = is_a<dummy>(P))
      {
	assert(D->index == -1);
	assert(P->size() == 0);
      }
#endif

      P = P->head;
      B = indexify(B, variables2);
    }

    return make_case_expression(T, patterns, bodies);
  }

  // If we've gotten this far, just transform the sub-expressions.
  // (This could be: Op, constructor, 
  expression* V = new expression(*E);
  for(int i=0;i<V->size();i++)
    V->sub[i] = indexify(V->sub[i], variables);
  return V;
}

expression_ref indexify(const expression_ref& E)
{
  return indexify(E,{});
}

dummy get_named_dummy(int n)
{
  if (n<26) 
    return dummy(string{char(97+n)});
  else
    return dummy(n-26);
}

/// Convert to using de Bruijn indices.
expression_ref deindexify(const expression_ref& E, const vector<object_ref>& variables)
{
  if (not E->size())
  {
    // Indexed Variable - This is assumed to be a free variable, so just shift it.
    if (object_ptr<const index_var> V = is_a<index_var>(E))
    {
      if (V->index >= variables.size())
	return new index_var(V->index - variables.size());

      return variables[variables.size()-1 - V->index];
    }
    // Constant
    else
      return E;
  }
  
  // Lambda expression - /\x.e
  if (object_ptr<const lambda2> L = is_a<lambda2>(E))
  {
    vector<object_ref> variables2 = variables;
    dummy d = get_named_dummy(variables.size());
    variables2.push_back(d);
    return lambda_quantify(d,deindexify(E->sub[0],variables2));
  }

  // Let expression
  vector<expression_ref> bodies;
  expression_ref T;
  if (parse_indexed_let_expression(E, bodies, T))
  {
    vector<object_ref> variables2 = variables;
    vector<expression_ref> vars;
    for(int i=0;i<bodies.size();i++)
    {
      dummy d = get_named_dummy(variables2.size());
      vars.push_back( d );
      variables2.push_back( d );
    }

    for(auto& body: bodies)
      body = deindexify(body, variables2);

    T = deindexify(T, variables2);

    return let_expression(vars, bodies,T);
  }

  // case expression
  vector<expression_ref> patterns;
  if (parse_case_expression(E, T, patterns, bodies))
  {
    T = deindexify(T, variables);

    for(int i=0;i<bodies.size();i++)
    {
      assert(not patterns[i]->size());
      // Make a new expression so we can add variables to the pattern if its a constructor
      object_ptr<expression> P = patterns[i]->clone();
      expression_ref& B = bodies[i];

      // Find the number of arguments in the constructor
      int n_args = 0;
      if (object_ptr<const constructor> C = is_a<constructor>(P))
	n_args = C->n_args();

      // Add n_arg variables to the stack and to the pattern
      vector<object_ref> variables2 = variables;
      for(int j=0;j<n_args;j++)
      {
	dummy d = get_named_dummy(variables2.size());
	variables2.push_back( d );
	P->sub.push_back( d );
      }

#ifndef NDEBUG
      if (object_ptr<const dummy> D = is_a<dummy>(P))
      {
	assert(D->index == -1);
	assert(P->size() == 0);
      }
#endif

      patterns[i] = P;
      B = deindexify(B, variables2);
    }

    return make_case_expression(T, patterns, bodies);
  }

  // If we've gotten this far, just transform the sub-expressions.
  // (This could be: Op, constructor, 
  expression* V = new expression(*E);
  for(int i=0;i<V->size();i++)
    V->sub[i] = deindexify(V->sub[i], variables);
  return V;
}

expression_ref deindexify(const expression_ref& E)
{
  return deindexify(E,{});
}

vector<int> pop_vars(int n, vector<int> vars)
{
  assert(n >= 0);
  if (n == 0) return vars;

  for(int& var: vars)
    var -= n;
  while(vars.size() and vars[0] < 0)
    vars.erase(vars.begin());
  return vars;
}

vector<int> merge_vars(const vector<int>& v1, const vector<int>& v2)
{
  int i=0;
  int j=0;
  vector<int> v3;
  while(i<v1.size() or j<v2.size())
  {
    if (i >= v1.size())
      v3.push_back(v2[j++]);
    else if (j >= v2.size())
      v3.push_back(v1[i++]);
    else if (v1[i] < v2[j])
      v3.push_back(v1[i++]);
    else if (v1[i] > v2[j])
      v3.push_back(v2[j++]);
    else {
      assert(v1[i] == v2[j]);
      v3.push_back(v1[i]);
      i++; j++;
    }
  }
  assert(v3.size() >= v1.size());
  assert(v3.size() >= v2.size());
  return v3;
}

vector<int> get_free_index_vars(const expression_ref& E)
{
  if (not E->size()) 
  {
    // Variable
    if (object_ptr<const index_var> D = is_a<index_var>(E))
    {
      assert(D->index != -1);
      return {D->index};
    }
    // Constant
    else
      return {};
  }

  vector<expression_ref> bodies;
  vector<expression_ref> patterns;
  expression_ref T;

  vector<int> vars;
  
  if (is_a<Trim>(E))
  {
    // Which vars are we not throwing away?
    // This should also be an assert.
    vars = is_a<Vector<int>>(E->sub[0])->t;
    
#ifndef NDEBUG
    vector<int> vars2 = get_free_index_vars(E->sub[1]);
    assert(vars.size() == vars2.size());
    for(int i=0;i<vars.size();i++)
      assert(vars2[i] == i);
#endif
  }
  // Lambda expression - /\x.e
  else if (object_ptr<const lambda2> L = is_a<lambda2>(E))
    vars = pop_vars(1, get_free_index_vars(E->sub[0]));

  // Let expression
  else if (parse_indexed_let_expression(E, bodies, T))
  {
    vars = get_free_index_vars(T);

    for(const auto& body: bodies)
      vars = merge_vars(vars, get_free_index_vars(body));

    vars = pop_vars(bodies.size(), vars);
  }

  // case expression
  else if (parse_case_expression(E, T, patterns, bodies))
  {
    vars = get_free_index_vars(T);

    for(int i=0;i<bodies.size();i++)
    {
      int n = 0;

      // Handle c[i] x[i][1..n] -> body[i]
      if (object_ptr<const constructor> C = is_a<constructor>(patterns[i]))
	n = C->n_args();

      vars = merge_vars(vars, pop_vars(n, get_free_index_vars(bodies[i])) );
    }
  }
  else
  {
    for(int i=0;i<E->size();i++)
      vars = merge_vars(vars, get_free_index_vars(E->sub[i]) );
  }

  //  std::cerr<<"fv("<<R<<"): "<<join(vars,",")<<"\n";

  return vars;
}

expression_ref trim_normalize(const expression_ref& E)
{
  // Already normalized (though not trimmed)
  if (not E->size()) return E;

  vector<expression_ref> bodies;
  vector<expression_ref> patterns;
  expression_ref T;

  vector<int> vars;
  
  // Let expressions need to be normalized
  if (parse_indexed_let_expression(E, bodies, T))
  {
    T = trim(trim_normalize(T));

    for(auto& body: bodies)
      body = trim(trim_normalize(body));

    return indexed_let_expression(bodies,T);
  }

  // case expression
  else if (parse_case_expression(E, T, patterns, bodies))
  {
    // T should already be a variable, so don't bother about it.
    assert(is_a<index_var>(T));

    for(auto& body: bodies)
      body = trim(trim_normalize(body));

    return make_case_expression(T, patterns, bodies);
  }
  else
  {
    expression* V = new expression(*E);
    for(int i=0;i<E->size();i++)
      V->sub[i] = trim_normalize(V->sub[i]);

    return V;
  }
}

expression_ref make_trim(const expression_ref& E, const vector<int>& indices)
{
#ifndef NDEBUG
  vector<int> vars = get_free_index_vars(E);
  for(int i=0;i<vars.size();i++)
    assert(vars[i] == i);
  assert(indices.size() == vars.size());
#endif

  expression* V = new expression(Trim());
  V->sub.push_back(Vector<int>(indices));
  V->sub.push_back(E);

  return V;
}

// This compresses free variables according to the supplied mapping.
expression_ref remap_free_indices(const expression_ref& E, const vector<int>& mapping, int depth)
{
  if (not E->size())
  {
    // Variable
    if (object_ptr<const index_var> D = is_a<index_var>(E))
    {
      assert(D->index != -1);
      int delta = D->index - depth;
      if (delta >= 0)
      {
	assert(delta < mapping.size());
	assert(mapping[delta] != -1);

	return index_var(depth + mapping[delta]);
      }
      else
	// Var that is to new to be remapped.
	return E;
    }
    // Constant
    else
      return E;
  }

  vector<expression_ref> bodies;
  vector<expression_ref> patterns;
  expression_ref T;

  vector<int> vars;
  
  if (is_a<Trim>(E))
  {
    // Which vars are we not throwing away?
    // This should also be an assert.
    vars = is_a<Vector<int>>(E->sub[0])->t;

    // remap free vars
    for(auto& var:vars)
    {
      int delta = var - depth;
      if (delta >= 0)
      {
	assert(delta < mapping.size());
	assert(mapping[delta] != -1);

	var = depth + mapping[delta];
      }
    }
    
#ifndef NDEBUG
    vector<int> vars2 = get_free_index_vars(E->sub[1]);
    assert(vars.size() == vars2.size());
    for(int i=0;i<vars.size();i++)
      assert(vars2[i] == i);
#endif

    return make_trim(E->sub[1], vars);

  }
  // Lambda expression - /\x.e
  else if (object_ptr<const lambda2> L = is_a<lambda2>(E))
  {
    expression* V = new expression(lambda2());
    V->sub.push_back(remap_free_indices(E->sub[0], mapping, depth+1));
    return V;
  }

  // Let expression
  else if (parse_indexed_let_expression(E, bodies, T))
  {
    int n = bodies.size();
    T = remap_free_indices(T, mapping, depth + n);

    for(auto& body: bodies)
      body = remap_free_indices(body, mapping, depth + n);

    return indexed_let_expression(bodies, T);
  }
  
  // case expression
  else if (parse_case_expression(E, T, patterns, bodies))
  {
    T = remap_free_indices(T, mapping, depth);

    for(int i=0;i<bodies.size();i++)
    {
      int n = 0;

      // Handle c[i] x[i][1..n] -> body[i]
      if (object_ptr<const constructor> C = is_a<constructor>(patterns[i]))
	n = C->n_args();

      bodies[i] = remap_free_indices(bodies[i], mapping, depth + n);
    }

    return make_case_expression(T, patterns, bodies);
  }
  else
  {
    expression* V = new expression(*E);
    for(int i=0;i<E->size();i++)
      V->sub[i] = remap_free_indices(V->sub[i], mapping, depth);
    return V;
  }
}

expression_ref trim(const expression_ref& E)
{
  // Well, it would seem that the relevant matter is that we are at depth n.
  vector<int> indices = get_free_index_vars(E);

  vector<int> mapping;

  if (indices.size())
  {
    mapping = vector<int>(indices.back()+1, -1);
    for(int i=0;i<indices.size();i++)
      mapping[indices[i]] = i;
  }

  return make_trim( remap_free_indices(E, mapping, 0), indices);
}

expression_ref untrim(const expression_ref& E)
{
  if (object_ptr<const Trim> T = is_a<Trim>(E))
  {
    object_ptr<const Vector<int>> V = ::is_a<Vector<int>>(E->sub[0]);

    return remap_free_indices(E->sub[1], V->t, 0);
  }
  else
    return E;
}

// This only removes trimmers from the places that trim_normalize puts them.
// (Since remap_free_indices( ) doesn't enter trimmers, this should be relatively efficient,
//  just like trim_normalize( ))
expression_ref trim_unnormalize(const expression_ref& E)
{
  // Already normalized (though not trimmed)
  if (not E->size()) return E;

  vector<expression_ref> bodies;
  vector<expression_ref> patterns;
  expression_ref T;

  vector<int> vars;
  
  // Let expressions need to be normalized
  if (parse_indexed_let_expression(E, bodies, T))
  {
    T = trim_unnormalize(untrim(T));

    for(auto& body: bodies)
      body = trim_unnormalize(untrim(body));

    return indexed_let_expression(bodies,T);
  }

  // case expression
  else if (parse_case_expression(E, T, patterns, bodies))
  {
    // T should already be a variable, so don't bother about it.
    assert(is_a<index_var>(T));

    for(auto& body: bodies)
      body = trim_unnormalize(untrim(body));

    return make_case_expression(T, patterns, bodies);
  }
  else
  {
    expression* V = new expression(*E);
    for(int i=0;i<E->size();i++)
      V->sub[i] = trim_unnormalize(untrim(V->sub[i]));

    return V;
  }
}

/// 1. Hey, could we solve the problem of needing to rename dummies by doing capture-avoiding substitution?
/// I think we could!
///
/// Suppose we have Lf.Lx.fx, and we apply it to Lx.x (the identity function), then we get
///    (Lf.Lx.fx)(Lx.x) = (Lx.fx)[f := Lx.x] = Lx.(Lx.x)x.
/// Then, if we apply this to y, we get
///    (Lx.(Lx.x)x)y = (Lx.x)x[x := y] = (Lx.x)y 
/// And
///    (Lx.x)y = y;
///
/// 2. However, is sometimes still necessary to rename dummies.  This is true if E2 contains unbound dummies
///    that are bound in E1.
///
///    For example, apply Lx.y to x, then we would get Lx.x, which is not allowed.
///    Instead, we must "alpha-convert" Lx.y to Lz.y, and then apply Lz.y to x, leading to Lz.x .

/// Literally E2 for D in E1. (e.g. don't rename variables in E2).  Throw an exception if D is a lambda-bound dummy variable.

std::set<dummy> get_free_indices(const expression_ref& E);

// Return the list of dummy variable indices that are bound at the top level of the expression
std::set<dummy> get_bound_indices(const expression_ref& E)
{
  std::set<dummy> bound;

  if (not E->size()) return bound;

  // Make sure we don't try to substitute for lambda-quantified dummies
  if (object_ptr<const lambda> L = is_a<lambda>(E))
  {
    if (object_ptr<const dummy> D  = is_a<dummy>(E->sub[0]))
      bound.insert(*D);
  }
  else 
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;
    if (parse_let_expression(E, vars, bodies, T))
    {
      // Don't substitute into local variables.
      for(int i=0;i<vars.size();i++)
	if (object_ptr<const dummy> D = is_a<dummy>(vars[i]))
	  bound.insert(*D);
    }
    else if (is_a<Case>(E))
      std::abort();
  }

  return bound;
}

// Return the list of dummy variable indices that are bound at the top level of the expression
void alpha_rename(object_ptr<expression>& E, const expression_ref& x, const expression_ref& y)
{
  // assert: x is bound in E
  // assert: y is neither bound in E nor free in E

  // std::cout<<" replacing "<<x<<" with "<<y<<" in "<<E->print()<<":\n";
  // Make sure we don't try to substitute for lambda-quantified dummies
  if (object_ptr<const lambda> L = is_a<lambda>(E))
  {
    assert(same_head(E->sub[0], x));
    E->sub[0] = y;
    E->sub[1] = substitute(E->sub[1], x, y);
  }
  else if (is_a<let_obj>(E))
  {
    for(int i=0;i<E->size();i++)
      E->sub[i] = substitute(E->sub[i], x, y);
  }
  else
    assert(false);
  // std::cout<<"    "<<E->print()<<"\n";
}

std::set<dummy> get_free_indices(const expression_ref& E)
{
  std::set<dummy> S;

  // fv x = { x }
  if (object_ptr<const dummy> D = is_a<dummy>(E)) 
    if (D->index != -1)
      return {*D};

  // fv c = { }
  if (not E->size()) return S;

  // for case expressions get_bound_indices doesn't work correctly.
  if (is_a<Case>(E))
  {
    const int L = (E->size()-1)/2;

    S = get_free_indices(E->sub[0]);

    for(int i=0;i<L;i++)
    {
      std::set<dummy> bound_i = get_free_indices(E->sub[1+2*i]);
      std::set<dummy> free_i = get_free_indices(E->sub[2+2*i]);
      for(const auto& b: bound_i)
	free_i.erase(b);

      add(S,free_i);
    }
    for(const auto& s: S)
      assert(s.index != -1);

    return S;
  }

  for(int i=0;i<E->size();i++)
    add(S, get_free_indices(E->sub[i]));

  std::set<dummy> bound = get_bound_indices(E);
  for(const auto& b: bound)
    S.erase(b);


  for(const auto& s: S)
    assert(s.index != -1);

  return S;
}

/// Return the min of v
template<typename T>
T max(const std::set<T>& v)
{
  T t = *v.begin();
  for(const auto& i: v)
    t = std::max(t,i);

  return t;
}

int max_index(const std::set<dummy>& s)
{
  if (s.empty()) return -1;
  return max(s).index;
}

/// Return the min of v
template<typename T>
T min(const std::set<T>& v)
{
  T t = *v.begin();
  for(const auto& i: v)
    t = std::min(t,*i);

  return t;
}

int get_safe_binder_index(const expression_ref& E)
{
  std::set<dummy> free = get_free_indices(E);
  if (free.empty()) 
    return 0;
  else
    return max_index(free)+1;
}

// If we use de Bruijn indices, then, as before bound indices in R2 are no problem.
// If we do NOT use de Bruijn indices for FREE variables, then we don't have to adjust them when we substitute.

// Current problem: we need a generic way to represent which indices are bound.

// For de bruijn indices in case expressions, we could specify (?:?):(_:?) where ? means "keep" and "_" means not to keep.
// When the user specifies such a pattern we could number the ? variables on the horizontal level.

// We could also number individual variables in letrec expressions horizontally.

// However, how do we add names back in when we want to print them?


// Idea: switch to de bruijn indices for bound variables only.  Makes substitution much simpler!
// Question: how would I encode names?

bool do_substitute(expression_ref& E1, const expression_ref& D, const expression_ref& E2)
{
#ifndef NDEBUG
  expression_ref orig = E1;
#endif
  assert(not is_wildcard(D));

  // If this is the relevant dummy, then substitute
  if (E1->size() == 0)
  {
    if (same_head(E1,D))
    {
      E1 = E2;
      return true;
    }
    // If this is any other constant, then it doesn't contain the dummy
    else
      return false;
  }

  // Handle case expressions differently
  {
    expression_ref T;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    bool changed = false;
    if (parse_case_expression(E1,T,patterns,bodies))
    {
      changed = do_substitute(T, D, E2) or changed;

      const int L = (E1->size()-1)/2;

      for(int i=0;i<L;i++)
      {
	// don't substitute into subtree where this variable is bound
	std::set<dummy> bound = get_free_indices(patterns[i]);
	for(const auto& b: bound)
	  if (D->is_exactly(b)) continue;

	changed = do_substitute(bodies[i], D, E2) or changed;
      }

      if (changed)
	E1 = make_case_expression(T, patterns, bodies);

      return changed;
    }
  }

  // What indices are bound at the top level?
  std::set<dummy> bound = get_bound_indices(E1);

  bool changed = false;
  if (not bound.empty())
  {
    // Don't substitute into local variables
    for(const auto& b: bound)
      if (D->is_exactly(b)) return false;
    
    std::set<dummy> fv2 = get_free_indices(E2);
    std::set<dummy> overlap = intersection(bound,fv2);
    
    // If some of the free variables in E2 are bound in E1, then do alpha-renaming on E1 to avoid name capture.
    if (not overlap.empty())
    {
      // Determine the free variables of E1 so that we can avoid them in alpha renaming
      std::set<dummy> fv1 = get_free_indices(E1);

      // If E1 does not contain D, then we won't do any substitution anyway, so avoid alpha renaming.
      if (object_ptr<const dummy> D2 = is_a<dummy>(D))
      {
	if (fv1.find(*D2) == fv1.end()) return false;
      }

      // Compute the total set of free variables to avoid clashes with when alpha renaming.
      add(fv2, fv1);

      // we don't want to rename on top of any other variables bound here
      int new_index = std::max(max_index(fv2),max_index(bound))+1;

      // Do the alpha renaming
      object_ptr<expression> E1_ (E1->clone());
      for(const auto& i:overlap)
	alpha_rename(E1_, dummy(i), dummy(new_index++));
      E1 = E1_;
      changed = true;

      // We rename a bound variable dummy(i) in E1 that is free in E2 to a new variable dummy(new_index)
      //   that is not bound or free in the initial version of E1 and free in E2.

      // The conditions are therefore:
      //   dummy(*i) must be bound in E1
      //   dummy(new_index) must be neither bound nor free in E1
      //   dummy(new_index) must not be free in E2
    }
  }

  // Since this is an expression, substitute into sub-expressions
  object_ptr<expression> E1_ (E1->clone());
  for(int i=0;i<E1_->size();i++)
    changed = (do_substitute(E1_->sub[i], D, E2) or changed);

  if (changed)
    E1 = E1_;

  assert((E1 != orig) == changed);
  return changed;
}

// ?- Determine which let statements have bound vars (bound_indices) and which do not (unbound_indices).
//    (The ones with no bound vars can be floated.)
bool find_let_statements_with_bound_vars(const vector<expression_ref>& let_vars, const vector<expression_ref>& let_bodies,
					 const set<dummy>& bound,
					 vector<int>& bound_indices, vector<int>& unbound_indices)
{
  set<dummy> let_bound;
  for(int i=0;i<let_vars.size();i++)
    let_bound.insert(*is_a<dummy>(let_vars[i]));

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
	new_bound_next.insert(*is_a<dummy>(let_vars[index]));
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
    dummy D = *is_a<dummy>(vars[i]);
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
      dummy D = *is_a<dummy>(E_vars[i]);
      avoid.insert(D);
      new_index = std::max(new_index, D.index + 1);
    }

    /******************** alpha-rename E -> EE ********************/
    
    object_ptr<expression> EE ( E->clone() );                  // Make a copy of E that we can alpha-rename.
    
    for(int index: unbound_indices)
    {
      dummy D = *is_a<dummy>(E_vars[index]);
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
      for(int j=0;j<vars.size();j++)
	assert(not includes(vars, E_vars[index]));
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
  if (is_parameter(E2) or is_index_var(E2) or is_var(E2) or is_dummy(E2))
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
  if (not E->size()) return E;
  
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
    dummy D = *is_a<dummy>(E->sub[0]);

    // First float lets in sub-expressions
    expression_ref M = let_float(E->sub[1]);

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
      bound_vars_let.insert(*is_a<dummy>(vars[i]));

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
      bodies[i] = move_lets(false, bodies[i], vars, bodies, set<dummy>(), free_in_E);

    E2 = let_expression(vars,bodies,T);
  }

  // 6. Handle application, constructors, and operations.
  else if (object_ptr<const Operator> O =  is_a<Operator>(E))
  {
    // First float lets in sub-expressions
    object_ptr<expression> V ( E->clone() );
    
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    
    // Move lets from arguments into (vars,bodies)
    for(int i=0;i<E->size();i++)
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

expression_ref substitute(const expression_ref& R1, const expression_ref& D, const expression_ref& R2)
{
  expression_ref R1b = R1;
  do_substitute(R1b, D, R2);
  return R1b;
}


expression_ref apply_expression(const expression_ref& R,const expression_ref& arg)
{
  return new expression(Apply(),{R,arg});
}

expression_ref apply_expression(const expression_ref& E,
				const vector< expression_ref > args)
{
  expression_ref E2 = E;
  for(int i=0;i<args.size();i++)
    E2 = apply_expression(E2,args[i]);
  return E2;
}


// When applying Lx.M to N, we need to make sure that no occurrence of x has the free variables in N bound.
// At each occurence of x, we need to know 
// (i) what are the lambda's that class with the free variables of N
// (ii) what free variables of M are 
expression_ref apply(const expression_ref& E,const expression_ref& arg)
{
  assert(E);

  if (E->size())
  {
    if (object_ptr<const lambda> L = is_a<lambda>(E))
      return substitute(E->sub[1], E->sub[0], arg);
  }

  // Allow applying non-lambda expressions to arguments.
  // We need this to apply variables that turn out to be functions.
  return apply_expression(E,arg);
}

expression_ref apply(const expression_ref& E,
		     const vector< expression_ref > args)
{
  expression_ref E2 = E;
  for(int i=0;i<args.size();i++)
    E2 = apply(E2,args[i]);
  return E2;
}

expression_ref operator,(const expression_ref& E1, const expression_ref& E2)
{
  return apply(E1, E2);
}

expression_ref operator&(const expression_ref& E1, const expression_ref& E2)
{
  return (Cons,E1,E2);
}

void find_named_parameters(const expression_ref& E, std::set<string>& names)
{
  assert(E);
  // If this is a parameter, then makes sure we've got its name.
  if (object_ptr<const parameter> n = is_a<parameter>(E))
  {
    assert(not E->size());
    if (names.find(n->parameter_name) == names.end())
      names.insert(n->parameter_name);
  }

  // Check the sub-objects of this expression.
  for(int i=0;i<E->size();i++)
    find_named_parameters(E->sub[i], names);
}

set<string> find_named_parameters(const expression_ref& e)
{
  set<string> names;
  find_named_parameters(e, names);
  return names;
}

set<string> find_named_parameters(const vector<expression_ref>& notes)
{
  set<string> names;
  for(int i=0;i<notes.size();i++)
    find_named_parameters(notes[i], names);
  return names;
}

expression_ref add_prefix(const string& prefix, const expression_ref& E)
{
  std::set<string> names = find_named_parameters(E);

  expression_ref E2 = E;
  for(const auto& name: names)
    E2 = substitute(E2, parameter(name), parameter(prefix+"::"+name));

  return E2;
}

string tuple_name(int n)
{
  if (n == 0)
    return "()";

  if (n == 1)
    std::abort();

  string s;
  s.resize(n+1);
  s[0] = '(';
  for(int i=1;i<n;i++)
    s[i] = ',';
  s[n] = ')';
  return s;
}

constructor tuple_head(int n)
{
  assert(n != 1);

  string s = tuple_name(n);
  return constructor(s,n);
}

expression_ref Tuple(int n)
{
  assert(n >= 0);
  return lambda_expression( tuple_head(n) );
}

expression_ref Cons = lambda_expression( right_assoc_constructor(":",2) );

expression_ref ListEnd = lambda_expression( constructor("[]",0) );

expression_ref _ = match(-1);
expression_ref _1 = match(0);
expression_ref _2 = match(1);
expression_ref _3 = match(2);
expression_ref _4 = match(3);

expression_ref default_value = lambda_expression(constructor("default_value",2));

expression_ref var_bounds = lambda_expression(constructor("var_bounds",2));

// Fields: (prob_density) (random vars) (parameter expressions)
expression_ref distributed = lambda_expression( constructor("~",2) );

expression_ref sys_print = lambda_expression( Print() );

expression_ref concat = lambda_expression( Concat() );

expression_ref prob = lambda_expression( constructor("probability",1) );

expression_ref defun = lambda_expression( constructor("defun",3) );

vector<expression_ref> get_ref_vector_from_list(const expression_ref& E)
{
  vector<expression_ref> V;

  expression_ref E2 = E;
  while(is_exactly(E2,":"))
  {
    assert(E2->size() == 2);
    V.push_back(E2->sub[0]);
    E2 = E2->sub[1];
  }

  return V;
}

//FIXME - try to eliminate this!
std::vector<expression_ref> get_ref_vector_from_tuple(const expression_ref& E)
{
  // FIXME - this doesn't handle 1-tuples very well!
  // REMOVE -- only used in model_prior::operator()
  //   We should convert model parameters to a list, since we
  //   don't know in advance how many there will be.

  // Tuples only work when you know in advance how many there will be.
  //   -> REPLACE - get_ref_vector_from_tuple(E,n)

  if (not E->size())
    return {E};

  std::vector<expression_ref> v2(E->size());
  for(int i=0;i<v2.size();i++)
    v2[i] = E->sub[i];
  return v2;
}

template<> expression_ref get_tuple<>(const vector<expression_ref>& S)
{
  if (S.size() == 1) return S[0]->head;

  constructor H = tuple_head(S.size());

  if (not S.size()) return H;

  return new expression(H,S);
}

expression_ref get_list(const vector<expression_ref>& v)
{
  expression_ref E = ListEnd;

  for(int i=v.size()-1;i>=0;i--)
    E = Cons(v[i],E);

  return E;
}

/* Legal terms are:

T,U,V -> x
      -> Lx.T
      -> U T
      -> c U[]
      -> let {x=U} in T
      -> case U of {c x[i] -> V[i]}
*/

/* The normalization rules are:

   1. (x)* -> x
   2. (Lx.T)* -> Lx.(T)*
   3. (U T)* -> let x = (T)* in (U)* x , x fresh
   4. (c U[i]) -> let x[i] = (U[i])* in c x[i], x fresh
   5. (let {x[i] = U[i]} in T)* -> let {x=(U[i])*} in (T)*
   6. (case U of {c[i] x[i][] -> T[i]})* -> case (U)* of {c[i] x[i][] -> (T[i])*}
 */

/*
  x -> dummy[index]
  Lx.T ->(lambda[index] T)
  (U T) -> (U T)
  (c U[i]) -> (c U[i])
  (let {x[i] = U[i]} in T) -> (let [(x[i],U[i])] T)
  (case T in {c[i] x[i] -> U[i]}) -> (case T [(c[i] x[i],U[i])]
 */

/*
 *  Perhaps switch to (lambda dummy E) instead of (lambda[index] E)
 */ 

// FIXME: add operator expressions :-P

bool is_irrefutable_pattern(const expression_ref& E)
{
  return is_a<dummy>(E);
}

/// Is this either (a) irrefutable, (b) a constant, or (c) a constructor whose arguments are irrefutable patterns?
bool is_simple_pattern(const expression_ref& E)
{
  // (a) Is this irrefutable?
  if (is_irrefutable_pattern(E)) return true;

  // (b) Is this a constant with no arguments? (This can't be an irrefutable pattern, since we've already bailed on dummy variables.)
  if (not E->size()) return true;

  assert(is_a<constructor>(E));

  // Arguments of multi-arg constructors must all be irrefutable patterns
  for(int j=0;j<E->size();j++)
    if (not is_irrefutable_pattern(E->sub[j]))
      return false;

  // (c) Is this a constructor who arguments are irrefutable patterns?
  return true;
}

// This function currently assumes that all the patterns are just variables.

expression_ref case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies);

/// Create the expression case T of {patterns[i] -> bodies[i]} --- AND all patterns are just C[i] v1 v2 ... vn
expression_ref simple_case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
  expression_ref E = make_case_expression(T, patterns, bodies);

  for(int i=patterns.size()-1;i>=0;i--)
  {
    if (not is_simple_pattern(patterns[i]))
      throw myexception()<<"simple_case_expression( ): pattern '"<<patterns[i]<<"' is not free variable in expression '"<<E<<"'";
  }

  return E;
}

template <typename T>
vector<T> skip(int n, const vector<T>& v)
{
  if (v.size() <= n) return vector<T>();

  vector<T> v2(v.size() - n);
  for(int i=0;i<v2.size();i++)
    v2[i] = v[i+n];

  return v2;
}

expression_ref make_case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
  assert(patterns.size() == bodies.size());

  expression* E = new expression( Case() );
  E->sub.push_back(T);
  
  for(int i=0;i<patterns.size();i++)
  {
    E->sub.push_back( patterns[i] );
    E->sub.push_back( bodies[i] );
  }
  return E;
}

int find_object(const vector<object_ref>& v, const object_ref& O)
{
  for(int i=0;i<v.size();i++)
    if (O->compare(*v[i]))
      return i;
  return -1;
}

object_ref get_constructor(const expression_ref& E)
{
  object_ref C = is_a<constructor>(E);
  assert(C);
  return C;
}
 

/*
 * case (x[0],..,x[N-1]) of (p[0...M-1][0...N-1] -> b[0..M-1])
 *
 * 1. Categorize each rule according to the type of its top-level pattern.
 * 2. Substitute for the irrefutable rules to find the 'otherwise' branch.
 * 3. Find the bodies for what happens after we match the various constants.
 *
 * If the otherwise branch is used twice, then construct a let-expression for it.
 *
 */
expression_ref block_case(const vector<expression_ref>& x, const vector<vector<expression_ref>>& p, const vector<expression_ref>& b)
{
  assert(x.size() > 0);

  const int N = x.size();
  const int M = p.size();

  assert(p.size() == b.size());

  // Each pattern must have N components.
  for(int j=0;j<M;j++)
    assert(p[j].size() == N);

  // 1. Categorize each rule according to the type of its top-level pattern
  vector<object_ref> constants;
  vector< vector<int> > rules;
  vector<int> irrefutable_rules;
  for(int j=0;j<M;j++)
  {
    if (is_dummy(p[j][0]))
    {
      irrefutable_rules.push_back(j);
      continue;
    }

    object_ref C = p[j][0]->head;
    int which = find_object(constants, C);

    if (which == -1)
    {
      which = constants.size();
      constants.push_back(C);
      rules.push_back({});
    }

    rules[which].push_back(j);
  }

  // 2. Substitute for the irrefutable rules to find the 'otherwise' branch
  // This is substitute(x[1],p[2..m][1], case x2...xN of p[2..M][i] -> b[2..M] )
  expression_ref otherwise;
  if (irrefutable_rules.empty())
    ; // otherwise = NULL
  else
  {
    vector<expression_ref> x2 = x;
    x2.erase(x2.begin());

    vector<vector<expression_ref>> p2;
    vector<expression_ref> b2;
    for(int i=0;i<irrefutable_rules.size();i++)
    {
      int r = irrefutable_rules[i];
      p2.push_back(p[r]);
      p2.back().erase(p2.back().begin());

      b2.push_back(b[r]);

      object_ptr<const dummy> d = is_a<dummy>(p[r][0]);
      if (d->index == -1)
	assert(d->name.size() == 0);
      else
	// FIXME! What if x[0] isn't a var?
	// Then if *d occurs twice, then we should use a let expression, right?
	b2[i] = substitute(b2[i], *d, x[0]);
    }
      
    if (x2.empty())
    {
      if (b2.size() > 1)
	throw myexception()<<"You may not have duplicate irrefutable rules!";
      otherwise = b2[0];
    }
    else
      otherwise = block_case(x2, p2, b2);
  }
      
  // If there are no conditions on x[0], then we are done.
  if (constants.empty())
  {
    assert(otherwise);
    return otherwise;
  }

  // Find the first safe var index
  std::set<dummy> free;

  for(int i=0;i<x.size();i++)
    add(free, get_free_indices(x[i]));

  for(int i=0;i<p.size();i++)
  {
    add(free, get_free_indices(b[i]));
  
    for(int j=0; j<p[i].size(); j++)
      add(free, get_free_indices(p[i][j]));
  }
  
  int var_index = 0;
  if (not free.empty()) var_index = max_index(free)+1;

  // WHEN should we put the otherwise expression into a LET variable?
  expression_ref O;
  if (otherwise) O = dummy(var_index++);

  // 3. Find the modified bodies for the various constants
  vector<expression_ref> simple_patterns;
  vector<expression_ref> simple_bodies;
  bool all_simple_followed_by_irrefutable = true;

  for(int c=0;c<constants.size();c++)
  {
    // Find the arity of the constructor
    int arity = 0;
    if (object_ptr<const constructor> C = dynamic_pointer_cast<const constructor>(constants[c]))
      arity = C->n_args();

    // Construct the simple pattern for constant C
    object_ref H = constants[c];

    vector<expression_ref> S(arity);
    for(int j=0;j<arity;j++)
      S[j] = dummy(var_index+j);

    int r0 = rules[c][0];

    simple_patterns.push_back(new expression{H,S});
    simple_bodies.push_back({});
    
    // Construct the objects for the sub-case expression: x2[i] = v1...v[arity], x[2]...x[N]
    vector<expression_ref> x2;
    for(int j=0;j<arity;j++)
      x2.push_back(S[j]);
    x2.insert(x2.end(), x.begin()+1, x.end());

    // Are all refutable patterns on x[1] simple and followed by irrefutable patterns on x[2]...x[N]?
    bool future_patterns_all_irrefutable = true;

    // Construct the various modified bodies and patterns
    vector<expression_ref> b2;
    vector<vector<expression_ref> > p2;
    for(int i=0;i<rules[c].size();i++)
    {
      int r = rules[c][i];

      // Add the pattern
      p2.push_back({});
      assert(p[r][0]->size() == arity);

      // Add sub-patterns of p[r][1]
      for(int k=0;k<arity;k++)
	p2.back().push_back(p[r][0]->sub[k]);

      p2.back().insert(p2.back().end(), p[r].begin()+1, p[r].end());

      // Add the body
      b2.push_back(b[r]);

      // Check if p2[i] are all irrefutable
      for(int i=0;i<p2.back().size();i++)
	if (not is_irrefutable_pattern(p2.back()[i]))
	{
	  future_patterns_all_irrefutable = false;
	  all_simple_followed_by_irrefutable = false;
	}
    }

    // If x[1] matches a simple pattern in the only alternative, we may as well
    // not change the variable names for the match slots in this pattern.
    if (rules[c].size() == 1 and is_simple_pattern(p[r0][0]))
    {
      simple_patterns.back() = p[r0][0];

      // case x[1] of p[r0][1] -> case (x[2],..,x[N]) of (p[r0][2]....p[r0][N]) -> b[r0]
      x2 = x;
      x2.erase(x2.begin());

      p2.back() = p[r0];
      p2.back().erase( p2.back().begin() );
    }

    // If all future patterns are irrefutable, then we won't need to backtrack to the otherwise case.
    if (future_patterns_all_irrefutable)
    {
      // There can be only one alternative.
      assert(rules[c].size() == 1);

      if (x2.size())
	simple_bodies.back() = block_case(x2, p2, b2);
      else
	simple_bodies.back() = b[r0];
    }
    else
    {
      if (otherwise)
      {
	p2.push_back(vector<expression_ref>(arity+p[r0].size(), dummy(-1)));
	// Since we could backtrack, use the dummy.  It will point to otherwise
	b2.push_back(O);
      }
      simple_bodies.back() = block_case(x2, p2, b2);
    }
  }

  if (otherwise)
  {
    simple_patterns.push_back(dummy(-1));
    // If we have any backtracking, then use the otherwise dummy, like the bodies.
    if (not all_simple_followed_by_irrefutable)
      simple_bodies.push_back(O);
    else
      simple_bodies.push_back(otherwise);
  }

  // Construct final case expression
  expression_ref CE = make_case_expression(x[0], simple_patterns, simple_bodies);

  if (otherwise and not all_simple_followed_by_irrefutable)
    CE = let_expression(O, otherwise, CE);

  return CE;
}

// Create the expression 'case T of {patterns[i] -> bodies[i]'
expression_ref case_expression(const expression_ref& T, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
  return block_case({T}, {patterns}, {bodies});
}

expression_ref case_expression(const expression_ref& T, const expression_ref& pattern, const expression_ref& body, const expression_ref& otherwise)
{
  vector<expression_ref> patterns = {pattern};
  vector<expression_ref> bodies = {body};
  if (otherwise and not is_a<dummy>(pattern))
  {
    patterns.push_back(dummy(-1));
    bodies.push_back(otherwise);
  }
  return case_expression(T,patterns, bodies);
}

expression_ref def_function(const vector< vector<expression_ref> >& patterns, const vector<expression_ref>& bodies)
{
  // Find the first safe var index
  std::set<dummy> free;

  for(int i=0;i<patterns.size();i++)
  {
    add(free, get_free_indices(bodies[i]));
  
    for(int j=0; j<patterns[i].size(); j++)
      add(free, get_free_indices(patterns[i][j]));
  }
  
  int var_index = 0;
  if (not free.empty()) var_index = max_index(free)+1;

  // All versions of the function must have the same arity
  assert(patterns.size());
  for(int i=1;i<patterns.size();i++)
    assert(patterns[0].size() == patterns[i].size());

  // Construct the dummies
  vector<expression_ref> args;
  for(int i=0;i<patterns[0].size();i++)
    args.push_back(dummy(var_index+i));
    
  // Construct the case expression
  expression_ref E = block_case(args, patterns, bodies);

  // Turn it into a function
  for(int i=patterns[0].size()-1;i>=0;i--)
    E = lambda_quantify(var_index+i, E);

  return E;
}

/*
expression_ref def_function(const vector<expression_ref>& patterns, const expression_ref& body)
{
  return def_function(vector< vector<expression_ref> >(1,patterns), vector<expression_ref>(1,body));
}


expression_ref def_function(const vector<expression_ref>& pattern, const vector<expression_ref>& bodies)
{
  vector< vector<expression_ref> > patterns;

  for(const auto& p: pattern)
  {
    patterns.push_back( vector<expression_ref>() );

    if (not p->size())
      patterns.back().push_back(p);
    else
      for(int i=0;i<p->size();i++)
	patterns.back().push_back(p->sub[i]);
  }

  return def_function(patterns, bodies);
}

expression_ref def_function(const expression_ref& pattern, const expression_ref& body)
{
  return def_function(vector<expression_ref>(1,pattern), vector<expression_ref>(1,body));
}
*/

// Def: a redex is an expression that matches the LHS of a reduction rule.

// NF = the expression contains no redexes.

// HNF = an expression that is either:
//  * a variable
//  * a data value
//  * a built-in function applied to too few arguments
//  * a lambda abstraction whose body is not reducible.
// An expression in HNF may contain redexes in argument positions whereas a NF may not.
// - but how?

// WHNF = Weak head normal form. WHNF terms have no restriction on the body of lambdas, and so include:
//  * a variable
//  + a data value
//  + a built-in Operation (like "+") with too few arguments.
//  * a lambda expression
//  + a constructor
// The terms in + are extensions to the basic lambda calculus.
// Question: how about the expression (@ x y), which cannot be reduced?

// Basically, HNF requires that the body of a lambda is reduced as well, while WHNF does not have this requirement.
// Therefore, \x -> 1+1 is WHNF but not HNF.

bool is_WHNF(const expression_ref& E)
{
  if (E->size())
  {
    // 3. An Operation whose arguments cannot be evaluated, or that does not have all its arguments?
    // Neither case is allowed, currently.

    // 4. Lambda
    object_ptr<const lambda> L = is_a<lambda>(E);
    if (L) return true;

    object_ptr<const lambda2> L2 = is_a<lambda2>(E);
    if (L2) return true;

    // 5. Constructor
    object_ptr<const constructor> C = is_a<constructor>(E);
    if (C) return true;

    return false;
  }
  else
  {
    if (object_ptr<const parameter> p = is_a<parameter>(E))
      return false;

    // 1. a (dummy) variable.
    // 2. Literal constant.  Treat as 0-arg constructor.
    return true;
  }
}

bool is_index_var(const expression_ref& E)
{
  return is_a<index_var>(E);
}

bool is_dummy(const expression_ref& E)
{
  return is_a<dummy>(E);
}

bool is_parameter(const expression_ref& E)
{
  return is_a<parameter>(E);
}

// Remove in favor of is_dummy?
bool is_wildcard(const expression_ref& E)
{
  object_ptr<const dummy> D = is_a<dummy>(E);
  if (not D) return false;
  if (D->name.size()) return false;

  return (D->index < 0);
}

expression_ref launchbury_normalize(const expression_ref& E)
{
  // 1. Var
  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E->size()) return E;
  
  // 2. Lambda
  object_ptr<const lambda> L = is_a<lambda>(E);
  if (L)
  {
    assert(E->size() == 2);
    expression* V = new expression(*E);
    V->sub[1] = launchbury_normalize(E->sub[1]);

    if (V->sub[1] == E->sub[1])
      return E;
    else
      return V;
  }

  // 3. Application
  if (is_a<Apply>(E))
  {
    assert(E->size() == 2);
    if (is_dummy(E->sub[1]))
    { 
      expression* V = new expression(*E);
      V->sub[0] = launchbury_normalize(E->sub[0]);
      return V;
    }
    else
    {
      int var_index = get_safe_binder_index(E);
      expression_ref x = dummy(var_index);

      return let_expression(x, launchbury_normalize(E->sub[1]), apply_expression(launchbury_normalize(E->sub[0]),x));
    }
  }

  // 6. Case
  object_ptr<const Case> IsCase = is_a<Case>(E);
  if (IsCase)
  {
    expression* V = new expression(*E);
    // Normalize the object
    V->sub[1] = launchbury_normalize(V->sub[0]);

    const int L = (V->sub.size()-1)/2;
    // Just normalize the bodies
    for(int i=0;i<L;i++)
      V->sub[2+2*i] = launchbury_normalize(V->sub[2+2*i]);
    
    return V;
  }

  // FIXME! Handle operations.
  // Extend the stack handling to be able to work on more than one argument.
  // Currently there is no need to evaluate arguments before applying them to functions.
  // Can we avoid evaluating functions before calling an operation?
  // - well, we could make the operation throw an exception identifying which is the first argument that needs to be
  //   evaluated.
  // - then, we could put the operation on the stack and begin evaluating just that one argument.
  
  // 4. Constructor
  if (is_a<constructor>(E) or is_a<Operation>(E))
  {
    int var_index = get_safe_binder_index(E);

    expression* C = new expression(E->head);

    // Actually we probably just need x[i] not to be free in E->sub[i]
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    for(int i=0;i<E->size();i++)
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
	bodies.push_back( launchbury_normalize(E->sub[i]) );
      }
    }

    return let_expression(vars, bodies, C);
  }

  // 5. Let 
  object_ptr<const let_obj> Let = is_a<let_obj>(E);
  if (Let)
  {
    expression* V = new expression(*E);
    // Normalize the object
    V->sub[0] = launchbury_normalize(V->sub[0]);

    const int L = (V->sub.size()-1)/2;
    // Just normalize the bodies
    for(int i=0;i<L;i++)
      V->sub[2+2*i] = launchbury_normalize(V->sub[2+2*i]);

    return V;
  }

  std::cerr<<"I don't recognize expression '"+ E->print() + "'\n";
  return E;
}

expression_ref launchbury_unnormalize(const expression_ref& E)
{
  // 1. Var
  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E->size())
    return E;
  
  // 2. Lambda
  object_ptr<const lambda> L = is_a<lambda>(E);
  if (L)
  {
    assert(E->size() == 2);
    expression* V = new expression(*E);
    V->sub[1] = launchbury_unnormalize(E->sub[1]);

    if (V->sub[1] == E->sub[1])
      return E;
    else
      return V;
  }

  // 3. Application
  if (E->size() or is_dummy(E->head))
  {
    // this should never happen!
    std::abort();
    expression* V = new expression(*E);
    V->sub[0] = launchbury_unnormalize(E->sub[0]);
    V->sub[1] = launchbury_unnormalize(E->sub[1]);
    return V;
  }
  
  // 6. Case
  object_ptr<const Case> IsCase = is_a<Case>(E);
  if (IsCase)
  {
    expression* V = new expression(*E);

    // Unormalize the object
    V->sub[1] = launchbury_unnormalize(V->sub[1]);

    const int L = V->sub.size()/2 - 1;
    // Just unnormalize the bodies
    for(int i=0;i<L;i++)
      V->sub[3+2*i] = launchbury_unnormalize(V->sub[3+2*i]);
    
    return V;
  }

  // 4. Constructor
  if (is_a<constructor>(E) or is_a<Operation>(E))
  {
    expression* V = new expression(*E);
    for(int i=0;i<E->size();i++)
      V->sub[i] = launchbury_unnormalize(E->sub[i]);
    return V;
  }

  // 5. Let 
  object_ptr<const let_obj> Let = is_a<let_obj>(E);
  if (Let)
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;
    parse_let_expression(E, vars, bodies, T);

    // unnormalize T and the bodies
    T = launchbury_unnormalize(T);
    for(int i=0; i<vars.size(); i++)
      bodies[i] = launchbury_unnormalize(bodies[i]);

    /*
      Identify cycles of size > 1...
      But what is the optimal behavior in that case?
    ublas::matrix<int> U(vars.size(), vars.size());
    for(int i=0;i<vars.size();i++)
    {
      std::set<dummy> free = get_free_indices(bodies[i]);
      for(int j=0;j<vars.size();j++)
	if (free.find(vars[j]))
	U(i,j) = 0;
    }
    */

    // substitute for non-recursive lets
    bool changed = true;
    while(changed)
    {
      changed = false;

      for(int i=vars.size()-1; i>=0; i--)
      {
	object_ptr<const dummy> V = is_a<dummy>(vars[i]);
	assert(V);
	std::set<dummy> free = get_free_indices(bodies[i]);
	if (free.find(*V) != free.end()) continue;
	
	changed = true;
	
	expression_ref var = vars[i];
	expression_ref body = bodies[i];
	
	vars.erase(vars.begin() + i);
	bodies.erase(bodies.begin() + i);
	
	// substitute for the value of this variable in T and in the remaining bodies;
	for(int j=0;j<vars.size();j++)
	  bodies[j] = substitute(bodies[j], var, body);
	T = substitute(T, var, body);
      }
    }

    return let_expression(vars, bodies, T);
  }

  std::cerr<<"I don't recognize expression '"+ E->print() + "'\n";
  return E;
}

bool is_exactly(const expression_ref& E, const Object& O)
{
  return E->is_exactly(O);
}

bool is_exactly(const expression_ref& E, const string& s)
{
  return is_exactly(E, constructor(s,-1));
}

bool same_head(const expression_ref& E1, const expression_ref& E2)
{
  if (E1->head->compare(*E2->head))
    return true;
  else
    return false;
}

const expression_ref v0 = dummy(0);
const expression_ref v1 = dummy(1);
const expression_ref v2 = dummy(2);
const expression_ref v3 = dummy(3);
const expression_ref v4 = dummy(4);
const expression_ref v5 = dummy(5);
const expression_ref v6 = dummy(6);

expression_ref operator^(const expression_ref& x, const expression_ref& T)
{
  return lambda_quantify(x,T);
}


