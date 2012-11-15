#include "parse.H"
#include "computation/program.H"
#include <deque>
#include <set>
#include "io.H"
#include "models/parameters.H"

using std::string;
using std::vector;
using std::set;
using std::deque;

// 1. Add ability to change the prior on variables.
// 2. Add ability to add new variables.

// 3. Add operators for: &&, ||, not, 
//   - Then define /= as "not x == y"
// 5. Change expression_refs to just define to a var("name").  (For example, seq)

// 7. Add EnumFrom, EnumFromTo, EnumFromToBy to enable parsing [1..n]
//  -----Prelude: http://www.haskell.org/onlinereport/standard-prelude.html
// 8. Enable left sections.
// 9. Enable right sections.
// 10. Enable list comprehensions...
// 11. Add constructors to programs!
// 12. Add the ability to store newtype definitions.
// 13. Define patterns for case expressions...
// 14. Define patterns for let expressions..
// 15. Allow declarations...

expression_ref infix_parse(const Program& m, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T);

/// Expression is of the form ... op1 [E1 ...]. Get right operand of op1.
expression_ref infix_parse_neg(const Program& m, const symbol_info& op1, deque<expression_ref>& T)
{
  assert(not T.empty());

  expression_ref E1 = T.front();
  T.pop_front();

  // We are starting with a Neg
  if (E1->head->compare(AST_node("neg")))
  {
    if (op1.precedence >= 6) throw myexception()<<"Cannot parse '"<<op1.name<<"' -";

    E1 = infix_parse_neg(m, symbol_info("-",variable_symbol,unknown_scope, 2,6,left_fix), T);

    return infix_parse(m, op1, (var("Prelude.negate"),E1), T);
  }
  // If E1 is not a neg, E1 should be an expression, and the next thing should be an Op.
  else
    return infix_parse(m, op1, E1, T);
}

/// Expression is of the form ... op1 E1 [op2 ...]. Get right operand of op1.
expression_ref infix_parse(const Program& m, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T)
{
  if (T.empty())
    return E1;

  symbol_info op2 = m.get_operator( assert_is_a<const var>(T.front())->name );

  // illegal expressions
  if (op1.precedence == op2.precedence and (op1.fixity != op2.fixity or op1.fixity == non_fix))
    throw myexception()<<"Must use parenthesis to order operators '"<<op1.name<<"' and '"<<op2.name<<"'";

  // left association: ... op1 E1) op2 ...
  if (op1.precedence > op2.precedence or (op1.precedence == op2.precedence and op1.fixity == left_fix))
    return E1;

  // right association: .. op1 (E1 op2 {...E3...}) ...
  else
  {
    T.pop_front();
    expression_ref E3 = infix_parse_neg(m, op2, T);
    return infix_parse(m, op1, (var(op2.name), E1, E3), T);
  }
}

expression_ref desugar_infix(const Program& m, const vector<expression_ref>& T)
{
  deque<expression_ref> T2;
  T2.insert(T2.begin(), T.begin(), T.end());

  return infix_parse_neg(m, {"",variable_symbol,unknown_scope,2,-1,non_fix}, T2);
}

set<string> find_bound_vars(const expression_ref& E)
{
  if (object_ptr<const AST_node> n = E.is_a<AST_node>())
  {
    if (n->type == "id")
    {
      assert(not E->size());
      return {n->value};
    }
  }

  set<string> bound;
  for(const auto& e:E->sub)
    add(bound, find_bound_vars(e));

  return bound;
}

expression_ref make_apply(const vector<expression_ref>& v)
{
  assert(not v.empty());
  expression_ref E = v[0];
  for(int i=1;i<v.size();i++)
    E = (E,v[i]);
  return E;
}

string get_func_name(const expression_ref& decl)
{
  assert(decl.assert_is_a<AST_node>()->type == "Decl");

  expression_ref lhs = decl->sub[0];
  assert(lhs.assert_is_a<AST_node>()->type == "funlhs1");

  expression_ref name = lhs->sub[0];

  return name.assert_is_a<dummy>()->name;
}

vector<expression_ref> get_patterns(const expression_ref& decl)
{
  assert(decl.assert_is_a<AST_node>()->type == "Decl");

  expression_ref lhs = decl->sub[0];
  assert(lhs.assert_is_a<AST_node>()->type == "funlhs1");

  vector<expression_ref> patterns = lhs->sub;
  patterns.erase(patterns.begin());
  return patterns;
}

expression_ref get_body(const expression_ref& decl)
{
  expression_ref rhs = decl->sub[1];
  return rhs->sub[0];
}

expression_ref desugar(const Program& m, const expression_ref& E, const set<string>& bound)
{
  vector<expression_ref> v = E->sub;
      
  if (object_ptr<const AST_node> n = E.is_a<AST_node>())
  {
    if (n->type == "infixexp")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);
      return desugar_infix(m, v);
    }
    else if (n->type == "Tuple")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);
      return get_tuple(v);
    }
    else if (n->type == "List")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);
      return get_list(v);
    }
    else if (n->type == "Decls")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);

      // Now we go through and translate groups of FunDecls.
      vector<expression_ref> decls;
      for(int i=0;i<v.size();i++)
      {
	string lhs_type = v[i]->sub[0].assert_is_a<AST_node>()->type;
	// If its not a function binding, accept it as is, and continue.
	if (lhs_type == "id")
	  decls.push_back(v[i]);
	else if (lhs_type == "funlhs1")
	{
	  vector<vector<expression_ref> > patterns;
	  vector<expression_ref> bodies;
	  string name = get_func_name(v[i]);
	  patterns.push_back( get_patterns(v[i]) );
	  bodies.push_back( get_body(v[i]) );

	  for(int j=i+1;j<v.size();j++)
	  {
	    if (v[j].assert_is_a<AST_node>()->type != "funlhs1") break;
	    if (get_func_name(v[j]) != name) break;

	    patterns.push_back( get_patterns(v[j]) );
	    bodies.push_back( get_body(v[j]) );
	  }
	  decls.push_back(new expression(AST_node("Decl"),
					 {new expression(AST_node("id",name)),
					  new expression(AST_node("rhs"),{def_function(patterns,bodies)})
					 }
					)
			  );

	  // skip the other bindings for this function
	  i += (patterns.size()-1);
	}
      }
    }
    else if (n->type == "Decl")
    {
      // Issue, if we are defining functions inside a let binding, then f needs to
      // be a dummy that also binds identifiers inside the main let body.
      // Thus, we can't make the function be a "var".
      // .. does that only happen AFTER we allocate a cell for the "f" in the top-level let expression?
      // Basically, how do we handle fixpoints?  At the top level?

      // To some extent, EVERYthing is a bound variable!
      // However, some things are bound at the top level, while some things are bound at a lower level.

      // Is this a set of function bindings?
      if (v[0].assert_is_a<AST_node>()->type == "funlhs1")
      {
	set<string> bound2 = bound;
	for(const auto& e: v[0]->sub)
	  add(bound2, find_bound_vars(e));

	// Replace bound vars in (a) the patterns and (b) the body
	for(auto& e: v)
	  e = desugar(m, e, bound2);

	return new expression(E->head,v);
      }

      // Is this a set of pattern bindings?
    }
    else if (n->type == "id")
    {
      if (includes(bound,n->value))
	return dummy(n->value);
      else if (m.is_declared(n->value))
      {
	string qualified_name = m.lookup_symbol(n->value).name;
	return var(qualified_name);
      }
    }
    else if (n->type == "Lambda")
    {
      const int n_args = E->size()-1;
      vector<string> arg_names;
      set<string> bound2 = bound;
      for(int i=0;i<n_args;i++)
      {
	object_ptr<const AST_node> m = E->sub[i]->is_a<AST_node>();
	if (m->type != "id")
	  throw myexception()<<"Lambda arguments must be irrefutable!";
	arg_names.push_back(m->value);
	bound2.insert(m->value);
      }
      expression_ref E2 = E->sub.back();
      E2 = desugar(m, E2, bound2);
      for(int j=n_args-1;j>=0;j--)
	E2 = lambda_quantify(dummy(arg_names[j]),E2);
      return E2;;
    }
  }

  for(auto& e: v)
    e = desugar(m, e, bound);
  if (E->size())
    return new expression(E->head,v);
  else
    return E;
}

expression_ref desugar(const Program& m, const expression_ref& E)
{
  return desugar(m,E,{});
}

expression_ref parse_haskell_line(const Program& P, const string& line)
{
  return desugar(P, parse_haskell_line(line));
}

bugs_cmd parse_bugs_line(const Program& P, const string& line)
{
  bugs_cmd cmd;
  try
  {
    cmd = parse_bugs_line(line);

    std::cerr<<"BUGS phrase parse: "<<cmd.var<<" ~ "<<cmd.dist<<"(";
    for(int i=0;i<cmd.arguments.size();i++)
    {
      std::cerr<<cmd.arguments[i];
      if (i != cmd.arguments.size()-1)
	std::cerr<<", ";
    }
    std::cerr<<")\n";
    cmd.var = desugar(P, cmd.var);
    for(auto& e: cmd.arguments)
      e = desugar(P, e);
    std::cerr<<"        processed: "<<cmd.var<<" ~ "<<cmd.dist<<"(";
    for(int i=0;i<cmd.arguments.size();i++)
    {
      std::cerr<<cmd.arguments[i];
      if (i != cmd.arguments.size()-1)
	std::cerr<<", ";
    }
    std::cerr<<")\n";
  }
  catch (const myexception& e)
  {
    std::cerr<<e.what()<<"\n";
  }
  return cmd;
}

void add_BUGS(const Parameters& P, const string& filename)
{
  // Um, so what is the current program?

  // 1. Well, its got a collection of identifiers.
  //   (a) Some of these are functions
  //   (b) Some of these are parameters
  // 2. We've got a collection of heads.

  checked_ifstream file(filename,"BUGS file");
  vector<string> lines;

  {
    string line;
    while(getline(file,line))
      lines.push_back(line);
  }

  std::cerr<<"Read "<<lines.size()<<" lines from Hierarchical Model Description file '"<<filename<<"'\n";

  for(const auto& line: lines)
  {
    bugs_cmd cmd = parse_bugs_line(P.get_Program(), line);

    // Here, we want to convert the stream of tokens to an expression ref of the form (distributed,x,(D,args)) where
    //  D is of the form (prob_density,name,density,quantile)
    // The line should look like "x ~ name(args).
    // - x should be a parameter or a tuple of parameters.
    // - args should be empty, or a comma-separated list of haskell expressions.
  }
  exit(0);
}

