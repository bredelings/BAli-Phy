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
// 16. Move to only desugaring entire programs!
//     - A program is a collection of modules.
//     - We can desugar entire modules, BUT we also have the names from other modules to import.
//     (?) How do we handle modules with non-parsed code here, like the Prelude?
// 17. Move to only compiling entire programs, where programs are entire module collections.
//     


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

bool is_function_binding(const expression_ref& decl)
{
  assert(decl.assert_is_a<AST_node>()->type == "Decl");

  expression_ref lhs = decl->sub[0];
  return (lhs.assert_is_a<AST_node>()->type == "funlhs1");
}

bool is_pattern_binding(const expression_ref& decl)
{
  return not is_function_binding(decl);
}

set<string> get_pattern_bound_vars(const expression_ref& decl)
{
  assert(decl.assert_is_a<AST_node>()->type == "Decl");

  expression_ref lhs = decl->sub[0];

  return find_bound_vars(lhs);
}

string get_func_name(const expression_ref& decl)
{
  assert(decl.assert_is_a<AST_node>()->type == "Decl");

  expression_ref lhs = decl->sub[0];
  assert(lhs.assert_is_a<AST_node>()->type == "funlhs1");

  expression_ref name = lhs->sub[0];

  if (name.is_a<dummy>())
    return name.is_a<dummy>()->name;
  else if (name.is_a<AST_node>())
  {
    assert(name.is_a<AST_node>()->type == "id");
    return name.is_a<AST_node>()->value;
  }
  else
    std::abort();
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

vector<expression_ref> parse_fundecls(const vector<expression_ref>& v)
{
  // Now we go through and translate groups of FunDecls.
  vector<expression_ref> decls;
  for(int i=0;i<v.size();i++)
  {
    // If its not a function binding, accept it as is, and continue.
    if (object_ptr<const dummy> d = v[i]->sub[0].is_a<dummy>())
      decls.push_back(new expression(v[i]->head,
				     {
				       v[i]->sub[0],
					 v[i]->sub[1]->sub[0]
					 }
				     )
		      );
    else if (v[i]->sub[0].assert_is_a<AST_node>()->type == "funlhs1")
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
				     {dummy(name),
				      def_function(patterns,bodies)
				     }
				    )
		      );

      // skip the other bindings for this function
      i += (patterns.size()-1);
    }
    else
      std::abort();
  }
  return decls;
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
      set<string> bound2 = bound;

      // Find all the names bound here
      for(auto& e: v)
      {
	if (is_function_binding(e))
	  bound2.insert(get_func_name(e));
	else if (is_pattern_binding(e))
	  add(bound2, get_pattern_bound_vars(e));
      }

      // Replace ids with dummies
      for(auto& e: v)
	e = desugar(m, e, bound2);

      // Convert fundecls to normal decls
      vector<expression_ref> decls = parse_fundecls(v);

      return new expression{E->head,decls};
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
	const symbol_info& S = m.lookup_symbol(n->value);
	string qualified_name = S.name;
	if (S.symbol_type == parameter_symbol)
	  return parameter(qualified_name);
	else
	  return var(qualified_name);
      }
    }
    else if (n->type == "Apply")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);
      expression_ref E2 = v[0];

      // For constructors, actually substitute the body
      if (object_ptr<const var> V = v[0]->is_a<var>())
      {
	if (m.is_declared(V->name))
	{
	  auto S = m.lookup_symbol(V->name);
	  if (S.symbol_type == constructor_symbol)
	    E2 = S.body;
	}
      }

      //      / FIXME - if the first element is a known constructor, then substitute in the arguments!
      //      / FIXME - why aren't constructor name ids being recognized?

      for(int i=1;i<v.size();i++)
	E2 = (E2,v[i]);
      return E2;
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
    else if (n->type == "Let")
    {
      // parse the decls and bind declared names internally to the decls.
      v[0] = desugar(m, v[0], bound);

      vector<expression_ref> decls = v[0]->sub;
      expression_ref body = v[1];

      // find the bound var names + construct arguments to let_obj()
      vector<expression_ref> w = {body};
      set<string> bound2 = bound;
      for(const auto& decl: decls)
      {
	bound2.insert(decl->sub[0].assert_is_a<dummy>()->name);
	w.push_back(decl->sub[0]);
	w.push_back(decl->sub[1]);
      }

      // finally desugar let-body, now that we know the bound vars.
      w[0] = desugar(m, w[0], bound2);

      // construct the new let expression.
      return new expression{ let_obj(), w };
    }
    else if (n->type == "BugsNote")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);

      return v[0];
    }
    else if (n->type == "BugsDefaultValue")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);

      expression_ref default_value = lambda_expression(constructor("DefaultValue",2));

      return (default_value, v[0], v[1]);
    }
    else if (n->type == "BugsDist")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);
      // Fields: (prob_density) (random vars) (parameter expressions)
      expression_ref distributed = lambda_expression( constructor(":~",2) );

      string dist_name = *(v[1]->is_a<const String>());
      expression_ref dist_family;
      if (dist_name == "Normal")
	dist_family = var("normalDist");
      else if (dist_name == "Exponential")
	dist_family = var("exponentialDist");
      else if (dist_name == "Gamma")
	dist_family = var("gammaDist");
      else if (dist_name == "Beta")
	dist_family = var("betaDist");
      else
	throw myexception()<<"Can't translate distribution name '"<<dist_name<<"'";
      vector<expression_ref> args = v; args.erase(args.begin()); args.erase(args.begin());
      expression_ref dist_args = get_tuple(args);
      return (distributed, v[0], Tuple(dist_family,dist_args));
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

expression_ref parse_bugs_line(const Program& P, const string& line)
{
  Program P2 = P;
  // This doesn't declare any aliases!
  {
    Program BUGS("BUGS");
    BUGS.def_constructor("DeclareParameter",1);
    BUGS.def_constructor("DefaultValue",2);
    P2.import_module(BUGS,"BUGS",false);
  }

  expression_ref cmd;
  try
  {
    cmd = parse_bugs_line(line);

    std::cerr<<"BUGS phrase parse: "<<cmd<<"\n";
    cmd = desugar(P2, cmd);
    std::cerr<<"        processed: "<<cmd<<"\n";
    std::cerr<<std::endl;
  }
  catch (const myexception& e)
  {
    std::cerr<<e.what()<<"\n";
  }
  return cmd;
}

void add_BUGS(Parameters& P, const string& filename)
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

  Model_Notes N;
  for(const auto& line: lines)
  {
    // FIXME: Allow blank lines and comments: parse the entire file.
    // FIXME: How will we decide to care about line endings?
    // FIXME: For declaring things (e.g. parameters) I'd like the position in the file not to matter.

    expression_ref cmd = parse_bugs_line(P.get_Program(), line);

    if (is_exactly(cmd, "DeclareParameter"))
    {
      string name = *(cmd->sub[0].assert_is_a<String>());
      cmd = new expression(cmd->head,{parameter(name)});
      Model_Notes N2;
      N2.add_note(cmd);
      P.add_submodel(N2);
    }
    else if (is_exactly(cmd,"DefaultValue") or is_exactly(cmd, ":~"))
      N.add_note(cmd);

    // Here, we want to convert the stream of tokens to an expression ref of the form (distributed,x,(D,args)) where
    //  D is of the form (prob_density,name,density,quantile)
    // The line should look like "x ~ name(args).
    // - x should be a parameter or a tuple of parameters.
    // - args should be empty, or a comma-separated list of haskell expressions.
  }
  P.add_submodel(N);
}

