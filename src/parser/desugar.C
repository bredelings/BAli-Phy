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
// 18. Note that in add_BUGS( ) we use a program to parse the lines into a Model_Notes submodel.
//     This submodel can then be prefixed and everything.
//     HKY would be something like:
//
//     Module HKY where {
//       DeclareParameter kappa
//       import SModel (HKY,dna)
//       import Distributions
//       kappa ~ LogLaplace(log(2), 0.25)
//       main = HKY dna kappa [piA,piT,piG,piC]
//     }
//
//     Module PlusF where
//     {
//       import Distributions
//       DeclareParameter piA
//       DeclareParameter piG
//       DeclareParameter piT
//       DeclareParameter piC
//       piA = 0.25
//       piG = 0.25
//       piT = 0.25
//       piC = 0.25
//       [piA, piT, piG, piC] ~ Dirichlet([1.0, 1.0, 1.0, 1.0])
//       bounds piA (0.0, 1.0)
//       bounds piG (0.0, 1.0)
//       bounds piT (0.0, 1.0)
//       bounds piC (0.0, 1.0)
//
//       main = [piA, piT, piG, piC]
//     }
/*
 * OK, in a formula_expression_ref, how would I
 * (a) define local variables & parse identifiers to refer to them.
 * (b) import external variables to reference
 * (c) 
 * Perhaps a formula_expression_ref is just a tool for constructing a Model_Notes with a focussed expression.
 * Two formula_expression_ref's should only be combined if they have the same module name, I would think...
 */


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

    expression_ref E1_op2_E3 = (var(op2.name), E1, E3);

    if (op2.symbol_type == constructor_symbol)
    {
      assert(op2.arity == 2);
      E1_op2_E3 = {constructor(op2.name, 2),{E1,E3}};
    }

    return infix_parse(m, op1, E1_op2_E3, T);
  }
}

expression_ref desugar_infix(const Program& m, const vector<expression_ref>& T)
{
  deque<expression_ref> T2;
  T2.insert(T2.begin(), T.begin(), T.end());

  return infix_parse_neg(m, {"",variable_symbol,unknown_scope,2,-1,non_fix}, T2);
}

expression_ref infixpat_parse(const Program& m, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T);

/// Expression is of the form ... op1 [E1 ...]. Get right operand of op1.
expression_ref infixpat_parse_neg(const Program& m, const symbol_info& op1, deque<expression_ref>& T)
{
  assert(not T.empty());

  expression_ref E1 = T.front();
  T.pop_front();

  // We are starting with a Neg float
  if (E1->head->compare(AST_node("neg_h_float")))
  {
    if (op1.precedence >= 6) throw myexception()<<"Cannot parse '"<<op1.name<<"' -";

    string d = *E1->sub[0].is_a<String>();
    Double D = -convertTo<double>(d);

    return infixpat_parse(m, op1, D, T);
  }
  // We are starting with a Neg integer
  else if (E1->head->compare(AST_node("neg_h_integer")))
  {
    if (op1.precedence >= 6) throw myexception()<<"Cannot parse '"<<op1.name<<"' -";

    string i = *E1->sub[0].is_a<String>();
    Int I = -convertTo<int>(i);

    return infixpat_parse(m, op1, I, T);
  }
  // If E1 is not a neg, E1 should be an expression, and the next thing should be an Op.
  else
    return infixpat_parse(m, op1, E1, T);
}

/// Expression is of the form ... op1 E1 [op2 ...]. Get right operand of op1.
expression_ref infixpat_parse(const Program& m, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T)
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
    expression_ref E3 = infixpat_parse_neg(m, op2, T);

    if (op2.symbol_type != constructor_symbol)
      throw myexception()<<"Using non-constructor operator '"<<op2.name<<"' in pattern is not allowed.";
    if (op2.arity != 2)
      throw myexception()<<"Using constructor operator '"<<op2.name<<"' with arity '"<<op2.arity<<"' is not allowed.";
    expression_ref constructor_pattern = {constructor(op2.name, 2),{E1,E3}};

    return infixpat_parse(m, op1, constructor_pattern, T);
  }
}

expression_ref desugar_infixpat(const Program& m, const vector<expression_ref>& T)
{
  deque<expression_ref> T2;
  T2.insert(T2.begin(), T.begin(), T.end());

  return infixpat_parse_neg(m, {"",variable_symbol,unknown_scope,2,-1,non_fix}, T2);
}

set<string> find_bound_vars(const expression_ref& E)
{
  if (object_ptr<const AST_node> n = E.is_a<AST_node>())
  {
    if (n->type == "apat_var")
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
	if (v[j]->sub[0].assert_is_a<AST_node>()->type != "funlhs1") break;
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
    else if (n->type == "pat")
    {
      // 1. Collect the entire pat expression in 'args'.
      vector<expression_ref> args = v;
      while(args.back().is_a<AST_node>() and args.back().is_a<AST_node>()->type == "pat")
      {
	expression_ref rest = args.back();
	args.pop_back();
	args.insert(args.end(), rest->sub.begin(), rest->sub.end());
      }

      // 2. We could probably do this later.
      for(auto& arg: args)
	arg = desugar(m, arg, bound);

      return desugar_infixpat(m, args);
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

      /*
       * Wait.... so we want to do a recursive de-sugaring, but we can't do that because we 
       * don't know the set of bound variables yet.
       */
    }
    else if (n->type == "apat_var")
    {
      return dummy(n->value);
    }
    else if (n->type == "WildcardPattern")
    {
      return dummy(-1);
    }
    else if (n->type == "id")
    {
      // Local vars bind id's tighter than global vars.
      if (includes(bound,n->value))
	return dummy(n->value);
      // If the variable is free, then try top-level names.
      else if (m.is_declared(n->value))
      {
	const symbol_info& S = m.lookup_symbol(n->value);
	string qualified_name = S.name;
	if (S.symbol_type == parameter_symbol)
	  return parameter(qualified_name);
	else
	  return var(qualified_name);
      }
      else
	throw myexception()<<"Can't find id '"<<n->value<<"'";
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
      // FIXME: This should use def_function( ) and thus block_case( ) to handle pattern matching in the arguments.
      // FIXME: Try to preserve argument names (in block_case( ), probably) when they are irrefutable apat_var's.
      // FIXME: Don't use a separate code path when we have a pattern.
      const int n_args = E->size()-1;
      vector<string> arg_names;
      set<string> bound2 = bound;
      for(int i=0;i<n_args;i++)
      {
	object_ptr<const AST_node> m = E->sub[i]->is_a<AST_node>();
	if (m->type != "apat_var")
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
    else if (n->type == "constructor_pattern")
    {
      string gcon = *v[0].assert_is_a<String>();
      v.erase(v.begin());

      // If the variable is free, then try top-level names.
      if (not m.is_declared(gcon))
	throw myexception()<<"Constructor pattern '"<<E<<"' has unknown constructor '"<<gcon<<"'";

      const symbol_info& S = m.lookup_symbol(gcon);
      if (S.symbol_type != constructor_symbol)
	throw myexception()<<"Constructor pattern '"<<E<<"' has head '"<<gcon<<"' which is not a constructor!";

      if (S.arity != v.size())
	throw myexception()<<"Constructor pattern '"<<E<<"' has arity "<<v.size()<<" which does not equal "<<S.arity<<".";

      // Desugar the 
      for(auto& e: v)
	e = desugar(m, e, bound);

      return new expression{ constructor(S.name,S.arity),v };
    }
    else if (n->type == "If")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);

      return case_expression(v[0],true,v[1],v[2]);
    }
    else if (n->type == "LeftSection")
    {
      // FIXME... probably we need to do a disambiguation on the infix expression. (x op infixexp)
      // FIXME... the infixexp needs to parse the same as if it was parenthesized.
      std::set<dummy> free_vars;
      for(auto& e: v) {
	e = desugar(m, e, bound);
	add(free_vars, get_free_indices(e));
      }
      return apply_expression(v[1],v[0]);
    }
    else if (n->type == "RightSection")
    {
      // FIXME... probably we need to do a disambiguation on the infix expression. (infixexp op x)
      // FIXME... the infixexp needs to parse the same as if it was parenthesized.
      std::set<dummy> free_vars;
      for(auto& e: v) {
	e = desugar(m, e, bound);
	add(free_vars, get_free_indices(e));
      }
      int safe_dummy_index = 0;
      if (not free_vars.empty())
	safe_dummy_index = max_index(free_vars)+1;
      dummy vsafe(safe_dummy_index);
      return vsafe^apply_expression(apply_expression(v[0],vsafe),v[1]);
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
    else if (n->type == "Case")
    {
      expression_ref case_obj = desugar(m, v[0], bound);
      vector<expression_ref> alts = v[1]->sub;
      vector<expression_ref> patterns;
      vector<expression_ref> bodies;
      for(int i=0;i<alts.size();i++)
      {
	set<string> bound2 = bound;
	add(bound2, find_bound_vars(alts[i]->sub[0]));
	patterns.push_back(desugar(m, alts[i]->sub[0], bound2) );
	bodies.push_back(desugar(m, alts[i]->sub[1], bound2) );
      }
      return case_expression(case_obj, patterns, bodies);
    }
    else if (n->type == "EnumFrom")
    {
      expression_ref E2 = var("Prelude.enumFrom");
      for(auto& e: v) {
	e = desugar(m, e, bound);
	E2 = (E2,e);
      }
      return E2;
    }
    else if (n->type == "EnumFromTo")
    {
      expression_ref E2 = var("Prelude.enumFromTo");
      for(auto& e: v) {
	e = desugar(m, e, bound);
	E2 = (E2,e);
      }
      return E2;
    }
    else if (n->type == "BugsNote")
    {
      string con_name = *E->sub[0].assert_is_a<String>();
      v.erase(v.begin());
      for(auto& e: v)
	e = desugar(m, e, bound);

      return new expression{ constructor(con_name,v.size()), v};
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
  expression_ref cmd;
  try
  {
    cmd = parse_bugs_line(line);

    std::cerr<<"BUGS phrase parse: "<<cmd<<"\n";
    cmd = desugar(P, cmd);
    std::cerr<<"        processed: "<<cmd<<"\n";
    std::cerr<<std::endl;
  }
  catch (const myexception& e)
  {
    std::cerr<<e.what()<<"\n";
  }
  return cmd;
}

Model_Notes read_BUGS(const Parameters& P, const string& filename, const string& module_name)
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

  Program BUGS(module_name);
  BUGS.import_module(P.get_Program(),"Prelude", false);
  BUGS.import_module(P.get_Program(),"Distributions", false);
  BUGS.import_module(P.get_Program(),"SModel", false);
  BUGS.import_module(P.get_Program(),"Main", false);
  for(const auto& line: lines)
  {
    expression_ref cmd = parse_bugs_line(BUGS, line);

    if (is_exactly(cmd, "DeclareParameter"))
    {
      string name = *(cmd->sub[0].assert_is_a<String>());
      BUGS.declare_parameter(name);
    }
  }

  Model_Notes N;
  for(const auto& line: lines)
  {
    expression_ref cmd = parse_bugs_line(BUGS, line);
    if (is_exactly(cmd, "DeclareParameter"))
    {
      string name = *(cmd->sub[0].assert_is_a<String>());
      cmd = new expression{cmd->head,{parameter(BUGS.lookup_symbol(name).name)}};
    }

    N.add_note(cmd);
  }

  return N;
}

void add_BUGS(Parameters& P, const std::string& filename, const std::string& module_name)
{
  Model_Notes N = read_BUGS(P, filename, module_name);

  P.add_submodel(N);

  for(int i=0;i<P.n_notes();i++)
    std::cerr<<"note "<<i<<" = "<<P.get_note(i)->print()<<"\n\n";
}
