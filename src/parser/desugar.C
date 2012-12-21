#include "parse.H"
#include "computation/program.H"
#include <deque>
#include <set>
#include "io.H"
#include "models/parameters.H"
#include "AST.H"

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

bool is_irrefutable_pat(const expression_ref& E)
{
  assert(E.assert_is_a<AST_node>()->type == "pat");

  if (E->sub.size() == 1 and E->sub[0]->assert_is_a<AST_node>()->type == "apat_var")
    return true;
  else
    return false;
}


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

  symbol_info op2;
  if (auto v = T.front().is_a<const var>())
    op2 = m.get_operator( v->name );
  else if (auto d = T.front().is_a<const dummy>())
  {
    if (m.is_declared( d->name) )
      op2 = m.get_operator( d->name );
    else
    {
      op2.precedence = 9;
      op2.fixity = left_fix;
    }
  }
  else
    throw myexception()<<"Can't use expression '"<<T.front()->print()<<"' as infix operator.";

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
  if (auto n = E.is_a<AST_node>())
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

set<string> find_all_ids(const expression_ref& E)
{
  if (auto n = E.is_a<AST_node>())
  {
    if (n->type == "id")
    {
      assert(not E->size());
      return {n->value};
    }
  }

  set<string> bound;
  for(const auto& e:E->sub)
    add(bound, find_all_ids(e));

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
  if (not is_AST(decl,"Decl"))
    return false;

  expression_ref lhs = decl->sub[0];
  assert(not is_AST(lhs,"funlhs2"));
  assert(not is_AST(lhs,"funlhs3"));
  return is_AST(lhs,"funlhs1");
}

bool is_pattern_binding(const expression_ref& decl)
{
  return is_AST(decl,"Decl") and not is_function_binding(decl);
}

set<string> get_pattern_bound_vars(const expression_ref& decl)
{
  assert(is_AST(decl,"Decl"));

  expression_ref lhs = decl->sub[0];

  return find_bound_vars(lhs);
}

string get_func_name(const expression_ref& decl)
{
  assert(is_AST(decl,"Decl"));

  expression_ref lhs = decl->sub[0];
  assert(is_AST(lhs,"funlhs1"));

  expression_ref name = lhs->sub[0];

  if (name.is_a<dummy>())
    return name.is_a<dummy>()->name;
  else if (name.is_a<AST_node>())
  {
    assert(is_AST(name,"id"));
    return name.is_a<AST_node>()->value;
  }
  else
    std::abort();
}

vector<expression_ref> get_patterns(const expression_ref& decl)
{
  assert(is_AST(decl,"Decl"));

  expression_ref lhs = decl->sub[0];
  assert(is_AST(lhs,"funlhs1"));

  vector<expression_ref> patterns = lhs->sub;
  patterns.erase(patterns.begin());
  return patterns;
}

expression_ref get_body(const expression_ref& decl)
{
  expression_ref rhs = decl->sub[1];
  if (rhs->sub.size() == 1)
    return rhs->sub[0];
  else
  {
    assert(rhs->sub.size() == 2);
    expression_ref decls = rhs->sub[1];
    assert(is_AST(decls,"Decls"));
    return {AST_node("Let"),{decls,rhs->sub[0]}};
  }
}

expression_ref append(const expression_ref& E1, const expression_ref& E2)
{
  vector<expression_ref> sub = E1->sub;
  sub.push_back(E2);
  return {E1->head, sub};
}

expression_ref append(const expression_ref& E1, const vector<expression_ref>& E2s)
{
  vector<expression_ref> sub = E1->sub;
  for(const auto& E2: E2s)
    sub.push_back(E2);
  return {E1->head, sub};
}

expression_ref translate_funlhs(const expression_ref& E)
{
  if (is_AST(E,"funlhs1"))
    return E;
  else if (is_AST(E,"funlhs2"))
  {
    // Let's just ignore pat elements here -- they can be fixed up by desugar, I think.

    // TODO: We want to look at the infix patterns here to make sure that operator parses as the top-level element.
    
    return {AST_node("funlhs1"),{E->sub[1],E->sub[0],E->sub[2]}};
  }
  else if (is_AST(E,"funlhs3"))
  {
    expression_ref fun1 = translate_funlhs(E->sub[0]);
    vector<expression_ref> args = fun1->sub;
    for(int i=1;i<E->sub.size();i++)
      fun1 = append(fun1,E->sub[i]);
    return fun1;
  }
  else
    return {};
}

expression_ref translate_funlhs_decl(const expression_ref& E)
{
  if (expression_ref funlhs = translate_funlhs(E->sub[0]))
  {
    vector<expression_ref> sub = E->sub;
    sub[0] = funlhs;
    return {E->head,sub};
  }
  else
    return E;
}

vector<expression_ref> parse_fundecls(const vector<expression_ref>& v)
{
  // Now we go through and translate groups of FunDecls.
  vector<expression_ref> decls;
  for(int i=0;i<v.size();i++)
  {
    if (is_AST(v[i],"EmptyDecl")) continue;

    if (is_AST(v[i],"FixityDecl"))
    {
      decls.push_back(v[i]);
      continue;
    }

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
	if (not is_function_binding(v[j])) break;
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

expression_ref get_fresh_id(const string& s, const expression_ref& E)
{
  return AST_node("id",s);
}

/*
 * To handle funlhs1, funlhs2, and funlhs3, we want to
 * (a) Transform the ASTs to funlhs1 before we try to bind variables.  This is because the function names will be bound
 *     in other decls in the same unit.
 * (b) Can we make a 1st pass over decls that doesn't bind ids?
 * (c) This first pass must
 *     (i) Translate funlhs2 to funlhs1.
 *        - Be able to complain about incorrect operator precedence at this stage.
 *     (ii) Translate funlhs3 to funlhs1. Children are translated first.
 * (d) A second stage translates funlhs1 to a standard let expression, I suppose.
 */


/*
 * We probably want to move away from using dummies to represent patterns.
 * - Dummies can't represent e.g. irrefutable patterns.
 */

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
    else if (n->type == "Decls" or n->type == "TopDecls")
    {
      set<string> bound2 = bound;

      // Find all the names bound here
      for(auto& e: v)
      {
	if (is_AST(e,"EmptyDecl")) continue;
	if (is_AST(e,"FixityDecl")) continue;
	// Translate funlhs2 and funlhs3 declaration forms to funlhs1 form.
	e = translate_funlhs_decl(e);

	// Bind the function id to avoid errors on the undeclared id later.
	if (is_function_binding(e))
	  bound2.insert(get_func_name(e));
	else if (is_pattern_binding(e))
	  add(bound2, get_pattern_bound_vars(e));
      }

      // Replace ids with dummies
      for(auto& e: v)
      {
	if (is_AST(e,"EmptyDecl")) continue;
	if (is_AST(e,"FixityDecl")) continue;
	e = desugar(m, e, bound2);
      }

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
    else if (n->type == "ListComprehension")
    {
      expression_ref E2 = E;
      // [ e | True   ]  =  [ e ]
      // [ e | q      ]  =  [ e | q, True ]
      // [ e | b, Q   ]  =  if b then [ e | Q ] else []
      // [ e | p<-l, Q]  =  let {ok p = [ e | Q ]; ok _ = []} in Prelude.concatMap ok l
      // [ e | let decls, Q] = let decls in [ e | Q ]

      expression_ref True {AST_node("SimpleQual"),{constructor("True",0)}};

      assert(v.size() >= 2);
      if (v.size() == 2 and v[1]->compare(*True))
	E2 = {AST_node("List"),{v[0]}};
      else if (v.size() == 2)
	E2 = {E->head,{v[0],v[1],True}};
      else 
      {
	expression_ref B = v[1];
	v.erase(v.begin()+1);
	E2 = {E->head,v};

	if (B->assert_is_a<AST_node>()->type == "SimpleQual")
	  E2 = {AST_node("If"),{B->sub[0],E2,AST_node("id","[]")}};
	else if (B->assert_is_a<AST_node>()->type == "PatQual")
	{
	  expression_ref p = B->sub[0];
	  expression_ref l = B->sub[1];
	  if (is_irrefutable_pat(p))
	  {
	    expression_ref f {AST_node("Lambda"),{p,E2}};
	    E2 = {AST_node("Apply"),{AST_node("id","Prelude.concatMap"),f,l}};
	  }
	  else
	  {
	    // Problem: "ok" needs to be a fresh variable.
	    expression_ref ok = get_fresh_id("ok",E);

	    expression_ref lhs1 = {AST_node("funlhs1"),{ok,p}};
	    expression_ref rhs1 = {AST_node("rhs"),{E2}};
	    expression_ref decl1 = {AST_node("Decl"),{lhs1,rhs1}};

	    expression_ref lhs2 = {AST_node("funlhs1"),{ok,AST_node("WildcardPattern")}};
	    expression_ref rhs2 = {AST_node("rhs"),{AST_node("id","[]")}};
	    expression_ref decl2 = {AST_node("Decl"),{lhs2,rhs2}};

	    expression_ref decls = {AST_node("Decls"),{decl1, decl2}};
	    expression_ref body = {AST_node("Apply"),{AST_node("id","Prelude.concatMap"),ok,l}};

	    E2 = {AST_node("Let"),{decls,body}};
	  }
	}
	else if (B->assert_is_a<AST_node>()->type == "LetQual")
	  E2 = {AST_node("Let"),{B->sub[0],E2}};
      }
      return desugar(m,E2,bound);
    }
    else if (n->type == "Lambda")
    {
      // FIXME: Try to preserve argument names (in block_case( ), probably) when they are irrefutable apat_var's.

      // 1. Extract the lambda body
      expression_ref body = v.back();
      v.pop_back();

      // 2. Find bound vars and convert vars to dummies.
      set<string> bound2 = bound;
      for(auto& e: v) {
	add(bound2, find_bound_vars(e));
	e = desugar(m, e, bound);
      }

      // 3. Desugar the body, binding vars mentioned in the lambda patterns.
      body = desugar(m, body, bound2);

      return def_function({v},{body}); 
    }
    else if (n->type == "Do")
    {
      assert(is_AST(E->sub[0],"Stmts"));
      vector<expression_ref> stmts = E->sub[0]->sub;

      // do { e }  =>  e
      if (stmts.size() == 1) 
	return desugar(m,stmts[0],bound);

      expression_ref first = stmts[0];
      stmts.erase(stmts.begin());
      expression_ref do_stmts = {AST_node("Do"),{{AST_node("Stmts"),stmts}}};
      expression_ref result;
      
      // do {e ; stmts }  =>  e >> do { stmts }
      if (is_AST(first,"SimpleStmt"))
      {
	expression_ref e = first->sub[0];
	expression_ref qop = AST_node("id","Prelude.>>");
	result = {AST_node("infixexp"),{e, qop, do_stmts}};
      }

      // do { p <- e ; stmts} => let {ok p = do {stmts}; ok _ = fail "..."} in e >>= ok
      // do { v <- e ; stmts} => e >>= (\v -> do {stmts})
      else if (is_AST(first,"PatStmt"))
      {
	expression_ref p = first->sub[0];
	expression_ref e = first->sub[1];
	expression_ref qop = AST_node("id","Prelude.>>=");

	if (is_irrefutable_pat(p))
	{
	  expression_ref lambda = {AST_node("Lambda"),{p,do_stmts}};
	  result = {AST_node("infixexp"),{e,qop,lambda}};
	}
	else
	{
	  expression_ref fail = {AST_node("Apply"),{AST_node("id","Prelude.fail"),"Fail!"}};
	  expression_ref ok = get_fresh_id("ok",E);
	  
	  expression_ref lhs1 = {AST_node("funlhs1"),{ok,p}};
	  expression_ref rhs1 = {AST_node("rhs"),{do_stmts}};
	  expression_ref decl1 = {AST_node("Decl"),{lhs1,rhs1}};
	  
	  expression_ref lhs2 = {AST_node("funlhs1"),{ok,AST_node("WildcardPattern")}};
	  expression_ref rhs2 = {AST_node("rhs"),{fail}};
	  expression_ref decl2 = {AST_node("Decl"),{lhs2,rhs2}};
	  expression_ref decls = {AST_node("Decls"),{decl1, decl2}};

	  expression_ref body = {AST_node("infixexp"),{e,qop,ok}};

	  result = {AST_node("Let"),{decls,body}};
	}
      }
      // do {let decls ; rest} = let decls in do {stmts}
      else if (is_AST(first,"LetStmt"))
      {
	expression_ref decls = first->sub[0];
	result = {AST_node("Let"),{decls,do_stmts}};
      }
      else if (is_AST(first,"EmptyStmt"))
	result = do_stmts;

      return desugar(m,result,bound);
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
    else if (n->type == "enumFrom")
    {
      expression_ref E2 = var("Prelude.enumFrom");
      for(auto& e: v) {
	e = desugar(m, e, bound);
	E2 = (E2,e);
      }
      return E2;
    }
    else if (n->type == "enumFromTo")
    {
      expression_ref E2 = var("Prelude.enumFromTo");
      for(auto& e: v) {
	e = desugar(m, e, bound);
	E2 = (E2,e);
      }
      return E2;
    }
    else if (n->type == "Integer")
    {
      string s = *E->sub[0].assert_is_a<String>();
      return Int(convertTo<int>(s));
    }
    else if (n->type == "Float")
    {
      string s = *E->sub[0].assert_is_a<String>();
      return Double(convertTo<double>(s));
    }
    else if (n->type == "Char")
    {
      E->sub[0].assert_is_a<Char>();
      return E->sub[0];
    }
    else if (n->type == "String")
    {
      return E->sub[0];
    }
    else if (n->type == "BugsNote")
    {
      // This expression should have the form 'AST[BugsNote] (AST[infixexp] (AST[Apply] con_name arg1 arg2 ... arg_n) )'.
      v = E->sub[0]->sub[0]->sub;

      string con_name = v[0].assert_is_a<AST_node>()->value;
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
    else if (n->type == "BugsDist" or n->type == "BugsExternalDist" or n->type == "BugsDataDist")
    {
      for(auto& e: v)
	e = desugar(m, e, bound);
      // Fields: (prob_density) (random vars) (parameter expressions)
      expression_ref distributed = lambda_expression( constructor(":~",2) );

      string dist_name = *(v[1]->is_a<const String>());

      vector<expression_ref> args = v; args.erase(args.begin()); args.erase(args.begin());
      expression_ref dist_args = get_tuple(args);
      return (distributed, v[0], (var(dist_name), dist_args));
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
  expression_ref cmd = parse_bugs_line(line);

  std::cerr<<"BUGS phrase parse: "<<cmd<<"\n";
  cmd = desugar(P, cmd);
  std::cerr<<"        processed: "<<cmd<<"\n";
  std::cerr<<std::endl;

  return cmd;
}

bool is_all_space(const string& line)
{
  for(int i=0;i<line.size();i++)
    if (not ::isspace(line[i])) return false;
  return true;
}

Model_Notes read_BUGS(const vector<string>& modules_path, const Parameters& P, const string& filename, const string& module_name_)
{
  // 1. Read file into string.
  string file_contents = read_file(filename, "BUGS File");

  // 2. bugs_file = module + notes
  expression_ref bugs_file = parse_bugs_file(file_contents);
  assert(is_AST(bugs_file,"BugsFile"));

  expression_ref module = bugs_file->sub[0];
  assert(is_AST(module,"Module"));
  expression_ref bugs_notes;
  if (bugs_file->sub.size() == 2)
  {
    bugs_notes = bugs_file->sub[1];
    assert(is_AST(bugs_notes,"BugsLines"));
  }

  // 3. module = [optional name] + body
  string module_name = module_name_;
  expression_ref body;
  if (module->sub.size() == 1)
    body = module->sub[0];
  else
  {
    module_name = *module->sub[0].is_a<String>();
    body = module->sub[1];
  }
  assert(is_AST(body,"Body"));

  // 4. body = impdecls + [optional topdecls]
  expression_ref impdecls;
  expression_ref topdecls;
  if (body->sub.size() == 1)
  {
    topdecls = body->sub[0];
    assert(is_AST(topdecls,"TopDecls"));
  }

  // 5. Process imports
  Program BUGS(module_name);
  BUGS.import_module(modules_path,"Prelude", false);
  BUGS.import_module(modules_path,"Distributions", false);
  BUGS.import_module(modules_path,"Range", false);
  BUGS.import_module(modules_path,"SModel", false);
  BUGS.import_module(modules_path,"PopGen", false);
  BUGS.import_module(P.get_Program(), false);

  Model_Notes N;

  // 6a. Find explicitly and implicitly-declared parameters
  set<string> new_parameters;
  for(const auto& cmd: bugs_notes->sub)
  {
    // Separate into BugsDist and ForeignBugsDist?
    // Then only declare params in BugsDist which would require previous (foreign) declarations for all params in ForeignBugsDist.
    if (is_AST(cmd,"BugsDist"))
      add(new_parameters, find_all_ids(cmd->sub[0]));
    else if (is_AST(cmd,"Parameter"))
    {
      string name = *(cmd->sub[0].assert_is_a<String>());
      new_parameters.insert(name);
    }
  }

  // 7. Actually declare them.
  for(const auto& id: new_parameters)
  {
    if (not BUGS.is_declared(id))
    {
      BUGS.declare_parameter(id);
      def_parameter(N,module_name+"."+id);
    }
  }

  // 8. Declare objects from the Haskell module
  if (topdecls)
    BUGS += topdecls;

  // 9. Add notes
  for(const auto& cmd: bugs_notes->sub)
    N.add_note(desugar(BUGS,cmd));

  // 10. Add Loggers for any locally declared parameters
  for(const auto& name: new_parameters)
  {
    expression_ref make_logger = lambda_expression( constructor("MakeLogger",1) );
    N.add_note((make_logger,parameter(name)));
  }

  N.add_module(BUGS);

  return N;
}

void add_BUGS(const vector<string>& modules_path, Parameters& P, const std::string& filename, const std::string& module_name)
{
  Model_Notes N = read_BUGS(modules_path, P, filename, module_name);

  P.add_submodel(N);

  for(int i=0;i<P.n_notes();i++)
    std::cerr<<"note "<<i<<" = "<<P.get_note(i)->print()<<"\n\n";
}
