#include "BUGS.H"
#include "computation/program.H"
#include <deque>
#include "io.H"
#include "parameters.H"

using std::string;
using std::vector;
using std::deque;

// 1. Add ability to change the prior on variables.
// 2. Add ability to add new variables.

// 3. Add operators for: &&, ||, not, 
//   - Then define /= as "not x == y"
// 4. Handle (), [], :, and (,,,,) which are not valid is_haskell_op.
//    These should not be Prelude.{sym}.
// 5. Change expression_refs to just define to a var("name").  (For example, seq)

// 6. Run lookup_symbol on ALL names.
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

expression_ref postprocess_infix(const Program& m, const vector<expression_ref>& T)
{
  deque<expression_ref> T2;
  T2.insert(T2.begin(), T.begin(), T.end());

  return infix_parse_neg(m, {"",variable_symbol,unknown_scope,2,-1,non_fix}, T2);
}

expression_ref postprocess(const Program& m, const expression_ref& E)
{
  vector<expression_ref> v = E->sub;
  for(auto& e: v)
    e = postprocess(m, e);
      
  if (E->head->compare(AST_node("infixexp")))
    return postprocess_infix(m, v);
  else if (E->head->compare(AST_node("Tuple")))
    return get_tuple(v);
  else if (E->head->compare(AST_node("List")))
    return get_list(v);
  else if (E->size())
    return new expression(E->head,v);
  else if (object_ptr<const var> V = E.is_a<var>())
  {
    if (m.is_declared(V->name))
    {
      string qualified_name = m.lookup_symbol(V->name).name;
      return var(qualified_name);
    }
  }

  return E;
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

