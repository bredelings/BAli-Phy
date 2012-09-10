#include "computation/program.H"
#include <deque>

using std::string;
using std::vector;
using std::deque;

// 1. TODO: move symbol definitions into the Program
// 2. Define bodies for these operators in the Prelude
// 3. Move grammar into a separate file somehow.
// 4. Add ability to change the prior on variables.
// 5. Add ability to add new variables.

expression_ref infix_parse(const Program& m, const symbol_info& op1, const expression_ref& E1, deque<expression_ref>& T);

/// Expression is of the form ... op1 [E1 ...]. Get right operand of op1.
expression_ref infix_parse_neg(const Program& m, const symbol_info& op1, deque<expression_ref>& T)
{
  assert(not T.empty());

  expression_ref E1 = T.front();
  T.pop_front();

  // We are starting with a Neg
  if (is_a<var>(E1) and is_a<var>(E1)->name == "-")
  {
    if (op1.precedence >= 6) throw myexception()<<"Cannot parse '"<<op1.name<<"' -";

    E1 = infix_parse_neg(m, symbol_info("-",variable_symbol,2,6,left_fix), T);

    return infix_parse(m, op1, (var("negate"),E1), T);
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

  return infix_parse_neg(m, {"",variable_symbol,2,-1,non_fix}, T2);
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
  return E;
}

