#include "BUGS.H"
#include <deque>

using std::string;
using std::vector;
using std::deque;

symbol_info module::get_operator(const string& name) const
{
  auto s = symbols.find(name);
  if (s == symbols.end()) throw myexception()<<"Cannot find symbol '"<<name<<"'";

  if (s->second.precedence == -1) throw myexception()<<"Symbol '"<<name<<"' is not an operator.";

  if (s->second.arity != 2) throw myexception()<<"Operator '"<<name<<"' does not have arity 2!";

  return s->second;
}

/// Expression is of the form {E1 op1} E2 [op2 E3 ... ].  Get 
expression_ref parse_infix(const module& m, expression_ref E1, string op1, deque<expression_ref>& T)
{
  while (T.size() > 1)
  {
    symbol_info S1 = m.get_operator(op1);
    expression_ref E2 = T.front(); T.pop_front();
    const string op2 = convert<const var>(T.front())->name; T.pop_front();
    symbol_info S2 = m.get_operator(op2);

    // illegal expressions
    if (S1.precedence == S2.precedence and (S1.fixity != S2.fixity or S1.fixity == 0))
      throw myexception()<<"Must use parenthesis to order operators '"<<op1<<"' and '"<<op2<<"'";

    // left association: (E1 op1 E2)@E1' op2 ...
    if (S1.precedence > S2.precedence or (S1.precedence == S2.precedence and S1.fixity == 1))
    {
      E1 = (var(op1), E1, E2);
      op1 = op2;
    }
    // right association: E1 op1 (E2 op2 ... )@E3 op3 ...
    else
    {
      expression_ref E3 = parse_infix(m, E2, op2, T);
      T.push_front( E3 );
    }
  }

  E1 = (var(op1), E1, T.front());
  T.pop_front();
  return E1;
}

expression_ref postprocess_infix(const module& m, const vector<expression_ref>& T)
{
  deque<expression_ref> T2;
  T2.insert(T2.begin(), T.begin(), T.end());

  expression_ref E1 = T2.front();
  T2.pop_front();
  string op1 = convert<const var>(T2.front())->name; T2.pop_front();

  return parse_infix(m, E1, op1, T2);
}

expression_ref postprocess(const module& m, const expression_ref& E)
{
  if (E->head->compare(AST_node("infixexp")))
  {
    vector<expression_ref> v = E->sub;
    for(auto& e: v)
      e = postprocess(m, e);
      
    return postprocess_infix(m, v);
  }
  else
    return E;
}

