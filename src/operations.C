#include "operations.H"

using std::vector;
using std::string;

using boost::dynamic_pointer_cast;
using boost::shared_ptr;

shared_ptr<const Object> Apply::operator()(OperationArgs& Args) const
{
  shared_ptr<const Object> f = Args.evaluate(0);
  shared_ptr<const Object> arg = Args.reference(1);

  if (shared_ptr<const expression> fE = dynamic_pointer_cast<const expression>(f))
  {
    if (shared_ptr<const lambda> fL = dynamic_pointer_cast<const lambda>(fE->sub[0]))
      return substitute(fE->sub[2], fE->sub[1], arg);
  }

  throw myexception()<<"Can't apply non-lambda object '"<<f->print()<<"' to argument '"<<arg->print()<<"'";
}

std::string Apply::name() const {
  return "@";
}

shared_ptr<const Object> Case::operator()(OperationArgs& Args) const
{
  shared_ptr<const Object> obj = Args.evaluate(0);
  shared_ptr<const Object> alts = Args.reference(1);

  vector<expression_ref> cases;
  vector<expression_ref> results;
  parse_alternatives(alts, cases, results);

  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(obj);

  expression_ref result;
  for(int i=0;i<cases.size() and not result;i++)
  {
    // If its a dummy, then match it.
    if (shared_ptr<const dummy> D2 = dynamic_pointer_cast<const dummy>(cases[i]))
    {
      result = results[i];

      // Substitute the matched value into the expression if the dummy isn't "_";
      if (D2->index >= 0)
	result = substitute(result, cases[i], obj);
    }
    // If we are a 0-arg literal constant constructor, then match iff obj==cases[i]
    else if (not E)
    {
      if (obj->compare(*cases[i]))
	result = results[i];
    }
    // If we are an n-arg constructor, then match iff the case is an expression and the head matches.
    else if (E)
    {
      if (shared_ptr<const expression> E2 = dynamic_pointer_cast<const expression>(cases[i]))
      {
	if (E->sub[0]->compare(*E2->sub[0]))
	{
	  assert(E->size() == E2->size());

	  result = results[i];

	  for(int j=1;j<E->size();j++)
	  {
	    if (not is_wildcard(E2->sub[j]))
	      result = substitute(result, E2->sub[j], E->sub[j]);
	  }
	}
      }
    }
  }

  return result;
}

std::string Case::name() const {
  return "case";
}
