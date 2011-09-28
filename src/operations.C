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

  std::abort();
}

std::string Case::name() const {
  return "case";
}
