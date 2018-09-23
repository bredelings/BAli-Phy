#include "driver.hh"
#include "parser.hh"

void driver::pop_context()
{
    contexts.pop_back();
}

boost::optional<LayoutContext> driver::get_context()
{
    if (contexts.empty())
	throw myexception()<<"No layout context to access!";
    return contexts.back();
}

void driver::push_context(const boost::optional<LayoutContext>& lc)
{
    contexts.push_back(lc);
}

driver::driver ()
  : trace_parsing (false), trace_scanning (false)
{
}

int
driver::parse (const std::string &f)
{
  file = f;
  location.initialize (&file);
  scan_begin ();
  yy::parser parser (*this);
  parser.set_debug_level (trace_parsing);
  int res = parser.parse ();
  scan_end ();
  return res;
}

