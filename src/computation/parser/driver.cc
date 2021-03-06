#include "driver.hh"
#include "parser.hh"
#include "util/myexception.H"
#include "util/io.H"

using std::string;

void driver::pop_context()
{
    if (contexts.empty())
	throw myexception()<<"Trying to pop empty context!";
    contexts.pop_back();
}

std::optional<LayoutContext> driver::get_context()
{
    if (contexts.empty())
	return {};
    return contexts.back();
}

void driver::push_context(const std::optional<LayoutContext>& lc)
{
    contexts.push_back(lc);
}

void driver::push_context(const LayoutContext& lc)
{
    contexts.push_back(lc);
}

void driver::push_module_context()
{
//    std::cerr<<"running push_module_context\n";
    contexts.push_back(LayoutContext{1,true});
}

void driver::push_context()
{
    contexts.push_back({});
}

LayoutContext driver::get_offside(const yy::parser::location_type& loc)
{
    int offset = loc.end.column;
    if (auto layout_context = get_context())
	return {offset - layout_context->offset, layout_context->gen_semis};
    else
	return {1,false};
}

void driver::push_error_message(const std::pair<location_type,std::string>& e)
{
//    std::cerr<<"Pushing error message '"<<e.second<<"' at "<<e.first<<"\n";
    error_messages.push_back(e);
}

void driver::pop_error_message()
{
//    std::cerr<<"Popping error message\n";
    if (error_messages.empty())
	throw myexception()<<"No message to pop!";
    error_messages.pop_back();
}

driver::driver ()
    : trace_parsing (false), trace_scanning (false)
{
}

int
driver::parse_file (const std::string &filename)
{
  string file_contents = read_file(filename,"module");
  return parse_string(file_contents, filename);
}

int
driver::parse_string (const string& file_contents, const std::string &input_name)
{
  file = input_name;
  location.initialize (&input_name);
  scan_begin (file_contents);
  yy::parser parser (*this);
  parser.set_debug_level (trace_parsing);
  int res = parser.parse ();
  scan_end ();
  for(auto& e: error_messages)
  {
      std::cerr << e.first << ": " << e.second << '\n';
  }
  if (error_messages.size())
      throw myexception()<<"Parsing failed.";
  return res;
}

Haskell::Module parse_module_file(const string& content, const std::string& input_name)
{
    driver D;
    D.parse_string(content, input_name);
    return D.result;
}
