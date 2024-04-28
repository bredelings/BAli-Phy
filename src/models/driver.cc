#include "driver.hh"
#include "parser.hh"
#include "util/myexception.H"
#include "util/io.H"

using std::string;
using std::set;

void zz_driver::push_error_message(const location_type& loc, const std::string& err)
{
//    std::cerr<<"Pushing error message '"<<e.second<<"' at "<<e.first<<"\n";
    auto ee = Note()<<err;
    messages.push_back(Message{ErrorMsg, loc, {ee}});
}

void zz_driver::pop_error_message()
{
//    std::cerr<<"Popping error message\n";
    if (messages.empty())
	throw myexception()<<"No message to pop!";
    messages.pop_back();
}

zz_driver::zz_driver ()
{
    using namespace zz;

    reserved_words = {
       {"function",{parser::token::TOK_FUNCTION,0}},
    };
};

int
zz_driver::parse_file (const std::string &filename, int stok)
{
  string file_contents = read_file(filename,"module");
  return parse_string(file_contents, filename, stok);
}

constexpr int START_EXP = 1;
constexpr int START_TYPE = 2;

int
zz_driver::parse_string (const string& file_contents, const std::string &input_name, int stok)
{
  file = input_name;
  location.initialize (&input_name);

  // Choose which symbol to start parsing.
  start_token = stok;

  scan_begin (file_contents);
  zz::parser parser (*this);
  parser.set_debug_level (trace_parsing);
  int res = parser.parse ();
  scan_end ();

  show_messages( {input_name, file_contents}, std::cerr, messages);
  exit_on_error(messages);

  return res;
}

ptree parse_expression(const string& content, const string& input_name)
{
    zz_driver D;
    D.parse_string(content, input_name, 1);
    return D.result;
}

ptree parse_type(const string& content, const string& input_name)
{
    zz_driver D;
    D.parse_string(content, input_name, 2);
    return D.result;
}

int zz_driver::get_start_token()
{
    int t = start_token;
    start_token = 0;
    return t;
}
