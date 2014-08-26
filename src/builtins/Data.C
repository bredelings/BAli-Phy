#include <vector>
#include <string>
#include "computation/computation.H"
#include "myexception.H"
#include "io.H"

using boost::dynamic_pointer_cast;
using std::vector;
using std::string;

extern "C" closure builtin_function_read_file(OperationArgs& Args)
{
  const string& filename = *Args.evaluate_as<String>(0);

  checked_ifstream text_file(filename,"text file");

  OVector v;

  string line;
  while(portable_getline(text_file, line))
    v.push_back(new String(line));

  return v;
}

extern "C" closure builtin_function_string_to_double(OperationArgs& Args)
{
  object_ptr<const String> s = Args.evaluate_as<String>(0);
  return Double(convertTo<double>(*s));
}
