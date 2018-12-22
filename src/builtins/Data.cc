#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <string>
#include "computation/machine/args.H"
#include "myexception.H"
#include "util/io.H"
#include "computation/expression/expression.H"

using boost::dynamic_pointer_cast;
using std::vector;
using std::string;

extern "C" closure builtin_function_read_file(OperationArgs& Args)
{
  const string filename = Args.evaluate(0).as_<String>();

  checked_ifstream text_file(filename,"text file");

  EVector v;

  string line;
  while(portable_getline(text_file, line))
    v.push_back(String(line));

  return v;
}

extern "C" closure builtin_function_string_to_double(OperationArgs& Args)
{
  string s = Args.evaluate(0).as_<String>();
  return {convertTo<double>(s)};
}
