#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <string>
#include "computation/machine/args.H"
#include "util/myexception.H"
#include "util/io.H"
#include "util/string/split.H"

using boost::dynamic_pointer_cast;
using std::vector;
using std::string;

extern "C" closure builtin_function_read_csv(OperationArgs& Args)
{
  const string filename = Args.evaluate(0).as_<String>();

  const char sep = Args.evaluate(1).as_char();

  checked_ifstream text_file(filename,"csv file");

  EVector vec_all_lines;

  string line;
  while(portable_getline(text_file, line))
  {
      // This is probably not very smart -- ignores quoting, etc.
      // See https://tools.ietf.org/html/rfc4180
      // Probably I should write an actual parser.
      EVector vec_line;
      for(auto field: split(line, sep))
          vec_line.push_back(String(field));
      vec_all_lines.push_back(vec_line);
  }

  return vec_all_lines;
}

extern "C" closure builtin_function_read_file_lines(OperationArgs& Args)
{
  const string filename = Args.evaluate(0).as_<String>();

  checked_ifstream text_file(filename,"text file");

  EVector v;

  string line;
  while(portable_getline(text_file, line))
    v.push_back(String(line));

  return v;
}

extern "C" closure builtin_function_readFile(OperationArgs& Args)
{
  const string filename = Args.evaluate(0).as_<String>();

  std::ifstream in(filename, std::ios::in | std::ios::binary);
  if (in)
  {
      auto contents = new String;
      in.seekg(0, std::ios::end);
      contents->resize(in.tellg());
      in.seekg(0, std::ios::beg);
      in.read(&(*contents)[0], contents->size());
      in.close();
      return {contents};
  }

  throw myexception()<<"readFile: can't open file \""<<filename<<"\"";
}

extern "C" closure builtin_function_string_to_double(OperationArgs& Args)
{
  string s = Args.evaluate(0).as_<String>();
  return {convertTo<double>(s)};
}

extern "C" closure builtin_function_string_to_int(OperationArgs& Args)
{
  string s = Args.evaluate(0).as_<String>();
  return {convertTo<int>(s)};
}
