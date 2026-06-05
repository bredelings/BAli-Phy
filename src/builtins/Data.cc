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
    const string filename = Args.evaluate_slot_to_value(0).as_string();

    const char sep = Args.evaluate_slot_to_value(1).as_char();

    checked_ifstream text_file(filename,"csv file");

    R::RVector vec_all_lines;

    string line;
    while(portable_getline(text_file, line))
    {
        // This is probably not very smart -- ignores quoting, etc.
        // See https://tools.ietf.org/html/rfc4180
        // Probably I should write an actual parser.
        R::RVector vec_line;
        for(auto field: split(line, sep))
            vec_line.push_back(field);
        vec_all_lines.push_back(vec_line);
    }

    return vec_all_lines;
}
