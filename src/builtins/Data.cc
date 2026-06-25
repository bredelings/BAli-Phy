#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <string>
#include "computation/machine/args.H"
#include "util/myexception.H"
#include "util/io.H"
#include "util/string/split.H"
#include <limits>

using boost::dynamic_pointer_cast;
using std::vector;
using std::string;

extern "C" closure builtin_function_read_csv(OperationArgs& Args)
{
    const string filename = Args.evaluate_slot_to_value(0).as_string();

    auto sep_code = Args.evaluate_slot_to_value(1).as_char();
    // FIXME-UNICODE: Temporary byte boundary. CSV splitting still accepts only
    // one raw byte separator until split() can handle UTF-8 separators.
    if (sep_code > std::numeric_limits<unsigned char>::max())
        throw myexception()<<"read_csv: non-byte separator Char values are not yet supported.";
    const char sep = static_cast<char>(static_cast<unsigned char>(sep_code));

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
