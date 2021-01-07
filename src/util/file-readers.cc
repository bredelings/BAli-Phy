#include "util/assert.hh"
#include "util/file-readers.H"
#include "util/io.H"

using std::optional;
using std::string;

optional<string> line_reader::next_one()
{
    std::string line;
    if (portable_getline(file,line))
        return line;
    else
        return {};
}

line_reader::line_reader(std::istream& f)
    :file_reader<string>(f)
{ }


