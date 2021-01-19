#include "util/assert.hh"
#include "util/file-readers.H"
#include "util/io.H"

using std::optional;
using std::string;

// See tools/read-trees.{H,cc}
// See alignment/load.{H,cc}

void line_reader::next()
{
    if (not done())
        portable_getline(file,current);
}

line_reader::line_reader(std::istream& f)
    :file_reader<string>(f)
{
    next();
}


