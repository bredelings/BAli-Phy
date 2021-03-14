#include "haskell.H"
#include "util/string/join.H"

using std::string;
using std::vector;

string HList::print() const
{
    vector<string> parts;
    for(auto& element: elements)
        parts.push_back(element.print());
    return "[" + join(parts,",") +"]";
}
