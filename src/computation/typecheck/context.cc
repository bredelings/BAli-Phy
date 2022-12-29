#include "context.H"
#include "util/string/join.H"

using std::vector;
using std::string;

void TypeCheckerContext::pop_note()
{
    notes.pop_back();
}

void TypeCheckerContext::push_note(const Note& e)
{
    notes.push_back(e);
}

string TypeCheckerContext::print_note() const
{
    vector<string> estrings;
    for(auto& note: notes)
        estrings.push_back("    • "+note.print()+"\n");
    return "Error:\n"+join(estrings, "\n");
}

