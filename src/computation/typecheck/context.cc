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

std::optional<yy::location> TypeCheckerContext::source_span() const
{
    if (locs.empty())
        return {};
    else
        return locs.back();
}

void TypeCheckerContext::push_source_span(const yy::location& loc)
{
    locs.push_back(loc);
}

void TypeCheckerContext::pop_source_span()
{
    locs.pop_back();
}

void TypeCheckerContext::push_binder(const binder_info& binder)
{
    binder_stack.push_back(binder);
}

void TypeCheckerContext::pop_binder()
{
    binder_stack.pop_back();
}

