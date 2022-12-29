#ifndef TYPECHECKERCONTEXT
#define TYPECHECKERCONTEXT

#include <string>
#include <vector>
#include <sstream>
#include "computation/parser/located.H"
#include "computation/module.H"

struct Note
{
    std::string ctx;

    void prepend(const std::string& s) {ctx = s + ctx;}

    std::string print() const {return ctx;}

    template <typename T> Note& operator<<(const T& t)
    {
        std::ostringstream oss;
        oss<<ctx<<t;
        ctx = oss.str();
        return *this;
    }

    Note() = default;
};

struct TypeCheckerContext
{
//    yy::location source_span;
    std::vector< Note > notes;
//    std::vector< Hs::Var > binder_stack;
//    int level = 0;

    void pop_note();
    void push_note(const Note& e);
    std::string print_note() const;
};

enum MessageType { ErrorMsg, WarningMsg};

struct Message
{
    // Error or Warning
    MessageType message_type;

    // Should a message have a single location?
    std::optional<yy::location> loc;
    std::vector<Note> notes;

    std::string print(const FileContents& file) const;

    Message(MessageType t, std::optional<yy::location> l, const std::vector<Note>& e);
};

#endif
