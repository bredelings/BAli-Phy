#ifndef TYPECHECKERCONTEXT
#define TYPECHECKERCONTEXT

#include <string>
#include <vector>
#include <sstream>
#include "computation/parser/located.H"
#include "computation/module.H"

struct ErrorContext
{
    std::string ctx;

    void prepend(const std::string& s) {ctx = s + ctx;}

    std::string print() const {return ctx;}

    template <typename T> ErrorContext& operator<<(const T& t)
    {
        std::ostringstream oss;
        oss<<ctx<<t;
        ctx = oss.str();
        return *this;
    }

    ErrorContext() = default;
};

struct TypeCheckerContext
{
//    yy::location source_span;
    std::vector< ErrorContext > err_contexts;
//    std::vector< Hs::Var > binder_stack;
//    int level = 0;

    void pop_err_context();
    void push_err_context(const ErrorContext& e);
    std::string print_err_context() const;
};

enum MessageType { ErrorMsg, WarningMsg};

struct Message
{
    // Error or Warning
    MessageType message_type;

    // Should a message have a single location?
    std::optional<yy::location> loc;
    std::vector<ErrorContext> err_contexts;

    std::string print(const FileContents& file) const;

    Message(MessageType t, std::optional<yy::location> l, const std::vector<ErrorContext>& e);
};

#endif
