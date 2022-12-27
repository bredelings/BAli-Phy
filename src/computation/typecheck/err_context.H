#ifndef TYPECHECKERCONTEXT
#define TYPECHECKERCONTEXT

#include <string>
#include <vector>
#include <sstream>

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

#endif
