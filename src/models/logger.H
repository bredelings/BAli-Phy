#ifndef LOGGER_H
#define LOGGER_H

#include <string>                                   // for string
#include <vector>                                   // for vector
#include "computation/haskell/haskell.H"            // for Hs::Exp
#include "computation/haskell/var.H"                // for Hs::Var
#include "util/owned-ptr.H"                         // for owned_ptr

namespace Haskell { struct Stmts; }

struct LoggerObj
{
    virtual std::string get_name() const = 0;
    virtual LoggerObj* clone() const = 0;
    virtual ~LoggerObj() {};
};

typedef owned_ptr<LoggerObj> Logger;

typedef std::vector<Logger> Loggers;

enum class LogValueKind
{
    parameter,
    context
};

struct LogValue: public LoggerObj
{
    std::string name;
    Hs::Exp value;
    LogValueKind kind;

    LogValue* clone() const {return new LogValue(*this);}

    std::string get_name() const {return name;}

    LogValue(const std::string& s, const Hs::Exp& e, LogValueKind k = LogValueKind::parameter)
        :name(s),value(e),kind(k)
    {}
};

struct LogSub: public LoggerObj
{
    std::string prefix;
    Hs::Var log_var; // we might emit logger_var = loggers, and then do prefix %>% logger_var
    Loggers loggers;

    LogSub* clone() const {return new LogSub(*this);}

    std::string get_name() const {return prefix+"/";}

    LogSub(const std::string& s, const Hs::Var& v, const Loggers& ls)
        :prefix(s), log_var(v), loggers(ls)
    {}
};

bool has_loggers(const Loggers& loggers, LogValueKind kind);
Hs::Exp generate_logger_values(Haskell::Stmts& code, const Loggers& loggers);

void simplify(Loggers& loggers);

#endif
