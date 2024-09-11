#ifndef LOGGER_H
#define LOGGER_H

#include <string>                                   // for string
#include <vector>                                   // for vector
#include "computation/expression/var.H"             // for var
#include "computation/expression/expression_ref.H"  // for expression_ref
#include "util/owned-ptr.H"                         // for owned_ptr
#include "computation/expression/do_block.H"        // for do_block

struct LoggerObj
{
    virtual std::string get_name() const = 0;
    virtual LoggerObj* clone() const = 0;
    virtual ~LoggerObj() {};
};

typedef owned_ptr<LoggerObj> Logger;

typedef std::vector<Logger> Loggers;

struct LogValue: public LoggerObj
{
    std::string name;
    expression_ref value;

    LogValue* clone() const {return new LogValue(*this);}

    std::string get_name() const {return name;}

    LogValue(const std::string& s, const expression_ref& e)
        :name(s),value(e)
    {}
};

struct LogSub: public LoggerObj
{
    std::string prefix;
    var log_var; // we might emit logger_var = loggers, and then do prefix %>% logger_var
    Loggers loggers;

    LogSub* clone() const {return new LogSub(*this);}

    std::string get_name() const {return prefix+"/";}

    LogSub(const std::string& s, const var& v, const Loggers& ls)
        :prefix(s), log_var(v), loggers(ls)
    {}
};

expression_ref generate_loggers(do_block& code, const Loggers& Loggers);

void simplify(Loggers& loggers);

#endif
