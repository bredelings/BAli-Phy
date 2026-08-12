#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/machine/error_exception.H"
#include "computation/machine/requested_exit.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "util/rng.H"
#include "probability/choose.H"
#include "util/log-level.H"
#include <cstdlib> // for getenv

using namespace std;

extern "C" closure builtin_function_getArgsRaw(OperationArgs& Args)
{
    reg_heap& M = Args.memory();

    R::RVector V;
    for(const auto& arg: M.args)
        V.push_back(arg);

    return V;
}

extern "C" closure builtin_function_getEnvRaw(OperationArgs& Args)
{
    auto x = Args.evaluate_slot_to_value(0).as_string();
    const char* value = getenv(x.c_str());
    if (not value)
        throw error_exception("Environment variable '" + x + "' is not defined");

    return std::string(value);
}

extern "C" closure builtin_function_lookupEnvRaw(OperationArgs& Args)
{
    auto name = Args.evaluate_slot_to_value(0).as_string();
    const char* value = getenv(name.c_str());
    if (value)
        return R::RMaybe(std::string(value));
    else
        return R::RMaybe();
}

extern "C" closure builtin_function_getProgNameRaw(OperationArgs& Args)
{
    return Args.memory().prog_name;
}

extern "C" closure builtin_function_getVerbosity(OperationArgs&)
{
    return log_verbose;
}

// Decode ExitCode and unwind to the top-level runner; this must not terminate inside evaluation.
extern "C" closure builtin_function_exitWithRaw(OperationArgs& Args)
{
    const closure& exit_code = Args.evaluate_reg_to_closure(Args.reg_for_slot(0));
    auto constructor = exit_code.get_code().to<Runtime::ConstructorApp>();
    if (not constructor)
        throw myexception()<<"exitWithRaw: expected an ExitCode constructor";

    if (constructor->head.name() == "System.Exit.ExitSuccess" and constructor->head.n_args() == 0)
        throw requested_exit{0};
    if (constructor->head.name() == "System.Exit.ExitFailure" and constructor->head.n_args() == 1)
    {
        int status_reg = exit_code.reg_for_constructor_slot(0);
        int status = Args.evaluate_reg_to_closure(status_reg).get_code().as_int();
        if (status == 0)
            throw error_exception("exitWith: invalid argument (ExitFailure 0)");
        throw requested_exit{status};
    }

    throw myexception()<<"exitWithRaw: invalid ExitCode constructor '"<<constructor->head.print()<<"'";
}
