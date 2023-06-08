#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"
#include <filesystem>
#include <fstream>

namespace fs = std::filesystem;
using std::fstream;

// FilePath -> IOMode -> IO Handle
extern "C" closure builtin_function_rawOpenFile(OperationArgs& Args)
{
    fs::path filename = Args.evaluate(0).as_<String>().value();
    int io_mode = Args.evaluate(1).as_int();
    std::ios_base::openmode mode;
    if (io_mode == 0)
        mode = fstream::in;
    else if (io_mode == 1)
        mode = fstream::out;
    else if (io_mode == 2)
        mode = fstream::out | fstream::app;
    else if (io_mode == 3)
        mode = fstream::in | fstream::out;
    else
        std::abort();
    
    PtrBox<std::iostream> handle = std::make_shared<std::fstream>(filename, mode);

    return handle;
}

