#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"
#include <filesystem>
#include <fstream>

namespace fs = std::filesystem;
using std::fstream;

typedef Box<std::shared_ptr<std::iostream>> Handle;

typedef Box<std::shared_ptr<std::fstream>> FHandle;

// FilePath -> Int -> RealWorld -> Handle
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
    
    Handle handle = std::make_shared<std::fstream>(filename, mode);

    return handle;
}

extern "C" closure builtin_function_getStdin(OperationArgs& Args)
{
    Handle handle = std::make_shared<std::iostream>( std::cin.rdbuf() );

    return handle;
}


extern "C" closure builtin_function_getStdout(OperationArgs& Args)
{
    Handle handle = std::make_shared<std::iostream>( std::cout.rdbuf() );

    return handle;
}


extern "C" closure builtin_function_getStderr(OperationArgs& Args)
{
    Handle handle = std::make_shared<std::iostream>( std::cerr.rdbuf() );

    return handle;
}


// FilePath -> IOMode -> IO Handle
extern "C" closure builtin_function_hCloseRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    if (auto fhandle = std::dynamic_pointer_cast<std::fstream>(handle))
        fhandle->close();

    return constructor("()",0);
}

