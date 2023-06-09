#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"
#include <filesystem>
#include <fstream>
#include "util/io.H"   // for portable_getline( ).
#include "computation/haskell/Integer.H" // for Integer

namespace fs = std::filesystem;
using std::fstream;

typedef Box<std::shared_ptr<std::iostream>> Handle;

typedef Box<std::shared_ptr<std::fstream>> FHandle;

// FilePath -> Int -> RealWorld -> Handle
extern "C" closure builtin_function_openFileRaw(OperationArgs& Args)
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

    if (not *handle)
        throw myexception()<<"readFile: can't open file "<<filename;

    return handle;
}

// FilePath -> Int -> RealWorld -> Handle
extern "C" closure builtin_function_openBinaryFileRaw(OperationArgs& Args)
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

    Handle handle = std::make_shared<std::fstream>(filename, mode | fstream::binary);

    if (not *handle)
        throw myexception()<<"readFile: can't open file "<<filename;

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


// Handle -> RealWorld -> Bool
extern "C" closure builtin_function_hIsEOFRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    if (handle->eof())
        return bool_true;
    else
        return bool_false;

}

// Handle -> Char -> RealWorld -> ()
extern "C" closure builtin_function_hPutCharRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    char c = Args.evaluate(1).as_char();

    handle->put(c);

    return constructor("()",0);
}

// Handle -> Char -> RealWorld -> ()
extern "C" closure builtin_function_hPutStrRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    auto data = Args.evaluate(1).as_<String>();

    handle->write(data.c_str(), data.size());

    return constructor("()",0);
}

// Handle -> RealWorld -> Char
extern "C" closure builtin_function_hGetCharRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    char c = handle->get();

    return {c};
}

// Handle -> RealWorld -> CPPString
extern "C" closure builtin_function_hGetLineRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    object_ptr<String> result = new String;

    portable_getline(*handle, *result);

    return result;
}

// Handle -> RealWorld -> CPPString
extern "C" closure builtin_function_hGetContentsRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    handle->seekg(0, std::ios::end);
    auto length = handle->tellg();
    handle->seekg(0, std::ios::beg);

    object_ptr<String> contents = new String;
    contents->resize(length);
    handle->read(&(*contents)[0], length);

    return contents;
}

// Handle -> RealWorld -> Integer
extern "C" closure builtin_function_hFileSizeRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    auto old_pos = handle->tellg();
    handle->seekg(0, std::ios::end);
    unsigned long length = handle->tellg();
    handle->seekg(old_pos, std::ios::beg);

    return Integer(length);
}

// Handle -> RealWorld -> Integer
extern "C" closure builtin_function_hFlushRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    handle->flush();

    return constructor("()",0);
}

// Handle -> Int -> Integer -> RealWorld -> ()
extern "C" closure builtin_function_hSeekRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();
    int seekmode = Args.evaluate(1).as_int();
    long int pos = (long int)Args.evaluate(2).as_<Integer>();

    if (seekmode == 0)
    {
        handle->seekg(pos, std::ios::beg);
        handle->seekp(pos, std::ios::beg);
    }
    else if (seekmode == 1)
    {
        handle->seekg(pos, std::ios::cur);
        handle->seekp(pos, std::ios::cur);
    }
    else if (seekmode == 2)
    {
        handle->seekg(pos, std::ios::end);
        handle->seekp(pos, std::ios::end);
    }

    return constructor("()",0);
}

// Handle -> RealWorld -> Integer
extern "C" closure builtin_function_hTellRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    unsigned long pos_read = handle->tellg();
    unsigned long pos_write = handle->tellp();

    return Integer( std::max(pos_read, pos_write) );
}

// Handle -> RealWorld -> Bool
extern "C" closure builtin_function_hIsOpenRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    if (auto fhandle = std::dynamic_pointer_cast<std::fstream>(handle))
    {
        if (fhandle->is_open())
            return bool_true;
        else
            return bool_false;
    }
    else
        return bool_true;
}

extern "C" closure builtin_function_hLookAheadRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate(0).as_<Handle>();

    char c = handle->peek();

    return {c};
}
