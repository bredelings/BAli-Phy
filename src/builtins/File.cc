#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include <filesystem>
#include <fstream>
#include "util/io.H"   // for portable_getline( ).
#include "computation/haskell/Integer.H" // for Integer
#include "bali-phy/files.H" // for create_unique_dir

namespace fs = std::filesystem;
using std::fstream;

typedef Box<std::shared_ptr<std::iostream>> Handle;

typedef Box<std::shared_ptr<std::fstream>> FHandle;

namespace
{
    std::string file_string(const Runtime::Exp& E)
    {
        if (E.to<Runtime::String>())
            return E.as_string();
        else
            return E.as_<String>().value();
    }

    integer file_integer(const Runtime::Exp& E)
    {
        if (E.to<Runtime::Integer>())
            return E.as_integer();
        else
            return E.as_<Integer>();
    }
}

// FilePath -> Int -> RealWorld -> Handle
extern "C" closure builtin_function_openFileRaw(OperationArgs& Args)
{
    fs::path filename = file_string(Args.evaluate_slot_to_value(0));
    int io_mode = Args.evaluate_slot_to_value(1).as_int();

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
    fs::path filename = file_string(Args.evaluate_slot_to_value(0));
    int io_mode = Args.evaluate_slot_to_value(1).as_int();

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

extern "C" closure builtin_function_stdin(OperationArgs& /*Args*/)
{
    Handle handle = std::make_shared<std::iostream>( std::cin.rdbuf() );

    return handle;
}


extern "C" closure builtin_function_stdout(OperationArgs& /*Args*/)
{
    Handle handle = std::make_shared<std::iostream>( std::cout.rdbuf() );

    return handle;
}


extern "C" closure builtin_function_stderr(OperationArgs& /*Args*/)
{
    Handle handle = std::make_shared<std::iostream>( std::cerr.rdbuf() );

    return handle;
}


// FilePath -> IOMode -> IO Handle
extern "C" closure builtin_function_hClose(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    if (auto fhandle = std::dynamic_pointer_cast<std::fstream>(handle))
        fhandle->close();

    return closure(R::Constructor("()", 0));
}


// Handle -> RealWorld -> Bool
extern "C" closure builtin_function_hIsEOF(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    if (handle->eof())
        return true;

    std::streambuf* sb = handle->rdbuf();
    int c = sb->sgetc();
    if (c == std::streambuf::traits_type::eof())
        return true;

    return false;
}

// Handle -> Char -> RealWorld -> ()
extern "C" closure builtin_function_hPutChar(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    char c = Args.evaluate_slot_to_value(1).as_char();

    handle->put(c);

    return closure(R::Constructor("()", 0));
}

// Handle -> Char -> RealWorld -> ()
extern "C" closure builtin_function_hPutStrRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    auto data = file_string(Args.evaluate_slot_to_value(1));

    handle->write(data.c_str(), data.size());

    return closure(R::Constructor("()", 0));
}

// Handle -> RealWorld -> Char
extern "C" closure builtin_function_hGetChar(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    char c = handle->get();

    return {c};
}

// Handle -> RealWorld -> CPPString
extern "C" closure builtin_function_hGetLineRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    object_ptr<String> result = new String;

    portable_getline(*handle, *result);

    return result;
}

// Handle -> RealWorld -> CPPString
extern "C" closure builtin_function_hGetContentsRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    object_ptr<String> contents = new String(std::istreambuf_iterator<char>(*handle), std::istreambuf_iterator<char>());

    return contents;
}

// Handle -> RealWorld -> Integer
extern "C" closure builtin_function_hFileSize(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    auto old_pos = handle->tellg();
    handle->seekg(0, std::ios::end);
    unsigned long length = handle->tellg();
    handle->seekg(old_pos, std::ios::beg);

    return Integer(length);
}

// Handle -> RealWorld -> Integer
extern "C" closure builtin_function_hFlush(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    handle->flush();

    return closure(R::Constructor("()", 0));
}

// Handle -> Int -> Integer -> RealWorld -> ()
extern "C" closure builtin_function_hSeekRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();
    int seekmode = Args.evaluate_slot_to_value(1).as_int();
    long int pos = (long int)file_integer(Args.evaluate_slot_to_value(2));

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

    return closure(R::Constructor("()", 0));
}

// Handle -> RealWorld -> Integer
extern "C" closure builtin_function_hTellRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    unsigned long pos_read = handle->tellg();
    unsigned long pos_write = handle->tellp();

    return Integer( std::max(pos_read, pos_write) );
}

// Handle -> RealWorld -> Bool
extern "C" closure builtin_function_hIsOpen(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    if (auto fhandle = std::dynamic_pointer_cast<std::fstream>(handle))
    {
        if (fhandle->is_open())
            return true;
        else
            return false;
    }
    else
        return true;
}

extern "C" closure builtin_function_hLookAhead(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    char c = handle->peek();

    return {c};
}

extern "C" closure builtin_function_combine(OperationArgs& Args)
{
    fs::path path1 = file_string(Args.evaluate_slot_to_value(0));
    fs::path path2 = file_string(Args.evaluate_slot_to_value(1));

    auto path3 = path1 / path2;

    return String( path3.string() );
}

// FilePath -> IO ()
extern "C" closure builtin_function_createDirectoryRaw(OperationArgs& Args)
{
    fs::path dirname = file_string(Args.evaluate_slot_to_value(0));

    std::error_code ec;
    if (not fs::create_directory(dirname))
        throw myexception()<<"createDirectory: directory "<<dirname<<" already exists.";

    return closure(R::Constructor("()", 0));
}

// FilePath -> IO FilePath
extern "C" closure builtin_function_createUniqueDirectoryRaw(OperationArgs& Args)
{
    fs::path prefix = file_string(Args.evaluate_slot_to_value(0));

    auto dir_path = create_unique_dir(prefix);

    return String(dir_path.string());
}

// IO FilePath
extern "C" closure builtin_function_getCurrentDirectoryRaw(OperationArgs& /*Args*/)
{
    fs::path cwd = fs::current_path();

    return String(cwd.string());
}

// FilePath -> IO ()
extern "C" closure builtin_function_setCurrentDirectory(OperationArgs& Args)
{
    fs::path dirname = file_string(Args.evaluate_slot_to_value(0));

    fs::current_path(dirname);

    return closure(R::Constructor("()", 0));
}

// FilePath -> FilePath -> IO ()
extern "C" closure builtin_function_copyFileRaw(OperationArgs& Args)
{
    fs::path from_path = file_string(Args.evaluate_slot_to_value(0));
    fs::path to_path = file_string(Args.evaluate_slot_to_value(1));

    fs::copy(from_path, to_path);

    return closure(R::Constructor("()", 0));
}

// FilePath -> FilePath -> IO ()
extern "C" closure builtin_function_removeFileRaw(OperationArgs& Args)
{
    fs::path path = file_string(Args.evaluate_slot_to_value(0));

    if (fs::exists(path) and fs::is_directory(path))
        throw myexception()<<"removeFile: can't remove "<<path<<" because it is a directory";

    fs::remove(path);

    return closure(R::Constructor("()", 0));
}

// FilePath -> FilePath -> IO ()
extern "C" closure builtin_function_renameFileRaw(OperationArgs& Args)
{
    fs::path from_path = file_string(Args.evaluate_slot_to_value(0));
    fs::path to_path = file_string(Args.evaluate_slot_to_value(1));

    if (fs::exists(from_path) and fs::is_directory(from_path))
        throw myexception()<<"renameFile: can't rename "<<from_path<<" because it is a directory";

    fs::rename(from_path, to_path);

    return closure(R::Constructor("()", 0));
}

// FilePath -> FilePath -> IO ()
extern "C" closure builtin_function_renamePathRaw(OperationArgs& Args)
{
    fs::path from_path = file_string(Args.evaluate_slot_to_value(0));
    fs::path to_path = file_string(Args.evaluate_slot_to_value(1));

    fs::rename(from_path, to_path);

    return closure(R::Constructor("()", 0));
}

// FilePath -> FilePath
extern "C" closure builtin_function_takeFileNameRaw(OperationArgs& Args)
{
    fs::path pathname = file_string(Args.evaluate_slot_to_value(0));

    return String(pathname.filename().string());
}
