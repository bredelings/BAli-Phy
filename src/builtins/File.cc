#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include <filesystem>
#include <fstream>
#include "util/io.H"   // for portable_getline( ).
#include "util/utf8.H"
#include "computation/haskell/Integer.H" // for Integer
#include "bali-phy/files.H" // for create_unique_dir

namespace fs = std::filesystem;
using std::fstream;

typedef Box<std::shared_ptr<std::iostream>> Handle;

typedef Box<std::shared_ptr<std::fstream>> FHandle;

struct utf8_read_result
{
    char32_t code_point;
    std::string bytes;
};

// Read one UTF-8 scalar from a stream.  Invalid or incomplete byte sequences
// are rejected at the IO boundary, and callers can inspect the consumed bytes.
static utf8_read_result read_utf8_char(std::iostream& handle)
{
    int first = handle.get();
    if (first == std::streambuf::traits_type::eof())
        throw myexception()<<"hGetChar: end of file.";

    auto byte0 = static_cast<unsigned char>(first);
    int length = 0;
    if (byte0 <= 0x7F)
        length = 1;
    else if (0xC2 <= byte0 and byte0 <= 0xDF)
        length = 2;
    else if (0xE0 <= byte0 and byte0 <= 0xEF)
        length = 3;
    else if (0xF0 <= byte0 and byte0 <= 0xF4)
        length = 4;
    else
        throw myexception()<<"hGetChar: invalid UTF-8 leading byte.";

    std::string bytes;
    bytes.push_back(static_cast<char>(byte0));
    for(int i=1; i<length; i++)
    {
        int next = handle.get();
        if (next == std::streambuf::traits_type::eof())
            throw myexception()<<"hGetChar: incomplete UTF-8 sequence.";
        bytes.push_back(static_cast<char>(static_cast<unsigned char>(next)));
    }

    auto decoded = utf8::decode_next(bytes, 0);
    if (not decoded or decoded->next_byte != bytes.size())
        throw myexception()<<"hGetChar: invalid UTF-8 sequence.";
    return {decoded->code_point, bytes};
}

// FilePath -> Int -> RealWorld -> Handle
extern "C" closure builtin_function_openFileRaw(OperationArgs& Args)
{
    fs::path filename = Args.evaluate_slot_to_value(0).as_string();
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
    fs::path filename = Args.evaluate_slot_to_value(0).as_string();
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

    return closure(R::ConstructorApp("()", 0, {}));
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

    auto c = Args.evaluate_slot_to_value(1).as_char();

    auto bytes = utf8::encode(c);
    handle->write(bytes.data(), bytes.size());

    return closure(R::ConstructorApp("()", 0, {}));
}

// Handle -> Char -> RealWorld -> ()
extern "C" closure builtin_function_hPutStrRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    auto data = Args.evaluate_slot_to_value(1).as_string();

    handle->write(data.c_str(), data.size());

    return closure(R::ConstructorApp("()", 0, {}));
}

// Handle -> RealWorld -> Char
extern "C" closure builtin_function_hGetChar(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    return read_utf8_char(*handle).code_point;
}

// Handle -> RealWorld -> CPPString
extern "C" closure builtin_function_hGetLineRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    std::string result;

    portable_getline(*handle, result);

    return result;
}

// Handle -> RealWorld -> CPPString
extern "C" closure builtin_function_hGetContentsRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    return std::string(std::istreambuf_iterator<char>(*handle), std::istreambuf_iterator<char>());
}

// Handle -> RealWorld -> Integer
extern "C" closure builtin_function_hFileSize(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    auto old_pos = handle->tellg();
    handle->seekg(0, std::ios::end);
    unsigned long length = handle->tellg();
    handle->seekg(old_pos, std::ios::beg);

    return integer(length);
}

// Handle -> RealWorld -> Integer
extern "C" closure builtin_function_hFlush(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    handle->flush();

    return closure(R::ConstructorApp("()", 0, {}));
}

// Handle -> Int -> Integer -> RealWorld -> ()
extern "C" closure builtin_function_hSeekRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();
    int seekmode = Args.evaluate_slot_to_value(1).as_int();
    long int pos = (long int)Args.evaluate_slot_to_value(2).as_integer();

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

    return closure(R::ConstructorApp("()", 0, {}));
}

// Handle -> RealWorld -> Integer
extern "C" closure builtin_function_hTellRaw(OperationArgs& Args)
{
    auto handle = Args.evaluate_slot_to_value(0).as_<Handle>();

    unsigned long pos_read = handle->tellg();
    unsigned long pos_write = handle->tellp();

    return integer( std::max(pos_read, pos_write) );
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

    auto result = read_utf8_char(*handle);
    for(auto i = result.bytes.rbegin(); i != result.bytes.rend(); ++i)
    {
        if (not handle->putback(*i))
            throw myexception()<<"hLookAhead: could not restore stream after lookahead.";
    }

    return result.code_point;
}

extern "C" closure builtin_function_combine(OperationArgs& Args)
{
    fs::path path1 = Args.evaluate_slot_to_value(0).as_string();
    fs::path path2 = Args.evaluate_slot_to_value(1).as_string();

    auto path3 = path1 / path2;

    return path3.string();
}

// FilePath -> IO ()
extern "C" closure builtin_function_createDirectoryRaw(OperationArgs& Args)
{
    fs::path dirname = Args.evaluate_slot_to_value(0).as_string();

    std::error_code ec;
    if (not fs::create_directory(dirname))
        throw myexception()<<"createDirectory: directory "<<dirname<<" already exists.";

    return closure(R::ConstructorApp("()", 0, {}));
}

// FilePath -> IO FilePath
extern "C" closure builtin_function_createUniqueDirectoryRaw(OperationArgs& Args)
{
    fs::path prefix = Args.evaluate_slot_to_value(0).as_string();

    auto dir_path = create_unique_dir(prefix);

    return dir_path.string();
}

// IO FilePath
extern "C" closure builtin_function_getCurrentDirectoryRaw(OperationArgs& /*Args*/)
{
    fs::path cwd = fs::current_path();

    return cwd.string();
}

// FilePath -> IO ()
extern "C" closure builtin_function_setCurrentDirectory(OperationArgs& Args)
{
    fs::path dirname = Args.evaluate_slot_to_value(0).as_string();

    fs::current_path(dirname);

    return closure(R::ConstructorApp("()", 0, {}));
}

// FilePath -> FilePath -> IO ()
extern "C" closure builtin_function_copyFileRaw(OperationArgs& Args)
{
    fs::path from_path = Args.evaluate_slot_to_value(0).as_string();
    fs::path to_path = Args.evaluate_slot_to_value(1).as_string();

    fs::copy(from_path, to_path);

    return closure(R::ConstructorApp("()", 0, {}));
}

// FilePath -> FilePath -> IO ()
extern "C" closure builtin_function_removeFileRaw(OperationArgs& Args)
{
    fs::path path = Args.evaluate_slot_to_value(0).as_string();

    if (fs::exists(path) and fs::is_directory(path))
        throw myexception()<<"removeFile: can't remove "<<path<<" because it is a directory";

    fs::remove(path);

    return closure(R::ConstructorApp("()", 0, {}));
}

// FilePath -> FilePath -> IO ()
extern "C" closure builtin_function_renameFileRaw(OperationArgs& Args)
{
    fs::path from_path = Args.evaluate_slot_to_value(0).as_string();
    fs::path to_path = Args.evaluate_slot_to_value(1).as_string();

    if (fs::exists(from_path) and fs::is_directory(from_path))
        throw myexception()<<"renameFile: can't rename "<<from_path<<" because it is a directory";

    fs::rename(from_path, to_path);

    return closure(R::ConstructorApp("()", 0, {}));
}

// FilePath -> FilePath -> IO ()
extern "C" closure builtin_function_renamePathRaw(OperationArgs& Args)
{
    fs::path from_path = Args.evaluate_slot_to_value(0).as_string();
    fs::path to_path = Args.evaluate_slot_to_value(1).as_string();

    fs::rename(from_path, to_path);

    return closure(R::ConstructorApp("()", 0, {}));
}

// FilePath -> FilePath
extern "C" closure builtin_function_takeFileNameRaw(OperationArgs& Args)
{
    fs::path pathname = Args.evaluate_slot_to_value(0).as_string();

    return pathname.filename().string();
}
