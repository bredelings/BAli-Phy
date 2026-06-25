#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/byte-string.H"
#include "computation/machine/args.H"

using std::vector;

namespace
{
    int byte_to_word8(std::byte byte)
    {
        return std::to_integer<int>(byte);
    }

    // Convert the current Int-backed Word8 runtime value to a byte.  The check
    // catches any internal value that bypassed Data.Word's modulo constructor.
    std::byte word8_to_byte(const R::Exp& value, const char* function_name)
    {
        int byte = value.as_int();
        if (byte < 0 or byte > 255)
            throw myexception()<<function_name<<": Word8 runtime value "<<byte<<" is outside [0,255].";
        return static_cast<std::byte>(static_cast<unsigned char>(byte));
    }

    // Convert public Int indices to size_t only after validating their sign.
    // This avoids accidentally turning negative Haskell indices into huge sizes.
    std::size_t nonnegative_size(int value, const char* function_name, const char* label)
    {
        if (value < 0)
            throw myexception()<<function_name<<": negative "<<label<<".";
        return static_cast<std::size_t>(value);
    }
}

extern "C" R::Exp simple_function_empty(vector<R::Exp>& /*args*/)
{
    return ByteString();
}

extern "C" R::Exp simple_function_length(vector<R::Exp>& args)
{
    auto arg = get_arg(args);
    const auto& bytes = arg.as_<ByteString>();
    return static_cast<int>(bytes.size());
}

extern "C" R::Exp simple_function_index(vector<R::Exp>& args)
{
    auto arg = get_arg(args);
    const auto& bytes = arg.as_<ByteString>();
    auto index = nonnegative_size(get_arg(args).as_int(), "ByteString.index", "index");
    return byte_to_word8(bytes[index]);
}

// Pack a vector of abstract Word8 values into raw bytes.  Word8 is currently
// Int-backed, so this is the boundary where the invariant is checked.
extern "C" R::Exp simple_function_pack(vector<R::Exp>& args)
{
    auto arg = get_arg(args);
    const auto& values = arg.as_<R::RVector>();

    ByteString::storage_type bytes;
    bytes.reserve(values.size());
    for(const auto& value: values)
        bytes.push_back(word8_to_byte(value, "ByteString.pack"));

    return ByteString(std::move(bytes));
}

// Unpack bytes as Word8 runtime values.  The Haskell type stays abstract even
// though each value is represented as an Int for now.
extern "C" R::Exp simple_function_unpack(vector<R::Exp>& args)
{
    auto arg = get_arg(args);
    const auto& bytes = arg.as_<ByteString>();

    R::RVector values;
    values.reserve(bytes.size());
    for(std::size_t i = 0; i < bytes.size(); i++)
        values.push_back(byte_to_word8(bytes[i]));

    return values;
}

extern "C" R::Exp simple_function_take(vector<R::Exp>& args)
{
    auto length = nonnegative_size(get_arg(args).as_int(), "ByteString.take", "length");
    auto arg = get_arg(args);
    const auto& bytes = arg.as_<ByteString>();
    return bytes.slice(0, length);
}

extern "C" R::Exp simple_function_drop(vector<R::Exp>& args)
{
    auto offset = nonnegative_size(get_arg(args).as_int(), "ByteString.drop", "offset");
    auto arg = get_arg(args);
    const auto& bytes = arg.as_<ByteString>();
    return bytes.slice(offset, bytes.size() - offset);
}

// Append copies the visible slices into a new immutable backing vector.  Slices
// remain cheap, but concatenation has to allocate contiguous storage.
extern "C" R::Exp simple_function_append(vector<R::Exp>& args)
{
    auto arg1 = get_arg(args);
    const auto& bytes1 = arg1.as_<ByteString>();
    auto arg2 = get_arg(args);
    const auto& bytes2 = arg2.as_<ByteString>();

    ByteString::storage_type bytes;
    bytes.reserve(bytes1.size() + bytes2.size());
    for(std::size_t i = 0; i < bytes1.size(); i++)
        bytes.push_back(bytes1[i]);
    for(std::size_t i = 0; i < bytes2.size(); i++)
        bytes.push_back(bytes2[i]);

    return ByteString(std::move(bytes));
}

extern "C" R::Exp simple_function_equals(vector<R::Exp>& args)
{
    auto arg1 = get_arg(args);
    const auto& bytes1 = arg1.as_<ByteString>();
    auto arg2 = get_arg(args);
    const auto& bytes2 = arg2.as_<ByteString>();
    return bytes1 == bytes2;
}
