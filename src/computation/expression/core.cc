#include "core.H"

// Actually, we have multiple stages

// 1. Haskell
// 2. "core"?            -- this is what the optimizer works on
// 3. "final"/indexified -- this is runnable

namespace Core
{

var error()
{
    return var("Compiler.Error.error");    
}

var unpack_cpp_string()
{
    return var("Foreign.String.unpack_cpp_string");
}

var unsafePerformIO()
{
    return var("Compiler.IO.unsafePerformIO");
}

expression_ref unpack_cpp_string(const std::string& s)
{
    return { unpack_cpp_string(), String(s) };
}

expression_ref error(const std::string& s)
{
    return { error(), unpack_cpp_string(s) };
}

expression_ref unsafePerformIO(const expression_ref& x)
{
    return {unsafePerformIO(),x};
}

}
