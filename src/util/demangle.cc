#include "util/demangle.H"

#include <cstdlib>
#include <memory>
#include <string>

#ifdef __GNUC__
#include <cxxabi.h>
#endif

using std::string;

#ifdef __GNUC__
string demangle(const string& s)
{
    int status = 0;

    char* raw = abi::__cxa_demangle(s.c_str(), nullptr, nullptr, &status);

    std::unique_ptr<char, decltype(&std::free)> result_str(raw, &std::free);

    if (status == 0 && result_str)
        return string(result_str.get());

    return s;
}
#else
string demangle(const string& s)
{
    return s;
}
#endif
