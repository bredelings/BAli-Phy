#include "util/demangle.H"

using std::string;

#ifdef __GNUC__
#include <cxxabi.h>
string demangle(const string& s)
{
    int     status;
    char* result_str = abi::__cxa_demangle(s.c_str(), 0, 0, &status);

    string result = result_str;
    free(result_str);
    return result;
}
#else
string demangle(const string& s)
{
    return s;
}
#endif
