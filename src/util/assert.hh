#define BOOST_ENABLE_ASSERT_HANDLER
#include <boost/assert.hpp>
#include "../myexception.H"
#ifdef assert
#undef assert
#endif
#ifdef NDEBUG
#define assert(expr)  (0?(void(expr)):(void(0)))
#else
#define assert(expr)  BOOST_ASSERT(expr)
#endif

#ifndef UTIL_ASSERT_H
#define UTIL_ASSERT_H
namespace boost
{
    inline void assertion_failed(char const * expr, char const * function, char const * file, long line)
    {
	throw myexception()<<"Assertion ("<<expr<<") failed at line "<<line<<" of '"<<function<<"' ('"<<file<<"')";
    }

    inline void assertion_failed_msg(char const * expr, char const * msg, char const * function, char const * file, long line)
    {
	throw myexception()<<"Assertion ("<<expr<<") failed at line "<<line<<" of '"<<function<<"' ('"<<file<<"'):\n  "<<msg;
    }
}
#endif
