#define BOOST_ENABLE_ASSERT_HANDLER
#include <boost/assert.hpp>
#include "util/myexception.H"
#ifdef assert
#undef assert
#endif
#ifdef assert_msg
#undef assert_msg
#endif
#ifdef NDEBUG
#define assert(expr)          (0?(void(expr)):(void(0)))
#define assert_msg(expr,message)  (0?(void(expr)):(void(0)))
#else
#define assert(expr)  BOOST_ASSERT(expr)
#define assert_msg(expr,message)  BOOST_ASSERT_MSG(expr,message)
#endif

#ifndef UTIL_ASSERT_H
#define UTIL_ASSERT_H
namespace boost
{
    inline void assertion_failed(char const * expr, char const * function, char const * file, long line)
    {
	throw myexception()<<"Assertion ("<<expr<<") failed in '"<<function<<"' at "<<file<<":"<<line;
    }

    inline void assertion_failed_msg(char const * expr, char const * msg, char const * function, char const * file, long line)
    {
	throw myexception()<<"Assertion ("<<expr<<") failed in '"<<function<<"' at "<<file<<":"<<line<<":\n   "<<msg;
    }
}
#endif
