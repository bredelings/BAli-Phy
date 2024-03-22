#include "util/assert.hh"

#include "util/myexception.H"

namespace boost
{
    void assertion_failed(char const * expr, char const * function, char const * file, long line)
    {
	throw myexception()<<"Assertion ("<<expr<<") failed in '"<<function<<"' at "<<file<<":"<<line;
    }

    void assertion_failed_msg(char const * expr, char const * msg, char const * function, char const * file, long line)
    {
	throw myexception()<<"Assertion ("<<expr<<") failed in '"<<function<<"' at "<<file<<":"<<line<<":\n   "<<msg;
    }
}
