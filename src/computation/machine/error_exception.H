#ifndef ERROR_EXCEPTION_H
#define ERROR_EXCEPTION_H
#include "util/myexception.H"

class error_exception: public myexception
{
    using myexception::myexception;
};
#endif
