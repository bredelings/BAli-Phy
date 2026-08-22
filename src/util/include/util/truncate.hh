#ifndef UTIL_TRUNCATE_H
#define UTIL_TRUNCATE_H

template<typename V>
void truncate(V& v)
{
    (&v)->~V();
    new (&v) V;
}

#endif
