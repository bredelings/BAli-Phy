#ifndef REFCOUNTED_H
#define REFCOUNTED_H

#include <boost/intrusive_ptr.hpp>

class ref_counted
{
    mutable int _refs = 0;

    friend inline void intrusive_ptr_release(const ref_counted* pThis)
	{
	    if(--pThis->_refs == 0 ) {
		delete pThis;
	    }
	}

    friend inline void intrusive_ptr_add_ref(const ref_counted* pThis)
	{
	    pThis->_refs++;
	}
public:

    int ref_count() const {return _refs;}
    void swap(ref_counted&) {}

    ref_counted& operator=(const ref_counted&) {return *this;}  // doesn't copy ref count
    ref_counted() = default;
    ref_counted(const ref_counted&) {}                          // doesn't copy ref count
    virtual ~ref_counted() = default;
};

// Pointer to ref-counted objects
template <typename T>
using ref_ptr = boost::intrusive_ptr<T>;

#endif
