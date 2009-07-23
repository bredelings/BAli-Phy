//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2005-2008. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/interprocess for documentation.
//
//////////////////////////////////////////////////////////////////////////////

#include <boost/interprocess/sync/posix/ptime_to_timespec.hpp>
#include <boost/interprocess/detail/posix_time_types_wrk.hpp>

namespace boost {

namespace interprocess {

inline interprocess_semaphore::~interprocess_semaphore()
{}

inline interprocess_semaphore::interprocess_semaphore(int initialCount)
   :  m_sem(initialCount)
{}

inline void interprocess_semaphore::post()
{  m_sem.post();   }

inline void interprocess_semaphore::wait()
{  m_sem.wait();   }

inline bool interprocess_semaphore::try_wait()
{  return m_sem.try_wait();   }

inline bool interprocess_semaphore::timed_wait(const boost::posix_time::ptime &abs_time)
{  return m_sem.timed_wait(abs_time);   }
/*
inline int interprocess_semaphore::get_count() const
{  return m_sem.get_count();  }
*/
}  //namespace interprocess {

}  //namespace boost {

