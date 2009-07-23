//  (C) Copyright Gennadiy Rozental 2004-2007.
//  Distributed under the Boost Software License, Version 1.0.
//  (See accompanying file LICENSE_1_0.txt or copy at 
//  http://www.boost.org/LICENSE_1_0.txt)

//  See http://www.boost.org/libs/test for the library home page.
//
//  File        : $RCSfile$
//
//  Version     : $Revision: 41369 $
//
//  Description : suppress some warnings 
// ***************************************************************************

#ifdef BOOST_MSVC
# pragma warning(push)
# pragma warning(disable: 4511) // copy constructor could not be generated
# pragma warning(disable: 4512) // assignment operator could not be generated
# pragma warning(disable: 4100) // unreferenced formal parameter 
# pragma warning(disable: 4996) // <symbol> was declared deprecated 
# pragma warning(disable: 4355) // 'this' : used in base member initializer list
# pragma warning(disable: 4706) // assignment within conditional expression
# pragma warning(disable: 4251) // class 'A<T>' needs to have dll-interface to be used by clients of class 'B'
# pragma warning(disable: 4127) // conditional expression is constant
# pragma warning(disable: 4290) // C++ exception specification ignored except to ...
# pragma warning(disable: 4180) // qualifier applied to function type has no meaning; ignored
#endif

