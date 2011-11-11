if (HOST_CYGWIN)
  set(CPPFLAGS -DBOOST_POSIX ${CPPFLAGS})
endif()  

add_library(boost_system
  src/error_code.cpp
)