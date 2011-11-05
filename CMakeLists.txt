cmake_minimum_required(VERSION 2.6)
project (BAli-Phy)

set(BAli-Phy_VERSION_MAJOR 2)
set(BAli-Phy_VERSION_MINOR 1)
set(BAli-Phy_VERSION_LAST 1)


include(CheckIncludeFile)
include(CheckFunctionExists)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckCXXSourceCompiles) 

# configure a header file to pass some of the CMake settings to the source code.
# (If we write the *.h.in version, cmake will substitute @variables@ in the *.h version)
#configure_file (
#  "${PROJECT_SOURCE_DIR}/bali-phy-Config.h.in"
#  "${PROJECT_BINARY_DIR}/bali-phy-Config.h"
#  )

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-sign-compare -Woverloaded-virtual -Wstrict-aliasing")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -g -pg -D_GLIBCXX_DEBUG")

# Add extra CMake libraries into ./CMake
#set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})

# Set source root relate to project file
set(PROJECT_SOURCE_DIR ${CMAKE_SOURCE_DIR})

include_directories(
  ${PROJECT_BINARY_DIR}
  ${PROJECT_SOURCE_DIR}/src
  ${PROJECT_SOURCE_DIR}/include
)

#AC_CHECK_FUNCS([floor pow sqrt strchr log2 getrlimit setrlimit])

check_function_exists (floor HAVE_FLOOR)
check_function_exists(pow HAVE_POW)
check_function_exists(sqrt HAVE_SQRT)
check_function_exists(strchr HAVE_STRCHR)
check_function_exists(log2 HAVE_LOG2)
check_function_exists(getrlimit HAVE_GETRLIMIT)
check_function_exists(setrlimit HAVE_SETRLIMIT)

#    add_dependencies(target dependency1 dependency2 ...)


string(TOLOWER ${CMAKE_HOST_SYSTEM_NAME} HOST_OS)

if (HOST_OS MATCHES linux)
  set(HOST_LINUX TRUE)
  CHECK_LIBRARY_EXISTS("rt"     "sem_init"   "" HAVE_LIBRT) 

elseif (HOST_OS MATCHES cygwin)
  set(HOST_CYGWIN TRUE)

endif()

option (USE_SYSTEM_BOOST "Use the Boost libraries installed on the system, instead of building our own." OFF) 

option (USE_MPI "Compile with MPI." OFF) 

if (USE_SYSTEM_BOOST)
  find_package(Boost 1.46.0)
  if (Boost_FOUND)
    include_directories(${Boost_INCLUDE_DIRS})
  endif()
else()
  include_directories( ${PROJECT_SOURCE_DIR}/boost/include )
  add_subdirectory(boost)
endif (USE_SYSTEM_BOOST)

add_subdirectory(src)


# install (FILES file.h DESTINATION include)
