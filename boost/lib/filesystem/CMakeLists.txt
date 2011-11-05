if (HOST_CYGWIN)
  set(CPPFLAGS -DBOOST_POSIX ${CPPFLAGS})
endif()

add_library(boost_filesystem
  v3/src/operations.cpp
  v3/src/path.cpp
  v3/src/path_traits.cpp
  v3/src/portability.cpp
  v3/src/unique_path.cpp
  v3/src/utf8_codecvt_facet.cpp
  v3/src/windows_file_codecvt.cpp
  v3/src/codecvt_error_category.cpp
)
