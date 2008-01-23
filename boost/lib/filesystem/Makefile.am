noinst_LIBRARIES = libboost_filesystem.a

libboost_filesystem_a_SOURCES = convenience.cpp exception.cpp path_posix_windows.cpp \
                                operations_posix_windows.cpp 

if HOST_CYGWIN
CPPFLAGS += -DBOOST_POSIX
endif
