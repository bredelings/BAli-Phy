noinst_LIBRARIES = libboost_system.a

INCLUDES = -I$(top_srcdir)/boost/

libboost_system_a_SOURCES = src/error_code.cpp

if HOST_CYGWIN
CPPFLAGS += -DBOOST_POSIX
endif
