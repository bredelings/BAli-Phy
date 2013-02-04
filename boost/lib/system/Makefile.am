noinst_LTLIBRARIES = libboost_system.la

AM_CPPFLAGS = -I$(top_srcdir)/boost/

libboost_system_la_SOURCES = src/error_code.cpp

if HOST_CYGWIN
CPPFLAGS += -DBOOST_POSIX
endif
