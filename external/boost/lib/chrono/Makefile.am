noinst_LTLIBRARIES = libboost_chrono.la

AM_CPPFLAGS = -I$(top_srcdir)/boost/

libboost_chrono_la_SOURCES = src/chrono.cpp \
                            src/thread_clock.cpp \
                            src/process_cpu_clocks.cpp
libboost_chrono_la_LIBADD = 
if HOST_LINUX
libboost_chrono_la_LIBADD += -lrt
endif
