noinst_LTLIBRARIES = libboost_filesystem.la

AM_CPPFLAGS = -I$(top_srcdir)/boost/

libboost_filesystem_la_SOURCES = src/operations.cpp \
				src/path.cpp \
				src/path_traits.cpp \
				src/portability.cpp \
				src/unique_path.cpp \
				src/utf8_codecvt_facet.cpp \
				src/windows_file_codecvt.cpp \
				src/codecvt_error_category.cpp

if HOST_CYGWIN
CPPFLAGS += -DBOOST_POSIX
endif

#     lib_LTLIBRARIES = libtop.la
#     libtop_la_SOURCES =
#     # Dummy C++ source to cause C++ linking.
#     nodist_EXTRA_libtop_la_SOURCES = dummy.cxx
#     libtop_la_LIBADD = \
#       sub1/libsub1.la \
#       sub2/libsub2.la \
#
