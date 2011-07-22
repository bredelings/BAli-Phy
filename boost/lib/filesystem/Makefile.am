noinst_LTLIBRARIES = libboost_filesystem.la

INCLUDES = -I$(top_srcdir)/boost/

libboost_filesystem_la_SOURCES = v3/src/operations.cpp \
				v3/src/path.cpp \
				v3/src/path_traits.cpp \
				v3/src/portability.cpp \
				v3/src/unique_path.cpp \
				v3/src/utf8_codecvt_facet.cpp \
				v3/src/windows_file_codecvt.cpp \
				v3/src/codecvt_error_category.cpp

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