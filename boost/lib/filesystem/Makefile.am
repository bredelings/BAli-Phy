noinst_LIBRARIES = libboost_filesystem.a

INCLUDES = -I$(top_srcdir)/boost/

libboost_filesystem_a_SOURCES = v3/src/operations.cpp \
				v3/src/path.cpp \
				v3/src/path_traits.cpp \
				v3/src/portability.cpp \
				v3/src/unique_path.cpp \
				v3/src/utf8_codecvt_facet \
				v3/src/windows_file_codecvt \
				v3/src/codecvt_error_category

if HOST_CYGWIN
CPPFLAGS += -DBOOST_POSIX
endif
