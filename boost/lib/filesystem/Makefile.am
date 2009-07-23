noinst_LIBRARIES = libboost_filesystem.a

libboost_filesystem_a_SOURCES = operations.cpp path.cpp portability.cpp \
				utf8_codecvt_facet.cpp ../system/error_code.cpp

if HOST_CYGWIN
CPPFLAGS += -DBOOST_POSIX
endif
