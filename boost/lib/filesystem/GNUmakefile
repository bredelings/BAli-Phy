CPPFLAGS = -I ../../include -D BOOST_POSIX

SOURCES = convenience.cpp exception.cpp operations_posix_windows.cpp \
	  path_posix_windows.cpp

libboost_filesystem.a: ${SOURCES:%.cpp=%.o}
	ar rc $@ ${SOURCES:%.cpp=%.o}
	ranlib $@

clean:
	rm -f *.o