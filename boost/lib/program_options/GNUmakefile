CPPFLAGS = -I ../../include


SOURCES = cmdline.cpp config_file.cpp convert.cpp options_description.cpp \
	  parsers.cpp positional_options.cpp value_semantic.cpp \
	  variables_map.cpp utf8_codecvt_facet.cpp

libboost_program_options.a: ${SOURCES:%.cpp=%.o}
	ar rc $@ ${SOURCES:%.cpp=%.o}
	ranlib $@

clean:
	rm -f *.o