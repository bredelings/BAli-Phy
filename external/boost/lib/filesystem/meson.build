filesystem_sources = ['src/operations.cpp',
                      'src/directory.cpp',
                      'src/exception.cpp',
                      'src/operations.cpp',
		      'src/path.cpp',
		      'src/path_traits.cpp',
		      'src/portability.cpp',
		      'src/unique_path.cpp',
		      'src/utf8_codecvt_facet.cpp',
		      'src/windows_file_codecvt.cpp',
		      'src/codecvt_error_category.cpp']

boost_libs += static_library('boost_filesystem', filesystem_sources,
			include_directories: boost_inc, cpp_args: ['-Wno-deprecated-declarations','-Wno-unused-parameter'])
