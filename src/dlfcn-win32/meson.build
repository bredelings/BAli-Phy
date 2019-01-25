libdl_inc = include_directories('.', is_system: true)

dlfcn_lib = static_library('libdl',
			  'dlfcn.c',
			  include_directories: libdl_inc)

libdl = declare_dependency(include_directories: libdl_inc, link_with: dlfcn_lib)

