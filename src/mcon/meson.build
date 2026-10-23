libmcon_sources = [
  'mcon.cc',
  'numeric-table.cc']

libmcon_inc = include_directories('include')

libmcon = static_library('mcon', libmcon_sources,
			 include_directories: libmcon_inc,
			 dependencies: [boost])

