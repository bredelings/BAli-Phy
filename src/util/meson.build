libutil_sources = [
  'assert.cc',
  'string/pred.cc',
  'string/sanitize.cc',
  'string/strip.cc',
  'string/split.cc',
  'log-level.cc',
  'io.cc',
  'graph.cc',
  'range.cc',
  'demangle.cc',
  'mapping.cc',
  'cmdline.cc',
  'rng.cc',
  'permute.cc',
  'file-paths.cc',
  'ptree.cc',
  'text.cc',
  'myexception.cc',
  'time.cc',
  'file-readers.cc']

libutil_inc = include_directories('include')

libutil = static_library('util', libutil_sources,
			 include_directories: libutil_inc,
			 dependencies: [boost, eigen, json, librange, fmt])

